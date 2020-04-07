/*
 * Copyright (c) 2017-2020 Boucher, Antoni <bouanto@zoho.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::mem;
use std::rc::Rc;

use ast::{
    Declaration,
    DeclarationWithPos,
    Expr,
    ExprWithPos,
    FieldWithPos,
    FuncDeclaration,
    Operator,
    RecordFieldWithPos,
    Ty,
    TypeDec,
    TypeDecWithPos,
    TyWithPos,
};
use env::{Env, Entry};
use error::{Error, Result};
use frame::{Fragment, Frame, Memory};
use gen;
use gen::{
    Gen,
    Level,
    array_subscript,
    binary_oper,
    class_create,
    field_access,
    function_call,
    goto,
    if_expression,
    init_array,
    method_call,
    num,
    record_create,
    relational_oper,
    simple_var,
    string_equality,
    unit,
    var_dec,
    var_decs,
    while_loop,
};
use ir::{Exp, Statement, _Statement};
use position::{Pos, WithPos};
use self::AddError::*;
use symbol::{Strings, Symbol, SymbolWithPos};
use temp::{Label, TempMap};
use types::{
    ClassField,
    ClassMethod,
    FunctionType,
    Type,
    Unique,
};

// Offset 2, because offset 0 is the object type (class) and offset 1 is the data layout.
pub const VTABLE_OFFSET: usize = 2;

#[derive(PartialEq)]
enum AddError {
    AddError,
    DontAddError,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExpTy {
    pub exp: Exp,
    pub ty: Type,
}

pub enum FieldType {
    Class,
    Record,
}

const EXP_TYPE_ERROR: ExpTy =
    ExpTy {
        exp: Exp::Error,
        ty: Type::Error,
    };

pub struct SemanticAnalyzer<'a, F: Clone + Frame + 'a> {
    env: &'a mut Env<F>,
    errors: Vec<Error>,
    escaping_vars: Vec<i64>,
    gen: Gen<F>,
    in_loop: bool,
    methods_level: HashMap<(Symbol, Symbol), Level<F>>,
    self_symbol: Symbol,
    strings: Rc<Strings>,
    temp_map: TempMap,
}

impl<'a, F: Clone + Debug + Frame + PartialEq> SemanticAnalyzer<'a, F> {
    pub fn new(env: &'a mut Env<F>, strings: Rc<Strings>, self_symbol: Symbol, object_symbol: Symbol) -> Self {
        let object_class = Type::Class {
            data_layout: String::new(),
            fields: vec![],
            methods: vec![],
            name: object_symbol,
            parent_class: None,
            unique: Unique::new(),
            vtable_name: Label::with_name("__vtable_Object"),
        };
        env.enter_type(object_symbol, object_class);
        SemanticAnalyzer {
            env,
            errors: vec![],
            escaping_vars: vec![],
            gen: Gen::new(),
            in_loop: false,
            methods_level: HashMap::new(),
            self_symbol,
            strings,
            temp_map: TempMap::new(),
        }
    }

    fn add_error(&mut self, error: Error) {
        self.errors.push(error);
    }

    pub fn analyze(mut self, main_symbol: Symbol, expr: ExprWithPos) -> Result<Vec<Fragment<F>>> {
        let pos = expr.pos;
        let body = WithPos::new(
            Expr::Sequence(vec![expr, WithPos::new(Expr::Int { value: 0 }, pos)]),
            pos,
        );
        let result = Some(WithPos::new(self.env.type_symbol("int"), pos));
        self.trans_dec(&WithPos::new(Declaration::Function(vec![
            WithPos::new(FuncDeclaration {
                body,
                name: WithPos::dummy(main_symbol),
                params: vec![],
                result,
            }, pos)
        ]), pos), &gen::outermost(), None);
        if self.errors.is_empty() {
            Ok(self.gen.get_result())
        }
        else {
            Err(Error::Multi(self.errors))
        }
    }

    fn actual_ty(&mut self, typ: &Type) -> Type {
        match *typ {
            Type::Name(_, Some(ref typ)) => *typ.clone(),
            Type::Name(ref symbol, None) => self.get_type(symbol, DontAddError),
            ref typ => typ.clone(),
        }
    }

    fn check_binary_op(&mut self, oper: Operator, left: &ExprWithPos, right: &ExprWithPos, level: &Level<F>,
                       done_label: Option<Label>) -> ExpTy
    {
        let left_pos = left.pos;
        let left = self.trans_exp(left, level, done_label.clone(), true);
        self.check_int(&left, left_pos);
        let right_pos = right.pos;
        let right = self.trans_exp(right, level, done_label, true);
        self.check_int(&right, right_pos);
        ExpTy {
            exp: binary_oper(oper, left.exp, right.exp),
            ty: Type::Int,
        }
    }

    fn check_duplicate_types(&mut self, types: &[TypeDecWithPos]) {
        let mut names = HashSet::new();
        for typ in types {
            names.insert(typ.node.name.node);
            if let Ty::Name { ref ident } = typ.node.ty.node {
                if names.contains(&ident.node) {
                    self.add_error(Error::Cycle {
                        pos: typ.node.ty.pos,
                    });
                    return;
                }
            }
        }
    }

    fn check_int(&mut self, expr: &ExpTy, pos: Pos) {
        if expr.ty != Type::Int && expr.ty != Type::Error {
            return self.add_error(Error::Type {
                expected: Type::Int,
                pos,
                unexpected: expr.ty.clone(),
            });
        }
    }

    fn check_function_types(&mut self, expected: &FunctionType, unexpected: &FunctionType, pos: Pos) {
        if expected != unexpected {
            return self.add_error(Error::FunctionType {
                expected: expected.clone(),
                pos,
                unexpected: unexpected.clone(),
            });
        }
    }

    fn check_types(&mut self, expected: &Type, unexpected: &Type, pos: Pos) {
        let expected = self.actual_ty(expected);
        let unexpected = self.actual_ty(unexpected);
        if expected != unexpected && expected != Type::Error && unexpected != Type::Error {
            if let Type::Class { .. } | Type::Record { .. } = expected {
                if unexpected == Type::Nil {
                    return;
                }
            }

            if let Type::Class { name: parent_class, .. } = expected {
                if let Type::Class { name, .. } = unexpected {
                    let mut current_class = name;
                    loop {
                        if current_class == parent_class {
                            // NOTE: the expected type is a parent class.
                            return;
                        }
                        let class = self.get_type(&WithPos::dummy(current_class), DontAddError);
                        if let Type::Class { parent_class, .. } = class {
                            if let Some(parent_class) = parent_class {
                                current_class = parent_class.node;
                            }
                            else {
                                break;
                            }
                        }
                    }
                }
            }

            return self.add_error(Error::Type {
                expected: expected.clone(),
                pos,
                unexpected: unexpected.clone(),
            });
        }
    }

    fn get_type(&mut self, symbol: &SymbolWithPos, add: AddError) -> Type {
        if let Some(typ) = self.env.look_type(symbol.node) {
            return typ.clone();
        }
        if add == AddError {
            self.undefined_type(symbol)
        }
        else {
            Type::Error
        }
    }

    fn trans_dec(&mut self, declaration: &DeclarationWithPos, parent_level: &Level<F>, done_label: Option<Label>)
        -> Option<Statement>
    {
        match declaration.node {
            Declaration::ClassDeclaration { ref declarations, ref name, ref parent_class } => {
                struct Method<F> {
                    body: ExprWithPos,
                    level: Level<F>,
                    param_names: Vec<Symbol>,
                    param_types: Vec<Type>,
                    return_type: Type,
                }

                let empty_class_type = Type::Class {
                    data_layout: String::new(),
                    fields: vec![],
                    methods: vec![],
                    name: name.node,
                    parent_class: Some(parent_class.clone()),
                    unique: Unique::new(),
                    vtable_name: Label::new(),
                };
                self.env.enter_type(name.node, empty_class_type);

                let old_escaping_vars = mem::replace(&mut self.escaping_vars, vec![]);
                let old_temp_map = mem::replace(&mut self.temp_map, TempMap::new());

                let mut pending_methods = vec![];
                let parent_type = self.get_type(parent_class, AddError);
                match parent_type {
                    Type::Class { .. } | Type::Error => (),
                    _ => {
                        self.add_error(Error::NotAClass {
                            pos: parent_class.pos,
                            typ: parent_type,
                        });
                    },
                }
                let (mut fields, mut data_layout, parent_methods) = self.parent_members(parent_class);
                let mut methods = vec![];
                for declaration in declarations {
                    match declaration.node {
                        Declaration::Function(ref functions) => {
                            for function in functions {
                                let params = &function.node.params;
                                let mut param_names = vec![];
                                let mut param_types = vec![];
                                let mut param_set = HashSet::new();
                                for param in params {
                                    param_types.push(self.get_type(&param.node.typ, AddError));
                                    param_names.push(param.node.name);
                                    if !param_set.insert(param.node.name) {
                                        self.duplicate_param(&param);
                                    }
                                }
                                let return_type =
                                    if let Some(ref result) = function.node.result {
                                        self.get_type(result, AddError)
                                    }
                                    else {
                                        Type::Unit
                                    };

                                let mut formals: Vec<_> = params.iter()
                                    .map(|param| self.env.look_escape(param.node.name))
                                    .collect();
                                formals.insert(0, true); // NOTE: self implicit parameter.
                                let func_name = function.node.name.node;
                                let label = self.method_label(name.node, func_name);
                                let level = Level::new(parent_level, label.clone(), formals);
                                self.methods_level.insert((name.node, func_name), level.clone());
                                methods.push(ClassMethod {
                                    class_name: name.node,
                                    label,
                                    name: function.node.name.clone(),
                                    typ: FunctionType {
                                        param_types: param_types.clone(),
                                        return_type: return_type.clone(),
                                    },
                                });

                                pending_methods.push(Method {
                                    body: function.node.body.clone(),
                                    level,
                                    param_names,
                                    param_types,
                                    return_type,
                                });
                            }
                        },
                        Declaration::VariableDeclaration { ref init, name, ref typ, .. } => {
                            let exp = self.trans_exp(init, parent_level, done_label.clone(), true);
                            let is_pointer =
                                match exp.ty {
                                    Type::Name(ref symbol, None) if symbol.node == name =>
                                        true,
                                    _ => self.actual_ty(&exp.ty).is_pointer(),
                                };
                            let typ =
                                if let Some(ref typ) = typ {
                                    let typ = self.get_type(typ, AddError);
                                    self.check_types(&typ, &exp.ty, init.pos);
                                    typ
                                }
                                else {
                                    exp.ty
                                };
                            if is_pointer {
                                data_layout.push('p')
                            }
                            else {
                                data_layout.push('n')
                            }
                            fields.push(ClassField {
                                name,
                                typ,
                                value: init.clone(),
                            });
                        },
                        _ => unreachable!("cannot get that kind of declaration in a class"),
                    }
                }
                let class_name = self.strings.get(name.node).expect("string get");
                let vtable_name = Label::with_name(&format!("__vtable_{}", class_name));
                let methods = self.inherit_methods(parent_methods, &methods);
                let class_type = Type::Class {
                    data_layout,
                    fields,
                    methods: methods.clone(),
                    name: name.node,
                    parent_class: Some(parent_class.clone()),
                    unique: Unique::new(),
                    vtable_name: vtable_name.clone(),
                };
                self.env.replace_type(name.node, class_type.clone());

                for method in pending_methods {
                    let body = &method.body;
                    self.env.begin_scope();
                    let mut formals = method.level.formals().into_iter();
                    self.env.enter_var(self.self_symbol, Entry::Var {
                        access: formals.next().expect("self parameter").clone(),
                        typ: class_type.clone(),
                    });
                    let fields =
                        match class_type {
                            Type::Class { ref fields, .. } => fields,
                            _ => unreachable!(),
                        };
                    for field in fields {
                        self.env.enter_var(field.name, Entry::ClassField { class: class_type.clone() });
                    }
                    for ((param, name), access) in method.param_types.into_iter().zip(method.param_names).zip(formals) {
                        self.env.enter_var(name, Entry::Var { access, typ: param });
                    }
                    let exp = self.trans_exp(body, &method.level, done_label.clone(), true);
                    self.check_types(&method.return_type, &exp.ty, body.pos);
                    let current_temp_map = mem::replace(&mut self.temp_map, TempMap::new());
                    let escaping_vars = mem::replace(&mut self.escaping_vars, vec![]);
                    self.gen.proc_entry_exit(&method.level, exp.exp, current_temp_map, escaping_vars);
                    self.env.end_scope();
                }

                let method_labels: Vec<_> = methods.iter()
                    .map(|method| method.label.clone())
                    .collect();
                self.gen.vtable(vtable_name, method_labels);

                self.escaping_vars = old_escaping_vars;
                self.temp_map = old_temp_map;

                None
            },
            Declaration::Function(ref declarations) => {
                let old_temp_map = mem::replace(&mut self.temp_map, TempMap::new());
                let old_escaping_vars = mem::replace(&mut self.escaping_vars, vec![]);
                let mut levels = vec![];
                for &WithPos { node: FuncDeclaration { ref name, ref params, ref result, .. }, .. } in declarations {
                    let func_name = name.node;
                    let formals = params.iter()
                        .map(|param| self.env.look_escape(param.node.name))
                        .collect();
                    let level = Level::new(parent_level, Label::with_name(&self.strings.get(func_name).expect("string get")), formals);
                    let result_type =
                        if let Some(ref result) = *result {
                            self.get_type(result, AddError)
                        }
                        else {
                            Type::Unit
                        };
                    // TODO: error when name already exist?
                    let mut param_names = vec![];
                    let mut parameters = vec![];
                    let mut param_set = HashSet::new();
                    for param in params {
                        parameters.push(self.get_type(&param.node.typ, AddError));
                        param_names.push(param.node.name);
                        if !param_set.insert(param.node.name) {
                            self.duplicate_param(param);
                        }
                    }
                    levels.push(level.clone());
                    self.env.enter_var(func_name, Entry::Fun {
                        external: false,
                        label: Label::with_name(&self.strings.get(func_name).expect("strings get")),
                        level,
                        parameters,
                        result: result_type.clone(),
                    });
                }

                for (&WithPos { node: FuncDeclaration { ref params, ref body, ref result, .. }, .. }, ref level) in
                    declarations.iter().zip(&levels)
                {
                    let result_type =
                        if let Some(ref result) = *result {
                            self.get_type(result, DontAddError)
                        }
                        else {
                            Type::Unit
                        };
                    let mut param_names = vec![];
                    let mut parameters = vec![];
                    for param in params {
                        parameters.push(self.get_type(&param.node.typ, DontAddError));
                        param_names.push(param.node.name);
                    }
                    self.env.begin_scope();
                    for ((param, name), access) in parameters.into_iter().zip(param_names).zip(level.formals().into_iter()) {
                        self.env.enter_var(name, Entry::Var { access, typ: param });
                    }
                    let exp = self.trans_exp(body, level, done_label.clone(), true);
                    self.check_types(&result_type, &exp.ty, body.pos);
                    let current_temp_map = mem::replace(&mut self.temp_map, TempMap::new());
                    let escaping_vars = mem::replace(&mut self.escaping_vars, vec![]);
                    self.gen.proc_entry_exit(&level, exp.exp, current_temp_map, escaping_vars);
                    self.env.end_scope();
                }
                self.escaping_vars = old_escaping_vars;
                self.temp_map = old_temp_map;
                None
            },
            Declaration::Type(ref type_declarations) => {
                self.check_duplicate_types(type_declarations);
                for &WithPos { node: TypeDec { ref name, .. }, .. } in type_declarations {
                    self.env.enter_type(name.node, Type::Name(name.clone(), None));
                }

                for &WithPos { node: TypeDec { ref name, ref ty }, .. } in type_declarations {
                    let new_type = self.trans_ty(name.node, ty);
                    self.env.replace_type(name.node, new_type);
                }
                None
            },
            Declaration::VariableDeclaration { ref init, name, ref typ, .. } => {
                let exp = self.trans_exp(init, parent_level, done_label, true);
                let is_collectable =
                    if type_is_collectable(&exp.ty) {
                        true
                    }
                    else {
                        false
                    };
                let escape = self.env.look_escape(name);
                let access = gen::alloc_local(parent_level, escape || is_collectable); // TODO: check if this is necessary.
                if escape {
                    if let Some(stack_var) = access.1.as_stack() {
                        self.escaping_vars.push(stack_var);
                    }
                }
                if is_collectable {
                    self.temp_map.insert::<F>(&access.1);
                }
                if let Some(ref ident) = *typ {
                    let typ = self.get_type(ident, AddError);
                    self.check_types(&typ, &exp.ty, ident.pos);
                } else if exp.ty == Type::Nil {
                    self.add_error(Error::RecordType { pos: declaration.pos });
                    return None;
                }
                let var = var_dec(&access, exp.exp);
                self.env.enter_var(name, Entry::Var { access, typ: exp.ty });
                Some(var)
            },
        }
    }

    pub fn trans_exp(&mut self, expr: &ExprWithPos, level: &Level<F>, done_label: Option<Label>, outer_array: bool) -> ExpTy {
        let pos = expr.pos;
        match expr.node {
            Expr::Array { ref init, ref size, ref typ } => {
                // NOTE: Since an array can contains heap-allocated values, which could make the
                // heap grow and thus moving the newly allocated array, we should put this array
                // on the stack immediately, because the initialization happens before the array
                // would normally be put on the stack.
                let var =
                    if outer_array {
                        let access = gen::alloc_local(level, true);
                        self.temp_map.insert::<F>(&access.1);
                        if let Some(stack_var) = access.1.as_stack() {
                            self.escaping_vars.push(stack_var);
                        }
                        Some(access)
                    }
                    else {
                        None
                    };

                let size_expr = self.trans_exp(size, level, done_label.clone(), true);
                self.check_int(&size_expr, size.pos);
                let ty = self.get_type(typ, AddError);
                let inner_type =
                    match ty {
                        Type::Array(ref typ, _) => typ,
                        Type::Error => &Type::Error,
                        _ => unreachable!(),
                    };
                let init_expr = self.trans_exp(init, level, done_label, false);
                self.check_types(inner_type, &init_expr.ty, init.pos);
                let is_pointer = self.array_contains_pointer(&ty);
                let is_pointer = num(is_pointer as i64);
                let exp = init_array::<F>(var, size_expr.exp, is_pointer, init_expr.exp, level);
                ExpTy {
                    exp,
                    ty,
                }
            },
            Expr::Assign { ref expr, ref var } => {
                match var.node {
                    Expr::Field { .. } | Expr::Subscript { .. } | Expr::Variable(_) => (),
                    _ => self.add_error(Error::Assign {
                        pos: var.pos,
                    }),
                }
                let var = self.trans_exp(var, level, done_label.clone(), true);
                let expr_expr = self.trans_exp(expr, level, done_label, true);
                self.check_types(&var.ty, &expr_expr.ty, expr.pos);
                ExpTy {
                    exp: Exp::ExpSequence(Box::new(_Statement::Move(var.exp, expr_expr.exp).into()), Box::new(unit())),
                    ty: Type::Unit,
                }
            },
            Expr::Break => {
                if !self.in_loop {
                    self.add_error(Error::BreakOutsideLoop {
                        pos: expr.pos,
                    });
                    return EXP_TYPE_ERROR;
                }
                ExpTy {
                    exp: goto(done_label.expect("break should be in while loop")),
                    ty: Type::Unit,
                }
            },
            Expr::Call { ref args, function } => {
                if let Some(entry@Entry::Fun { .. }) = self.env.look_var(function).cloned() { // TODO: remove this clone.
                    return match entry {
                        Entry::Fun { external, ref label, ref parameters, ref result, level: ref current_level } => {
                            let mut expr_args = vec![];
                            if parameters.len() != args.len() {
                                self.add_error(Error::InvalidNumberOfParams {
                                    actual: args.len(),
                                    expected: parameters.len(),
                                    pos,
                                });
                            }
                            for (arg, param) in args.iter().zip(parameters) {
                                let exp = self.trans_exp(arg, level, done_label.clone(), true);
                                self.check_types(param, &exp.ty, arg.pos);
                                expr_args.push(exp.exp);
                            }
                            let collectable_return_type =
                                if type_is_collectable(result) {
                                    true
                                }
                                else {
                                    false
                                };
                            let exp =
                                if external {
                                    F::external_call(&label.to_name(), expr_args, collectable_return_type)
                                }
                                else {
                                    function_call(label, expr_args, level, current_level, collectable_return_type)
                                };
                            ExpTy {
                                exp,
                                ty: self.actual_ty(result),
                            }
                        },
                        _ => unreachable!(),
                    };
                }
                return self.undefined_function(function, expr.pos);
            },
            Expr::Field { ref ident, ref this } => {
                let var = self.trans_exp(this, level, done_label, true);
                match var.ty {
                    Type::Class { name: class_type, ref fields, .. } => {
                        for (index, class_field) in fields.iter().enumerate() {
                            if class_field.name == ident.node {
                                return ExpTy {
                                    exp: field_access::<F>(var.exp, index, FieldType::Class),
                                    ty: class_field.typ.clone(),
                                };
                            }
                        }
                        self.unexpected_field(ident, ident.pos, class_type)
                    },
                    Type::Record { name: record_type, ref types, .. } => {
                        for (index, &(name, ref typ)) in types.iter().enumerate() {
                            if name == ident.node {
                                return ExpTy {
                                    exp: field_access::<F>(var.exp, index, FieldType::Record),
                                    ty: typ.clone(),
                                };
                            }
                        }
                        self.unexpected_field(ident, ident.pos, record_type)
                    },
                    typ => {
                        self.add_error(Error::NotARecordOrClass {
                            pos: this.pos,
                            typ,
                        });
                        return EXP_TYPE_ERROR;
                    },
                }
            },
            Expr::If { ref else_, ref test, ref then } => {
                let test_expr = self.trans_exp(test, level, done_label.clone(), true);
                self.check_int(&test_expr, then.pos);
                let if_expr = self.trans_exp(then, level, done_label.clone(), true);
                let (else_expr, ty) =
                    match *else_ {
                        Some(ref else_) => {
                            let else_expr = self.trans_exp(&else_, level, done_label, true);
                            self.check_types(&if_expr.ty, &else_expr.ty, else_.pos);
                            (Some(else_expr), if_expr.ty)
                        },
                        None => {
                            self.check_types(&Type::Unit, &if_expr.ty, then.pos);
                            (None, Type::Unit)
                        },
                    };
                ExpTy {
                    exp: if_expression(test_expr.exp, if_expr.exp, else_expr.map(|expr| expr.exp), level),
                    ty,
                }
            },
            Expr::Int { value } =>
                ExpTy {
                    exp: num(value),
                    ty: Type::Int,
                },
            Expr::Let { ref body, ref declarations } => {
                let old_in_loop = self.in_loop;
                self.in_loop = false;
                self.env.begin_scope();
                let mut vars = vec![];
                for declaration in declarations {
                    if let Some(statement) = self.trans_dec(declaration, level, done_label.clone()) {
                        vars.push(statement);
                    }
                }
                self.in_loop = old_in_loop;
                let result = self.trans_exp(body, level, done_label, true);
                self.env.end_scope();
                ExpTy {
                    exp: var_decs(vars, result.exp),
                    ty: result.ty,
                }
            },
            Expr::MethodCall { ref args, ref method, ref this } => {
                let this = self.trans_exp(this, level, done_label.clone(), true);
                let methods =
                    match this.ty {
                        Type::Class { ref methods, .. } => {
                            methods
                        },
                        _ => return self.undefined_method(method.node, method.pos),
                    };

                for (index, class_method) in methods.iter().enumerate() {
                    if method.node == class_method.name.node {
                        let mut expr_args = vec![this.exp];
                        let method_type = &class_method.typ;
                        if method_type.param_types.len() != args.len() {
                            self.add_error(Error::InvalidNumberOfParams {
                                actual: args.len(),
                                expected: method_type.param_types.len(),
                                pos,
                            });
                        }
                        for (arg, param) in args.iter().zip(method_type.param_types.iter()) {
                            let exp = self.trans_exp(arg, level, done_label.clone(), true);
                            self.check_types(param, &exp.ty, arg.pos);
                            expr_args.push(exp.exp);
                        }
                        let result = &method_type.return_type;
                        let collectable_return_type =
                            if type_is_collectable(result) {
                                true
                            }
                            else {
                                false
                            };
                        let current_level = self.methods_level.get(&(class_method.class_name, method.node)).expect("level");
                        let exp = method_call(index, expr_args, level, current_level, collectable_return_type);
                        return ExpTy {
                            exp,
                            ty: self.actual_ty(result),
                        }
                    }
                }

                self.undefined_method(method.node, method.pos)
            },
            Expr::New { ref class_name } => {
                // TODO: forbid calling new Object?
                let class = self.get_type(class_name, AddError);
                let (data_layout, fields, vtable_name) =
                    match class {
                        Type::Class { ref data_layout, ref fields, ref vtable_name, .. } => {
                            (self.gen.string_literal(data_layout.clone()), fields.clone(), vtable_name.clone())
                        },
                        Type::Error => (Exp::Error, vec![], Label::new()),
                        _ => {
                            self.add_error(Error::UnexpectedType {
                                kind: "record".to_string(),
                                pos: class_name.pos,
                            });
                            return EXP_TYPE_ERROR;
                        },
                    };
                // NOTE: we put the class on the stack immediately, because it could contain
                // heap-allocated values, which could causes the heap to be moved.
                let access = gen::alloc_local(level, true);
                self.temp_map.insert::<F>(&access.1);
                if let Some(stack_var) = access.1.as_stack() {
                    self.escaping_vars.push(stack_var);
                }
                let mut field_exprs = vec![];
                for field in &fields {
                    field_exprs.push(self.trans_exp(&field.value, level, done_label.clone(), false).exp);
                }
                let exp = class_create::<F>(access, data_layout, field_exprs, vtable_name);
                ExpTy {
                    exp,
                    ty: class,
                }
            },
            Expr::Nil =>
                ExpTy {
                    exp: num(0),
                    ty: Type::Nil,
                },
            Expr::Oper { ref left, oper: WithPos { node: oper@Operator::Plus, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: oper@Operator::Minus, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: oper@Operator::Times, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: oper@Operator::And, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: oper@Operator::Or, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: oper@Operator::Divide, .. }, ref right } =>
                self.check_binary_op(oper, left, right, level, done_label),
            Expr::Oper { ref left, oper: WithPos { node: oper@Operator::Equal, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: oper@Operator::Neq, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: oper@Operator::Lt, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: oper@Operator::Gt, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: oper@Operator::Ge, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: oper@Operator::Le, .. }, ref right } => {
                let left = self.trans_exp(left, level, done_label.clone(), true);
                let right_pos = right.pos;
                let right = self.trans_exp(right, level, done_label, true);
                self.check_types(&left.ty, &right.ty, right_pos);
                let exp =
                    if left.ty == Type::String && right.ty == Type::String {
                        string_equality::<F>(oper, left.exp, right.exp) // FIXME: strings work with <, <=, > and >= ?
                    }
                    else {
                        relational_oper(oper, left.exp, right.exp, level)
                    };
                ExpTy {
                    exp,
                    ty: Type::Int,
                }
            },
            Expr::Record { ref fields, ref typ } => {
                let ty = self.get_type(typ, AddError);
                let mut field_exprs = vec![];
                let data_layout =
                    match ty {
                        Type::Record { ref data_layout, ref types, .. } => {
                            for &(type_field_name, ref type_field) in types {
                                let mut found = false;
                                for field in fields {
                                    if type_field_name == field.node.ident {
                                        found = true;
                                        let field_expr = self.trans_exp(&field.node.expr, level, done_label.clone(), true);
                                        self.check_types(&type_field, &field_expr.ty, field.node.expr.pos);
                                        field_exprs.push(field_expr.exp);
                                    }
                                }
                                if !found {
                                    return self.missing_field(type_field_name, typ);
                                }
                            }

                            for field in fields {
                                let found = types.iter()
                                    .any(|&(type_field_name, _)| field.node.ident == type_field_name);
                                if !found {
                                    return self.extra_field(field, typ);
                                }
                            }

                            data_layout.clone()
                        },
                        Type::Error => Exp::Error,
                        _ => {
                            self.add_error(Error::UnexpectedType {
                                kind: "record".to_string(),
                                pos: typ.pos,
                            });
                            return EXP_TYPE_ERROR;
                        },
                    };
                let exp = record_create::<F>(data_layout, field_exprs);
                ExpTy {
                    exp,
                    ty,
                }
            },
            Expr::Sequence(ref exprs) => {
                if let Some((last_expr, exprs)) = exprs.split_last() {
                    let mut new_exprs = vec![];
                    for expr in exprs {
                        new_exprs.push(self.trans_exp(expr, level, done_label.clone(), true));
                    }
                    let last_expr = self.trans_exp(last_expr, level, done_label, true);
                    if new_exprs.is_empty() {
                        last_expr
                    }
                    else {
                        let mut exprs = _Statement::Exp(new_exprs.pop().expect("pop").exp).into();
                        for expr in new_exprs.into_iter().rev() {
                            exprs = _Statement::Sequence(Box::new(_Statement::Exp(expr.exp).into()), Box::new(exprs)).into();
                        }
                        ExpTy {
                            exp: Exp::ExpSequence(Box::new(exprs), Box::new(last_expr.exp)),
                            ty: last_expr.ty,
                        }
                    }
                }
                else {
                    panic!("Unexpected empty sequence.");
                }
            },
            Expr::Str { ref value } =>
                ExpTy {
                    exp: self.gen.string_literal(value.clone()),
                    ty: Type::String,
                },
            Expr::Subscript { ref expr, ref this } => {
                let var = self.trans_exp(this, level, done_label.clone(), true);
                let subscript_expr = self.trans_exp(expr, level, done_label, true);
                self.check_int(&subscript_expr, expr.pos);
                match var.ty {
                    Type::Array(typ, _) => ExpTy {
                        exp: array_subscript::<F>(var.exp, subscript_expr.exp),
                        ty: self.actual_ty(&typ),
                    },
                    Type::Error => ExpTy {
                        exp: Exp::Error,
                        ty: Type::Error,
                    },
                    typ => {
                        self.add_error(Error::CannotIndex {
                            pos: this.pos,
                            typ,
                        });
                        EXP_TYPE_ERROR
                    },
                }
            },
            Expr::Variable(ref ident) => {
                match self.env.look_var(ident.node).cloned() { // TODO: remove this clone.
                    Some(Entry::Var { ref access, ref typ, }) => {
                        return ExpTy {
                            exp: simple_var(access.clone(), level),
                            ty: self.actual_ty(typ),
                        };
                    },
                    Some(Entry::ClassField { class }) => {
                        let fields =
                            match class {
                                Type::Class { ref fields, .. } => fields,
                                _ => unreachable!(),
                            };
                        for (index, class_field) in fields.iter().enumerate() {
                            if class_field.name == ident.node {
                                let this = self.trans_exp(&WithPos::dummy(Expr::Variable(WithPos::dummy(self.self_symbol))),
                                    level, done_label, true);
                                return ExpTy {
                                    exp: field_access::<F>(this.exp, index, FieldType::Class),
                                    ty: class_field.typ.clone(),
                                };
                            }
                        }
                        unreachable!();
                    },
                    _ => self.undefined_variable(ident.node, ident.pos),
                }
            },
            Expr::While { ref body, ref test } => {
                let test_expr = self.trans_exp(test, level, done_label, true);
                self.check_int(&test_expr, test.pos);
                let old_in_loop = self.in_loop;
                self.in_loop = true;
                let while_done_label = Label::new();
                let result = self.trans_exp(body, level, Some(while_done_label.clone()), true);
                self.in_loop = old_in_loop;
                ExpTy {
                    exp: while_loop(&while_done_label, test_expr.exp, result.exp),
                    ty: result.ty,
                }
            },
        }
    }

    fn trans_ty(&mut self, name: Symbol, ty: &TyWithPos) -> Type {
        match ty.node {
            Ty::Array { ref ident } => {
                let ty = self.get_type(ident, AddError);
                Type::Array(Box::new(ty), Unique::new())
            },
            Ty::Name { ref ident } => self.get_type(ident, AddError),
            Ty::Record { ref fields } => {
                let mut types = vec![];
                let mut data_layout = String::new();
                for field in fields {
                    let typ = self.get_type(&field.node.typ, AddError);
                    let is_pointer =
                        match typ {
                            Type::Name(ref symbol, None) if symbol.node == name =>
                                true,
                            _ => self.actual_ty(&typ).is_pointer(),
                        };
                    if is_pointer {
                        data_layout.push('p')
                    }
                    else {
                        data_layout.push('n')
                    }
                    types.push((field.node.name, typ));
                }
                let data_layout = self.gen.string_literal(data_layout);
                Type::Record {
                    data_layout,
                    name,
                    types,
                    unique: Unique::new(),
                }
            },
        }
    }

    fn array_contains_pointer(&mut self, ty: &Type) -> bool {
        match ty {
            Type::Array(ref typ, _) => typ.is_pointer(),
            Type::Error => false,
            _ => ty.is_pointer()
        }
    }

    fn method_label(&self, class: Symbol, method: Symbol) -> Label {
        Label::with_name(&format!("{}_{}",
            self.strings.get(class).expect("strings get"),
            self.strings.get(method).expect("strings get"),
        ))
    }

    fn parent_members(&mut self, class: &SymbolWithPos) -> (Vec<ClassField>, String, Vec<ClassMethod>) {
        let mut parent_fields = vec![];
        let mut parent_data_layout = vec![];
        let mut parent_methods: Vec<ClassMethod> = vec![];
        let mut parent = self.get_type(class, DontAddError);
        while let Type::Class { ref data_layout, ref fields, name, ref methods, ref parent_class, .. } = parent {
            parent_fields.push(fields.clone());
            parent_data_layout.push(data_layout.clone());

            for method in methods {
                if let Some(parent_method) = parent_methods.iter_mut().find(|parent_method| parent_method.name == method.name) {
                    parent_method.label = method.label.clone();
                }
                else {
                    parent_methods.push(ClassMethod {
                        class_name: name,
                        label: method.label.clone(),
                        name: method.name.clone(),
                        typ: method.typ.clone(),
                    });
                }
            }

            if let Some(parent_class) = parent_class {
                parent = self.get_type(parent_class, DontAddError);
            }
            else {
                break;
            }
        }
        let fields = parent_fields.into_iter().rev()
            .flatten()
            .collect();
        let data_layout = parent_data_layout.into_iter().rev()
            .collect::<Vec<_>>()
            .join("");
        (fields, data_layout, parent_methods)
    }

    fn duplicate_param(&mut self, param: &FieldWithPos) {
        let ident = self.env.var_name(param.node.name).to_string();
        self.add_error(Error::DuplicateParam {
            ident,
            pos: param.pos,
        });
    }

    fn extra_field(&mut self, field: &RecordFieldWithPos, typ: &SymbolWithPos) -> ExpTy {
        let ident = self.env.type_name(field.node.ident);
        let struct_name = self.env.type_name(typ.node);
        self.add_error(Error::ExtraField {
            ident,
            pos: field.pos,
            struct_name,
        });
        EXP_TYPE_ERROR
    }

    fn inherit_methods(&mut self, parent_methods: Vec<ClassMethod>, method_labels: &[ClassMethod]) -> Vec<ClassMethod> {
        let mut labels = parent_methods.clone();
        for method in method_labels {
            if let Some(index) = parent_methods.iter().position(|parent_method| parent_method.name == method.name) {
                self.check_function_types(&parent_methods[index].typ, &method.typ, method.name.pos);
                labels[index] = method.clone();
            }
            else {
                labels.push(method.clone());
            }
        }
        labels
    }

    fn missing_field(&mut self, field_type: Symbol, typ: &SymbolWithPos) -> ExpTy {
        let ident = self.env.type_name(field_type);
        let struct_name = self.env.type_name(typ.node);
        self.add_error(Error::MissingField {
            ident,
            pos: typ.pos,
            struct_name,
        });
        EXP_TYPE_ERROR
    }

    fn undefined_function(&mut self, ident: Symbol, pos: Pos) -> ExpTy {
        let ident = self.env.var_name(ident).to_string();
        self.add_error(Error::Undefined {
            ident,
            item: "function".to_string(),
            pos,
        });
        EXP_TYPE_ERROR
    }

    fn undefined_method(&mut self, ident: Symbol, pos: Pos) -> ExpTy {
        let ident = self.env.var_name(ident).to_string();
        self.add_error(Error::Undefined {
            ident,
            item: "method".to_string(),
            pos,
        });
        EXP_TYPE_ERROR
    }

    fn undefined_type(&mut self, symbol: &SymbolWithPos) -> Type {
        let ident = self.env.type_name(symbol.node);
        self.add_error(Error::Undefined {
            ident,
            item: "type".to_string(),
            pos: symbol.pos,
        });
        Type::Error
    }

    fn undefined_variable(&mut self, ident: Symbol, pos: Pos) -> ExpTy {
        let ident = self.env.var_name(ident).to_string();
        self.add_error(Error::Undefined {
            ident,
            item: "variable".to_string(),
            pos,
        });
        EXP_TYPE_ERROR
    }

    fn unexpected_field(&mut self, ident: &SymbolWithPos, pos: Pos, typ: Symbol) -> ExpTy {
        let ident = self.env.type_name(ident.node);
        let struct_name = self.env.type_name(typ);
        self.add_error(Error::UnexpectedField {
            ident,
            pos,
            struct_name,
        });
        EXP_TYPE_ERROR
    }
}

fn type_is_collectable(typ: &Type) -> bool {
    match *typ {
        Type::Array { .. } | Type::Class { .. } | Type::Record { .. } | Type::String => true,
        _ => false,
    }
}
