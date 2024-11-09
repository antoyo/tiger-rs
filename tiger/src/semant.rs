/*
 * Copyright (c) 2017-2024 Boucher, Antoni <bouanto@zoho.com>
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

use std::collections::{BTreeSet, HashSet};
use std::fmt::Debug;
use std::mem;
use std::rc::Rc;

use ast::{
    Declaration,
    DeclarationWithPos,
    Expr,
    ExprWithPos,
    Field,
    FieldWithPos,
    FuncDeclaration,
    Operator,
    RecordField,
    RecordFieldWithPos,
    Ty,
    TypeDec,
    TypeDecWithPos,
    TyWithPos,
};
use env::{ClosureField, Env, Entry};
use error::{Error, Result};
use frame::{Frame, Memory};
use gen;
use gen::{
    Gen,
    IR,
    Level,
    array_subscript,
    binary_oper,
    field_access,
    function_pointer_call,
    if_expression,
    init_array,
    num,
    record_create,
    relational_oper,
    simple_var,
    string_equality,
    var_dec,
    var_decs,
};
use ir::{Exp, Statement, _Statement};
use log::Logger;
use position::{Pos, WithPos};
use self::AddError::*;
use symbol::{Strings, Symbol, Symbols, SymbolWithPos};
use temp::{Label, TempMap};
use types::{
    Type,
    Unique,
};
use visitor::Visitor;

const CLOSURE_PARAM: &str = "__closure_param";
const CLOSURE_FIELD: &str = "function_pointer";

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

const EXP_TYPE_ERROR: ExpTy =
    ExpTy {
        exp: Exp::Error,
        ty: Type::Error,
    };

pub struct SemanticAnalyzer<'a, F: Clone + Frame + 'a> {
    closure_index: usize,
    env: &'a mut Env<F>,
    errors: Vec<Error>,
    escaping_vars: Vec<Vec<ClosureField>>,
    escaping_var_locations: Vec<i64>,
    gen: Gen<F>,
    in_loop: bool,
    strings: Rc<Strings>,
    symbols: &'a mut Symbols<()>,
    temp_map: TempMap,

    #[allow(dead_code)]
    logger: Logger,
}

impl<'a, F: Clone + Debug + Frame + PartialEq> SemanticAnalyzer<'a, F> {
    pub fn new(env: &'a mut Env<F>, symbols: &'a mut Symbols<()>, strings: Rc<Strings>) -> Self {
        SemanticAnalyzer {
            closure_index: 0,
            env,
            errors: vec![],
            escaping_vars: vec![],
            escaping_var_locations: vec![],
            gen: Gen::new(),
            in_loop: false,
            symbols,
            strings,
            temp_map: TempMap::new(),

            logger: Logger::new(),
        }
    }

    fn add_error(&mut self, error: Error) {
        self.errors.push(error);
    }

    pub fn analyze(mut self, expr: ExprWithPos) -> Result<IR<F>> {
        let main_symbol = self.symbols.symbol("main");
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
            self.add_error(Error::Type {
                expected: Type::Int,
                pos,
                unexpected: expr.ty.clone(),
            });
        }
    }

    fn compatible_types(&mut self, expected: &Type, unexpected: &Type) -> bool {
        let expected = self.actual_ty(expected);
        let unexpected = self.actual_ty(unexpected);
        if expected != unexpected && expected != Type::Error && unexpected != Type::Error {
            if let Type::Record { .. } = expected {
                if unexpected == Type::Nil {
                    return true;
                }
            }

            if let (&Type::Function { parameters: ref left_parameters, return_type: ref left_return_type },
                &Type::Closure { parameters: ref right_parameters, return_type: ref right_return_type }) =
                (&expected, &unexpected)
            {
                let mut types_match = true;
                if left_parameters.len() != right_parameters.len() {
                    types_match = false;
                }
                for (left_param, right_param) in left_parameters.iter().zip(right_parameters) {
                    if !self.compatible_types(left_param, right_param) {
                        types_match = false;
                    }
                }
                if !self.compatible_types(left_return_type, right_return_type) {
                    types_match = false;
                }

                return types_match;
            }
            return false;
        }

        true
    }

    fn check_types(&mut self, expected: &Type, unexpected: &Type, pos: Pos) {
        let expected = self.actual_ty(expected);
        let unexpected = self.actual_ty(unexpected);
        if expected != unexpected && expected != Type::Error && unexpected != Type::Error {
            if let Type::Record { .. } = expected {
                if unexpected == Type::Nil {
                    return;
                }
            }

            if !self.compatible_types(&expected, &unexpected) {
                self.add_error(Error::Type {
                    expected,
                    pos,
                    unexpected,
                });
            }
        }
    }

    fn closure_call(&mut self, expr: &ExprWithPos, args: &[ExprWithPos], level: &Level<F>, done_label: Option<Label>) -> ExpTy {
        let closure_name = self.symbols.unnamed();
        let pos = expr.pos;
        let closure_symbol = self.symbols.symbol(CLOSURE_FIELD);
        self.env.enter_escape(closure_name, false); // This variable does not escape, because it cannot be used by the user. It's only used here.
        self.trans_exp(&WithPos::new(Expr::Let {
            body: Box::new(WithPos::new(Expr::FunctionPointerCall {
                args: args.to_vec(),
                closure_name,
                function: Box::new(WithPos::new(Expr::ClosureParamField {
                    ident: WithPos::new(closure_symbol, pos),
                    this: Box::new(WithPos::new(Expr::Variable(WithPos::new(closure_name, pos)), pos)),
                }, pos)),
            }, pos)),
            declarations: vec![WithPos::new(Declaration::VariableDeclaration {
                escape: false,
                init: expr.clone(),
                name: closure_name,
                typ: None,
            }, pos)],
        }, pos),
        level, done_label, true)
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
            Declaration::Function(ref declarations) => {
                let old_temp_map = mem::replace(&mut self.temp_map, TempMap::new());
                let old_escaping_vars = mem::take(&mut self.escaping_var_locations);
                let mut levels = vec![];
                for &WithPos { node: FuncDeclaration { ref name, ref params, ref result, .. }, .. } in declarations {
                    let func_symbol = name.node;
                    let func_name = self.strings.get(func_symbol).expect("string get");
                    let formals = params.iter()
                        .map(|param| self.env.look_escape(param.node.name))
                        .collect();
                    let level = Level::new(parent_level, Label::with_name(&func_name), formals);
                    let result_type =
                        if let Some(ref result) = *result {
                            self.get_type(result, AddError)
                        }
                        else {
                            Type::Answer
                        };
                    // TODO: error when name already exist?
                    let mut param_names = vec![];
                    let mut parameters = vec![];
                    let mut param_type_symbols = vec![];
                    let mut param_set = HashSet::new();
                    for param in params {
                        param_type_symbols.push(param.node.typ.clone());
                        parameters.push(self.get_type(&param.node.typ, AddError));
                        param_names.push(param.node.name);
                        if !param_set.insert(param.node.name) {
                            self.duplicate_param(param);
                        }
                    }
                    levels.push(level.clone());

                    self.env.enter_var(func_symbol, Entry::Fun {
                        external: false,
                        label: Label::with_name(&func_name),
                        parameters,
                        param_type_symbols,
                        result: result_type.clone(),
                        result_symbol: result.clone(),
                    });
                }

                for (&WithPos { node: FuncDeclaration { ref params, ref body, ref result, .. }, .. }, level) in
                    declarations.iter().zip(&levels)
                {
                    let result_type =
                        if let Some(ref result) = *result {
                            self.get_type(result, DontAddError)
                        }
                        else {
                            Type::Answer
                        };
                    let mut param_names = vec![];
                    let mut parameters = vec![];
                    self.escaping_vars.push(vec![]);
                    for param in params {
                        let param_type = self.get_type(&param.node.typ, DontAddError);
                        if self.env.look_escape(param.node.name) {
                            self.escaping_vars.last_mut().expect("scope")
                                .push(ClosureField {
                                    ident: param.node.name,
                                    typ: param_type.clone(),
                                });
                        }
                        parameters.push(param_type);
                        param_names.push(param.node.name);
                    }
                    self.env.begin_scope();
                    for ((param, name), access) in parameters.into_iter().zip(param_names).zip(level.formals().into_iter()) {
                        if type_is_collectable(&param) {
                            self.temp_map.insert::<F>(&access.1);
                        }
                        self.env.enter_var(name, Entry::Var { access, typ: param });
                    }

                    let exp = self.trans_exp(body, level, done_label.clone(), true);
                    self.check_types(&result_type, &exp.ty, body.pos);
                    let current_temp_map = mem::replace(&mut self.temp_map, TempMap::new());
                    let escaping_var_locations = mem::take(&mut self.escaping_var_locations);
                    self.gen.proc_entry_exit(level, exp.exp, current_temp_map, escaping_var_locations);
                    self.env.end_scope();
                    self.escaping_vars.pop();
                }
                self.escaping_var_locations = old_escaping_vars;
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
                let closure_type =
                    if let Expr::Closure { ref params, ref result, .. } = init.node {
                        let return_type =
                            if let Some(ref result) = *result {
                                self.get_type(result, AddError)
                            }
                            else {
                                Type::Answer
                            };
                        let mut parameters = vec![];
                        for param in params {
                            parameters.push(self.get_type(&param.node.typ, DontAddError));
                        }
                        Some(Type::Closure {
                            parameters,
                            return_type: Box::new(return_type),
                        })
                    }
                    else {
                        None
                    };
                let mut access = gen::alloc_local(parent_level, false);
                if let Some(ref typ) = closure_type {
                    access = gen::alloc_local(parent_level, true);
                    self.env.enter_var(name, Entry::Var { access: access.clone(), typ: typ.clone() });
                }
                let exp = self.trans_exp(init, parent_level, done_label, true);
                let escape = self.env.look_escape(name);
                let is_collectable = type_is_collectable(&exp.ty);
                if closure_type.is_none() {
                    access = gen::alloc_local(parent_level, escape || is_collectable); // TODO: check if this is necessary.
                }
                if escape {
                    if let Some(stack_var) = access.1.as_stack() {
                        self.escaping_var_locations.push(stack_var);
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
                if escape {
                    self.escaping_vars.last_mut().expect("scope")
                        .push(ClosureField {
                            ident: name,
                            typ: exp.ty.clone(),
                        });
                }
                let var = var_dec(&access, exp.exp);
                if closure_type.is_none() {
                    self.env.enter_var(name, Entry::Var { access, typ: exp.ty });
                }
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
                            self.escaping_var_locations.push(stack_var);
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
            Expr::Call { ref args, ref function } => {
                if let Expr::Variable(ref func) = function.node {
                    match self.env.look_var(func.node).cloned() { // TODO: remove this clone.
                        Some(Entry::Fun { external, ref label, ref parameters, ref result, .. }) => {
                            if parameters.len() != args.len() {
                                self.add_error(Error::InvalidNumberOfParams {
                                    actual: args.len(),
                                    expected: parameters.len(),
                                    pos,
                                });
                            }

                            let mut expr_args = vec![];
                            for (arg, param) in args.iter().zip(parameters) {
                                let exp = self.trans_exp(arg, level, done_label.clone(), true);
                                self.check_types(param, &exp.ty, arg.pos);
                                expr_args.push(exp.exp);
                            }

                            let collectable_return_type = type_is_collectable(result);

                            if external {
                                return ExpTy {
                                    exp: F::external_call(&label.to_name(), expr_args, collectable_return_type),
                                    ty: self.actual_ty(result),
                                };
                            }
                        },
                        None => return self.undefined_function(func.node, func.pos),
                        _ => (),
                    }
                }
                self.closure_call(function, args, level, done_label)
            },
            Expr::Closure { ref body, ref name, ref params, ref result } => {
                let mut data_layout = String::new();
                data_layout.push('n'); // First field is the function pointer, which is not heap-allocated.

                let func_name = name.node;
                let formals: Vec<_> = params.iter()
                    .map(|param| self.env.look_escape(param.node.name))
                    .collect();
                let func_level = Level::new(level, Label::with_name(&self.strings.get(func_name).expect("string get")), formals);
                let closure_level = func_level.formals().last().expect("closure access").0.clone();

                let env_fields = find_closure_environment(body, self.env, closure_level);

                let function_pointer_symbol = self.symbols.symbol(CLOSURE_FIELD);

                let mut types = vec![(function_pointer_symbol, Type::Int)];

                for field in &env_fields {
                    let is_pointer = self.actual_ty(&field.typ).is_pointer();
                    if is_pointer {
                        data_layout.push('p')
                    }
                    else {
                        data_layout.push('n')
                    }
                    types.push((field.ident, field.typ.clone()));
                }
                let data_layout = self.gen.string_literal(data_layout);

                let closure_symbol = self.symbols.symbol(&format!("Closure{}", self.closure_index));
                self.closure_index += 1;
                let record_type = Type::Record {
                    data_layout,
                    name: closure_symbol,
                    types,
                    unique: Unique::new(),
                };
                self.env.enter_type(closure_symbol, record_type.clone());

                let function =
                    FuncDeclaration {
                        body: *body.clone(),
                        name: WithPos::new(func_name, pos),
                        params: params.clone(),
                        result: result.clone(),
                    };

                let result_type =
                    if let Some(ref result) = *result {
                        self.get_type(result, AddError)
                    }
                    else {
                        Type::Answer
                    };

                {
                    let old_temp_map = mem::replace(&mut self.temp_map, TempMap::new());
                    let old_escaping_vars = mem::take(&mut self.escaping_var_locations);

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
                    let closure_param_symbol = self.symbols.symbol(CLOSURE_PARAM);
                    param_names.push(closure_param_symbol);
                    parameters.push(record_type.clone());

                    self.env.begin_scope();
                    for field in &env_fields {
                        self.env.enter_var(field.ident, Entry::RecordField { record: record_type.clone() });
                    }

                    let closure_type = parameters.last().expect("closure parameter").clone();
                    let mut function_type = None;
                    if let Type::Record { ref types, .. } = closure_type {
                        for &(ident, ref typ) in types {
                            if ident == name.node {
                                function_type = Some(typ.clone());
                            }
                        }
                    }
                    let function_type = function_type.unwrap_or(closure_type);
                    let access = func_level.formals().last().expect("closure formal").clone();
                    self.env.enter_var(name.node, Entry::Var { access, typ: function_type });

                    for ((param, name), access) in parameters.into_iter().zip(param_names).zip(func_level.formals().into_iter()) {
                        if type_is_collectable(&param) {
                            self.temp_map.insert::<F>(&access.1);
                        }
                        self.env.enter_var(name, Entry::Var { access, typ: param });
                    }
                    self.env.enter_escape(closure_param_symbol, false);

                    // TODO: forbid declaring normal functions in closure (because they could
                    // access variables from outside the closure)?
                    // Or could we just put the variables in the closure?
                    let exp = self.trans_exp(&function.body, &func_level, done_label.clone(), true);
                    self.check_types(&result_type, &exp.ty, function.body.pos);
                    let current_temp_map = mem::replace(&mut self.temp_map, TempMap::new());
                    let escaping_var_locations = mem::take(&mut self.escaping_var_locations);
                    self.gen.proc_entry_exit(&func_level, exp.exp, current_temp_map, escaping_var_locations);
                    self.env.end_scope();

                    self.escaping_var_locations = old_escaping_vars;
                    self.temp_map = old_temp_map;
                }

                let mut parameters = vec![];
                for param in params {
                    parameters.push(self.get_type(&param.node.typ, DontAddError));
                }

                let ty = Type::Closure {
                    parameters,
                    return_type: Box::new(result_type),
                };

                let label = func_name;
                let mut fields = vec![WithPos::new(RecordField {
                    expr: WithPos::new(Expr::ClosurePointer {
                        label,
                    }, pos),
                    ident: function_pointer_symbol,
                }, pos)];

                for field in &env_fields {
                    fields.push(WithPos::new(RecordField {
                        expr: WithPos::new(Expr::Variable(WithPos::new(field.ident, pos)), pos),
                        ident: field.ident,
                    }, pos));
                }

                let closure = self.trans_exp(&WithPos::new(Expr::Record {
                        fields,
                        typ: WithPos::new(closure_symbol, pos),
                    }, pos), level, done_label, true);
                ExpTy {
                    exp: closure.exp,
                    ty,
                }
            },
            Expr::ClosureParamField { ref ident, ref this } => {
                let var = self.trans_exp(this, level, done_label, true);
                if ident.node == self.symbols.symbol(CLOSURE_FIELD) {
                    return ExpTy {
                        exp: field_access::<F>(var.exp, 0), // TODO: do we really want to hard-code this?
                        ty: var.ty.clone(),
                    };
                }
                self.add_error(Error::NotARecord {
                    pos: this.pos,
                    typ: var.ty,
                });
                EXP_TYPE_ERROR
            },
            Expr::Field { ref ident, ref this } => {
                let var = self.trans_exp(this, level, done_label.clone(), true);
                match var.ty {
                    Type::Record { name: record_type, ref types, .. } => {
                        for (index, &(name, ref typ)) in types.iter().enumerate() {
                            if name == ident.node {
                                return ExpTy {
                                    exp: field_access::<F>(var.exp, index),
                                    ty: typ.clone(),
                                };
                            }
                        }
                        self.unexpected_field(ident, ident.pos, record_type)
                    },
                    Type::Error => EXP_TYPE_ERROR,
                    typ => {
                        self.add_error(Error::NotARecord {
                            pos: this.pos,
                            typ,
                        });
                        EXP_TYPE_ERROR
                    },
                }
            },
            Expr::ClosurePointer { label } => {
                ExpTy {
                    exp: Exp::Name(Label::with_name(&self.strings.get(label).expect("label"))),
                    ty: Type::Int,
                }
            },
            Expr::FunctionPointerCall { ref args, closure_name, ref function } => {
                let pos = function.pos;
                let function = self.trans_exp(function, level, done_label.clone(), true);

                let (parameters, result) =
                    match function.ty {
                        Type::Closure { ref parameters, ref return_type } | Type::Function { ref parameters, ref return_type } => (parameters, return_type),
                        Type::Error => return EXP_TYPE_ERROR,
                        _ => return self.not_callable(&function.ty, pos),
                    };
                let collectable_return_type = type_is_collectable(result);

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
                let exp = self.trans_exp(&WithPos::new(Expr::Variable(WithPos::new(closure_name, pos)), pos), level, done_label, true);
                expr_args.push(exp.exp);

                ExpTy {
                    exp: function_pointer_call(function.exp, expr_args, collectable_return_type),
                    ty: self.actual_ty(result),
                }
            },
            Expr::If { ref else_, ref test, ref then } => {
                let test_expr = self.trans_exp(test, level, done_label.clone(), true);
                self.check_int(&test_expr, then.pos);
                let if_expr = self.trans_exp(then, level, done_label.clone(), true);
                let (else_expr, ty) =
                    match *else_ {
                        Some(ref else_) => {
                            let else_expr = self.trans_exp(else_, level, done_label, true);
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
                                        self.check_types(type_field, &field_expr.ty, field.node.expr.pos);
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
                        let exp = simple_var(access.clone(), level);
                        ExpTy {
                            exp,
                            ty: self.actual_ty(typ),
                        }
                    },
                    Some(Entry::Fun { external, ref parameters, ref param_type_symbols, ref result, ref result_symbol, .. }) => {
                        let closure =
                            if external {
                                let mut args = vec![];
                                let mut params = vec![];
                                for (index, param) in param_type_symbols.iter().enumerate() {
                                    let name = self.symbols.symbol(&format!("__arg{}", index));
                                    args.push(WithPos::new(Expr::Variable(WithPos::new(name, pos)), pos));
                                    self.env.enter_escape(name, false);
                                    params.push(WithPos::new(
                                        Field {
                                            escape: false,
                                            name,
                                            typ: param.clone(),
                                        },
                                        pos,
                                    ));
                                }
                                Expr::Closure {
                                    body: Box::new(WithPos::new(Expr::Call {
                                        args,
                                        function: Box::new(WithPos::new(Expr::Variable(ident.clone()), pos)),
                                    }, pos)),
                                    name: WithPos::new(self.symbols.symbol(&format!("_closure_{}", self.strings.get(ident.node).expect("get function name"))), pos),
                                    params,
                                    result: result_symbol.clone(),
                                }
                            }
                            else {
                                unreachable!();
                            };

                        let ty = Type::Closure { // TODO: check if it should always be a closure.
                            parameters: parameters.clone(),
                            return_type: Box::new(result.clone()),
                        };

                        let closure = self.trans_exp(&WithPos::new(closure, pos), level, done_label, true);

                        ExpTy {
                            exp: closure.exp,
                            ty,
                        }
                    },
                    Some(Entry::RecordField { record }) => {
                        let fields =
                            match record {
                                Type::Record { ref types, .. } => types,
                                _ => unreachable!(),
                            };
                        for (index, &(name, ref typ)) in fields.iter().enumerate() {
                            if name == ident.node {
                                let closure_symbol = self.symbols.symbol(CLOSURE_PARAM);
                                let this = self.trans_exp(&WithPos::dummy(Expr::Variable(WithPos::dummy(closure_symbol))),
                                    level, done_label, true);
                                return ExpTy {
                                    exp: field_access::<F>(this.exp, index),
                                    ty: typ.clone(),
                                };
                            }
                        }
                        unreachable!();
                    },
                    _ => self.undefined_variable(ident.node, ident.pos),
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
            Ty::Function { ref parameters, ref return_type } => {
                let parameters = parameters.iter()
                    .map(|param| self.trans_ty(name, param))
                    .collect();
                Type::Closure {
                    parameters,
                    return_type: Box::new(self.trans_ty(name, return_type)),
                }
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
            Ty::Unit => Type::Unit,
        }
    }

    fn array_contains_pointer(&mut self, ty: &Type) -> bool {
        match *ty {
            Type::Array(ref typ, _) => typ.is_pointer(),
            Type::Error => false,
            _ => ty.is_pointer()
        }
    }

    fn duplicate_param(&mut self, param: &FieldWithPos) {
        let ident = self.env.var_name(param.node.name);
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

    fn not_callable(&mut self, typ: &Type, pos: Pos) -> ExpTy {
        self.add_error(Error::NotCallable {
            pos,
            typ: typ.clone(),
        });
        EXP_TYPE_ERROR
    }

    fn undefined_function(&mut self, ident: Symbol, pos: Pos) -> ExpTy {
        let ident = self.env.var_name(ident);
        self.add_error(Error::Undefined {
            ident,
            item: "function".to_string(),
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
        let ident = self.env.var_name(ident);
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

struct EnvFinder<'a, F: Frame> {
    closure_level: Level<F>,
    env: &'a Env<F>,
    fields: BTreeSet<ClosureField>,
}

impl<'a, F: Frame> EnvFinder<'a, F> {
    fn new(env: &'a Env<F>, closure_level: Level<F>) -> Self {
        Self {
            closure_level,
            env,
            fields: BTreeSet::new(),
        }
    }
}

#[allow(clippy::needless_lifetimes)]
impl<'a, F: Frame + PartialEq> Visitor for EnvFinder<'a, F> {
    fn visit_closure(&mut self, body: &ExprWithPos) {
        self.visit_exp(body);
    }

    fn visit_var(&mut self, ident: &SymbolWithPos) {
        match self.env.look_var(ident.node) {
            Some(&Entry::Var { ref access, ref typ, }) => {
                if self.closure_level.current != access.0.current {
                    self.fields.insert(ClosureField {
                        ident: ident.node,
                        typ: typ.clone(),
                    });
                }
            },
            Some(&Entry::RecordField { ref record }) => {
                match *record {
                    Type::Record { ref types, .. } => {
                        for &(name, ref typ) in types {
                            if name == ident.node {
                                self.fields.insert(ClosureField {
                                    ident: ident.node,
                                    typ: typ.clone(),
                                });
                            }
                        }
                    },
                    _ => unreachable!(),
                }
            },
            _ => (),
        }
    }
}

fn find_closure_environment<F: Frame + PartialEq>(exp: &ExprWithPos, env: &Env<F>, closure_level: Level<F>) -> BTreeSet<ClosureField> {
    // FIXME: do not fetch the closure itself (in case of recursive closure).
    let mut finder = EnvFinder::new(env, closure_level);
    finder.visit_exp(exp);
    finder.fields
}

fn type_is_collectable(typ: &Type) -> bool {
    match *typ {
        Type::Array { .. } | Type::Closure { .. } | Type::Function { .. } | Type::Record { .. } | Type::String => true,
        _ => false,
    }
}
