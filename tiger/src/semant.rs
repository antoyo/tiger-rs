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
    function_call,
    function_pointer_call,
    goto,
    if_expression,
    init_array,
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
const STATIC_LINK_VAR: &str = "__tiger_static_link";

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
    in_pure_fun: bool,
    static_link_index: usize,
    strings: Rc<Strings>,
    symbols: &'a mut Symbols<()>,
    temp_map: TempMap,

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
            in_pure_fun: false,
            static_link_index: 0,
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
        let static_link_symbol = self.symbols.symbol(STATIC_LINK_VAR);
        self.env.enter_escape(static_link_symbol, false);
        self.trans_dec(&WithPos::new(Declaration::Function(vec![
            WithPos::new(FuncDeclaration {
                body,
                name: WithPos::dummy(main_symbol),
                params: vec![],
                pure: false,
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

    fn check_types(&mut self, expected: &Type, unexpected: &Type, pos: Pos) {
        let expected = self.actual_ty(expected);
        let unexpected = self.actual_ty(unexpected);
        if expected != unexpected && expected != Type::Error && unexpected != Type::Error {
            if let Type::Record { .. } = expected {
                if unexpected == Type::Nil {
                    return;
                }
            }

            panic!("{:?} != {:?}", expected, unexpected);

            self.add_error(Error::Type {
                expected,
                pos,
                unexpected,
            });
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
                let mut static_links = vec![];
                for &WithPos { node: FuncDeclaration { ref name, ref params, pure, ref result, .. }, .. } in declarations {
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
                        else if pure {
                            Type::Answer
                        }
                        else {
                            Type::Unit
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

                    let escaping_vars = self.find_escaping_vars();
                    static_links.push(escaping_vars.clone());

                    self.env.enter_var(func_symbol, Entry::Fun {
                        external: false,
                        is_normal_function: true,
                        label: Label::with_name(&func_name),
                        level,
                        escaping_vars,
                        parameters,
                        param_type_symbols,
                        pure,
                        result: result_type.clone(),
                        result_symbol: result.clone(),
                    });
                }

                let old_in_pure_fun = self.in_pure_fun;
                for ((&WithPos { node: FuncDeclaration { ref name, ref params, ref body, pure, ref result, .. }, .. }, level), static_link) in
                    declarations.iter().zip(&levels).zip(&static_links)
                {
                    let func_name = self.strings.get(name.node).expect("string get");
                    self.logger.print(&format!("Function {}", func_name));
                    self.logger.print("{");
                    self.logger.begin_scope();

                    self.logger.print("Escaping:");
                    for field in static_link {
                        self.logger.print(&format!("{:?}", self.strings.get(field.ident)));
                    }

                    self.in_pure_fun = pure;
                    let result_type =
                        if let Some(ref result) = *result {
                            self.get_type(result, DontAddError)
                        }
                        else if pure {
                            Type::Answer
                        }
                        else {
                            Type::Unit
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
                        self.env.enter_var(name, Entry::Var { access, typ: param });
                    }

                    self.enter_static_link_param(level, static_link);

                    let exp = self.trans_exp(body, level, done_label.clone(), true);
                    self.check_types(&result_type, &exp.ty, body.pos);
                    let current_temp_map = mem::replace(&mut self.temp_map, TempMap::new());
                    let escaping_var_locations = mem::take(&mut self.escaping_var_locations);
                    self.gen.proc_entry_exit(level, exp.exp, current_temp_map, escaping_var_locations);
                    self.env.end_scope();
                    self.logger.end_scope();
                    self.logger.print(&format!("}} # Function {}", func_name));
                    self.escaping_vars.pop();
                }
                self.in_pure_fun = old_in_pure_fun;
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
                let exp = self.trans_exp(init, parent_level, done_label, true);
                let is_collectable = type_is_collectable(&exp.ty);
                let escape = self.env.look_escape(name);
                let access = gen::alloc_local(parent_level, escape || is_collectable); // TODO: check if this is necessary.
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
            Expr::Assign { ref expr, ref var } => {
                if self.in_pure_fun {
                    self.add_error(Error::CannotAssignInPureFun {
                        pos,
                    });
                    return EXP_TYPE_ERROR;
                }
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
            Expr::Call { ref args, ref function } => {
                match function.node {
                    Expr::Variable(ref func) => {
                        match self.env.look_var(func.node).cloned() { // TODO: remove this clone.
                            Some(Entry::Fun { external, ref parameters, level: ref function_level, ref escaping_vars, .. }) => {
                                // TODO: remove this code.
                                /*if self.in_pure_fun && !pure {
                                    self.add_error(Error::CannotCallImpureFun {
                                        pos,
                                    });
                                }*/

                                if parameters.len() != args.len() {
                                    self.add_error(Error::InvalidNumberOfParams {
                                        actual: args.len(),
                                        expected: parameters.len(),
                                        pos,
                                    });
                                }

                                let mut args = args.clone();
                                let static_link_symbol = self.symbols.symbol("__callee_static_link");
                                let mut declarations = vec![];
                                if !external {
                                    self.logger.print(&format!("Calling {:?}", self.strings.get(func.node)));
                                    self.logger.print("*** Access static link:");
                                    for field in escaping_vars {
                                        self.logger.print(&format!("Name: {:?}", self.strings.get(field.ident)));
                                    }
                                    self.logger.print("*** End access static link:");
                                    let (init, _) = self.access_static_link(escaping_vars, level, function_level);
                                    args.push(WithPos::dummy(Expr::Variable(WithPos::dummy(static_link_symbol))));
                                    self.env.enter_escape(static_link_symbol, false);
                                    declarations.push(
                                        WithPos::new(Declaration::VariableDeclaration {
                                            escape: true,
                                            name: static_link_symbol,
                                            init,
                                            typ: None,
                                        }, pos)
                                    );
                                }
                                self.trans_exp(&WithPos::new(Expr::Let {
                                    declarations,
                                    body: Box::new(WithPos::new(Expr::CallWithStaticLink {
                                        args,
                                        function: function.clone(),
                                    }, pos)),
                                }, pos),
                                level, done_label, true)
                            }
                            Some(_) => self.closure_call(function, args, level, done_label),
                            None => self.undefined_function(func.node, func.pos),
                        }
                    },
                    _ => self.closure_call(function, args, level, done_label),
                }
            },
            Expr::CallWithStaticLink { ref args, ref function } => {
                match function.node {
                    Expr::Variable(ref func) => {
                        let entry = self.env.look_var(func.node).cloned(); // TODO: remove this clone.
                        if entry.is_none() {
                            return self.undefined_function(func.node, func.pos);
                        }
                        if let Some(entry@Entry::Fun { .. }) = entry {
                            match entry {
                                Entry::Fun { ref escaping_vars, external, ref label, ref parameters, ref result, .. } => {
                                    let mut expr_args = vec![];
                                    for (arg, param) in args.iter().zip(parameters) {
                                        let exp = self.trans_exp(arg, level, done_label.clone(), true);
                                        println!("Callwith {:?} != {:?}", param, exp.ty);
                                        self.check_types(param, &exp.ty, arg.pos);
                                        expr_args.push(exp.exp);
                                    }

                                    let collectable_return_type = type_is_collectable(result);
                                    let exp =
                                        if external {
                                            F::external_call(&label.to_name(), expr_args, collectable_return_type)
                                        }
                                        else {
                                            let static_link = args.last().expect("static link");
                                            let exp = self.trans_exp(static_link, level, done_label.clone(), true);

                                            let static_link_types =
                                                match exp.ty {
                                                    Type::Record { ref types, .. } | Type::StaticLink { ref types, .. } => types,
                                                    _ => unreachable!(),
                                                };
                                            for &(ident, _) in static_link_types {
                                                self.logger.print(&format!("Static link contains: {:?}", self.strings.get(ident)));
                                            }
                                            for var in escaping_vars {
                                                let mut found = false;
                                                for &(ident, _) in static_link_types {
                                                    if ident == var.ident {
                                                        found = true;
                                                    }
                                                }
                                                if !found {
                                                    panic!("Missing var {:?} in static link", self.strings.get(var.ident));
                                                }
                                            }

                                            expr_args.push(exp.exp);
                                            function_call(label, expr_args, collectable_return_type)
                                        };
                                    ExpTy {
                                        exp,
                                        ty: self.actual_ty(result),
                                    }
                                },
                                _ => unreachable!(),
                            }
                        }
                        else {
                            unreachable!();
                        }
                    },
                    _ => unreachable!(),
                }
            },
            Expr::Closure { ref body, ref params, pure, ref result } => {
                let mut data_layout = String::new();
                data_layout.push('n'); // First field is the function pointer, which is not heap-allocated.

                let name = format!("__closure_{}", self.closure_index);
                let func_name = self.symbols.symbol(&name);
                let formals: Vec<_> = params.iter()
                    .map(|param| self.env.look_escape(param.node.name))
                    .collect();
                // NOTE: do not push a formal for the closure (environment), because Level::new()
                // already push a formal for the static link that we can reuse since closures don't
                // have static link.
                let func_level = Level::new(level, Label::with_name(&self.strings.get(func_name).expect("string get")), formals);
                let closure_level = func_level.formals().last().expect("closure access").0.clone();

                let (mut env_fields, escaping_vars) = find_closure_environment(body, self.env, closure_level);
                let static_link_ident = self.symbols.symbol(STATIC_LINK_VAR);
                println!("*********** Equal {}", level == &func_level);
                let (init, typ) = self.access_static_link(&escaping_vars, level, &func_level);

                println!("****************** Static link type {:?}", typ);
                env_fields.insert(ClosureField {
                    ident: static_link_ident,
                    typ: typ.expect("static link"),
                });

                let function_pointer_symbol = self.symbols.symbol(CLOSURE_FIELD);

                let mut types = vec![(function_pointer_symbol, Type::Int)];

                println!("Start");
                for field in &env_fields {
                    let is_pointer = self.actual_ty(&field.typ).is_pointer();
                    if is_pointer {
                        data_layout.push('p')
                    }
                    else {
                        data_layout.push('n')
                    }
                    println!("------> {:?}: {:?}", self.strings.get(field.ident), field.typ);
                    if let Type::Record { name, .. } = field.typ {
                        println!("Record name: {:?}", self.strings.get(name));
                    }
                    types.push((field.ident, field.typ.clone()));
                }
                println!("End");
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
                        pure: true,
                        result: result.clone(),
                    };

                let result_type =
                    if let Some(ref result) = *result {
                        self.get_type(result, AddError)
                    }
                    else if pure {
                        Type::Answer
                    }
                    else {
                        Type::Unit
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
                    self.env.enter_var(func_name, Entry::Fun {
                        external: false,
                        is_normal_function: false,
                        label: Label::with_name(&self.strings.get(func_name).expect("strings get")),
                        level: func_level.clone(),
                        escaping_vars: BTreeSet::new(),
                        parameters: parameters.clone(),
                        param_type_symbols: vec![],
                        pure: true,
                        result: result_type.clone(),
                        result_symbol: None,
                    });

                    self.env.begin_scope();
                    for field in &env_fields {
                        self.env.enter_var(field.ident, Entry::RecordField { record: record_type.clone() });
                    }

                    for ((param, name), access) in parameters.into_iter().zip(param_names).zip(func_level.formals().into_iter()) {
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

                let ty = Type::Function {
                    parameters,
                    return_type: Box::new(result_type),
                };

                let label = self.symbols.symbol(&name);
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

                let declarations = vec![
                    WithPos::new(Declaration::VariableDeclaration {
                        escape: true,
                        name: static_link_ident,
                        init,
                        typ: None,
                    }, pos)
                ];
                let closure = self.trans_exp(&WithPos::new(Expr::Let {
                    declarations,
                    body: Box::new(WithPos::new(Expr::Record {
                        fields,
                        typ: WithPos::new(closure_symbol, pos),
                    }, pos)),
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
                    Type::StaticLink { ref types, .. } => {
                        self.logger.print("Access field of static link");
                        for &(symbol, ref typ) in types {
                            self.logger.print(&format!("{:?}: {:?}", self.strings.get(symbol), typ));
                        }
                        if types.is_empty() {
                            // Main function has an empty static link.
                            return self.trans_exp(&WithPos::dummy(Expr::Nil), level, done_label.clone(), true);
                        }
                        for (index, &(name, ref typ)) in types.iter().enumerate() {
                            self.logger.print(&format!("{:?} ({})", self.strings.get(name), name));
                            if name == ident.node {
                                self.logger.print(&format!("Was {:?} at {}", self.strings.get(name), index));
                                return ExpTy {
                                    exp: field_access::<F>(var.exp, index),
                                    ty: typ.clone(),
                                };
                            }
                        }
                        unreachable!();
                    },
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
            Expr::FunctionPointer { ref label } => {
                ExpTy {
                    exp: Exp::Name(label.clone()),
                    ty: Type::Int,
                }
            },
            Expr::FunctionPointerCall { ref args, closure_name, ref function } => {
                let pos = function.pos;
                let function = self.trans_exp(function, level, done_label.clone(), true);

                let (parameters, result) =
                    match function.ty {
                        Type::Function { ref parameters, ref return_type, .. } => (parameters, return_type),
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
                println!("Record {:?} ({})", self.strings.get(typ.node), typ.node);
                let ty = self.get_type(typ, AddError);
                let mut field_exprs = vec![];
                let data_layout =
                    match ty {
                        Type::Record { ref data_layout, ref types, .. } => {
                            println!("Types: {:#?}", types);
                            for &(type_field_name, ref type_field) in types {
                                let mut found = false;
                                for field in fields {
                                    if type_field_name == field.node.ident {
                                        found = true;
                                        let field_expr = self.trans_exp(&field.node.expr, level, done_label.clone(), true);
                                        println!("Field {:?}", self.strings.get(type_field_name));
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
            Expr::DirectVariable(ref ident) => {
                match self.env.look_var(ident.node).cloned() { // TODO: remove this clone.
                    Some(Entry::Var { ref access, ref typ, }) => {
                        let exp = simple_var(access.clone(), level);
                        ExpTy {
                            exp,
                            ty: self.actual_ty(typ),
                        }
                    },
                    _ => unreachable!("{:?}", self.strings.get(ident.node)),
                }
            },
            Expr::Variable(ref ident) => {
                match self.env.look_var(ident.node).cloned() { // TODO: remove this clone.
                    Some(Entry::Var { ref access, ref typ, }) => {
                        let exp = self.variable_access(ident, access.clone(), level, done_label.clone());
                        ExpTy {
                            exp,
                            ty: self.actual_ty(typ),
                        }
                    },
                    Some(Entry::Fun { is_normal_function, ref parameters, ref param_type_symbols, ref result, ref result_symbol, .. }) => {
                        let closure =
                            if is_normal_function { // TODO: remove is_normal_function.
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
                                println!("==================== Creating closure for {:?}", self.strings.get(ident.node));
                                Expr::Closure {
                                    body: Box::new(WithPos::new(Expr::Call {
                                        args,
                                        function: Box::new(WithPos::new(Expr::Variable(ident.clone()), pos)),
                                    }, pos)),
                                    params,
                                    pure: true,
                                    result: result_symbol.clone(),
                                }
                            }
                            else {
                                println!("*** using var for closure");
                                Expr::Variable(ident.clone())
                            };

                        /*let mut data_layout = String::new();
                        data_layout.push('n'); // First field is the function pointer, which is not heap-allocated.

                        let function_pointer_symbol = self.symbols.symbol(CLOSURE_FIELD);

                        let mut types = vec![(function_pointer_symbol, Type::Int)];

                        let static_link_ident = self.symbols.symbol(STATIC_LINK_VAR);
                        let (init, typ) = self.access_static_link(&escaping_vars, level, &func_level);
                        let mut env_fields = BTreeSet::new();
                        env_fields.insert(ClosureField {
                            ident: static_link_ident,
                            typ: typ.expect("static link"),
                        });
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
                        let record_type = Type::Record {
                            data_layout,
                            name: closure_symbol,
                            types,
                            unique: Unique::new(),
                        };
                        self.env.enter_type(closure_symbol, record_type.clone());

                        self.closure_index += 1;

                        let mut fields = vec![WithPos::new(RecordField {
                            expr: WithPos::new(Expr::FunctionPointer {
                                label: label.clone(),
                            }, pos),
                            ident: function_pointer_symbol,
                        }, pos)];
                        for field in &env_fields {
                            fields.push(WithPos::new(RecordField {
                                expr: WithPos::new(Expr::Variable(WithPos::new(field.ident, pos)), pos),
                                ident: field.ident,
                            }, pos));
                        }

                        // TODO: add static link.
                        let declarations = vec![
                            WithPos::new(Declaration::VariableDeclaration {
                                escape: true,
                                name: static_link_ident,
                                init,
                                typ: None,
                            }, pos)
                        ];
                        let closure = self.trans_exp(&WithPos::new(Expr::Let {
                            declarations,
                            body: Box::new(WithPos::new(Expr::Record {
                                fields,
                                typ: WithPos::new(closure_symbol, pos),
                            }, pos)),
                        }, pos), level, done_label, true);*/

                        let ty = Type::Function {
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
            Expr::While { ref body, ref test } => {
                if self.in_pure_fun {
                    self.add_error(Error::NoLoopInPureFun {
                        pos,
                    });
                    return EXP_TYPE_ERROR;
                }

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
            Ty::Function { ref parameters, ref return_type } => {
                let parameters = parameters.iter()
                    .map(|param| self.trans_ty(name, param))
                    .collect();
                Type::Function {
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

    fn access_static_link(&mut self, escaping_vars: &BTreeSet<ClosureField>, current_level: &Level<F>, function_level: &Level<F>) -> (ExprWithPos, Option<Type>) {
        let static_link_symbol = self.symbols.symbol(STATIC_LINK_VAR);
        let entry = self.env.look_var(static_link_symbol).cloned();
        if *function_level == *current_level {
            let typ =
                match entry {
                    Some(Entry::Var { ref typ, .. }) => typ.clone(),
                    Some(Entry::RecordField { ref record }) => {
                        let fields =
                            match *record {
                                Type::Record { ref types, .. } => types,
                                _ => unreachable!(),
                            };
                        let mut field_type = None;
                        for &(name, ref typ) in fields {
                            if name == static_link_symbol {
                                field_type = Some(typ);
                            }
                        }
                        println!("************************************** Record field {:?}", field_type);
                        field_type.expect("field type").clone()
                    },
                    _ => unreachable!("{:#?}", entry),
                };
            // For a recursive call, we simply pass the current static link, which represents the stack
            // frame of the parent function.
            println!("=================================== 0. Type for {:?} is {:?}", self.strings.get(static_link_symbol), typ);
            return (WithPos::dummy(Expr::Variable(WithPos::dummy(static_link_symbol))), Some(typ));
        }

        let static_link_type_symbol = self.symbols.symbol(&format!("StaticLink{}", self.static_link_index));
        self.static_link_index += 1;

        if function_level.parent.as_deref() == Some(current_level) {
            // When calling a function defined in the current frame, simply pass the current static link.
            match entry {
                Some(Entry::Var { typ: Type::Record { .. }, .. }) => {
                    let typ =
                        match entry {
                            Some(Entry::Var { ref typ, .. }) => typ.clone(),
                            _ => unreachable!(),
                        };
                    println!("=================================== 1. Type for {:?} is {:?}", self.strings.get(static_link_symbol), typ);
                    (WithPos::dummy(
                        Expr::Variable(WithPos::dummy(static_link_symbol)),
                    ), Some(typ))
                },
                _ => {
                    let mut data_layout = String::new();
                    // TODO: put that in a function since it's duplicated above.
                    let typ =
                        match entry {
                            Some(Entry::Var { ref typ, .. }) => typ.clone(),
                            Some(Entry::RecordField { ref record }) => {
                                let fields =
                                    match *record {
                                        Type::Record { ref types, .. } => types,
                                        _ => unreachable!(),
                                    };
                                let mut field_type = None;
                                for &(name, ref typ) in fields {
                                    if name == static_link_symbol {
                                        field_type = Some(typ);
                                    }
                                }
                                println!("************************************** Record field {:?}", field_type);
                                field_type.expect("field type").clone()
                            },
                            _ => unreachable!("{:#?}", entry),
                        };
                    let mut types = vec![(static_link_symbol, typ.clone())];
                    data_layout.push('p');
                    for field in escaping_vars {
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

                    self.logger.print(&format!("Static link creating types: {:#?}", types));
                    let record_type = Type::Record {
                        data_layout,
                        name: static_link_type_symbol,
                        types,
                        unique: Unique::new(),
                    };
                    self.env.enter_type(static_link_type_symbol, record_type.clone());
                    println!("=================================== 3. Type for {:?} is {:?}", self.strings.get(static_link_symbol), record_type);

                    let mut fields = vec![];
                    fields.push(WithPos::dummy(RecordField {
                        expr: WithPos::dummy(Expr::Variable(WithPos::dummy(static_link_symbol))),
                        ident: static_link_symbol,
                    }));
                    for field in escaping_vars {
                        self.logger.print(&format!("Field {} {:?}", fields.len(), self.strings.get(field.ident)));
                        fields.push(WithPos::dummy(RecordField {
                            expr: WithPos::dummy(Expr::DirectVariable(WithPos::dummy(field.ident))),
                            ident: field.ident,
                        }));
                    }
                    (WithPos::dummy(Expr::Record {
                        fields,
                        typ: WithPos::dummy(static_link_type_symbol),
                    }), Some(record_type))
                }
            }
        }
        else {
            self.logger.print("else");
            let mut static_link = WithPos::dummy(Expr::Variable(WithPos::dummy(static_link_symbol)));
            // When calling a function defined in a parent frame, go up throught the static links.
            let mut typ =
                match entry {
                    Some(Entry::Var { ref typ, .. }) => Some(typ.clone()),
                    _ => None,
                };
            println!("=================================== 4 Type for {:?} is {:?}", self.strings.get(static_link_symbol), typ);

            {
                // We start at the parent level, because the static link contains variables from the
                // parent scope.
                let mut current_level = current_level.parent.as_ref().unwrap_or_else(|| panic!("function level should have a parent"));
                // We want to reach the level in which the function is defined.
                let target_level = function_level.parent.as_ref().unwrap_or_else(|| panic!("function level should have a parent"));
                while current_level != target_level {
                    static_link = WithPos::dummy(Expr::Field {
                        ident: WithPos::dummy(static_link_symbol),
                        this: Box::new(static_link),
                    });
                    if let Some(ref mut typ) = typ {
                        if let Type::Record { types, .. } = typ.clone() {
                            for (ident, field_type) in types {
                                if ident == static_link_symbol {
                                    println!("=================================== 5 Type for {:?} is {:?}", self.strings.get(static_link_symbol), field_type);
                                    *typ = field_type;
                                }
                            }
                        }
                    }
                    match current_level.parent {
                        Some(ref parent) => current_level = parent,
                        None => unreachable!("variable not found in any scope"),
                    }
                }
            }
            (static_link, typ)
        }
    }

    fn find_escaping_vars(&self) -> BTreeSet<ClosureField> {
        if let Some(escaping_vars) = self.escaping_vars.last() {
            escaping_vars.iter()
                .cloned()
                .collect()
        }
        else {
            BTreeSet::new()
        }
    }

    fn enter_static_link_param(&mut self, level: &Level<F>, static_link: &BTreeSet<ClosureField>) {
        let static_link_symbol = self.symbols.symbol(STATIC_LINK_VAR);
        let access = level.formals().last().expect("static link access").clone();

        let entry = self.env.look_var(static_link_symbol).cloned();
        let static_link_type =
            if let Some(Entry::Var { ref typ, .. }) = entry {
                typ.clone()
            }
            else {
                // Main function doesn't have a static link, create an empty one.
                Type::StaticLink {
                    data_layout: self.gen.string_literal(String::new()),
                    types: vec![],
                }
            };

        let mut data_layout = String::new();
        let mut types = vec![(static_link_symbol, static_link_type)];
        for var in static_link.iter() {
            if var.typ.is_pointer() {
                data_layout.push('p');
            }
            else {
                data_layout.push('n');
            }
            types.push((var.ident, var.typ.clone()));
        }
        let data_layout = self.gen.string_literal(data_layout);

        self.env.enter_var(static_link_symbol, Entry::Var { access, typ: Type::StaticLink {
            data_layout,
            types,
        }});
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

    fn variable_access(&mut self, var_name: &SymbolWithPos, access: gen::Access<F>, level: &Level<F>, done_label: Option<Label>) -> Exp {
        let escape = self.env.look_escape(var_name.node);
        if !escape {
            return simple_var(access, level);
        }
        let mut function_level = level;
        let static_link_symbol = self.symbols.symbol(STATIC_LINK_VAR);
        if access.0 == *function_level {
            return simple_var(access, level);
        }

        // We start at the parent level, because the variable is not in the current frame.
        function_level = function_level.parent.as_ref().unwrap_or_else(|| panic!("function level should have a parent"));
        let var_level = access.0;

        self.logger.print(&format!("Variable {:?}", self.strings.get(var_name.node)));

        let mut var = Expr::Variable(WithPos::dummy(static_link_symbol));
        // Access the static link of each parent frame.
        while *function_level != var_level {
            self.logger.print("================================================== loop");
            var = Expr::Field {
                ident: WithPos::dummy(static_link_symbol),
                this: Box::new(WithPos::dummy(var)),
            };
            function_level = function_level.parent.as_ref().unwrap_or_else(|| panic!("function level should have a parent"));
        }
        var = Expr::Field {
            ident: var_name.clone(),
            this: Box::new(WithPos::dummy(var)),
        };

        self.trans_exp(&WithPos::dummy(var), level, done_label, true).exp
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
    escaping_vars: BTreeSet<ClosureField>,
    fields: BTreeSet<ClosureField>,
}

impl<'a, F: Frame> EnvFinder<'a, F> {
    fn new(env: &'a Env<F>, closure_level: Level<F>) -> Self {
        Self {
            closure_level,
            env,
            escaping_vars: BTreeSet::new(),
            fields: BTreeSet::new(),
        }
    }
}

#[allow(clippy::needless_lifetimes)]
impl<'a, F: Frame + PartialEq> Visitor for EnvFinder<'a, F> {
    fn visit_call(&mut self, function: &ExprWithPos, args: &[ExprWithPos]) {
        self.visit_exp(function);
        if let Expr::Variable(ref ident) = function.node {
            if let Some(&Entry::Fun { ref escaping_vars, .. }) = self.env.look_var(ident.node) {
                for var in escaping_vars {
                    if let Some(&Entry::Var { ref access, ref typ, }) = self.env.look_var(var.ident) {
                        if self.closure_level.current != access.0.current {
                            self.escaping_vars.insert(ClosureField {
                                ident: var.ident,
                                typ: typ.clone(),
                            });
                        }
                    }
                }
            }
        }
        for arg in args {
            self.visit_exp(arg);
        }
    }

    fn visit_var(&mut self, ident: &SymbolWithPos) {
        if let Some(&Entry::Var { ref access, ref typ, }) = self.env.look_var(ident.node) {
            if self.closure_level.current != access.0.current {
                self.fields.insert(ClosureField {
                    ident: ident.node,
                    typ: typ.clone(),
                });
            }
        }
    }
}

fn find_closure_environment<F: Frame + PartialEq>(exp: &ExprWithPos, env: &Env<F>, closure_level: Level<F>) -> (BTreeSet<ClosureField>, BTreeSet<ClosureField>) {
    let mut finder = EnvFinder::new(env, closure_level);
    finder.visit_exp(exp);
    (finder.fields, finder.escaping_vars)
}

fn type_is_collectable(typ: &Type) -> bool {
    match *typ {
        Type::Array { .. } | Type::Function { .. } | Type::Record { .. } | Type::StaticLink { .. } | Type::String => true,
        _ => false,
    }
}
