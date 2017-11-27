/*
 * Copyright (c) 2017 Boucher, Antoni <bouanto@zoho.com>
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

use std::collections::HashSet;

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
    Var,
    VarWithPos,
};
use env::{Env, Entry};
use error::{Error, Result};
use position::{Pos, WithPos};
use self::AddError::*;
use symbol::{Symbol, SymbolWithPos};
use tast;
use types::{Type, Unique};

#[derive(PartialEq)]
enum AddError {
    AddError,
    DontAddError,
}

pub struct ExpTy {
    pub exp: tast::Exp,
    pub ty: Type,
}

const EXP_TYPE_ERROR: ExpTy =
    ExpTy {
        exp: (),
        ty: Type::Error,
    };

pub struct SemanticAnalyzer<'a> {
    env: &'a mut Env,
    errors: Vec<Error>,
    in_loop: bool,
}

impl<'a> SemanticAnalyzer<'a> {
    pub fn new(env: &'a mut Env) -> Self {
        SemanticAnalyzer {
            env,
            errors: vec![],
            in_loop: false,
        }
    }

    fn add_error<T>(&mut self, error: Error, node: T) -> T {
        self.errors.push(error);
        node
    }

    pub fn analyze(mut self, expr: &ExprWithPos) -> Result<ExpTy> {
        let exp = self.trans_exp(expr);
        if self.errors.is_empty() {
            Ok(exp)
        }
        else {
            Err(self.errors)
        }
    }

    fn actual_ty(&mut self, typ: &Type) -> Type {
        match *typ {
            Type::Name(_, Some(ref typ)) => *typ.clone(),
            Type::Name(ref symbol, None) => self.get_type(symbol, DontAddError),
            ref typ => typ.clone(),
        }
    }

    fn actual_ty_var(&mut self, typ: &Type) -> Type {
        let typ =
            match *typ {
                Type::Name(_, Some(ref typ)) => *typ.clone(),
                Type::Name(ref symbol, None) =>
                    match self.get_var(symbol) {
                        Entry::Var { ref typ } => typ.clone(),
                        _ => panic!("type should be a variable, not a function"),
                    },
                ref typ => typ.clone(),
            };
        typ
    }

    fn check_binary_op(&mut self, left: &ExprWithPos, right: &ExprWithPos) -> ExpTy {
        let left_pos = left.pos;
        let left = self.trans_exp(left);
        self.check_int(left, left_pos);
        let right_pos = right.pos;
        let right = self.trans_exp(right);
        self.check_int(right, right_pos);
        ExpTy {
            exp: (),
            ty: Type::Int,
        }
    }

    fn check_duplicate_types(&mut self, types: &[TypeDecWithPos]) {
        let mut names = HashSet::new();
        for typ in types {
            names.insert(typ.node.name.node);
            if let Ty::Name { ref ident } = typ.node.ty.node {
                if names.contains(&ident.node) {
                    return self.add_error(Error::Cycle {
                        pos: typ.node.ty.pos,
                    }, ());
                }
            }
        }
    }

    fn check_int(&mut self, expr: ExpTy, pos: Pos) {
        if expr.ty != Type::Int && expr.ty != Type::Error {
            return self.add_error(Error::Type {
                expected: Type::Int,
                pos,
                unexpected: expr.ty,
            }, ());
        }
    }

    fn check_types(&mut self, expected: &Type, unexpected: &Type, pos: Pos) {
        let expected = self.actual_ty(expected);
        let unexpected = self.actual_ty(unexpected);
        if expected != unexpected && expected != Type::Error && unexpected != Type::Error {
            if let Type::Record(_, _, _) = expected {
                if unexpected == Type::Nil {
                    return;
                }
            }
            return self.add_error(Error::Type {
                expected: expected.clone(),
                pos,
                unexpected: unexpected.clone(),
            }, ());
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

    fn get_var(&mut self, symbol: &SymbolWithPos) -> Entry {
        if let Some(entry) = self.env.look_var(symbol.node) {
            return entry.clone();
        }
        self.undefined_identifier(symbol)
    }

    fn trans_dec(&mut self, declaration: &DeclarationWithPos) {
        match declaration.node {
            Declaration::Function(ref declarations) => {
                for &WithPos { node: FuncDeclaration { name, ref params, ref result, .. }, .. } in declarations {
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
                    self.env.enter_var(name, Entry::Fun { parameters, result: result_type.clone() });
                }

                for &WithPos { node: FuncDeclaration { ref params, ref body, ref result, .. }, .. } in declarations {
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
                    for (param, name) in parameters.drain(..).zip(param_names) {
                        self.env.enter_var(name, Entry::Var { typ: param.clone() });
                    }
                    let ExpTy { ty, .. } = self.trans_exp(body);
                    self.check_types(&result_type, &ty, body.pos);
                    self.env.end_scope();
                }
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
            },
            Declaration::VariableDeclaration { ref init, name, ref typ, .. } => {
                let ExpTy { ty, .. } = self.trans_exp(init);
                if let Some(ref ident) = *typ {
                    let typ = self.get_type(ident, AddError);
                    self.check_types(&typ, &ty, ident.pos);
                } else if ty == Type::Nil {
                    return self.add_error(Error::RecordType { pos: declaration.pos }, ());
                }
                self.env.enter_var(name, Entry::Var { typ: ty });
            },
        }
    }

    pub fn trans_exp(&mut self, expr: &ExprWithPos) -> ExpTy {
        match expr.node {
            Expr::Array { ref init, ref size, ref typ } => {
                let size_expr = self.trans_exp(size);
                self.check_int(size_expr, size.pos);
                let ty = self.get_type(typ, AddError);
                let init_expr = self.trans_exp(init);
                match ty {
                    Type::Array(ref typ, _) =>
                        self.check_types(typ, &init_expr.ty, init.pos),
                    Type::Error => (),
                    _ =>
                        return self.add_error(Error::UnexpectedType {
                            kind: "array".to_string(),
                            pos: typ.pos,
                        }, EXP_TYPE_ERROR),
                }
                ExpTy {
                    exp: (),
                    ty,
                }
            },
            Expr::Assign { ref expr, ref var } => {
                let var = self.trans_var(var);
                let expr_expr = self.trans_exp(expr);
                self.check_types(&var.ty, &expr_expr.ty, expr.pos);
                ExpTy {
                    exp: (),
                    ty: Type::Unit,
                }
            },
            Expr::Break => {
                if !self.in_loop {
                    return self.add_error(Error::BreakOutsideLoop {
                        pos: expr.pos,
                    }, EXP_TYPE_ERROR);
                }
                ExpTy {
                    exp: (),
                    ty: Type::Unit,
                }
            },
            Expr::Call { ref args, function } => {
                if let Some(entry@Entry::Fun { .. }) = self.env.look_var(function).cloned() { // TODO: remove this clone.
                    return match entry {
                        Entry::Fun { ref parameters, ref result } => {
                            for (arg, param) in args.iter().zip(parameters) {
                                let exp = self.trans_exp(arg);
                                self.check_types(param, &exp.ty, arg.pos);
                            }
                            ExpTy {
                                exp: (),
                                ty: self.actual_ty_var(result),
                            }
                        },
                        _ => unreachable!(),
                    };
                }
                return self.undefined_function(function, expr.pos);
            },
            Expr::For { ref body, ref end, ref start, var, .. } => {
                let start_expr = self.trans_exp(start);
                self.check_int(start_expr, start.pos);
                let end_expr = self.trans_exp(end);
                self.check_int(end_expr, end.pos);
                self.env.begin_scope();
                self.env.enter_var(var, Entry::Var { typ: Type::Int });
                let old_in_loop = self.in_loop;
                self.in_loop = true;
                let result = self.trans_exp(body);
                self.in_loop = old_in_loop;
                self.env.end_scope();
                result
            },
            Expr::If { ref else_, ref test, ref then } => {
                let test_expr = self.trans_exp(test);
                self.check_int(test_expr, then.pos);
                let if_expr = self.trans_exp(then);
                let ty =
                    match *else_ {
                        Some(ref else_) => {
                            let else_expr = self.trans_exp(&else_);
                            self.check_types(&if_expr.ty, &else_expr.ty, else_.pos);
                            if_expr.ty
                        },
                        None => {
                            self.check_types(&Type::Unit, &if_expr.ty, then.pos);
                            Type::Unit
                        },
                    };
                ExpTy {
                    exp: (),
                    ty,
                }
            },
            Expr::Int { .. } =>
                ExpTy {
                    exp: (),
                    ty: Type::Int,
                },
            Expr::Let { ref body, ref declarations } => {
                let old_in_loop = self.in_loop;
                self.in_loop = false;
                self.env.begin_scope();
                for declaration in declarations {
                    self.trans_dec(declaration);
                }
                self.in_loop = old_in_loop;
                let result = self.trans_exp(body);
                self.env.end_scope();
                result
            },
            Expr::Nil =>
                ExpTy {
                    exp: (),
                    ty: Type::Nil,
                },
            Expr::Oper { ref left, oper: WithPos { node: Operator::Plus, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: Operator::Minus, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: Operator::Times, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: Operator::Lt, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: Operator::Gt, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: Operator::And, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: Operator::Or, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: Operator::Ge, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: Operator::Le, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: Operator::Divide, .. }, ref right } =>
                self.check_binary_op(left, right),
            Expr::Oper { ref left, oper: WithPos { node: Operator::Equal, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: Operator::Neq, .. }, ref right } => {
                let left = self.trans_exp(left);
                let right_pos = right.pos;
                let right = self.trans_exp(right);
                self.check_types(&left.ty, &right.ty, right_pos);
                ExpTy {
                    exp: (),
                    ty: Type::Int,
                }
            },
            Expr::Record { ref fields, ref typ } => {
                let ty = self.get_type(typ, AddError);
                match ty {
                    Type::Record(_, ref type_fields, _) => {
                        for &(type_field_name, ref type_field) in type_fields {
                            let mut found = false;
                            for field in fields {
                                if type_field_name == field.node.ident {
                                    found = true;
                                    let field_expr = self.trans_exp(&field.node.expr);
                                    self.check_types(&type_field, &field_expr.ty, field.node.expr.pos);
                                }
                            }
                            if !found {
                                return self.missing_field(type_field_name, typ);
                            }
                        }

                        for field in fields {
                            let found = type_fields.iter()
                                .any(|&(type_field_name, _)| field.node.ident == type_field_name);
                            if !found {
                                return self.extra_field(field, typ);
                            }
                        }
                    },
                    Type::Error => (),
                    _ =>
                        return self.add_error(Error::UnexpectedType {
                            kind: "record".to_string(),
                            pos: typ.pos,
                        }, EXP_TYPE_ERROR),
                }
                ExpTy {
                    exp: (),
                    ty,
                }
            },
            Expr::Sequence(ref exprs) => {
                if let Some((last_expr, exprs)) = exprs.split_last() {
                    for expr in exprs {
                        self.trans_exp(expr);
                    }
                    self.trans_exp(last_expr)
                }
                else {
                    panic!("Unexpected empty sequence.");
                }
            },
            Expr::Str { .. } =>
                ExpTy {
                    exp: (),
                    ty: Type::String,
                },
            Expr::Variable(ref var) => self.trans_var(var),
            Expr::While { ref body, ref test } => {
                let test_expr = self.trans_exp(test);
                self.check_int(test_expr, test.pos);
                let old_in_loop = self.in_loop;
                self.in_loop = true;
                let result = self.trans_exp(body);
                self.in_loop = old_in_loop;
                result
            },
        }
    }

    fn trans_ty(&mut self, symbol: Symbol, ty: &TyWithPos) -> Type {
        match ty.node {
            Ty::Array { ref ident } => {
                let ty = self.get_type(ident, AddError);
                Type::Array(Box::new(ty), Unique::new())
            },
            Ty::Name { ref ident } => self.get_type(ident, AddError),
            Ty::Record { ref fields } => {
                let mut record_fields = vec![];
                for field in fields {
                    let typ = self.get_type(&field.node.typ, AddError);
                    record_fields.push((field.node.name, typ));
                }
                Type::Record(symbol, record_fields, Unique::new())
            },
        }
    }

    fn trans_var(&mut self, var: &VarWithPos) -> ExpTy {
        match var.node {
            Var::Field { ref ident, ref this } => {
                let var = self.trans_var(this);
                match var.ty {
                    Type::Record(record_type, ref fields, _) => {
                        for &(name, ref typ) in fields {
                            if name == ident.node {
                                return ExpTy {
                                    exp: (),
                                    ty: typ.clone(),
                                };
                            }
                        }
                        self.unexpected_field(ident, ident.pos, record_type)
                    },
                    typ =>
                        return self.add_error(Error::NotARecord {
                            pos: this.pos,
                            typ: typ.to_string(),
                        }, EXP_TYPE_ERROR),
                }
            },
            Var::Simple { ident } => {
                if let Some(Entry::Var { ref typ }) = self.env.look_var(ident).cloned() { // TODO: remove this clone.
                    return ExpTy {
                        exp: (),
                        ty: self.actual_ty_var(typ),
                    };
                }
                self.undefined_variable(ident, var.pos)
            },
            Var::Subscript { ref expr, ref this } => {
                let var = self.trans_var(this);
                let expr_expr = self.trans_exp(expr);
                self.check_int(expr_expr, expr.pos);
                match var.ty {
                    Type::Array(typ, _) => ExpTy {
                        exp: (),
                        ty: self.actual_ty_var(&typ),
                    },
                    Type::Error => ExpTy {
                        exp: (),
                        ty: Type::Error,
                    },
                    typ =>
                        self.add_error(Error::CannotIndex {
                            pos: this.pos,
                            typ: typ.to_string(),
                        }, EXP_TYPE_ERROR),
                }
            },
        }
    }

    fn duplicate_param(&mut self, param: &FieldWithPos) {
        let ident = self.env.var_name(param.node.name).to_string();
        self.add_error(Error::DuplicateParam {
            ident,
            pos: param.pos,
        }, ())
    }

    fn extra_field(&mut self, field: &RecordFieldWithPos, typ: &SymbolWithPos) -> ExpTy {
        let ident = self.env.type_name(field.node.ident);
        let struct_name = self.env.type_name(typ.node);
        self.add_error(Error::ExtraField {
            ident,
            pos: field.pos,
            struct_name,
        }, EXP_TYPE_ERROR)
    }

    fn missing_field(&mut self, field_type: Symbol, typ: &SymbolWithPos) -> ExpTy {
        let ident = self.env.type_name(field_type);
        let struct_name = self.env.type_name(typ.node);
        self.add_error(Error::MissingField {
            ident,
            pos: typ.pos,
            struct_name,
        }, EXP_TYPE_ERROR)
    }

    fn undefined_function(&mut self, ident: Symbol, pos: Pos) -> ExpTy {
        let ident = self.env.var_name(ident).to_string();
        self.add_error(Error::Undefined {
            ident,
            item: "function".to_string(),
            pos,
        }, EXP_TYPE_ERROR)
    }

    fn undefined_identifier(&mut self, symbol: &SymbolWithPos) -> Entry {
        let ident = self.env.type_name(symbol.node);
        self.add_error(Error::Undefined {
            ident,
            item: "identifier".to_string(),
            pos: symbol.pos,
        }, Entry::Error)
    }

    fn undefined_type(&mut self, symbol: &SymbolWithPos) -> Type {
        let ident = self.env.type_name(symbol.node);
        self.add_error(Error::Undefined {
            ident,
            item: "type".to_string(),
            pos: symbol.pos,
        }, Type::Error)
    }

    fn undefined_variable(&mut self, ident: Symbol, pos: Pos) -> ExpTy {
        let ident = self.env.var_name(ident).to_string();
        self.add_error(Error::Undefined {
            ident,
            item: "variable".to_string(),
            pos,
        }, EXP_TYPE_ERROR)
    }

    fn unexpected_field(&mut self, ident: &SymbolWithPos, pos: Pos, typ: Symbol) -> ExpTy {
        let ident = self.env.type_name(ident.node);
        let struct_name = self.env.type_name(typ);
        self.add_error(Error::UnexpectedField {
            ident,
            pos,
            struct_name,
        }, EXP_TYPE_ERROR)
    }
}
