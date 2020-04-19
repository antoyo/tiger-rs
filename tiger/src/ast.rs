/*
 * Copyright (c) 2017-2019 Boucher, Antoni <bouanto@zoho.com>
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

use position::WithPos;
use symbol::{Symbol, SymbolWithPos};
use temp::Label;

#[allow(clippy::enum_variant_names)]
#[derive(Clone, Debug, PartialEq)]
pub enum Declaration {
    Function(Vec<FuncDeclarationWithPos>),
    Type(Vec<TypeDecWithPos>),
    VariableDeclaration {
        escape: bool, // TODO: is this field actually used?
        init: ExprWithPos,
        name: Symbol,
        typ: Option<SymbolWithPos>,
    },
}

pub type DeclarationWithPos = WithPos<Declaration>;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Array {
        init: Box<ExprWithPos>,
        size: Box<ExprWithPos>,
        typ: SymbolWithPos,
    },
    Assign {
        expr: Box<ExprWithPos>,
        var: Box<ExprWithPos>,
    },
    Break,
    Call {
        args: Vec<ExprWithPos>,
        function: Box<ExprWithPos>,
    },
    CallWithStaticLink {
        args: Vec<ExprWithPos>,
        function: Box<ExprWithPos>,
    },
    Closure {
        body: Box<ExprWithPos>,
        params: Vec<FieldWithPos>,
        pure: bool,
        result: Option<SymbolWithPos>,
    },
    ClosureParamField {
        ident: SymbolWithPos,
        this: Box<ExprWithPos>,
    },
    ClosurePointer {
        label: Symbol,
    },
    DirectVariable(SymbolWithPos),
    Field {
        ident: SymbolWithPos,
        this: Box<ExprWithPos>,
    },
    FunctionPointer {
        label: Label,
    },
    FunctionPointerCall {
        args: Vec<ExprWithPos>,
        closure_name: Symbol,
        function: Box<ExprWithPos>,
    },
    If {
        else_: Option<Box<ExprWithPos>>,
        test: Box<ExprWithPos>,
        then: Box<ExprWithPos>,
    },
    Int {
        value: i64,
    },
    Let {
        body: Box<ExprWithPos>,
        declarations: Vec<DeclarationWithPos>,
    },
    Nil,
    Oper {
        left: Box<ExprWithPos>,
        oper: OperatorWithPos,
        right: Box<ExprWithPos>,
    },
    Record {
        fields: Vec<RecordFieldWithPos>,
        typ: SymbolWithPos,
    },
    Sequence(Vec<ExprWithPos>),
    Str {
        value: String,
    },
    Subscript {
        expr: Box<ExprWithPos>,
        this: Box<ExprWithPos>,
    },
    Variable(SymbolWithPos),
    While {
        body: Box<ExprWithPos>,
        test: Box<ExprWithPos>,
    },
}

pub type ExprWithPos = WithPos<Expr>;

#[derive(Clone, Debug, PartialEq)]
pub struct Field {
    pub escape: bool,
    pub name: Symbol,
    pub typ: SymbolWithPos,
}

pub type FieldWithPos = WithPos<Field>;

#[derive(Clone, Debug, PartialEq)]
pub struct FuncDeclaration {
    pub body: ExprWithPos,
    pub name: SymbolWithPos,
    pub params: Vec<FieldWithPos>,
    pub pure: bool,
    pub result: Option<SymbolWithPos>,
}

pub type FuncDeclarationWithPos = WithPos<FuncDeclaration>;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operator {
    And,
    Divide,
    Equal,
    Ge,
    Gt,
    Le,
    Lt,
    Minus,
    Neq,
    Or,
    Plus,
    Times,
}

pub type OperatorWithPos = WithPos<Operator>;

#[derive(Clone, Debug, PartialEq)]
pub struct RecordField {
    pub expr: ExprWithPos,
    pub ident: Symbol,
}

pub type RecordFieldWithPos = WithPos<RecordField>;

#[derive(Clone, Debug, PartialEq)]
pub enum Ty {
    Array {
        ident: SymbolWithPos,
    },
    Function {
        parameters: Vec<TyWithPos>,
        return_type: Box<TyWithPos>,
    },
    Name {
        ident: SymbolWithPos,
    },
    Record {
        fields: Vec<FieldWithPos>,
    },
    Unit,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeDec {
    pub name: SymbolWithPos,
    pub ty: TyWithPos,
}

pub type TypeDecWithPos = WithPos<TypeDec>;

pub type TyWithPos = WithPos<Ty>;

pub fn dummy_var_expr(symbol: Symbol) -> ExprWithPos {
    WithPos::dummy(Expr::Variable(WithPos::dummy(symbol)))
}
