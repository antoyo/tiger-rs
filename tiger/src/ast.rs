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

use position::Pos;
use symbol::Symbol;

#[derive(Debug)]
pub enum Declaration {
    Function(Vec<FuncDeclaration>),
    Type {
        name: Symbol,
        pos: Pos,
        ty: Ty,
    },
    VariableDeclaration {
        escape: bool,
        init: Expr,
        name: Symbol,
        pos: Pos,
        typ: Option<Ident>,
    },
}

#[derive(Debug)]
pub enum Expr {
    ArrayExpr {
        init: Box<Expr>,
        pos: Pos,
        size: Box<Expr>,
        typ: Symbol,
    },
    Assign {
        expr: Box<Expr>,
        pos: Pos,
        var: Var,
    },
    Break(Pos),
    Call {
        args: Vec<Expr>,
        function: Symbol,
        pos: Pos,
    },
    For {
        body: Box<Expr>,
        end: Box<Expr>,
        escape: bool,
        pos: Pos,
        start: Box<Expr>,
        var: Symbol,
    },
    If {
        else_: Option<Box<Expr>>,
        pos: Pos,
        test: Box<Expr>,
        then: Box<Expr>,
    },
    Int(i64),
    Let {
        body: Box<Expr>,
        declarations: Vec<Declaration>,
        pos: Pos,
    },
    Nil,
    Oper {
        left: Box<Expr>,
        oper: Operator,
        pos: Pos,
        right: Box<Expr>,
    },
    RecordExpr {
        fields: Vec<RecordField>,
        pos: Pos,
        typ: Symbol,
    },
    Sequence(Vec<ExprPos>),
    Str {
        pos: Pos,
        value: String,
    },
    Variable(Var),
    While {
        body: Box<Expr>,
        pos: Pos,
        test: Box<Expr>,
    },
}

#[derive(Debug)]
pub struct ExprPos {
    expr: Expr,
    pos: Pos,
}

#[derive(Debug)]
pub struct Field {
    escape: bool,
    name: Symbol,
    pos: Pos,
    typ: Symbol,
}

#[derive(Debug)]
pub struct FuncDeclaration {
    body: Expr,
    name: Symbol,
    params: Vec<Field>,
    pos: Pos,
    result: Option<Ident>,
}

#[derive(Debug)]
pub struct Ident {
    ident: Symbol,
    pos: Pos,
}

#[derive(Debug)]
pub enum Operator {
    Divide,
    Equal,
    Ge,
    Gt,
    Le,
    Lt,
    Minus,
    Neq,
    Plus,
    Times,
}

#[derive(Debug)]
pub struct RecordField {
    expr: Expr,
    ident: Symbol,
    pos: Pos,
}

#[derive(Debug)]
pub enum Ty {
    Array {
        ident: Symbol,
        pos: Pos,
    },
    Name {
        ident: Symbol,
        pos: Pos,
    },
    Record(Vec<Field>),
}

#[derive(Debug)]
pub enum Var {
    Field {
        ident: Symbol,
        pos: Pos,
        this: Box<Var>,
    },
    Simple {
        ident: Symbol,
        pos: Pos,
    },
    Subscript {
        expr: Box<Expr>,
        pos: Pos,
        this: Box<Var>,
    },
}
