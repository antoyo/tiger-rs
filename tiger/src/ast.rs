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
    Array {
        init: Box<Expr>,
        pos: Pos,
        size: Box<Expr>,
        typ: Symbol,
    },
    Assign {
        expr: Box<Expr>,
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
    Int {
        pos: Pos,
        value: i64,
    },
    Let {
        body: Box<Expr>,
        declarations: Vec<Declaration>,
        pos: Pos,
    },
    Nil,
    Oper {
        left: Box<Expr>,
        oper: Operator,
        oper_pos: Pos,
        right: Box<Expr>,
    },
    Record {
        fields: Vec<RecordField>,
        pos: Pos,
        typ: Symbol,
    },
    Sequence(Vec<Expr>),
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
pub struct Field {
    pub escape: bool,
    pub name: Symbol,
    pub pos: Pos,
    pub typ: Symbol,
}

#[derive(Debug)]
pub struct FuncDeclaration {
    pub body: Expr,
    pub name: Symbol,
    pub params: Vec<Field>,
    pub pos: Pos,
    pub result: Option<Ident>,
}

#[derive(Debug)]
pub struct Ident {
    pub ident: Symbol,
    pub pos: Pos,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct RecordField {
    pub expr: Expr,
    pub ident: Symbol,
    pub pos: Pos,
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
    Record {
        fields: Vec<Field>,
        pos: Pos,
    },
}

#[derive(Debug)]
pub enum Var {
    Field {
        ident: Symbol,
        this: Box<Var>,
    },
    Simple {
        ident: Symbol,
        pos: Pos,
    },
    Subscript {
        expr: Box<Expr>,
        this: Box<Var>,
    },
}
