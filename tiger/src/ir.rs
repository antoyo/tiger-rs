/*
 * Copyright (c) 2018-2019 Boucher, Antoni <bouanto@zoho.com>
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

#![allow(dead_code)]

use temp::{Label, Temp};

#[derive(Clone, Debug, PartialEq)]
pub enum Exp {
    Const(i64),
    /// Dummy expression to return when there is an error.
    Error,
    Name(Label),
    Temp(Temp),
    BinOp {
        op: BinOp,
        left: Box<Exp>,
        right: Box<Exp>,
    },
    Mem(Box<Exp>),
    Call {
        arguments: Vec<Exp>,
        function_expr: Box<Exp>,
    },
    ExpSequence(Box<Statement>, Box<Exp>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Move(Exp, Exp),
    Exp(Exp),
    Jump(Exp, Vec<Label>),
    CondJump {
        op: RelationalOp,
        left: Exp,
        right: Exp,
        true_label: Label,
        false_label: Label,
    },
    Sequence(Box<Statement>, Box<Statement>),
    Label(Label),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinOp {
    Plus,
    Minus,
    Mul,
    Div,
    And,
    Or,
    ShiftLeft,
    ShiftRight,
    ArithmeticShiftRight,
    Xor,
}

#[derive(Clone, Debug, PartialEq)]
pub enum RelationalOp {
    Equal,
    NotEqual,
    LesserThan,
    GreaterThan,
    LesserOrEqual,
    GreaterOrEqual,
    UnsignedLesserThan,
    UnsignedLesserOrEqual,
    UnsignedGreaterThan,
    UnsignedGreaterOrEqual,
}
