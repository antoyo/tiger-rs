/*
 * Copyright (c) 2018 Boucher, Antoni <bouanto@zoho.com>
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

use ir::BinOp::Plus;
use ir::Exp:: {
    self,
    BinOp,
    Call,
    Const,
    Mem,
    Name,
};
use ir::Statement;
use super::Frame;
use temp::{Label, Temp};

use self::Access::{InFrame, InReg};

const POINTER_SIZE: i64 = 8;

#[derive(Clone)]
pub struct X86_64 {
    formals: Vec<Access>, // Representation of parameters.
    fp: Temp,
    name: Label,
    pointer: i64,
    return_value: Temp,
}

impl PartialEq for X86_64 {
    fn eq(&self, other: &Self) -> bool {
        self.fp == other.fp
    }
}

#[derive(Clone)]
pub enum Access {
    InFrame(i64),
    InReg(Temp),
}

impl Frame for X86_64 {
    type Access = Access;

    const WORD_SIZE: i64 = 8;

    fn fp(&self) -> Temp {
        self.fp
    }

    fn return_value(&self) -> Temp {
        self.return_value
    }

    fn new(name: Label, formals: Vec<bool>) -> Self {
        let mut frame = X86_64 {
            formals: vec![],
            fp: Temp::new(),
            name,
            pointer: 0,
            return_value: Temp::new(),
        };
        let formals = formals.iter()
            .map(|&escape| frame.alloc_local(escape))
            .collect();
        frame.formals = formals;
        frame
    }

    fn name(&self) -> Label {
        Label::new()
    }

    fn formals(&self) -> &[Self::Access] {
        &self.formals
    }

    fn alloc_local(&mut self, escape: bool) -> Self::Access {
        if escape {
            self.pointer -= POINTER_SIZE;
            InFrame(self.pointer)
        }
        else {
            InReg(Temp::new())
        }
    }

    fn exp(&self, access: Self::Access, stack_frame: Exp) -> Exp {
        match access {
            InFrame(pos) => {
                Mem(Box::new(BinOp {
                    op: Plus,
                    left: Box::new(stack_frame),
                    right: Box::new(Const(pos)),
                }))
            },
            InReg(reg) => {
                Exp::Temp(reg)
            },
        }
    }

    fn external_call(name: &str, arguments: Vec<Exp>) -> Exp {
        Call(Box::new(Name(Label::with_name(name))), arguments)
    }

    fn proc_entry_exit1(&self, statement: Statement) -> Statement {
        statement
    }
}
