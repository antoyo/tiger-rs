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

use super::Frame;
use temp::{Label, Temp};

use self::Access::{InFrame, InReg};

const POINTER_SIZE: i32 = 8;

#[derive(Clone)]
pub struct X86_64 {
    formals: Vec<Access>, // Representation of parameters.
    name: Label,
    pointer: i32,
}

#[derive(Clone)]
pub enum Access {
    InFrame(i32),
    InReg(Temp),
}

impl Frame for X86_64 {
    type Access = Access;

    fn new(name: Label, formals: Vec<bool>) -> Self {
        let mut frame = X86_64 {
            formals: vec![],
            name,
            pointer: 0,
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
}
