/*
 * Copyright (c) 2019 Boucher, Antoni <bouanto@zoho.com>
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

use std::rc::Rc;

use frame::Frame;
use symbol::{Strings, Symbol};
use temp::{Label, Temp};

#[derive(Debug)]
pub enum Instruction {
    Call {
        assembly: String,
        destination: Vec<Temp>,
        source: Vec<Temp>,
        return_label: Label,
    },
    Debug {
        filename: Symbol,
        line: i32,
    },
    Operation {
        assembly: String,
        destination: Vec<Temp>,
        source: Vec<Temp>,
        stack_destination: Vec<i64>,
        stack_source: Vec<i64>,
        jump: Option<Vec<Label>>,
    },
    Label {
        assembly: String,
        label: Label,
    },
    Move {
        assembly: String,
        destination: Vec<Temp>,
        source: Vec<Temp>,
        stack_destination: Vec<i64>,
        stack_source: Vec<i64>,
    },
}

impl Instruction {
    pub fn to_string<F: Frame>(&self, strings: Rc<Strings>) -> String {
        match *self {
            Instruction::Debug { filename, line } => format!("%line {} {}", line, strings.get(filename).expect("filename")),
            Instruction::Label { ref assembly, .. } => assembly.clone(),
            Instruction::Call { ref assembly, ref destination, ref source, .. } |
                Instruction::Move { ref assembly, ref destination, ref source, .. } |
                Instruction::Operation { ref assembly, ref destination, ref source, .. } =>
            {
                let mut result = assembly.clone();
                for (index, temp) in destination.iter().enumerate() {
                    result = result.replace(&format!("'d{}", index), &temp.to_string::<F>());
                }
                for (index, temp) in source.iter().enumerate() {
                    result = result.replace(&format!("'s{}", index), &temp.to_string::<F>());
                }
                result
            },
        }
    }
}

pub struct Subroutine {
    pub prolog: String,
    pub body: Vec<Instruction>,
    pub epilog: String,
}
