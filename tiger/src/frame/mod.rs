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

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;

use ir::{Exp, Statement};
use temp::{Label, Temp};

pub mod x86_64;

pub enum Fragment<F: Frame> {
    Function {
        body: Statement,
        frame: Rc<RefCell<F>>,
    },
    Str(Label, String),
}

pub trait Frame: Clone {
    type Access: Clone + Debug;

    const WORD_SIZE: i64;

    fn registers() -> Vec<Temp>;
    fn register_count() -> usize;
    fn temp_map() -> HashMap<Temp, &'static str>;
    fn special_name(temp: Temp) -> Option<&'static str>;

    fn fp() -> Temp;
    fn return_value() -> Temp;

    fn new(name: Label, formals: Vec<bool>) -> Self;

    fn name(&self) -> Label;

    fn formals(&self) -> &[Self::Access];

    fn alloc_local(&mut self, escape: bool) -> Self::Access;

    fn exp(&self, access: Self::Access, stack_frame: Exp) -> Exp;

    fn external_call(name: &str, arguments: Vec<Exp>) -> Exp;
}
