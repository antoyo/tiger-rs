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
use std::hash::Hash;
use std::rc::Rc;

use asm::{Instruction, Subroutine};
use ir::{Exp, Statement};
use temp::{Label, Temp, TempMap};

pub mod x86_64;

pub enum Fragment<F: Frame> {
    // TODO: use a fragment for the pointer map?
    Function {
        body: Statement,
        escaping_vars: Vec<i64>,
        frame: Rc<RefCell<F>>,
        temp_map: TempMap,
    },
    Str(Label, String),
}

pub trait Memory {
    fn as_stack(&self) -> Option<i64>;
    fn as_temp(&self) -> Option<&Temp>;
}

pub trait Frame: Clone {
    type Access: Clone + Debug + Eq + Hash + Memory;

    const WORD_SIZE: i64;

    fn registers() -> Vec<Temp>;
    // TODO: This is not used. Check if this is really not needed.
    //fn register_count() -> usize;
    fn temp_map() -> HashMap<Temp, &'static str>;
    fn special_name(temp: Temp) -> Option<&'static str>;

    fn fp() -> Temp;
    fn return_value() -> Temp;

    fn new(name: Label, formals: Vec<bool>) -> Self;

    fn name(&self) -> Label;

    fn formals(&self) -> &[Self::Access];

    fn alloc_local(&mut self, escape: bool) -> Self::Access;

    fn exp(&self, access: Self::Access, stack_frame: Exp) -> Exp;

    fn external_call(name: &str, arguments: Vec<Exp>, collectable_return_type: bool) -> Exp;

    fn proc_entry_exit1(&mut self, statement: Statement) -> Statement;
    fn proc_entry_exit2(&self, instructions: Vec<Instruction>, escaping_vars: Vec<i64>) -> Vec<Instruction>;
    fn proc_entry_exit3(&self, body: Vec<Instruction>) -> Subroutine;
}
