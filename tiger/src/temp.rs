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

use std::collections::BTreeSet;
use std::fmt::{self, Display, Formatter};

use frame::{Frame, Memory};
use self::Label::{Named, Num};

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Temp {
    pub num: u32, // TODO: remove pub.
}

impl Temp {
    pub fn new() -> Self {
        static mut COUNTER: u32 = 0;
        unsafe {
            COUNTER += 1;
            Self {
                num: COUNTER,
            }
        }
    }

    #[cfg(test)]
    pub fn from_num(num: u32) -> Self {
        Self {
            num,
        }
    }

    pub fn to_string<F: Frame>(&self) -> String {
        F::special_name(*self)
            .map(ToString::to_string)
            .unwrap_or_else(|| format!("t{}", self.num))
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Label {
    Named(String),
    Num(u32),
}

impl Label {
    pub fn new() -> Self {
        static mut COUNTER: u32 = 0;
        unsafe {
            COUNTER += 1;
            Num(COUNTER)
        }
    }

    pub fn to_name(&self) -> String {
        match *self {
            Named(ref name) => name.clone(),
            Num(_) => panic!("Expected Named"),
        }
    }

    pub fn with_name(name: &str) -> Self {
        Named(name.to_string())
    }
}

impl Display for Label {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match *self {
            Named(ref name) => write!(formatter, "{}", name),
            Num(num) => write!(formatter, "l{}", num),
        }
    }
}

#[derive(Debug)]
pub struct TempMap {
    stack_vars: BTreeSet<i64>,
    temps: BTreeSet<Temp>,
}

impl TempMap {
    pub fn new() -> Self {
        Self {
            stack_vars: BTreeSet::new(),
            temps: BTreeSet::new(),
        }
    }

    pub fn contains_var(&self, stack_var: i64) -> bool {
        self.stack_vars.contains(&stack_var)
    }

    pub fn insert<F: Frame>(&mut self, access: &F::Access) {
        if let Some(temp) = access.as_temp() {
            self.temps.insert(*temp);
        }
        else if let Some(stack_location) = access.as_stack() {
            self.stack_vars.insert(stack_location);
        }
        else {
            unreachable!();
        }
    }
}
