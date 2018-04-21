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

use self::Label::{Named, Num};

#[derive(Clone)]
pub struct Temp {
    num: u32,
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

    pub fn make_string(&self) -> String {
        format!("temp{}", self.num)
    }
}

#[derive(Clone)]
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

    pub fn with_name(name: String) -> Self {
        Named(name)
    }
}
