/*
 * Copyright (c) 2017-2019 Boucher, Antoni <bouanto@zoho.com>
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

use std::fmt::{self, Display, Debug, Formatter};
use std::u32;

use symbol::{Symbol, Symbols};
use terminal::Terminal;

#[derive(Clone, Copy, Debug)]
pub struct Pos {
    pub byte: u64,
    pub column: u32,
    pub file: Symbol,
    pub length: usize,
    pub line: u32,
}

impl Pos {
    pub fn new(line: u32, column: u32, byte: u64, file: Symbol, length: usize) -> Self {
        Pos {
            byte,
            column,
            file,
            length,
            line,
        }
    }

    pub fn dummy() -> Self {
        Self::new(u32::MAX, u32::MAX, u64::MAX, 0, 0)
    }

    pub fn grow(&self, pos: Pos) -> Self {
        Pos {
            byte: self.byte,
            column: self.column,
            file: self.file,
            length: (pos.byte - self.byte) as usize + pos.length,
            line: self.line,
        }
    }

    pub fn show(&self, symbols: &Symbols<()>, terminal: &Terminal) {
        let filename = symbols.name(self.file);
        eprintln!("   {}{}-->{}{} {}:{}:{}", terminal.bold(), terminal.blue(), terminal.reset_color(), terminal.end_bold(), filename, self.line, self.column)
    }
}

impl Display for Pos {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}:{}:", self.line, self.column)
    }
}

#[derive(Debug, Clone)]
pub struct WithPos<T> {
    pub node: T,
    pub pos: Pos,
}

impl<T> WithPos<T> {
    pub fn new(node: T, pos: Pos) -> Self {
        Self {
            node,
            pos,
        }
    }

    pub fn dummy(node: T) -> Self {
        Self {
            node,
            pos: Pos::dummy(),
        }
    }
}

impl<T: PartialEq> PartialEq for WithPos<T> {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node
    }
}
