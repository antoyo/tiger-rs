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

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use position::WithPos;

pub type Symbol = i64;
pub type SymbolWithPos = WithPos<Symbol>;

#[derive(Debug)]
pub struct Strings {
    next_symbol: RefCell<Symbol>,
    strings: RefCell<HashMap<Symbol, String>>,
}

impl Strings {
    pub fn new() -> Self {
        Self {
            next_symbol: RefCell::new(0),
            strings: RefCell::new(HashMap::new()),
        }
    }
}

#[derive(Debug)]
pub struct Symbols<T> {
    stack: Vec<Vec<Symbol>>,
    strings: Rc<Strings>,
    table: HashMap<Symbol, Vec<T>>,
}

impl<T> Symbols<T> {
    pub fn new(strings: Rc<Strings>) -> Self {
        let mut symbols = Self {
            stack: vec![],
            strings,
            table: HashMap::new(),
        };
        symbols.begin_scope();
        symbols
    }

    pub fn begin_scope(&mut self) {
        self.stack.push(vec![]);
    }

    pub fn end_scope(&mut self) {
        for symbol in self.stack.last().expect("Call begin_scope() before end_scope()") {
            let bindings = self.table.get_mut(symbol).expect("Symbol not in table");
            bindings.pop();
        }
        self.stack.pop();
    }

    pub fn enter(&mut self, symbol: Symbol, data: T) {
        let bindings = self.table.entry(symbol).or_insert_with(Vec::new);
        bindings.push(data);
        let current_bindings = self.stack.last_mut().expect("Call begin_scope() before enter()");
        current_bindings.push(symbol);
    }

    pub fn look(&self, symbol: Symbol) -> Option<&T> {
        self.table.get(&symbol)
            .and_then(|vec| vec.last())
    }

    pub fn name(&self, symbol: Symbol) -> String {
        self.strings.strings.borrow()[&symbol].to_string()
    }

    pub fn replace(&mut self, symbol: Symbol, data: T) {
        let bindings = self.table.entry(symbol).or_insert_with(Vec::new);
        bindings.pop().expect("Call enter() before replace()");
        bindings.push(data);
    }

    pub fn symbol(&mut self, string: &str) -> Symbol {
        if let Some((&key, _)) = self.strings.strings.borrow().iter().find(|&(_, value)| value == string) {
            return key;
        }

        let symbol = *self.strings.next_symbol.borrow();
        self.strings.strings.borrow_mut().insert(symbol, string.to_string());
        *self.strings.next_symbol.borrow_mut() += 1;
        symbol
    }
}
