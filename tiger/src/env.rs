/*
 * Copyright (c) 2017-2018 Boucher, Antoni <bouanto@zoho.com>
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

use escape::EscapeEnv;
use frame::Frame;
use gen;
use gen::{Access, Level};
use symbol::{Strings, Symbol, Symbols};
use temp::Label;
use types::Type;

#[derive(Clone)]
pub enum Entry<F: Clone + Frame> {
    Fun {
        label: Label,
        level: Level<F>,
        parameters: Vec<Type>,
        result: Type,
    },
    Var {
        access: Access<F>,
        typ: Type,
    },
    Error,
}

pub struct Env<F: Clone + Frame> {
    escape_env: EscapeEnv,
    type_env: Symbols<Type>,
    var_env: Symbols<Entry<F>>,
}

impl<F: Clone + Frame> Env<F> {
    pub fn new(strings: &Rc<Strings>, escape_env: EscapeEnv) -> Self {
        let mut type_env = Symbols::new(Rc::clone(strings));
        let int_symbol = type_env.symbol("int");
        type_env.enter(int_symbol, Type::Int);
        let string_symbol = type_env.symbol("string");
        type_env.enter(string_symbol, Type::String);

        let var_env = Symbols::new(Rc::clone(strings));
        let mut env = Self {
            escape_env,
            type_env,
            var_env,
        };

        env.add_function("print", vec![Type::String], Type::Unit);
        env.add_function("flush", vec![], Type::Unit);
        env.add_function("getchar", vec![], Type::String);
        env.add_function("ord", vec![Type::String], Type::Int);
        env.add_function("chr", vec![Type::Int], Type::String);
        env.add_function("size", vec![Type::String], Type::Int);
        env.add_function("substring", vec![Type::String, Type::Int, Type::Int], Type::String);
        env.add_function("concat", vec![Type::String, Type::String], Type::String);
        env.add_function("not", vec![Type::Int], Type::Int);
        env.add_function("exit", vec![Type::Int], Type::Unit);

        env
    }

    fn add_function(&mut self, name: &str, parameters: Vec<Type>, result: Type) {
        let symbol = self.var_env.symbol(name);
        let entry = Entry::Fun {
            label: Label::new(),
            level: gen::outermost(),
            parameters,
            result,
        };
        self.var_env.enter(symbol, entry);
    }

    pub fn begin_scope(&mut self) {
        self.type_env.begin_scope();
        self.var_env.begin_scope();
    }

    pub fn end_scope(&mut self) {
        self.type_env.end_scope();
        self.var_env.end_scope();
    }

    pub fn enter_type(&mut self, symbol: Symbol, typ: Type) {
        self.type_env.enter(symbol, typ);
    }

    pub fn enter_var(&mut self, symbol: Symbol, data: Entry<F>) {
        self.var_env.enter(symbol, data);
    }

    pub fn look_escape(&self, symbol: Symbol) -> bool {
        self.escape_env.look(symbol)
            .expect("escape")
            .escape
    }

    pub fn look_type(&self, symbol: Symbol) -> Option<&Type> {
        self.type_env.look(symbol)
    }

    pub fn look_var(&self, symbol: Symbol) -> Option<&Entry<F>> {
        self.var_env.look(symbol)
    }

    pub fn replace_type(&mut self, symbol: Symbol, typ: Type) {
        self.type_env.replace(symbol, typ);
    }

    pub fn type_name(&self, symbol: Symbol) -> String {
        self.type_env.name(symbol)
    }

    pub fn var_name(&self, symbol: Symbol) -> String {
        self.var_env.name(symbol)
    }
}
