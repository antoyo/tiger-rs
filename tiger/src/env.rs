/*
 * Copyright (c) 2017-2024 Boucher, Antoni <bouanto@zoho.com>
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

use std::collections::BTreeMap;
use std::rc::Rc;

use escape::{DepthEscape, EscapeEnv};
use frame::Frame;
use gen;
use gen::{Access, Level};
use symbol::{Strings, Symbol, Symbols};
use temp::Label;
use types::Type;

use crate::types::{TyVar, TypeConstructor};

#[derive(Clone, Debug)]
pub enum Entry<F: Clone + Frame> {
    ClassField {
        class: Type,
    },
    Fun {
        access_outside_vars: bool,
        external: bool,
        label: Label,
        level: Level<F>,
        parameters: Vec<Type>,
        pure: bool,
        result: Type,
    },
    RecordField {
        record: Type,
    },
    Var {
        access: Access<F>,
        typ: Type,
    },
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
        type_env.enter(int_symbol, Type::new_int());
        let string_symbol = type_env.symbol("string");
        type_env.enter(string_symbol, Type::new_string());

        let object_symbol = type_env.symbol("Object");
        let answer_symbol = type_env.symbol("answer");
        let cont_symbol = type_env.symbol("cont");
        let string_consumer_symbol = type_env.symbol("stringConsumer");

        let object_class = Type::App(TypeConstructor::new_unique(TypeConstructor::Class {
            data_layout: String::new(),
            fields: vec![],
            methods: vec![],
            name: object_symbol,
            parent_class: None,
            vtable_name: Label::with_name("__vtable_Object"),
        }), vec![]);

        let var_env = Symbols::new(Rc::clone(strings));
        let mut env = Self {
            escape_env,
            type_env,
            var_env,
        };

        for (name, (param_types, return_type, pure)) in env.external_functions() {
            env.add_function(name, param_types, return_type, pure);
        }

        env.enter_type(object_symbol, object_class);

        env.enter_type(answer_symbol, Type::new_answer());
        env.enter_type(cont_symbol, Type::App(TypeConstructor::Arrow, vec![Type::new_answer()]));
        env.enter_type(string_consumer_symbol, Type::App(TypeConstructor::Arrow, vec![Type::new_string(), Type::new_answer()]));

        env
    }

    fn add_function(&mut self, name: &str, parameters: Vec<Type>, result: Type, pure: bool) {
        let symbol = self.var_env.symbol(name);
        let entry = Entry::Fun {
            access_outside_vars: false,
            external: true,
            label: Label::with_name(name),
            level: gen::outermost(), // FIXME: Might want to create a new level.
            parameters,
            pure,
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

    pub fn begin_type_scope(&mut self) {
        self.type_env.begin_scope();
    }

    pub fn end_type_scope(&mut self) {
        self.type_env.end_scope();
    }

    pub fn enter_escape(&mut self, symbol: Symbol, escape: bool) {
        self.escape_env.enter(symbol, DepthEscape {
            depth: 0, // This value is not used anymore.
            escape,
        });
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

    pub fn type_symbol(&mut self, string: &str) -> Symbol {
        self.type_env.symbol(string)
    }

    pub fn var_name(&self, symbol: Symbol) -> String {
        self.var_env.name(symbol)
    }

    pub fn external_functions(&mut self) -> BTreeMap<&'static str, (Vec<Type>, Type, bool)> {
        let mut functions = BTreeMap::new();
        functions.insert("print", (vec![Type::new_string()], Type::new_unit(), false));
        functions.insert("printi", (vec![Type::new_int()], Type::new_unit(), false));
        functions.insert("flush", (vec![], Type::new_unit(), false));
        functions.insert("getchar", (vec![], Type::new_string(), false));
        functions.insert("ord", (vec![Type::new_string()], Type::new_int(), true));
        functions.insert("chr", (vec![Type::new_int()], Type::new_string(), true));
        functions.insert("size", (vec![Type::new_string()], Type::new_int(), true));
        functions.insert("substring", (vec![Type::new_string(), Type::new_int(), Type::new_int()], Type::new_string(), true));
        functions.insert("concat", (vec![Type::new_string(), Type::new_string()], Type::new_string(), true));
        functions.insert("not", (vec![Type::new_int()], Type::new_int(), true));
        functions.insert("stringEqual", (vec![Type::new_string(), Type::new_string()], Type::new_int(), true));

        let cont = Type::Var(TyVar::from_symbol(self.type_env.symbol("cont")));
        functions.insert("printP", (vec![Type::new_string(), cont.clone()], Type::new_answer(), true));
        functions.insert("flushP", (vec![cont], Type::new_answer(), true));
        let string_consumer = Type::Var(TyVar::from_symbol(self.type_env.symbol("stringConsumer")));
        functions.insert("getcharP", (vec![string_consumer], Type::new_answer(), true));
        functions.insert("exit", (vec![], Type::new_answer(), true));

        functions.insert("allocClass", (vec![Type::new_int()], Type::new_int(), true));
        functions.insert("allocRecord", (vec![Type::new_int()], Type::new_int(), true));
        functions.insert("initArray", (vec![Type::new_int(), Type::new_int()], Type::new_int(), true));
        functions
    }
}
