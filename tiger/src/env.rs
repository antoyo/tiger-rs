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

use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::rc::Rc;

use escape::{DepthEscape, EscapeEnv};
use frame::Frame;
use gen::Access;
use position::WithPos;
use symbol::{
    Strings,
    Symbol,
    Symbols,
    SymbolWithPos,
};
use temp::Label;
use types::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct ClosureField {
    pub ident: Symbol,
    pub typ: Type,
}

impl Eq for ClosureField {
}

impl PartialOrd for ClosureField {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ClosureField {
    fn cmp(&self, other: &Self) -> Ordering {
        self.ident.cmp(&other.ident)
    }
}

#[derive(Clone, Debug)]
pub enum Entry<F: Clone + Frame> {
    Fun {
        external: bool,
        label: Label,
        parameters: Vec<Type>,
        param_type_symbols: Vec<SymbolWithPos>,
        result: Type,
        result_symbol: Option<SymbolWithPos>,
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
        type_env.enter(int_symbol, Type::Int);
        let string_symbol = type_env.symbol("string");
        type_env.enter(string_symbol, Type::String);

        let answer_symbol = type_env.symbol("answer");
        let cont_symbol = type_env.symbol("cont");
        let string_consumer_symbol = type_env.symbol("stringConsumer");

        let var_env = Symbols::new(Rc::clone(strings));
        let mut env = Self {
            escape_env,
            type_env,
            var_env,
        };

        for (name, (param_types, return_type)) in env.external_functions() {
            env.add_function(name, param_types, return_type);
        }

        env.enter_type(answer_symbol, Type::Answer);
        env.enter_type(cont_symbol, Type::Function {
            parameters: vec![],
            return_type: Box::new(Type::Answer),
        });
        env.enter_type(string_consumer_symbol, Type::Function {
            parameters: vec![Type::String],
            return_type: Box::new(Type::Answer),
        });

        env
    }

    fn add_function(&mut self, name: &str, parameters: Vec<Type>, result: Type) {
        let symbol = self.var_env.symbol(name);
        let entry = Entry::Fun {
            external: true,
            label: Label::with_name(name),
            parameters,
            param_type_symbols: vec![],
            result,
            result_symbol: None,
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

    pub fn external_functions(&mut self) -> BTreeMap<&'static str, (Vec<Type>, Type)> {
        let mut functions = BTreeMap::new();
        functions.insert("ord", (vec![Type::String], Type::Int));
        functions.insert("chr", (vec![Type::Int], Type::String));
        functions.insert("size", (vec![Type::String], Type::Int));
        functions.insert("substring", (vec![Type::String, Type::Int, Type::Int], Type::String));
        functions.insert("concat", (vec![Type::String, Type::String], Type::String));
        functions.insert("not", (vec![Type::Int], Type::Int));
        functions.insert("stringEqual", (vec![Type::String, Type::String], Type::Int));

        let cont = Type::Name(WithPos::dummy(self.type_env.symbol("cont")), None);
        functions.insert("debug", (vec![Type::String], Type::Answer));
        functions.insert("debugInt", (vec![Type::Int], Type::Answer));
        functions.insert("printi", (vec![Type::Int, cont.clone()], Type::Answer));
        functions.insert("print", (vec![Type::String, cont.clone()], Type::Answer));
        functions.insert("flush", (vec![cont], Type::Answer));
        functions.insert("getchar", (vec![Type::Name(WithPos::dummy(self.type_env.symbol("stringConsumer")), None)], Type::Answer));
        functions.insert("tigerExit", (vec![], Type::Answer));

        functions.insert("allocRecord", (vec![Type::Int], Type::Int));
        functions.insert("initArray", (vec![Type::Int, Type::Int], Type::Int));
        functions
    }
}
