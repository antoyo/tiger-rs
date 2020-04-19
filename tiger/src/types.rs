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

use ir::Exp;
use self::Type::*;
use symbol::{Symbol, Symbols, SymbolWithPos};

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Array(Box<Type>, Unique),
    Answer,
    Function {
        parameters: Vec<Type>,
        return_type: Box<Type>,
    },
    Int,
    Name(SymbolWithPos, Option<Box<Type>>),
    Nil,
    String,
    Record {
        data_layout: Exp,
        name: Symbol,
        types: Vec<(Symbol, Type)>,
        unique: Unique,
    },
    StaticLink {
        data_layout: Exp,
        types: Vec<(Symbol, Type)>,
    },
    Unit,
    Error,
}

impl Type {
    pub fn is_pointer(&self) -> bool {
        match *self {
            Array { .. } | Record { .. } | StaticLink { .. } | String  => true,
            Name(_, ref typ) => {
                if let Some(typ) = typ.as_ref() {
                    typ.is_pointer()
                }
                else {
                    false
                }
            },
            _ => false,
        }
    }

    pub fn show(&self, symbols: &Symbols<()>) -> std::string::String {
        match *self {
            Answer => "answer".to_string(),
            Array(ref typ, _) => {
                format!("[{}]", typ.show(symbols))
            },
            Function { ref parameters, ref return_type } => {
                let show_parens = parameters.len() != 1;
                let mut string = std::string::String::new();
                if show_parens {
                    string.push('(');
                }
                string.push_str(&parameters.iter()
                    .map(|param| param.show(symbols))
                    .collect::<Vec<_>>()
                    .join(", "));
                if show_parens {
                    string.push(')');
                }
                string.push_str(" -> ");
                string.push_str(&return_type.show(symbols));
                string
            },
            Int => "int".to_string(),
            Name(_, ref typ) => {
                if let Some(ref typ) = *typ {
                    typ.show(symbols)
                }
                else {
                    "unresolved type".to_string()
                }
            },
            Nil => "nil".to_string(),
            Record { name, ref types, .. } => {
                let mut string = format!("struct {} {{", symbols.name(name));
                for &(ident, ref typ) in types {
                     let ident = symbols.name(ident);
                     string.push_str("    ");
                     string.push_str(&ident);
                     string.push_str(": ");
                     string.push_str(&typ.show(symbols));
                     string.push('\n');
                }
                string.push('}');
                string
            },
            StaticLink { ref types, .. } => {
                let mut string = "static link {\n".to_string();
                for &(ident, ref typ) in types {
                     let ident = symbols.name(ident);
                     string.push_str("    ");
                     string.push_str(&ident);
                     string.push_str(": ");
                     string.push_str(&typ.show(symbols));
                     string.push('\n');
                }
                string.push('}');
                string
            },
            String => "string".to_string(),
            Unit => "()".to_string(),
            Error => "type error".to_string(),
        }
    }
}

static mut UNIQUE_COUNT: u64 = 0;

#[derive(Clone, Debug, PartialEq)]
pub struct Unique(u64);

impl Unique {
    pub fn new() -> Self {
        let value = unsafe { UNIQUE_COUNT };
        unsafe { UNIQUE_COUNT += 1 };
        Unique(value)
    }
}
