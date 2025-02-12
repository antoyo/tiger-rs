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

use ast::ExprWithPos;
use ir::Exp;
use self::Type::*;
use self::TypeConstructor::*;
use symbol::{Symbol, Symbols, SymbolWithPos};
use temp::Label;

#[derive(Clone, Debug, PartialEq)]
pub struct ClassField {
    pub name: Symbol,
    pub typ: Type,
    pub value: ExprWithPos,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionType {
    pub param_types: Vec<Type>,
    pub return_type: Type,
}

impl FunctionType {
    pub fn show(&self, symbols: &Symbols<()>) -> std::string::String {
        let param_types = self.param_types.iter()
            .map(|typ| typ.show(symbols))
            .collect::<Vec<_>>()
            .join(", ");
        format!("({}) -> {}", param_types, self.return_type.show(symbols))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassMethod {
    pub class_name: Symbol,
    pub label: Label,
    pub name: SymbolWithPos,
    pub typ: FunctionType,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TyVar(pub Symbol);

impl TyVar {
    pub fn from_symbol(symbol: Symbol) -> Self {
        Self(symbol)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    /// Apply a type constructor (such as list) to type arguments (such as <int>).
    App(TypeConstructor, Vec<Type>),
    Nil,
    //Poly(Vec<TyVar>, Box<Type>),
    Var(TyVar),
    Error,
}

impl Type {
    pub fn is_pointer(&self) -> bool {
        match *self {
            App(TypeConstructor::Unique(ref inner_type, _), _) => {
                match **inner_type {
                    Array | Class { .. } | Record { .. }  => true,
                    _ => false,
                }
            },
            App(String, _)  => true,
            //Poly(_, ref typ) => typ.is_pointer(),
            Var(_) => todo!(),
            _ => false,
        }
    }

    pub fn new_answer() -> Self {
        Self::App(TypeConstructor::Answer, vec![])
    }

    pub fn new_int() -> Self {
        Self::App(TypeConstructor::Int, vec![])
    }

    pub fn new_string() -> Self {
        Self::App(TypeConstructor::String, vec![])
    }

    pub fn new_unit() -> Self {
        Self::App(TypeConstructor::Unit, vec![])
    }

    pub fn show(&self, symbols: &Symbols<()>) -> std::string::String {
        match *self {
            App(TypeConstructor::Unique(ref typ, _), ref vec) => {
                App(*typ.clone(), vec.clone()).show(symbols)
            },
            App(ref type_constructor, ref vec) => {
                let mut types = std::string::String::new();
                for typ in vec {
                    types.push_str(&typ.show(symbols));
                }
                if !types.is_empty() {
                    types = format!("<{}>", types);
                }
                format!("{}{}", type_constructor.show(symbols), types)
            },
            Nil => "nil".to_string(),
            //Poly(_, _) => todo!(),
            Var(ref ty_var) => symbols.name(ty_var.0),
            Error => "type error".to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeConstructor {
    Array,
    /// Used for function types.
    Arrow,
    Answer,
    Class {
        data_layout: std::string::String,
        fields: Vec<ClassField>,
        methods: Vec<ClassMethod>,
        name: Symbol,
        parent_class: Option<SymbolWithPos>,
        vtable_name: Label,
    },
    Int,
    String,
    Record {
        data_layout: Exp,
        name: Symbol,
        types: Vec<(Symbol, Type)>,
    },
    // FIXME: having to wrap Array, Record and Class in Unique is very error-prone.
    Unique(Box<TypeConstructor>, Unique),
    Unit,
}

impl TypeConstructor {
    pub fn new_unique(type_constructor: TypeConstructor) -> Self {
        Self::Unique(Box::new(type_constructor), Unique::new())
    }

    pub fn show(&self, symbols: &Symbols<()>) -> std::string::String {
        match *self {
            Answer => "answer".to_string(),
            Arrow => "->".to_string(),
            Array => "[]".to_string(),
            Class { name, .. } => format!("class {}", symbols.name(name)),
            Int => "int".to_string(),
            Record { name, .. } => format!("struct {}", symbols.name(name)),
            String => "string".to_string(),
            TypeConstructor::Unique(_, _) => "".to_string(),
            Unit => "()".to_string(),
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
