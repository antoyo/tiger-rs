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

use std::fmt::{self, Display, Formatter};

use self::Type::*;
use symbol::{Symbol, SymbolWithPos};

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Int,
    String,
    Record(Symbol, Vec<(Symbol, Type)>, Unique),
    Array(Box<Type>, Unique),
    Nil,
    Unit,
    Name(SymbolWithPos, Option<Box<Type>>),
    Error,
}

impl Display for Type {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        let string =
            match *self {
                Int => "int".to_string(),
                String => "string".to_string(),
                Record(_, ref types, _) => {
                    write!(formatter, "{{")?;
                    if let Some((last_type, types)) = types.split_last() {
                        for &(_, ref typ) in types {
                            typ.fmt(formatter)?;
                            write!(formatter, ", ")?;
                        }
                        last_type.1.fmt(formatter)?;
                    }
                    "}".to_string()
                },
                Array(ref typ, _) => {
                    write!(formatter, "array of ")?;
                    typ.fmt(formatter)?;
                    return Ok(());
                },
                Nil => "nil".to_string(),
                Unit => "()".to_string(),
                Name(_, ref typ) => {
                    if let Some(ref typ) = *typ {
                        typ.fmt(formatter)?;
                    }
                    else {
                        write!(formatter, "unresolved type")?;
                    }
                    return Ok(());
                },
                Error => "type error".to_string(),
            };
        write!(formatter, "{}", string)
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
