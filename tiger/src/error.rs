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
use std::io;
use std::result;

use position::Pos;
use self::Error::*;
use token::Tok;
use types::Type;

pub type Result<T> = result::Result<T, Vec<Error>>;

#[derive(Clone, Debug)]
pub enum Error {
    BreakOutsideLoop {
        pos: Pos,
    },
    CannotIndex {
        pos: Pos,
        typ: String,
    },
    Cycle {
        pos: Pos,
    },
    DuplicateParam {
        ident: String,
        pos: Pos,
    },
    Eof,
    ExtraField {
        ident: String,
        pos: Pos,
        struct_name: String,
    },
    InvalidEscape {
        escape: String,
        pos: Pos,
    },
    MissingField {
        ident: String,
        pos: Pos,
        struct_name: String,
    },
    Msg(String),
    NotARecord {
        pos: Pos,
        typ: String,
    },
    RecordType {
        pos: Pos,
    },
    Type {
        expected: Type,
        pos: Pos,
        unexpected: Type,
    },
    Unclosed {
        pos: Pos,
        token: &'static str,
    },
    Undefined {
        ident: String,
        item: String,
        pos: Pos,
    },
    UnexpectedField {
        ident: String,
        pos: Pos,
        struct_name: String,
    },
    UnexpectedToken {
        expected: String,
        pos: Pos,
        unexpected: Tok,
    },
    UnexpectedType {
        kind: String,
        pos: Pos,
    },
    UnknownToken {
        pos: Pos,
        start: char,
    },
}

impl Display for Error {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match *self {
            BreakOutsideLoop { pos } =>
                write!(formatter, "{} Break statement used outside of loop at {:?}", pos, pos),
            CannotIndex { pos, ref typ } =>
                write!(formatter, "{} Cannot index value of type `{}` at {:?}", pos, typ, pos),
            Cycle { pos } =>
                write!(formatter, "{} Type cycle detected at {:?}", pos, pos),
            DuplicateParam { ref ident, pos } =>
                write!(formatter, "{} Duplicate parameter name `{}` at {:?}", pos, ident, pos),
            Eof => write!(formatter, "end of file"),
            ExtraField { ref ident, pos, ref struct_name } =>
                write!(formatter, "{} Extra field `{}` in struct of type `{}` at {:?}", pos, ident, struct_name, pos),
            InvalidEscape { ref escape, pos } =>
                write!(formatter, "{} Invalid escape \\{} at {:?}", pos, escape, pos),
            MissingField { ref ident, pos, ref struct_name } =>
                write!(formatter, "{} Missing field `{}` in struct of type `{}` at {:?}", pos, ident, struct_name, pos),
            Msg(ref string) => write!(formatter, "{}", string),
            NotARecord { pos, ref typ } =>
                write!(formatter, "{} Type `{}` is not a record type at {:?}", pos, typ, pos),
            Error::RecordType { pos } =>
                write!(formatter, "{} Expecting type when value is nil at {:?}", pos, pos),
            Error::Type { ref expected, pos, ref unexpected } =>
                write!(formatter, "{}, Unexpected type {}, expecting {} at {:?}", pos, unexpected, expected, pos),
            Unclosed { pos, token } =>
                write!(formatter, "{} Unclosed {} starting at {:?}", pos, token, pos),
            Undefined { ref ident, ref item, pos } =>
                write!(formatter, "{} Undefined {} `{}` at {:?}", pos, item, ident, pos),
            UnexpectedField { ref ident, pos, ref struct_name } =>
                write!(formatter, "{} Unexpected field `{}` in struct of type `{}` at {:?}", pos, ident, struct_name, pos),
            UnexpectedToken { ref expected, pos, ref unexpected } =>
                write!(formatter, "{} Unexpected token {}, expecting {} at {:?}", pos, unexpected, expected, pos),
            UnexpectedType { ref kind, pos } =>
                write!(formatter, "{} Expecting {} type at {:?}", pos, kind, pos),
            UnknownToken { pos, ref start } =>
                write!(formatter, "{} Unexpected start of token `{}` at {:?}", pos, start, pos),
        }
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Msg(error.to_string())
    }
}

impl<'a> From<&'a Error> for Error {
    fn from(error: &'a Error) -> Self {
        error.clone()
    }
}
