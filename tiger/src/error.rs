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

pub type Result<T> = result::Result<T, Error>;

#[derive(Clone, Debug)]
pub enum Error {
    Eof,
    InvalidEscape {
        escape: String,
        pos: Pos,
    },
    Msg(String),
    Unclosed {
        pos: Pos,
        token: &'static str,
    },
    UnexpectedToken {
        expected: String,
        pos: Pos,
        unexpected: Tok,
    },
    UnknownToken {
        pos: Pos,
        start: char,
    },
}

impl Display for Error {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match *self {
            Eof => write!(formatter, "end of file"),
            InvalidEscape { ref escape, ref pos } => write!(formatter, "{} Invalid escape \\{}", pos, escape),
            Msg(ref string) => write!(formatter, "{}", string),
            Unclosed { ref pos, ref token } =>
                write!(formatter, "Unclosed {} starting at line {}, column {}", token, pos.line, pos.column),
            UnexpectedToken { ref expected, ref pos, ref unexpected } =>
                write!(formatter, "Unexpected token {}, expecting {} at line {}, column {}", unexpected, expected,
                       pos.line, pos.column),
            UnknownToken { ref pos, ref start } =>
                write!(formatter, "{} Unexpected start of token `{}`", pos, start),
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
