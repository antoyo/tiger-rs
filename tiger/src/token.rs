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

use std::fmt::{self, Display, Formatter};

use position::Pos;
use self::Tok::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Ampersand,
    Array,
    Arrow,
    CloseCurly,
    CloseParen,
    CloseSquare,
    Colon,
    ColonEqual,
    Comma,
    Do,
    Dot,
    Else,
    End,
    EndOfFile,
    Equal,
    Function,
    Greater,
    GreaterOrEqual,
    Ident(String),
    If,
    In,
    Int(i64),
    Lesser,
    LesserOrEqual,
    Let,
    Minus,
    Nil,
    NotEqual,
    Of,
    OpenCurly,
    OpenParen,
    OpenSquare,
    Pipe,
    Plus,
    Semicolon,
    Slash,
    Star,
    Str(String),
    Then,
    Type,
    Var,
}

#[derive(Debug)]
pub struct Token {
    pub pos: Pos,
    pub token: Tok,
}

impl Display for Tok {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        let string = (|| {
            let string = match *self {
                Ampersand => "&",
                Array => "array",
                Arrow => "->",
                CloseCurly => "}",
                CloseParen => ")",
                CloseSquare => "]",
                Colon => ":",
                ColonEqual => ":=",
                Comma => ",",
                Do => "do",
                Dot => ".",
                Else => "else",
                EndOfFile => "<eof>",
                Equal => "=",
                End => "end",
                Function => "function",
                Greater => ">",
                GreaterOrEqual => ">=",
                Ident(ref ident) => ident,
                If => "if",
                In => "in",
                Int(num) => return num.to_string(),
                Lesser => "<",
                LesserOrEqual => "<=",
                Let => "let",
                Minus => "-",
                Nil => "nil",
                NotEqual => "<>",
                Of => "of",
                OpenCurly => "{",
                OpenParen => "(",
                OpenSquare => "[",
                Pipe => "|",
                Plus => "+",
                Semicolon => ";",
                Slash => "/",
                Star => "*",
                Str(ref string) => return format!("{:?}", string),
                Then => "then",
                Type => "type",
                Var => "var",
            };
            string.to_string()
        })();
        write!(formatter, "{}", string)
    }
}
