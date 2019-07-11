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

use std::char;
use std::io::{Bytes, Read};
use std::iter::Peekable;
use std::result;

use error::num_text_size;
use error::Error::{self, Eof, InvalidEscape, Msg, Unclosed, UnknownToken};
use position::Pos;
use token::{Tok, Token};
use token::Tok::*;

pub type Result<T> = result::Result<T, Error>;

pub struct Lexer<R: Read> {
    bytes_iter: Peekable<Bytes<R>>,
    pos: Pos,
    saved_pos: Pos,
}

impl<R: Read> Lexer<R> {
    pub fn new(reader: R, filename: &str) -> Self {
        Lexer {
            bytes_iter: reader.bytes().peekable(),
            pos: Pos::new(1, 1, 0, filename, 0),
            saved_pos: Pos::new(1, 1, 0, filename, 0),
        }
    }

    fn advance(&mut self) -> Result<()> {
        match self.bytes_iter.next() {
            Some(Ok(b'\n')) => {
                self.pos.line += 1;
                self.pos.column = 1;
                self.pos.byte += 1;
            },
            Some(Err(error)) => return Err(error.into()),
            None => return Err(Eof),
            _ => {
                self.pos.column += 1;
                self.pos.byte += 1;
            },
        }
        Ok(())
    }

    fn colon_and_optional_equal(&mut self) -> Result<Token> {
        self.two_char_token(vec![('=', ColonEqual)], Colon)
    }

    fn comment(&mut self) -> Result<()> {
        let mut depth = 1;
        loop {
            self.advance()?;
            let ch = self.current_char()?;
            // Check for nested comment.
            if ch == '/' {
                self.advance()?;
                let ch = self.current_char()?;
                if ch == '*' {
                    depth += 1;
                }
            }
            // Check for end of comment.
            if ch == '*' {
                self.advance()?;
                let ch = self.current_char()?;
                if ch == '/' {
                    depth -= 1;
                    if depth == 0 {
                        self.advance()?;
                        break;
                    }
                }
            }
        }
        Ok(())
    }

    fn current_char(&mut self) -> Result<char> {
        if let Some(&Ok(byte)) = self.bytes_iter.peek() {
            return Ok(byte as char);
        }

        match self.bytes_iter.next() {
            Some(Ok(_)) => unreachable!(),
            Some(Err(error)) => Err(error.into()),
            None => Err(Eof),
        }
    }

    fn current_pos(&self) -> Pos {
        self.pos.clone()
    }

    fn eat(&mut self, ch: char) -> Result<()> {
        if self.current_char()? != ch {
            panic!("Expected character `{}`, but found `{}`.", ch, self.current_char()?);
        }
        self.advance()
    }

    fn escape_ascii_code(&mut self, mut pos: Pos) -> Result<char> {
        let buffer = self.take_while(char::is_numeric)?;
        if buffer.len() == 3 {
            // The buffer only contains digit, hence unwrap().
            let ascii_code = buffer.parse().unwrap();
            match char::from_u32(ascii_code) {
                Some(ch) => Ok(ch),
                None => Err(Msg(format!("Invalid ascii code {}", ascii_code))),
            }
        }
        else {
            pos.length = buffer.len() + 1; // + 1 for the leading slash.
            Err(InvalidEscape {
                escape: buffer,
                pos,
            })
        }
    }

    fn escape_char(&mut self, mut pos: Pos) -> Result<char> {
        let escaped_char =
            match self.current_char()? {
                'n' => '\n',
                't' => '\t',
                '\\' => '\\',
                '"' => '"',
                ch if ch.is_digit(10) => return self.escape_ascii_code(pos),
                escape => {
                    pos.length = 2;
                    return Err(InvalidEscape {
                        escape: escape.to_string(),
                        pos,
                    })
                },
            };
        self.advance()?;
        Ok(escaped_char)
    }

    fn greater_or_greater_equal(&mut self) -> Result<Token> {
        self.two_char_token(vec![('=', GreaterOrEqual)], Greater)
    }

    fn identifier(&mut self) -> Result<Token> {
        let ident = self.take_while(|ch| ch.is_alphanumeric() || ch == '_')?;
        let len = ident.len();
        let token =
            match ident.as_str() {
                "array" => Array,
                "break" => Break,
                "do" => Do,
                "else" => Else,
                "end" => End,
                "for" => For,
                "function" => Function,
                "if" => If,
                "in" => In,
                "let" => Let,
                "nil" => Nil,
                "of" => Of,
                "then" => Then,
                "to" => To,
                "type" => Type,
                "var" => Var,
                "while" => While,
                _ => Ident(ident),
            };
        self.make_token(token, len)
    }

    fn integer(&mut self) -> Result<Token> {
        let buffer = self.take_while(char::is_numeric)?;
        // The buffer only contains digit, hence unwrap().
        let num = buffer.parse().unwrap();
        self.make_token(Int(num), num_text_size(num))
    }

    fn lesser_or_lesser_equal_or_not_equal(&mut self) -> Result<Token> {
        self.two_char_token(vec![('=', LesserOrEqual), ('>', NotEqual)], Lesser)
    }

    fn make_token(&self, token: Tok, length: usize) -> Result<Token> {
        if length > 10000 {
            panic!();
        }
        let mut pos = self.saved_pos.clone();
        pos.length = length;
        Ok(Token {
            pos,
            token,
        })
    }

    fn save_start(&mut self) {
        self.saved_pos = self.current_pos();
    }

    fn simple_token(&mut self, token: Tok) -> Result<Token> {
        let mut pos = self.pos.clone();
        pos.length = 1;
        self.advance()?;
        Ok(Token {
            pos,
            token,
        })
    }

    fn skip_until_slash(&mut self) -> Result<()> {
        loop {
            let ch = self.current_char()?;
            if ch == '\\' {
                self.advance()?;
                break;
            }
            else if !ch.is_whitespace() {
                let mut pos = self.current_pos();
                pos.length = 1;
                return Err(UnknownToken {
                    pos,
                    start: ch,
                });
            }
            self.advance()?;
        }
        Ok(())
    }

    fn slash_or_comment(&mut self) -> Result<Token> {
        self.save_start();
        self.advance()?;
        if self.current_char()? == '*' {
            match self.comment() {
                Err(Eof) => {
                    let mut pos = self.saved_pos.clone();
                    pos.length = 2;
                    return Err(Unclosed {
                        pos,
                        token: "comment",
                    })
                },
                Err(error) => return Err(error),
                _ => (),
            }
            self.token()
        }
        else {
            self.make_token(Slash, 1)
        }
    }

    fn string(&mut self) -> Result<Token> {
        let result = (|| {
            self.save_start();
            let mut string = String::new();
            let start = self.current_pos().byte;
            self.eat('"')?;
            let mut ch = self.current_char()?;
            while ch != '"' {
                // Look for escaped character.
                if ch == '\\' {
                    let pos = self.current_pos();
                    self.advance()?;
                    if self.current_char()?.is_whitespace() {
                        self.skip_until_slash()?;
                    }
                    else {
                        string.push(self.escape_char(pos)?);
                    }
                }
                else {
                    string.push(ch);
                    self.advance()?;
                }
                ch = self.current_char()?;
            }
            self.eat('"')?;
            let len = self.current_pos().byte - start;
            self.make_token(Str(string), len as usize)
        })();
        match result {
            Err(Eof) => {
                let mut pos = self.saved_pos.clone();
                pos.length = 1;
                Err(Unclosed {
                    pos,
                    token: "string",
                })
            },
            _ => result,
        }
    }

    fn take_while<F: Fn(char) -> bool>(&mut self, pred: F) -> Result<String> {
        self.save_start();
        let mut buffer = String::new();
        buffer.push(self.current_char()?);
        self.advance()?;
        let mut ch = self.current_char()?;
        while pred(ch) {
            buffer.push(ch);
            self.advance()?;
            ch = self.current_char()?;
        }
        Ok(buffer)
    }

    pub fn token(&mut self) -> Result<Token> {
        if let Some(&Ok(ch)) = self.bytes_iter.peek() {
            return match ch {
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.identifier(),
                b'0'..=b'9' => self.integer(),
                b' ' | b'\n' | b'\t' => {
                    self.advance()?;
                    self.token()
                }
                b'=' => self.simple_token(Equal),
                b'&' => self.simple_token(Ampersand),
                b'|' => self.simple_token(Pipe),
                b'.' => self.simple_token(Dot),
                b',' => self.simple_token(Comma),
                b';' => self.simple_token(Semicolon),
                b'*' => self.simple_token(Star),
                b'+' => self.simple_token(Plus),
                b'-' => self.simple_token(Minus),
                b'{' => self.simple_token(OpenCurly),
                b'}' => self.simple_token(CloseCurly),
                b'(' => self.simple_token(OpenParen),
                b')' => self.simple_token(CloseParen),
                b'[' => self.simple_token(OpenSquare),
                b']' => self.simple_token(CloseSquare),
                b':' => self.colon_and_optional_equal(),
                b'>' => self.greater_or_greater_equal(),
                b'<' => self.lesser_or_lesser_equal_or_not_equal(),
                b'/' => self.slash_or_comment(),
                b'"' => self.string(),
                _ => {
                    let mut pos = self.current_pos();
                    pos.length = 1;
                    Err(UnknownToken {
                        pos,
                        start: ch as char,
                    })
                },
            };
        }
        match self.bytes_iter.next() {
            Some(Ok(_)) => unreachable!(),
            Some(Err(error)) => Err(error.into()),
            None => Err(Eof),
        }
    }

    fn two_char_token(&mut self, tokens: Vec<(char, Tok)>, default: Tok) -> Result<Token> {
        self.save_start();
        self.advance()?;
        let token =
            match self.bytes_iter.peek() {
                Some(&Ok(byte)) => {
                    let mut token = None;
                    let next_char = byte as char;
                    for (ch, tok) in tokens {
                        if ch == next_char {
                            token = Some(tok);
                        }
                    }
                    token
                },
                _ => None,
            };
        let (token, len) =
            if let Some(token) = token {
                self.advance()?;
                (token, 2)
            }
            else {
                (default, 1)
            };
        self.make_token(token, len)
    }
}
