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

use std::cmp::{max, min};
use std::fs::File;
use std::io::{self, Read, Seek, SeekFrom};
use std::result;

use position::Pos;
use self::Error::*;
use symbol::Symbols;
use terminal::Terminal;
use token::Tok;
use types::{FunctionType, Type};

pub type Result<T> = result::Result<T, Error>;

#[derive(Clone, Debug)]
pub enum Error {
    Assign {
        pos: Pos,
    },
    BreakOutsideLoop {
        pos: Pos,
    },
    CannotIndex {
        pos: Pos,
        typ: Type,
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
    FunctionType {
        expected: FunctionType,
        pos: Pos,
        unexpected: FunctionType,
    },
    InvalidEscape {
        escape: String,
        pos: Pos,
    },
    InvalidNumberOfParams {
        actual: usize,
        expected: usize,
        pos: Pos,
    },
    MissingField {
        ident: String,
        pos: Pos,
        struct_name: String,
    },
    Msg(String),
    Multi(Vec<Error>),
    NotAClass {
        pos: Pos,
        typ: Type,
    },
    NotARecordOrClass {
        pos: Pos,
        typ: Type,
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

impl Error {
    pub fn show(&self, symbols: &Symbols<()>, terminal: &Terminal) -> io::Result<()> {
        if let Multi(ref errors) = *self {
            for error in errors.iter().rev() {
                error.show(symbols, terminal)?;
            }
            return Ok(());
        }
        eprint!("{}{}error: {}", terminal.bold(), terminal.red(), terminal.reset_color());
        match *self {
            Assign { pos } => {
                eprintln!("Can only assign to variable, field or array element{}", terminal.end_bold());
                pos.show(symbols, terminal);
                highlight_line(pos, symbols, terminal)?;
            },
            BreakOutsideLoop { pos } => {
                eprintln!("Break statement used outside of loop{}", terminal.end_bold());
                pos.show(symbols, terminal);
            },
            CannotIndex { pos, ref typ } => {
                eprintln!("Cannot index value of type `{}`{}", typ.show(symbols), terminal.end_bold());
                pos.show(symbols, terminal)
            },
            Cycle { pos } => {
                eprintln!("Type cycle detected:{}", terminal.end_bold());
                pos.show(symbols, terminal);
            },
            DuplicateParam { ref ident, pos } => {
                eprintln!("Duplicate param `{}`{}", ident, terminal.end_bold());
                pos.show(symbols, terminal);
                highlight_line(pos, symbols, terminal)?;
            },
            Eof => eprintln!("end of file"),
            ExtraField { ref ident, pos, ref struct_name } => {
                eprintln!("Extra field `{}` in struct of type `{}`{}", ident, struct_name, terminal.end_bold());
                pos.show(symbols, terminal);
            },
            Error::FunctionType { ref expected, pos, ref unexpected } => {
                eprintln!("Overridden method should have the same type as the inherited method:\nunexpected {}\n expecting {}{}", unexpected.show(symbols), expected.show(symbols), terminal.end_bold());
                pos.show(symbols, terminal);
                highlight_line(pos, symbols, terminal)?;
            },
            InvalidEscape { ref escape, pos } => {
                eprintln!("Invalid escape \\{}{}", escape, terminal.end_bold());
                pos.show(symbols, terminal);
                highlight_line(pos, symbols, terminal)?;
            },
            InvalidNumberOfParams { actual, expected, pos } => {
                eprintln!("Invalid number of parameters: expecting {}, but found {}{}", expected, actual, terminal.end_bold());
                pos.show(symbols, terminal);
                highlight_line(pos, symbols, terminal)?;
            },
            MissingField { ref ident, pos, ref struct_name } => {
                eprintln!("Missing field `{}` in struct of type `{}`{}", ident, struct_name, terminal.end_bold());
                pos.show(symbols, terminal);
            },
            Msg(ref string) => eprintln!("{}", string),
            Multi(_) => unreachable!(),
            NotAClass { pos, ref typ } => {
                eprintln!("Type `{}` is not a class type{}", typ.show(symbols), terminal.end_bold());
                pos.show(symbols, terminal);
                highlight_line(pos, symbols, terminal)?;
            },
            NotARecordOrClass { pos, ref typ } => {
                eprintln!("Type `{}` is not a struct or a class type{}", typ.show(symbols), terminal.end_bold());
                pos.show(symbols, terminal);
                highlight_line(pos, symbols, terminal)?;
            },
            Error::RecordType { pos } => {
                eprintln!("Expecting type when value is nil{}", terminal.end_bold());
                pos.show(symbols, terminal);
            },
            Error::Type { ref expected, pos, ref unexpected } => {
                eprintln!("Unexpected type {}, expecting {}{}", unexpected.show(symbols), expected.show(symbols), terminal.end_bold());
                pos.show(symbols, terminal);
                highlight_line(pos, symbols, terminal)?;
            },
            Unclosed { pos, token } => {
                eprintln!("Unclosed {}{}", token, terminal.end_bold());
                pos.show(symbols, terminal);
                highlight_line(pos, symbols, terminal)?;
            },
            Undefined { ref ident, ref item, pos } => {
                eprintln!("Undefined {} `{}`{}", item, ident, terminal.end_bold());
                pos.show(symbols, terminal);
                highlight_line(pos, symbols, terminal)?;
            },
            UnexpectedField { ref ident, pos, ref struct_name } => {
                eprintln!("Unexpected field `{}` in struct of type `{}`{}", ident, struct_name, terminal.end_bold());
                pos.show(symbols, terminal);
            },
            UnexpectedToken { ref expected, pos, ref unexpected } => {
                eprintln!("Unexpected token {}, expecting {}{}", unexpected, expected, terminal.end_bold());
                pos.show(symbols, terminal);
                highlight_line(pos, symbols, terminal)?;
            },
            UnexpectedType { ref kind, pos } => {
                eprintln!("Expecting {} type{}", kind, terminal.end_bold());
                pos.show(symbols, terminal);
            },
            UnknownToken { pos, ref start } => {
                eprintln!("Unexpected start of token `{}`{}", start, terminal.end_bold());
                pos.show(symbols, terminal);
                highlight_line(pos, symbols, terminal)?;
            },
        }
        eprintln!("");

        Ok(())
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

fn highlight_line(pos: Pos, symbols: &Symbols<()>, terminal: &Terminal) -> io::Result<()> {
    let filename = symbols.name(pos.file);
    let mut file = File::open(filename)?;
    // TODO: support longer lines.
    const LENGTH: i64 = 4096;
    let mut buffer = [0; LENGTH as usize];
    let start = max(0, pos.byte as i64 - LENGTH / 2);
    file.seek(SeekFrom::Start(start as u64))?;
    let size_read = file.read(&mut buffer)?;
    let buffer = &buffer[..size_read];
    let current_pos = min(pos.byte as usize - start as usize, buffer.len());
    let start_of_line = buffer[..current_pos].iter().rposition(|byte| *byte == b'\n')
        .map(|pos| pos + 1)
        .unwrap_or(0);
    let end_of_line = buffer[current_pos..].iter().position(|byte| *byte == b'\n')
        .map(|pos| pos + current_pos)
        .unwrap_or_else(|| buffer.len());
    let line = &buffer[start_of_line..end_of_line];
    let num_spaces = num_text_size(pos.line as i64);
    let spaces = " ".repeat(num_spaces);
    eprintln!("{}{}{} |", terminal.bold(), terminal.blue(), spaces);
    eprintln!("{} |{}{} {}", pos.line, terminal.end_bold(), terminal.reset_color(), String::from_utf8_lossy(line));
    let count = min(pos.column as usize, line.len());
    let spaces_before_hint = " ".repeat(count);
    let hint = "^".repeat(pos.length);
    eprintln!("{}{}{} |{}{}{}{}", terminal.bold(), terminal.blue(), spaces, terminal.red(), spaces_before_hint, hint, terminal.reset_color());
    Ok(())
}

pub fn num_text_size(num: i64) -> usize {
    if num == 0 {
        return 1;
    }
    1 + (num as f64).log10().floor() as usize
}
