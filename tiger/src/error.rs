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

use position::Pos;
use self::Error::*;
use terminal::Terminal;

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
    UnknownToken {
        pos: Pos,
        start: char,
    },
}

impl Error {
    pub fn show(&self, terminal: &Terminal) -> io::Result<()> {
        eprint!("{}{}error: {}", terminal.bold(), terminal.red(), terminal.reset_color());
        match *self {
            Eof => eprintln!("end of file"),
            InvalidEscape { ref escape, ref pos } => {
                eprintln!("Invalid escape \\{}{}", escape, terminal.end_bold());
                pos.show(terminal);
                highlight_line(pos, terminal)?;
            },
            Msg(ref string) => eprintln!("{}", string),
            Unclosed { ref pos, token } => {
                eprintln!("Unclosed {}{}", token, terminal.end_bold());
                pos.show(terminal);
                highlight_line(pos, terminal)?;
            },
            UnknownToken { ref pos, ref start } => {
                eprintln!("Unexpected start of token `{}`{}", start, terminal.end_bold());
                pos.show(terminal);
                highlight_line(pos, terminal)?;
            },
        }
        eprintln!("");

        Ok(())
    }
}

fn highlight_line(pos: &Pos, terminal: &Terminal) -> io::Result<()> {
    let mut file = File::open(&pos.filename)?;
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

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Msg(error.to_string())
    }
}
