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

mod error;
mod lexer;
mod position;
mod terminal;
mod token;

use std::env::args;
use std::fs::File;
use std::io::BufReader;

use error::Error;
use lexer::Lexer;
use terminal::Terminal;

fn main() {
    if let Err(error) = drive() {
        let terminal = Terminal::new();
        if let Err(error) = error.show(&terminal) {
            eprintln!("Error printing errors: {}", error);
        }
    }
}

fn drive() -> Result<(), Error> {
    let mut args = args();
    args.next();
    if let Some(filename) = args.next() {
        let file = BufReader::new(File::open(&filename)?);
        let mut lexer = Lexer::new(file, &filename);
        loop {
            let token = lexer.token();
            match token {
                Ok(token) => println!("{:?}", token),
                Err(Error::Eof) => break,
                Err(error) => return Err(error),
            }
        }
    }
    Ok(())
}
