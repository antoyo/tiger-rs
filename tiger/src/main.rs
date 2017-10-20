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

use std::env;
use std::fs::File;
use std::io::{self, BufReader};

mod ast;
mod error;
mod lexer;
mod parser;
mod position;
mod symbol;
mod token;

use lexer::Lexer;
use parser::Parser;

fn main() {
    drive().unwrap();
}

fn drive() -> Result<(), io::Error> {
    let mut args = env::args();
    args.next();
    if let Some(filename) = args.next() {
        let file = BufReader::new(File::open(filename)?);
        let lexer = Lexer::new(file);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse();
        println!("{:#?}", ast);
    }
    Ok(())
}
