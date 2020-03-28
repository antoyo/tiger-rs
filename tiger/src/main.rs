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

/*
 * TODO: escape ' in assembly strings.
 * FIXME: rdi calle-save register does not seem to be restored (useless spill?).
 * TODO: Clean mov rbx, [rbp + -16] into mov rbx, [rbp - 16].
 * TODO: emit mov, push, mov, push instead of mov, mov, push, push.
 * FIXME: escape analysis (tests/functions.tig) where argument are put in the frame.
 */

#![allow(unknown_lints)]
#![feature(box_patterns)]

mod asm;
mod asm_gen;
mod ast;
mod canon;
mod env;
mod error;
mod escape;
mod flow;
mod frame;
mod gen;
mod graph;
mod ir;
mod lexer;
mod liveness;
mod parser;
mod position;
mod reg_alloc;
mod semant;
mod symbol;
mod temp;
mod terminal;
mod token;
mod types;

use std::env::args;
use std::fs::{File, read_dir};
use std::io::{self, BufReader, Write};
use std::path::PathBuf;
use std::process::Command;
use std::rc::Rc;

use asm_gen::Gen;
use canon::{basic_blocks, linearize, trace_schedule};
use env::Env;
use error::Error;
use escape::find_escapes;
use frame::{Fragment, Frame};
use frame::x86_64::X86_64;
use lexer::Lexer;
use parser::Parser;
use reg_alloc::alloc;
use semant::SemanticAnalyzer;
use symbol::{Strings, Symbols};
use terminal::Terminal;

fn main() {
    let strings = Rc::new(Strings::new());
    let mut symbols = Symbols::new(Rc::clone(&strings));
    if let Err(error) = drive(strings, &mut symbols) {
        let terminal = Terminal::new();
        if let Err(error) = error.show(&symbols, &terminal) {
            eprintln!("Error printing errors: {}", error);
        }
    }
}

fn drive(strings: Rc<Strings>, symbols: &mut Symbols<()>) -> Result<(), Error> {
    let mut args = args();
    args.next();
    if let Some(filename) = args.next() {
        let file = BufReader::new(File::open(&filename)?);
        let file_symbol = symbols.symbol(&filename);
        let lexer = Lexer::new(file, file_symbol);
        let main_symbol = symbols.symbol("main");
        let mut parser = Parser::new(lexer, symbols);
        let ast = parser.parse()?;
        let escape_env = find_escapes(&ast, Rc::clone(&strings));
        let mut env = Env::<X86_64>::new(&strings, escape_env);
        {
            let semantic_analyzer = SemanticAnalyzer::new(&mut env, Rc::clone(&strings));
            let fragments = semantic_analyzer.analyze(main_symbol, ast)?;

            let mut asm_output_path = PathBuf::from(&filename);
            asm_output_path.set_extension("s");
            let mut file = File::create(&asm_output_path)?;

            writeln!(file, "global main\n")?;

            for (function_name, _) in env::external_functions() {
                writeln!(file, "extern {}", function_name)?;
            }
            writeln!(file, "")?;

            writeln!(file, "section .data")?;
            writeln!(file, "    align 2")?;

            for fragment in &fragments {
                match fragment {
                    Fragment::Function { .. } => (),
                    Fragment::Str(label, string) => {
                        writeln!(file, "    {}: db {}, 0", label, to_nasm(string))?;
                    },
                }
            }

            writeln!(file, "\nsection .text")?;

            for fragment in fragments {
                match fragment {
                    Fragment::Function { body, frame } => {
                        let mut frame = frame.borrow_mut();
                        let body = frame.proc_entry_exit1(body);

                        let statements = linearize(body);
                        let (basic_blocks, done_label) = basic_blocks(statements);
                        let statements = trace_schedule(basic_blocks, done_label);

                        let mut generator = Gen::new();
                        for statement in statements {
                            generator.munch_statement(statement);
                        }
                        let instructions = generator.get_result();
                        let instructions = frame.proc_entry_exit2(instructions);

                        let instructions = alloc::<X86_64>(instructions, &mut *frame);

                        let subroutine = frame.proc_entry_exit3(instructions);
                        writeln!(file, "{}", subroutine.prolog)?;
                        for instruction in subroutine.body {
                            let instruction = instruction.to_string::<X86_64>();
                            if !instruction.is_empty() {
                                writeln!(file, "    {}", instruction)?;
                            }
                        }
                        writeln!(file, "    {}", subroutine.epilog)?;
                    },
                    Fragment::Str(_, _) => (),
                }
            }

            let status = Command::new("nasm")
                .args(&["-f", "elf64", asm_output_path.to_str().expect("asm output path")])
                .status();

            if let Ok(return_code) = status {
                if return_code.success() {
                    let mut object_output_path = PathBuf::from(&filename);
                    object_output_path.set_extension("o");
                    let mut executable_output_path = PathBuf::from(&filename);
                    executable_output_path.set_extension("");
                    Command::new("ld")
                        .args(&[
                            "-dynamic-linker", "/lib64/ld-linux-x86-64.so.2", "-o",
                            executable_output_path.to_str().expect("executable output path"),
                            "/usr/lib/Scrt1.o", "/usr/lib/crti.o", &format!("-L{}", get_gcc_lib_dir()?),
                            "-L/usr/lib64/",
                            object_output_path.to_str().expect("object output path"),
                            "target/debug/libruntime.a", "-lpthread", "-ldl", "--no-as-needed", "-lc", "-lgcc", "--as-needed",
                            "-lgcc_s", "--no-as-needed", "/usr/lib/crtn.o"
                        ])
                        .status()
                        .expect("link");
                }
            }
        }
        env.end_scope(); // TODO: move after the semantic analysis?
    }
    Ok(())
}

fn to_nasm(string: &str) -> String {
    let mut result = "'".to_string();
    for char in string.chars() {
        let string =
            match char {
                '\n' | '\t' => format!("', {}, '", char as u32),
                _ => char.to_string(),
            };
        result.push_str(&string);
    }
    result.push('\'');
    result
}

fn get_gcc_lib_dir() -> io::Result<String> {
    let directory = "/usr/lib64/gcc/x86_64-pc-linux-gnu/";
    let files = read_dir(directory)?;
    for file in files {
        let file = file?;
        if file.metadata()?.is_dir() {
            return file.file_name().to_str()
                .map(|str| format!("{}{}", directory, str))
                .ok_or(io::ErrorKind::InvalidData.into());
        }
    }
    Err(io::ErrorKind::NotFound.into())
}
