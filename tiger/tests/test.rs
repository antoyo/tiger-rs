/*
 * Copyright (c) 2019-2024 Boucher, Antoni <bouanto@zoho.com>
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

use std::fs::{self, remove_file};
use std::io::{Read, Write};
use std::path::Path;
use std::process::{Command, Stdio};

#[test]
fn test_execution() {
    let files = [
        "array",
        "array_assignment",
        "class",
        "comments",
        "conditions",
        "cycle",
        "escapes",
        "functional",
        "functions",
        "gc",
        "hello",
        "hello1",
        "hello2",
        "hello3",
        "hello5",
        "helloPure",
        "integers",
        "lib",
        "loops",
        "merge",
        "nested",
        "polymorphism",
        "prettyprint",
        "pureTree",
        "queens",
        "record",
        "spill",
        "strings",
        "vars",
    ];

    let mut success = true;
    let mut success_count = 0;
    let mut failure_count = 0;
    for file in &files {
        print!("  Test {}... ", file);
        let _ = remove_file(format!("./tests/{}", file));
        Command::new("./target/debug/tiger")
            .arg(format!("tests/{}.tig", file))
            .status()
            .expect("compile");
        let mut child = Command::new(format!("./tests/{}", file))
            .stdout(Stdio::piped())
            .stdin(Stdio::piped())
            .spawn().expect("spawn");
        if Path::new(&format!("./tests/{}.stdin", file)).exists() {
            let input = fs::read(format!("./tests/{}.stdin", file)).expect("read");
            child.stdin.as_ref().expect("stdin").write_all(&input).expect("write stdin");
        }
        let mut buffer = vec![];
        let read_size = child.stdout.as_mut().expect("stdout").read_to_end(&mut buffer).expect("output");
        child.wait().expect("wait running compiled binary");
        let output = String::from_utf8_lossy(&buffer[..read_size]);
        let expected_output = String::from_utf8(fs::read(format!("./tests/{}.stdout", file)).expect("read")).expect("String::from_utf8");
        if output != expected_output {
            success = false;
            println!(" FAIL");
            failure_count += 1;
        }
        else {
            println!(" ok");
            success_count += 1;
        }
    }

    println!("\ntest result: {} passed, {} failed\n", success_count, failure_count);

    if !success {
        panic!();
    }
}
