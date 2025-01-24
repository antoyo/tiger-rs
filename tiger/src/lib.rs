/*
 * Copyright (c) 2019-2020 Boucher, Antoni <bouanto@zoho.com>
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

#![cfg(not(test))]

/*
 * Compile with:
 cargo run -- tests/hello.tig
 * Assembly with:
 nasm -f elf64 tests/hello.s
 * Link with:
  ld -dynamic-linker /lib64/ld-linux-x86-64.so.2 -o hello /usr/lib/Scrt1.o /usr/lib/crti.o -L/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.3.0 \
                      -L/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.3.0/../../.. tests/hello.o target/debug/libruntime.a -lpthread -ldl --no-as-needed -lc -lgcc --as-needed \
                      -lgcc_s --no-as-needed /usr/lib/crtn.o
 */

mod collector;
mod data_layout;

use std::ffi::CStr;
//use std::process;
use std::io::{Read, Write, stdin, stdout};
use std::mem;
use std::os::raw::{c_char, c_void};
use std::ptr;

use collector::{Layout, GARBAGE_COLLECTOR};
use data_layout::{RECORD_DATA_LAYOUT_SIZE, STRING_DATA_LAYOUT_SIZE};

const WORD_SIZE: usize = 8;

/*extern {
    fn main();
}*/

#[no_mangle]
extern "C" fn ord(string: *const c_char) -> i64 {
    let cstring = unsafe { CStr::from_ptr(string_offset(string)) };
    cstring.to_str().expect("cstr to_str").chars().next().expect("ord string is empty") as i64
}

#[no_mangle]
extern "C" fn chr(num: i64) -> *const c_char {
    let char = num as u8;
    let ptr = GARBAGE_COLLECTOR.with(|collector| {
        collector.borrow_mut().allocate(Layout::String(1))
    });
    let string = ptr as *mut c_char;
    unsafe {
        let string_ptr = string_offset(string) as *mut c_char;
        *string_ptr = char as c_char;
        let string_ptr = string_ptr.offset(1);
        *string_ptr = 0;
    }
    string
}

#[no_mangle]
extern "C" fn getchar() -> *const c_char {
    let stdin = stdin();
    let char = stdin.bytes().next().expect("next char").expect("read stdin") as char;

    let ptr = GARBAGE_COLLECTOR.with(|collector| {
        collector.borrow_mut().allocate(Layout::String(1))
    });
    let string = ptr as *mut c_char;
    unsafe {
        let string_ptr = string_offset(string) as *mut c_char;
        *string_ptr = char as c_char;
        let string_ptr = string_ptr.offset(1);
        *string_ptr = 0;
    }
    string
}

#[no_mangle]
extern "C" fn getcharP(continuation: *const c_void) {
    let char = getchar();
    unsafe {
        let function: fn(*const c_char) = mem::transmute(get_function_pointer(continuation));
        function(char);
    }
}

#[no_mangle]
extern "C" fn concat(string1: *const c_char, string2: *const c_char) -> *const c_char {
    let cstring1 = unsafe { CStr::from_ptr(string_offset(string1)) };
    let cstring2 = unsafe { CStr::from_ptr(string_offset(string2)) };
    let mut string1 = cstring1.to_str().expect("to_str").to_string();
    let string2 = cstring2.to_str().expect("to_str").to_string();
    string1.push_str(&string2);

    let length = string1.len();
    let ptr = GARBAGE_COLLECTOR.with(|collector| {
        collector.borrow_mut().allocate(Layout::String(length))
    });
    let string = ptr as *mut c_char;
    unsafe {
        let mut string_ptr = string_offset(string) as *mut c_char;
        for byte in string1.as_bytes() {
            *string_ptr = *byte as c_char;
            string_ptr = string_ptr.offset(1);
        }
        *string_ptr = 0;
    }
    string
}

#[no_mangle]
extern "C" fn stringEqual(string1: *const c_char, string2: *const c_char) -> i64 {
    let cstring1 = unsafe { CStr::from_ptr(string_offset(string1)) };
    let cstring2 = unsafe { CStr::from_ptr(string_offset(string2)) };
    (cstring1 == cstring2) as i64
}

#[no_mangle]
extern "C" fn allocClass(data_layout: *const c_char) -> i64 {
    GARBAGE_COLLECTOR.with(|collector| {
        collector.borrow_mut().allocate(Layout::Class(data_layout))
    })
}

#[no_mangle]
extern "C" fn allocRecord(data_layout: *const c_char) -> i64 {
    GARBAGE_COLLECTOR.with(|collector| {
        collector.borrow_mut().allocate(Layout::Record(data_layout))
    })
}

#[no_mangle]
extern "C" fn initArray(length: usize, is_pointer: i64) -> i64 {
    GARBAGE_COLLECTOR.with(|collector| {
        collector.borrow_mut().allocate(Layout::Array(length, is_pointer != 0))
    })
}

#[no_mangle]
extern "C" fn print(string: *const c_char) {
    let cstring = unsafe { CStr::from_ptr(string_offset(string)) };
    if let Ok(string) = cstring.to_str() {
        print!("{}", string);
    }
    let _ = stdout().flush();
}

#[no_mangle]
extern "C" fn printP(string: *const c_char, continuation: *const c_void) {
    print(string);
    let function = get_function_pointer(continuation);
    function();
}

#[no_mangle]
extern "C" fn printi(num: i32) {
    println!("{}", num);
}

// Get the pointer where the string starts, i.e. after the data layout.
fn string_offset(ptr: *const c_char) -> *const c_char {
    let ptr = ptr as *const usize;
    unsafe {
        ptr.add(STRING_DATA_LAYOUT_SIZE) as *const c_char
    }
}

fn get_function_pointer(closure: *const c_void) -> fn() {
    let ptr = closure as *const usize;
    unsafe {
        mem::transmute(ptr::read_unaligned(ptr.add(RECORD_DATA_LAYOUT_SIZE)))
    }
}

/*#[no_mangle]
extern fn _start() {
    unsafe {
        main();
    }
    process::exit(0);
}*/
