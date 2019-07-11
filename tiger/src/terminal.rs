/*
 * Copyright (c) 2020 Boucher, Antoni <bouanto@zoho.com>
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

use std::io::stderr;
use std::os::raw::c_int;
use std::os::unix::io::AsRawFd;

const BOLD: &str = "\x1b[1m";
const BLUE: &str = "\x1b[34m";
const END_BOLD: &str = "\x1b[22m";
const RED: &str = "\x1b[31m";
const RESET_COLOR: &str = "\x1b[39;49m";

pub struct Terminal {
    is_a_tty: bool,
}

impl Terminal {
    pub fn new() -> Self {
        Self {
            is_a_tty: stderr_is_a_tty(),
        }
    }

    pub fn bold(&self) -> &str {
        if self.is_a_tty {
            BOLD
        }
        else {
            ""
        }
    }

    pub fn blue(&self) -> &str {
        if self.is_a_tty {
            BLUE
        }
        else {
            ""
        }
    }

    pub fn end_bold(&self) -> &str {
        if self.is_a_tty {
            END_BOLD
        }
        else {
            ""
        }
    }

    pub fn red(&self) -> &str {
        if self.is_a_tty {
            RED
        }
        else {
            ""
        }
    }

    pub fn reset_color(&self) -> &str {
        if self.is_a_tty {
            RESET_COLOR
        }
        else {
            ""
        }
    }
}

fn stderr_is_a_tty() -> bool {
    unsafe {
        isatty(stderr().as_raw_fd()) != 0
    }
}

extern "C" {
    fn isatty(fd: c_int) -> c_int;
}
