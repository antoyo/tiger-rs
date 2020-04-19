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

use frame::Frame;
use gen::IR;

pub fn optimize<F: Frame>(ir: &mut IR<F>) {
    let mut optimizer = Opt::new(ir);
    optimizer.optimize();
}

struct Opt<'a, F: Frame> {
    ir: &'a mut IR<F>,
}

impl<'a, F: Frame> Opt<'a, F> {
    fn new(ir: &'a mut IR<F>) -> Self {
        Self {
            ir,
        }
    }

    fn optimize(&mut self) {
        let names: Vec<_> = self.ir.functions.keys()
            .cloned()
            .collect(); // TODO: remove collect and clone?
        for func_name in names {
            self.inline(&func_name);
        }
    }

    fn inline(&mut self, _func_name: &str) {
    }
}
