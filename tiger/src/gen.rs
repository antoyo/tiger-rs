/*
 * Copyright (c) 2018 Boucher, Antoni <bouanto@zoho.com>
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
use temp::Label;

#[allow(type_alias_bounds)]
pub type Access<F: Frame> = (Level<F>, F::Access);

#[derive(Clone)]
pub struct Level<F: Clone>(F);

pub fn outermost<F: Clone + Frame>() -> Level<F> {
    Level(F::new(Label::new(), vec![]))
}

impl<F: Clone + Frame> Level<F> {
    pub fn new(parent: &Level<F>, name: Label, mut formals: Vec<bool>) -> Level<F> {
        formals.push(true); // for the static link.
        Level(F::new(name, formals))
    }
}

fn formals<F: Clone + Frame>(level: Level<F>) -> Vec<Access<F>> {
    unimplemented!();
}

pub fn alloc_local<F: Clone + Frame>(level: &Level<F>, escape: bool) -> Access<F> {
    let mut level = level.clone();
    let frame_local = level.0.alloc_local(escape);
    (level, frame_local)
}
