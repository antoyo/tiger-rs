/*
 * Copyright (c) 2018-2019 Boucher, Antoni <bouanto@zoho.com>
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

use std::cell::RefCell;
use std::rc::Rc;

use frame::Frame;
use temp::Label;

#[allow(type_alias_bounds)]
pub type Access<F: Frame> = (Level<F>, F::Access);

pub struct Level<F> {
    pub current: Rc<RefCell<F>>,
    parent: Option<Box<Level<F>>>,
}

impl<F> Clone for Level<F> {
    fn clone(&self) -> Self {
        Self {
            current: self.current.clone(),
            parent: self.parent.clone(),
        }
    }
}

impl<F: PartialEq> PartialEq for Level<F> {
    fn eq(&self, other: &Self) -> bool {
        self.current == other.current
    }
}

pub fn outermost<F: Frame>() -> Level<F> {
    Level {
        current: Rc::new(RefCell::new(F::new(Label::new(), vec![]))),
        parent: None,
    }
}

impl<F: Frame> Level<F> {
    pub fn new(parent: &Level<F>, name: Label, mut formals: Vec<bool>) -> Level<F> {
        formals.push(true); // for the static link.
        Level {
            current: Rc::new(RefCell::new(F::new(name, formals))),
            parent: Some(Box::new(parent.clone())),
        }
    }

    pub fn formals(&self) -> Vec<Access<F>> {
        self.current.borrow().formals().iter()
            .map(|access| (self.clone(), access.clone()))
            .collect()
    }
}

pub fn alloc_local<F: Frame>(level: &Level<F>, escape: bool) -> Access<F> {
    let level = level.clone();
    let frame_local = level.current.borrow_mut().alloc_local(escape);
    (level, frame_local)
}
