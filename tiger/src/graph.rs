/*
 * Copyright (c) 2019 Boucher, Antoni <bouanto@zoho.com>
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

use std::ops::Deref;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Entry(usize);

impl Entry {
    pub fn index(&self) -> usize {
        self.0
    }
}

#[derive(Clone, Debug)]
pub struct Node<T> {
    element: T,
    predecessors: Vec<Entry>,
    successors: Vec<Entry>,
}

impl<T> Node<T> {
    fn new(element: T) -> Self {
        Self {
            element,
            predecessors: vec![],
            successors: vec![],
        }
    }

    /*pub fn get(&self) -> &T {
        &self.element
    }

    pub fn predecessors(&self) -> &[Entry] {
        &self.predecessors
    }*/

    pub fn successors(&self) -> &[Entry] {
        &self.successors
    }
}

impl<T> Deref for Node<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.element
    }
}

pub struct Graph<T> {
    nodes: Vec<Node<T>>,
}

impl<T> Graph<T> {
    pub fn new() -> Self {
        Self {
            nodes: vec![],
        }
    }

    pub fn insert(&mut self, node: T) -> Entry {
        let index = self.nodes.len();
        self.nodes.push(Node::new(node));
        Entry(index)
    }

    pub fn link(&mut self, node1: Entry, node2: Entry) {
        // TODO: we should not have these conditions.
        if !self.nodes[node1.index()].successors.contains(&node2) {
            self.nodes[node1.index()].successors.push(node2);
        }
        if !self.nodes[node2.index()].predecessors.contains(&node1) {
            self.nodes[node2.index()].predecessors.push(node1);
        }
    }

    pub fn nodes(&self) -> &[Node<T>] {
        &self.nodes
    }
}
