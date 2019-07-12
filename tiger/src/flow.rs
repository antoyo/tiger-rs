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

use std::collections::{HashMap, HashSet};

use asm::Instruction;
use graph::{self, Entry, Graph};
use temp::{Label, Temp};

pub struct Node {
    pub defines: HashSet<Temp>,
    pub uses: HashSet<Temp>,
    pub is_move: bool,
}

pub struct FlowGraph {
    control_flow_graph: Graph<Node>,
}

impl FlowGraph {
    pub fn nodes(&self) -> &[graph::Node<Node>] {
        self.control_flow_graph.nodes()
    }
}

struct GraphBuilder<'a> {
    instructions: &'a [Instruction],
    control_flow_graph: &'a mut Graph<Node>,
    label_map: HashMap<Label, usize>,
    visited: HashMap<usize, Entry>,
}

impl<'a> GraphBuilder<'a> {
    fn build(&mut self, current_index: usize, predecessor: Option<Entry>) {
        if let Some(&entry) = self.visited.get(&current_index) {
            if let Some(predecessor) = predecessor {
                self.control_flow_graph.link(predecessor, entry);
            }
            return;
        }

        let instruction = &self.instructions[current_index];
        let is_move =
            match instruction {
                Instruction::Move { .. } => true,
                _ => false,
            };
        let defines =
            match instruction {
                Instruction::Move { destination, .. } | Instruction::Operation { destination, .. } =>
                    destination.iter().cloned().collect(),
                _ => HashSet::new(),
            };
        let uses =
            match instruction {
                Instruction::Move { source, .. } | Instruction::Operation { source, .. } =>
                    source.iter().cloned().collect(),
                _ => HashSet::new(),
            };
        let node = Node {
            defines,
            uses,
            is_move,
        };
        let entry = self.control_flow_graph.insert(node);
        self.visited.insert(current_index, entry);
        if let Some(predecessor) = predecessor {
            self.control_flow_graph.link(predecessor, entry);
        }
        if let Instruction::Operation { ref assembly, ref jump, .. } = instruction {
            if let Some(ref jump) = *jump {
                for jump in jump {
                    self.build(self.label_map[jump], Some(entry));
                }
            }

            if assembly.starts_with("jmp ") {
                return; // Do not fallthrough for unconditional jump.
            }
        }

        if current_index + 1 < self.instructions.len() {
            self.build(current_index + 1, Some(entry));
        }
    }
}


pub fn instructions_to_graph(instructions: &[Instruction]) -> FlowGraph {
    let mut label_map = HashMap::new();

    for (index, instruction) in instructions.iter().enumerate() {
        if let Instruction::Label { ref label, .. } = instruction {
            label_map.insert(label.clone(), index);
        }
    }

    let mut control_flow_graph = Graph::new();
    let mut graph_builder = GraphBuilder {
        instructions,
        control_flow_graph: &mut control_flow_graph,
        label_map,
        visited: HashMap::new(),
    };
    graph_builder.build(0, None);

    FlowGraph {
        control_flow_graph,
    }
}
