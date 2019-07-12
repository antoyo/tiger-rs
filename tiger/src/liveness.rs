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

use std::collections::{BTreeSet, HashMap, HashSet};

use flow::FlowGraph;
use frame::{Frame, x86_64::X86_64};
use graph::{Entry, Graph, Node};
use temp::Temp;

pub struct InterferenceGraph {
    graph: Graph<Temp>,
    _temp_nodes: HashMap<Temp, Entry>,
    pub move_list: HashMap<Temp, BTreeSet<(Temp, Temp)>>,
    pub worklist_moves: BTreeSet<(Temp, Temp)>,
}

impl InterferenceGraph {
    pub fn nodes(&self) -> &[Node<Temp>] {
        self.graph.nodes()
    }

    pub fn _show(&self) {
        let nodes = self.graph.nodes();
        for node in self.graph.nodes() {
            let name = X86_64::special_name(node.get().clone())
                .map(ToString::to_string)
                .unwrap_or_else(|| format!("{:?}", node.get()));
            println!("Node: {}", name);
            for &neighbor in node.predecessors() {
                let name = X86_64::special_name(nodes[neighbor.index()].get().clone())
                    .map(ToString::to_string)
                    .unwrap_or_else(|| format!("{:?}", nodes[neighbor.index()].get()));
                println!("<<< {}", name);
            }
            for &neighbor in node.successors() {
                let name = X86_64::special_name(nodes[neighbor.index()].get().clone())
                    .map(ToString::to_string)
                    .unwrap_or_else(|| format!("{:?}", nodes[neighbor.index()].get()));
                println!(">>> {}", name);
            }
        }
    }
}

pub fn interference_graph(graph: FlowGraph) -> InterferenceGraph {
    let mut live_in: HashMap<usize, HashSet<Temp>> = HashMap::new();
    let mut live_out: HashMap<usize, HashSet<Temp>> = HashMap::new();

    let mut worklist_moves = BTreeSet::new();

    let mut new_live_in = HashMap::new();
    let mut new_live_out = HashMap::new();

    loop {
        for (index, node) in graph.nodes().iter().enumerate() {
            new_live_in.insert(index, live_in.get(&index).cloned().unwrap_or_default());
            new_live_out.insert(index, live_out.get(&index).cloned().unwrap_or_default());

            let mut set = node.uses.clone();
            let out = live_out.entry(index).or_insert_with(|| HashSet::new());
            set.extend(out.difference(&node.defines));
            live_in.insert(index, set);

            let mut set = HashSet::new();
            for &successor in node.successors() {
                let in_set = live_in.entry(successor.index()).or_insert_with(|| HashSet::new());
                set.extend(in_set.clone());
            }
            live_out.insert(index, set);
        }

        if new_live_in == live_in && new_live_out == live_out {
            break;
        }
    }

    let mut interference_graph = Graph::new();
    let mut temp_nodes = HashMap::new();
    let mut move_list = HashMap::new();

    for (index, node) in graph.nodes().iter().enumerate() {
        for define in &node.defines {
            let define_node = interference_graph.insert(define.clone());
            temp_nodes.insert(define.clone(), define_node);
            for temp in &live_out[&index] {
                let temp_node = interference_graph.insert(temp.clone());
                temp_nodes.insert(temp.clone(), temp_node);
                interference_graph.link(define_node, temp_node);
            }
        }
        if node.is_move {
            if let Some(define) = node.defines.iter().next() {
                if let Some(use_) = node.uses.iter().next() {
                    // NOTE: it is possible that a move does not include a define in case it moves a constant.
                    worklist_moves.insert((*define, *use_));

                    for temp in node.defines.iter().chain(node.uses.iter()) {
                        move_list.entry(*temp)
                            .or_insert_with(|| BTreeSet::new())
                            .insert((*define, *use_));
                    }
                }
            }
        }
    }

    InterferenceGraph {
        graph: interference_graph,
        _temp_nodes: temp_nodes,
        move_list,
        worklist_moves,
    }
}
