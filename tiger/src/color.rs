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

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::marker::PhantomData;

use frame::Frame;
use graph::Node;
use liveness::InterferenceGraph;
use reg_alloc::Allocation;
use temp::Temp;

pub fn color<F: Frame>(interference_graph: InterferenceGraph, initial: Vec<Temp>)
    -> (Allocation, Vec<Temp>, BTreeSet<Temp>, BTreeSet<Temp>)
{
    let nodes = interference_graph.nodes().to_vec();
    let mut allocator = Allocator::<F>::new(interference_graph.move_list, interference_graph.worklist_moves);
    let allocation = allocator.allocate(initial, &nodes);
    (allocation, allocator.spill_nodes, allocator.colored_nodes, allocator.coalesced_nodes)
}

struct Allocator<F> {
    active_moves: BTreeSet<(Temp, Temp)>,
    adjacency_list: HashMap<Temp, BTreeSet<Temp>>,
    adjacency_set: HashSet<(Temp, Temp)>,
    alias: HashMap<Temp, Temp>,
    coalesced_moves: BTreeSet<(Temp, Temp)>,
    coalesced_nodes: BTreeSet<Temp>,
    colored_nodes: BTreeSet<Temp>,
    constrained_moves: HashSet<(Temp, Temp)>,
    degree: HashMap<Temp, usize>,
    freeze_worklist: BTreeSet<Temp>,
    frozen_moves: HashSet<(Temp, Temp)>,
    move_list: HashMap<Temp, BTreeSet<(Temp, Temp)>>,
    precolored: HashMap<Temp, &'static str>,
    register_count: usize,
    select_stack: Vec<Temp>,
    simplify_worklist: BTreeSet<Temp>,
    spill_nodes: Vec<Temp>,
    spill_worklist: BTreeSet<Temp>,
    worklist_moves: BTreeSet<(Temp, Temp)>,
    _marker: PhantomData<F>,
}

impl<F: Frame> Allocator<F> {
    fn new(move_list: HashMap<Temp, BTreeSet<(Temp, Temp)>>, worklist_moves: BTreeSet<(Temp, Temp)>) -> Self {
        Self {
            active_moves: BTreeSet::new(),
            adjacency_list: HashMap::new(),
            adjacency_set: HashSet::new(),
            alias: HashMap::new(),
            coalesced_moves: BTreeSet::new(),
            coalesced_nodes: BTreeSet::new(),
            colored_nodes: BTreeSet::new(),
            constrained_moves: HashSet::new(),
            degree: HashMap::new(),
            freeze_worklist: BTreeSet::new(),
            frozen_moves: HashSet::new(),
            move_list,
            precolored: F::temp_map(),
            register_count: F::register_count(),
            select_stack: vec![],
            simplify_worklist: BTreeSet::new(),
            spill_nodes: vec![],
            spill_worklist: BTreeSet::new(),
            worklist_moves,
            _marker: PhantomData,
        }
    }

    fn add_edge(&mut self, u: Temp, v: Temp) {
        if !self.adjacency_set.contains(&(u, v)) && u != v {
            self.adjacency_set.insert((u, v));
            self.adjacency_set.insert((v, u));
            if !self.precolored.contains_key(&u) {
                self.adjacency_list.entry(u)
                    .or_insert(BTreeSet::new())
                    .insert(v);
                *self.degree.entry(u).or_insert(0) += 1;
            }
            if !self.precolored.contains_key(&v) {
                self.adjacency_list.entry(v)
                    .or_insert(BTreeSet::new())
                    .insert(u);
                *self.degree.entry(v).or_insert(0) += 1;
            }
        }
    }

    fn add_worklist(&mut self, u: Temp) {
        if !self.precolored.contains_key(&u) && !self.move_related(u) && self.degree[&u] < self.register_count {
            self.freeze_worklist.remove(&u);
            self.simplify_worklist.insert(u);
        }
    }

    fn adjacent(&mut self, temp: Temp) -> HashSet<Temp> {
        self.adjacency_list.entry(temp)
            .or_insert_with(|| BTreeSet::new()) // TODO: insert at another place?
            .difference(&self.select_stack.iter().cloned().collect::<BTreeSet<_>>().union(&self.coalesced_nodes).cloned().collect())
            .cloned()
            .collect()
    }

    fn allocate(&mut self, initial: Vec<Temp>, nodes: &[Node<Temp>]) -> Allocation {
        self.build(nodes);
        self.make_worklist(initial);

        while !self.simplify_worklist.is_empty() || !self.spill_worklist.is_empty() || !self.worklist_moves.is_empty()
            || !self.freeze_worklist.is_empty()
        {
            if !self.simplify_worklist.is_empty() {
                self.simplify();
            }
            else if !self.worklist_moves.is_empty() {
                self.coalesce();
            }
            else if !self.freeze_worklist.is_empty() {
                self.freeze();
            }
            else if !self.spill_worklist.is_empty() {
                self.select_spill();
            }
        }

        self.assign_colors()
    }

    fn assign_colors(&mut self) -> Allocation {
        let mut colors = BTreeMap::new();
        for (&precolored, _) in &self.precolored {
            colors.insert(precolored, precolored);
        }
        while let Some(temp) = self.select_stack.pop() {
            let mut ok_colors: BTreeSet<Temp> = F::registers().iter().cloned().collect();

            for neighbor in &self.adjacency_list[&temp] {
                let neighbor = self.get_alias(*neighbor);
                if self.colored_nodes.contains(&neighbor) || self.precolored.contains_key(&neighbor) {
                    if let Some(color) = colors.get(&neighbor) {
                        ok_colors.remove(color);
                    }
                }
            }

            if let Some(color) = set_pop(&mut ok_colors) {
                self.colored_nodes.insert(temp);
                colors.insert(temp, color);
            }
            else {
                self.spill_nodes.push(temp);
            }
        }

        for node in &self.coalesced_nodes {
            if let Some(&color) = colors.get(&self.get_alias(*node)) {
                colors.insert(*node, color);
            }
        }

        colors
    }

    fn build(&mut self, nodes: &[Node<Temp>]) {
        for node in nodes {
            let temp = *node.get();
            for &predecessor in node.predecessors() {
                self.add_edge(temp, *nodes[predecessor.index()].get());
            }
            for &successor in node.successors() {
                self.add_edge(temp, *nodes[successor.index()].get());
            }
        }

        for (node, _) in &self.precolored {
            *self.degree.entry(node.clone()).or_insert(0) = usize::max_value();
        }
    }

    fn coalesce(&mut self) {
        let mov = set_pop(&mut self.worklist_moves).expect("pop worklist_moves");
        let (x, y) = mov;
        let x = self.get_alias(x);
        let y = self.get_alias(y);
        let (u, v) =
            if self.precolored.contains_key(&y) {
                (y, x)
            }
            else {
                (x, y)
            };

        let nodes = self.adjacent(u).union(&self.adjacent(v)).cloned().collect();
        if u == v {
            self.coalesced_moves.insert(mov);
            self.add_worklist(u);
        }
        else if self.precolored.contains_key(&v) || self.adjacency_set.contains(&(u, v)) {
            self.constrained_moves.insert(mov);
            self.add_worklist(u);
            self.add_worklist(v);
        }
        else if self.precolored.contains_key(&u) && self.adjacent(v).iter().all(|temp| self.ok(*temp, u)) ||
            !self.precolored.contains_key(&u) && self.conservative(&nodes)
        {
            self.coalesced_moves.insert(mov);
            self.combine(u, v);
            self.add_worklist(u);
        }
        else {
            self.active_moves.insert(mov);
        }
    }

    fn combine(&mut self, u: Temp, v: Temp) {
        if self.freeze_worklist.contains(&v) {
            self.freeze_worklist.remove(&v);
        }
        else {
            self.spill_worklist.remove(&v);
        }

        self.coalesced_nodes.insert(v);
        self.alias.insert(v, u);
        let nodes = self.move_list.entry(v)
            .or_insert_with(|| BTreeSet::new()).clone();
        self.move_list.entry(u)
            .or_insert_with(|| BTreeSet::new())
            .extend(&nodes);
        let mut moves = HashSet::new();
        moves.insert(v);
        self.enable_moves(&moves);

        let nodes: HashSet<Temp> = self.adjacent(v);
        for temp in nodes {
            self.add_edge(temp, u);
            self.decrement_degree(temp);
        }
        if self.degree[&u] >= self.register_count && self.freeze_worklist.contains(&u) {
            self.freeze_worklist.remove(&u);
            self.spill_worklist.insert(u);
        }
    }

    fn conservative(&self, nodes: &HashSet<Temp>) -> bool {
        let mut k = 0;
        for node in nodes {
            if self.degree[&node] >= self.register_count {
                k += 1;
            }
        }
        k < self.register_count
    }

    fn decrement_degree(&mut self, temp: Temp) {
        let degree = self.degree[&temp];
        self.degree.get_mut(&temp)
            .map(|degree| *degree -= 1);

        if degree == self.register_count {
            let mut nodes = self.adjacent(temp);
            nodes.insert(temp);
            self.enable_moves(&nodes);
            self.spill_worklist.remove(&temp);
            if self.move_related(temp) {
                self.freeze_worklist.insert(temp);
            }
            else {
                self.simplify_worklist.insert(temp);
            }
        }
    }

    fn enable_moves(&mut self, nodes: &HashSet<Temp>) {
        for node in nodes {
            for mov in self.node_moves(*node) {
                if self.active_moves.contains(&mov) {
                    self.active_moves.remove(&mov);
                    self.worklist_moves.insert(mov);
                }
            }
        }
    }

    fn freeze(&mut self) {
        let u = set_pop(&mut self.freeze_worklist).expect("pop freeze_worklist");
        self.simplify_worklist.insert(u);
        self.freeze_moves(u);
    }

    fn freeze_moves(&mut self, u: Temp) {
        for mov in self.node_moves(u) {
            let (x, y) = mov;
            let v =
                if self.get_alias(y) == self.get_alias(u) {
                    self.get_alias(x)
                }
                else {
                    self.get_alias(y)
                };
            self.active_moves.remove(&mov);
            self.frozen_moves.insert(mov);
            if self.node_moves(v).is_empty() && self.degree[&v] < self.register_count {
                self.freeze_worklist.remove(&v);
                self.simplify_worklist.insert(v);
            }
        }
    }

    fn get_alias(&self, node: Temp) -> Temp {
        if self.coalesced_nodes.contains(&node) {
            self.get_alias(self.alias[&node])
        }
        else {
            node
        }
    }

    fn make_worklist(&mut self, initial: Vec<Temp>) {
        for n in initial.into_iter() {
            let degree = self.degree.entry(n).or_insert(0);
            if *degree >= self.register_count {
                self.spill_worklist.insert(n);
            }
            else if self.move_related(n) {
                self.freeze_worklist.insert(n);
            }
            else {
                self.simplify_worklist.insert(n);
            }
        }
    }

    fn move_related(&mut self, temp: Temp) -> bool {
        !self.node_moves(temp).is_empty()
    }

    fn node_moves(&mut self, temp: Temp) -> HashSet<(Temp, Temp)> {
        self.move_list.entry(temp).or_insert_with(|| BTreeSet::new())
            .intersection(&self.active_moves.union(&self.worklist_moves).cloned().collect())
            .cloned()
            .collect()
    }

    fn ok(&self, temp: Temp, u: Temp) -> bool {
        self.degree[&temp] < self.register_count || self.precolored.contains_key(&temp) || self.adjacency_set.contains(&(temp, u))
    }

    fn select_spill(&mut self) {
        let temp = self.spill_worklist.iter().min_by_key(|node| self.spill_cost(**node)).expect("empty spill_worklist");
        let temp = *temp;
        self.spill_worklist.remove(&temp);
        self.simplify_worklist.insert(temp);
        self.freeze_moves(temp);
    }

    fn simplify(&mut self) {
        let temp = set_pop(&mut self.simplify_worklist).expect("pop simplify_worklist");
        self.select_stack.push(temp);

        for neighbor in self.adjacent(temp) {
            self.decrement_degree(neighbor);
        }
    }

    fn spill_cost(&self, node: Temp) -> i32 {
        // FIXME: The number of uses and defs weighted by occurrence in loops and nested loops.
        -(self.degree[&node] as i32)
    }
}

fn set_pop<T: Clone + Ord>(set: &mut BTreeSet<T>) -> Option<T> {
    set.iter().next().cloned()
        .and_then(|element| set.take(&element))
}
