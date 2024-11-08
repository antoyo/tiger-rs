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

use std::cmp::Ordering;
use std::collections::{BTreeSet, BTreeMap, HashMap, HashSet};

use flow::FlowGraph;
use frame::Frame;
use temp::{Label, Temp, TempMap};

#[derive(Eq, PartialEq, PartialOrd, Ord)]
pub struct StackLocation(pub i64);

pub struct LiveIntervals {
    pub intervals: Vec<(Temp, Interval)>,
    pub precolored_intervals: HashMap<Temp, Interval>,
    pub instructions_visited: HashSet<usize>,
    pub temp_pointers: Vec<(Label, BTreeSet<StackLocation>)>,
}

pub fn live_intervals<F: Frame>(graph: FlowGraph, temp_map: &TempMap, do_stack_live_analysis: bool) -> LiveIntervals {
    let mut live_in: HashMap<usize, BTreeSet<Temp>> = HashMap::new();
    let mut live_out: HashMap<usize, BTreeSet<Temp>> = HashMap::new();

    let mut new_live_in = HashMap::new();
    let mut new_live_out = HashMap::new();

    let mut stack_live_in: HashMap<usize, BTreeSet<i64>> = HashMap::new();
    let mut stack_live_out: HashMap<usize, BTreeSet<i64>> = HashMap::new();

    let mut new_stack_live_in = HashMap::new();
    let mut new_stack_live_out = HashMap::new();

    loop {
        for (index, node) in graph.nodes().iter().enumerate().rev() {
            new_live_in.insert(index, live_in.get(&index).cloned().unwrap_or_default());
            new_live_out.insert(index, live_out.get(&index).cloned().unwrap_or_default());

            if do_stack_live_analysis {
                new_stack_live_in.insert(index, stack_live_in.get(&index).cloned().unwrap_or_default());
                new_stack_live_out.insert(index, stack_live_out.get(&index).cloned().unwrap_or_default());
            }

            let mut stack_set = BTreeSet::new();
            let mut set = BTreeSet::new();
            for &successor in node.successors() {
                let in_set = live_in.entry(successor.index()).or_default();
                set.extend(in_set.clone());

                if do_stack_live_analysis {
                    let in_set = stack_live_in.entry(successor.index()).or_default();
                    stack_set.extend(in_set.clone());
                }
            }
            live_out.insert(index, set);
            if do_stack_live_analysis {
                stack_live_out.insert(index, stack_set);
            }

            let mut set = node.uses.clone();
            let out = live_out.entry(index).or_default();
            set.extend(out.difference(&node.defines));
            live_in.insert(index, set);

            if do_stack_live_analysis {
                let mut set = node.stack_uses.clone();
                let out = stack_live_out.entry(index).or_default();
                set.extend(out.difference(&node.stack_defines));
                stack_live_in.insert(index, set);
            }
        }

        if new_live_in == live_in && new_live_out == live_out && new_stack_live_in == stack_live_in && new_stack_live_out == stack_live_out {
            break;
        }
    }

    if do_stack_live_analysis {
        // TODO: remove this extension of stack variable liveness. We probably want to identify derived pointers instead.
        let nodes = graph.nodes();
        let mut queue = vec![0];
        let mut visited = HashSet::new();
        visited.insert(0);
        while let Some(index) = queue.pop() {
            let node = &nodes[index];
            if node.stack_defines.len() == 1 {
                let define = *node.stack_defines.iter().next().expect("first stack define");
                stack_live_out.entry(0).or_default()
                    .insert(define);
            }
            for successor in node.successors() {
                let live_out = stack_live_out.entry(index).or_default().clone();
                let successor_live_out = stack_live_out.entry(successor.index()).or_default();
                if !live_out.is_empty() {
                    successor_live_out.extend(live_out.iter());
                }
                let index = successor.index();
                if !visited.contains(&index) {
                    visited.insert(index);
                    queue.push(index);
                }
            }
        }
    }

    let mut intervals: BTreeMap<Temp, Interval> = BTreeMap::new();
    let mut precolored_intervals: HashMap<Temp, Interval> = HashMap::new();

    let precolored = F::temp_map();

    let mut temps: BTreeSet<Temp> = BTreeSet::new();

    for node in graph.nodes() {
        temps.extend(&node.defines);
        temps.extend(&node.uses);
    }

    let nodes = graph.nodes();

    for temp in &temps {
        // Since we don't know how many instructions will be added yet, we set the range up to the
        // max value.
        let interval = Interval::new(*temp, usize::MAX);
        if precolored.contains_key(temp) {
            precolored_intervals.insert(*temp, interval);
        }
        else {
            intervals.insert(*temp, interval);
        }
    }

    let mut temp_pointers = vec![];

    for (index, node) in nodes.iter().enumerate() {
        if let Some(ref return_label) = node.return_label {
            let empty_set = BTreeSet::new();
            let stack_vars = stack_live_out.get(&index).unwrap_or(&empty_set);
            let pointer_temps: BTreeSet<_> = stack_vars.iter().filter_map(|&stack_var| {
                    if temp_map.contains_var(stack_var) {
                        Some(StackLocation(stack_var))
                    }
                    else {
                        None
                    }
            })
                .collect();
            temp_pointers.push((return_label.clone(), pointer_temps));
        }

        for temp in &temps {
            if !live_in[&index].contains(temp) && !live_out[&index].contains(temp) {
                if precolored.contains_key(temp) {
                    precolored_intervals.get_mut(temp).expect("precolored interval").remove(node.instruction_index);
                }
                else {
                    intervals.get_mut(temp).expect("interval").remove(node.instruction_index);
                }
            }
        }
    }

    let mut instructions_visited = HashSet::new();

    for (index, node) in graph.nodes().iter().enumerate() {
        instructions_visited.insert(node.instruction_index);
        for def in &node.defines {
            // If a temporary is unused, insert a one-instruction interval.
            if !live_out[&index].contains(def) {
                let interval =
                    if precolored.contains_key(def) {
                        precolored_intervals.get_mut(def)
                    }
                    else {
                        intervals.get_mut(def)
                    };
                interval.expect("interval").insert_range(node.instruction_index, node.instruction_index);
            }
        }
    }

    let intervals = intervals.into_iter().collect();
    LiveIntervals {
        intervals,
        precolored_intervals,
        instructions_visited,
        temp_pointers,
    }
}

#[derive(Clone, Debug)]
pub struct Interval {
    priority: usize,
    pub ranges: Vec<(usize, usize)>,
    pub temp: Temp,
}

impl Interval {
    fn new(temp: Temp, index: usize) -> Self {
        Self {
            priority: 0,
            ranges: vec![(0, index)],
            temp,
        }
    }

    pub fn empty(temp: Temp) -> Self {
        Self {
            priority: 0,
            ranges: vec![],
            temp,
        }
    }

    pub fn get_first(&self, index: usize) -> usize {
        self.ranges[index].0
    }

    pub fn get_last(&self, index: usize) -> usize {
        self.ranges[index].1
    }

    fn insert_range(&mut self, first: usize, last: usize) {
        let index = self.ranges.iter().position(|&(range_first, _)| first < range_first);
        if let Some(index) = index {
            self.ranges.insert(index, (first, last));
        }
        else {
            self.ranges.push((first, last));
        }
    }

    pub fn is_empty(&self) -> bool {
        self.ranges.is_empty()
    }

    pub fn len(&self) -> usize {
        self.ranges.len()
    }

    fn remove(&mut self, index: usize) {
        let mut new_ranges = vec![];
        for &range in &self.ranges {
            if index == range.0 && index == range.1 {
                // Complety remove the range.
            }
            else if index == range.0 {
                new_ranges.push((index + 1, range.1));
            }
            else if index == range.1 {
                new_ranges.push((range.0, range.1 - 1));
            }
            else if index > range.0 && index < range.1 {
                new_ranges.push((range.0, index - 1));
                new_ranges.push((index + 1, range.1));
            }
            else {
                new_ranges.push(range);
            }
        }
        self.ranges = new_ranges;
    }

    /// The source_index is used to have different ranges when there are multiple reloads for one
    /// instruction to avoid having these multiple reloads use the same register.
    pub fn split_for_reload(&mut self, index: usize, source_index: usize) {
        for range in &self.ranges {
            if index > range.0 && index <= range.1 {
                self.ranges = vec![(index - source_index, index)];
                return;
            }
        }

        panic!("Cannot split the range for reload: {} in {:?}", index, self.ranges);
    }

    /// The destination_index is used to have different ranges when there are multiple spills for one
    /// instruction to avoid having these multiple spills use the same register.
    pub fn split_for_spill(&mut self, index: usize, destination_index: usize) {
        for range in &self.ranges {
            if index >= range.0 && index <= range.1 {
                self.ranges = vec![(range.0, index + destination_index)];
                return;
            }
        }

        panic!("Cannot split the range for spill: {} in {:?}", index, self.ranges);
    }
}

impl PartialEq for Interval {
    fn eq(&self, other: &Interval) -> bool {
        self.priority.eq(&other.priority)
    }
}

impl Eq for Interval {
}

impl PartialOrd for Interval {
    fn partial_cmp(&self, other: &Interval) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Interval {
    fn cmp(&self, other: &Interval) -> Ordering {
        self.priority.cmp(&other.priority)
    }
}
