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

/*
 * LLVM Greedy Register Allocator
 *
 * Flow:
 *
 * * Live Interval Analysis (stand alone pass)
 * * (optional) Spill Weight Calculation (for every interval)
 * * Priority Queue Construction (for every interval)
 * * Register Assignment                 \
 * * (optional) Eviction                 |
 * * (optional) Split                    +---- For every interval in priority queue
 * * Spill                               /
 *
 * # Live Interval Analysis
 *
 * Analyze the live intervals of every variable as a collection of segments.
 *
 * # Spill Weight Calculation (optional for unoptimized version)
 *
 * Estimate spill weight of each interval based on interval characteristics:
 * * used in hot loop (higher spill weight)
 * * cheap to rematerialize (lower spill weight)
 *
 * # Priority Queue Construction
 *
 * Calculate interval allocation priority based on (optional for unoptimized version):
 * * local to one basic block (lower allocation priority)
 * * global and spans across a lot of instructions (higher allocation priority)
 *
 * This priority queue always dequeue the interval with highest priority.
 *
 * # Register assignment
 *
 * Dequeue interval with highest priority and assign to available register if possible.
 *
 * If not, there's an interference, then evict:
 *
 * # Eviction (seems optional for unoptimized version)
 *
 * Compare spill weights of interfering intervals.
 *
 * If one assigned interval is cheaper, evict this interval and assign the more expensive interval.
 * Enqueue the evicted interval (usually receives the same allocation priority).
 *
 * If no interval is cheaper, we cannot evict, so we mark the interval to be splitted.
 * Enqueue this interval in the queue (it receives a lower allocation priority).
 *
 * When only intervals marked to be splitted remain, go to next step.
 *
 * # Split (seems optional for unoptimized version)
 *
 * Check if the split is beneficial:
 * * Split reduces the amount of spills compared to spilling around uses.
 * * If the split can create regions of several basic blocks where the interval is passed by
 * * registers, this will reduce the amount of spill.
 * If it is:
 * Find best way to split the interval:
 * * Do the split of the interval for all registers (since they already have assigned intervals).
 * * * The region split is usually divided into 2 intervals:
 * * * By register:
 * * * * Where the interval does not interfere with the register allocation.
 * * * By stack:
 * * * * Where the interval interferes with the register allocation.
 * * Estimate the split cost (amount of spill code this split may cause).
 * * Choose the cheapest one.
 * *
 * * A good split reduces the transition between registers and stack and places these transitions
 * * in blocks less frequently executed.
 * Add copy instruction between the new splitted intervals.
 * Calculate the spill weights (may receive higher weight than the original).
 * Enqueue the splitted intervals (this calculate their allocation priority).
 * (Continue the process of the four last steps. A split artifact can evict an original interval.)
 *
 * If it is not beneficial, spill:
 *
 * # Spill
 *
 * Spill around uses:
 * * Spill after def
 * * Reload before use
 * Create new intervals for spills and reload.
 * Calculate spill weight.
 * Enqueue the intervals (this calculate their allocation priority).
 * (Continue the process of the four last steps.)
 */

use std::collections::{
    BinaryHeap,
    BTreeSet,
    HashMap,
    HashSet,
};
use std::mem;

use asm::Instruction;
use asm_gen::Gen;
use flow::instructions_to_graph;
use frame::Frame;
use ir::{Exp, _Statement};
use liveness::{Interval, StackLocation, live_intervals};
use temp::{Label, Temp, TempMap};

use crate::liveness::LiveIntervals;

#[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct Pointer(i64);

impl Pointer {
    pub fn to_label<F: Frame>(&self) -> String {
        self.0.to_string()
    }
}

struct IntervalAnalysis {
    intervals: Vec<(Temp, Interval)>,
    #[cfg(test)]
    precolored_intervals: HashMap<Temp, Interval>,
    temp_pointers: Vec<(Label, BTreeSet<StackLocation>)>,
}

pub fn alloc<F: Frame>(instructions: Vec<Instruction>, frame: &mut F, temp_map: TempMap) -> (Vec<Instruction>, Vec<(Label, Vec<Pointer>)>) {
    let mut allocator = Allocator::new::<F>(instructions, temp_map);
    //allocator.spill_weight_calculation();
    let IntervalAnalysis { intervals, temp_pointers, .. } = allocator.live_interval_analysis::<F>();
    allocator.create_priority_queue(intervals);
    allocator.register_assignment();
    allocator.spill(frame);
    allocator.replace_allocation();
    let temp_pointers = allocator.replace_temp_map(temp_pointers);
    (allocator.instructions, temp_pointers)
}

struct Allocator {
    instructions: Vec<Instruction>,
    memory_location: HashMap<Temp, i64>,
    priority_queue: BinaryHeap<Interval>,
    registers: Vec<Register>,
    register_map: HashMap<Temp, Temp>,
    spill_temps: HashMap<Temp, Interval>,
    spill_to_split: HashMap<Temp, HashSet<Temp>>,
    split_to_spill: HashMap<Temp, Temp>,
    temp_map: TempMap,
}

impl Allocator {
    fn new<F: Frame>(instructions: Vec<Instruction>, temp_map: TempMap) -> Self {
        let mut registers: Vec<_> = F::temp_map()
            .into_keys()
            .map(|temp| Register::new(temp, Interval::empty(temp)))
            .collect();
        registers.sort_by_key(|register| register.temp);

        Self {
            instructions,
            memory_location: HashMap::new(),
            priority_queue: BinaryHeap::new(),
            registers,
            register_map: HashMap::new(),
            spill_temps: HashMap::new(),
            spill_to_split: HashMap::new(),
            split_to_spill: HashMap::new(),
            temp_map,
        }
    }

    fn assign_to_register(&mut self, interval: &Interval) -> bool {
        for register in &mut self.registers {
            if !register.conflict(interval) {
                self.register_map.insert(interval.temp, register.temp);
                register.assign(interval);
                return true;
            }
        }
        false
    }

    // TODO: change from Vec<(Temp, Interval)> to Vec<Interval>?
    fn create_priority_queue(&mut self, live_intervals: Vec<(Temp, Interval)>) {
        self.priority_queue.extend(live_intervals.into_iter().map(|(_, interval)| interval));
    }

    fn live_interval_analysis<F: Frame>(&mut self) -> IntervalAnalysis {
        let flow_graph = instructions_to_graph(&self.instructions);

        let LiveIntervals { instructions_visited, .. } = live_intervals::<F>(flow_graph, &self.temp_map, false);

        let instructions = mem::take(&mut self.instructions);
        self.instructions = instructions.into_iter().enumerate()
            .filter(|&(index, _)| instructions_visited.contains(&index))
            .map(|(_, instruction)| instruction)
            .collect();
        // TODO: find a better way to remove the unreachable code than doing the live interval
        // analysis twice (because it's slow).
        let flow_graph = instructions_to_graph(&self.instructions);
        let LiveIntervals  { intervals, precolored_intervals, temp_pointers, .. } = live_intervals::<F>(flow_graph, &self.temp_map, true);

        for register in &mut self.registers {
            if let Some(interval) = precolored_intervals.get(&register.temp) {
                register.assign(interval);
            }
        }
        IntervalAnalysis {
            intervals,
            #[cfg(test)]
            precolored_intervals,
            temp_pointers,
        }
    }

    fn register_assignment(&mut self) {
        while let Some(interval) = self.priority_queue.pop() {
            if !self.assign_to_register(&interval) {
                self.spill_temps.insert(interval.temp, interval);
            }
        }
        //allocator.eviction();
        //allocator.split();
    }

    fn replace_allocation(&mut self) {
        for instruction in &mut self.instructions {
            match *instruction {
                Instruction::Label { .. } => (),
                Instruction::Call { ref mut destination, ref mut source, .. } |
                    Instruction::Move { ref mut destination, ref mut source, .. } |
                    Instruction::Operation { ref mut destination, ref mut source, .. } =>
                    {
                        for destination in destination {
                            if let Some(allocation) = self.register_map.get(destination) {
                                *destination = *allocation;
                            }
                        }
                        for source in source {
                            if let Some(allocation) = self.register_map.get(source) {
                                *source = *allocation;
                            }
                        }
                    },
            }
        }

        self.instructions.retain(|instruction| {
            match *instruction {
                Instruction::Move { ref assembly, ref destination, ref source, .. } =>
                    !(assembly == "mov 'd0, 's0" && destination[0] == source[0]),
                _ => true,
            }
        });
    }

    fn spill<F: Frame>(&mut self, frame: &mut F) {
        // Rewrite the program.
        let mut memory = HashMap::new();
        let mut intervals = HashMap::new();
        let mut new_intervals = vec![];
        for (temp, spill) in &self.spill_temps {
            let local = frame.alloc_local(true);
            let exp = frame.exp(local, Exp::Temp(F::fp()));
            memory.insert(temp, exp);
            intervals.insert(temp, spill);
        }
        let mut gen = Gen::<F>::new();

        let mut instructions = mem::take(&mut self.instructions);

        // Split the spilled temporaries.
        // This is important so that we do not assign both splits to the same register.
        for instruction in &mut instructions {
            match *instruction {
                Instruction::Call { ref mut destination, ref mut source, .. } | Instruction::Move { ref mut destination, ref mut source, .. } |
                    Instruction::Operation { ref mut destination, ref mut source, .. } =>
                    {
                        let mut source_temps = HashMap::new();
                        for source in source {
                            if self.spill_temps.contains_key(source) {
                                let temp = Temp::new();
                                source_temps.insert(*source, temp);
                                self.spill_to_split.entry(*source)
                                    .or_default()
                                    .insert(temp);
                                self.split_to_spill.insert(temp, *source);
                                *source = temp;
                            }
                        }
                        for destination in destination {
                            if self.spill_temps.contains_key(destination) {
                                let temp =
                                    match source_temps.get(destination) {
                                        Some(temp) => *temp,
                                        None => Temp::new(),
                                    };
                                self.spill_to_split.entry(*destination)
                                    .or_default()
                                    .insert(temp);
                                self.split_to_spill.insert(temp, *destination);
                                *destination = temp;
                            }
                        }
                    },
                    Instruction::Label { .. } => (),
            }
        }

        for (index, instruction) in instructions.into_iter().enumerate() {
            match instruction {
                Instruction::Call { ref destination, ref source, .. } | Instruction::Move { ref destination, ref source, .. } |
                    Instruction::Operation { ref destination, ref source, .. } =>
                    {
                        for (source_index, source) in source.iter().enumerate() {
                            if self.spill_temps.contains_key(self.split_to_spill.get(source).unwrap_or(source)) {
                                let original_spill = self.split_to_spill[source];
                                // Reload before use.
                                let temp = gen.munch_expression(memory[&original_spill].clone());
                                gen.munch_statement(_Statement::Move(Exp::Temp(*source), Exp::Temp(temp)).into());
                                let mut interval = intervals[&original_spill].clone();
                                interval.split_for_reload(index, source_index + 1);
                                interval.temp = temp;
                                new_intervals.push((temp, interval.clone()));

                                interval.temp = *source;
                                new_intervals.push((*source, interval));
                            }
                        }
                        let destination = destination.clone(); // TODO: remove this clone?
                        gen.emit(instruction);
                        for (destination_index, destination) in destination.iter().enumerate() {
                            if self.spill_temps.contains_key(self.split_to_spill.get(destination).unwrap_or(destination)) {
                                let original_spill = self.split_to_spill[destination];
                                // Spill after def.
                                let mut offset = None;
                                if let Exp::Mem(ref mem) = memory[&original_spill] {
                                    if let Exp::BinOp { ref right, .. } = **mem {
                                        if let Exp::Const(num) = **right {
                                            offset = Some(num);
                                        }
                                    }
                                }
                                debug_assert!(self.memory_location.insert(*destination, offset.expect("offset")).is_none());
                                gen.munch_statement(_Statement::Move(memory[&original_spill].clone(), Exp::Temp(*destination)).into());
                                let mut interval = intervals[&original_spill].clone();
                                interval.split_for_spill(index, destination_index);
                                interval.temp = *destination;
                                new_intervals.push((*destination, interval));
                            }
                        }
                    },
                    Instruction::Label { .. } => gen.emit(instruction),
            }
        }

        self.instructions = gen.get_result();


        self.spill_temps.clear();

        // Assign register with the new intervals.
        self.create_priority_queue(new_intervals);
        self.register_assignment();

        //debug_assert!(self.spill_temps.is_empty()); // FIXME: for some reasons, it fails when
        //having fewer registers available, even though the merge.tig example still executes
        //correctly.
    }

    fn replace_temp_map(&self, temp_map: Vec<(Label, BTreeSet<StackLocation>)>) -> Vec<(Label, Vec<Pointer>)> {
        let mut pointer_temps = vec![];
        for (label, locations) in temp_map {
            let new_temps = locations.iter().flat_map(|location| {
                match *location {
                    StackLocation(stack) => {
                        vec![Pointer(stack)]
                    },
                }
            }).collect();
            pointer_temps.push((label, new_temps));
        }
        pointer_temps
    }
}

#[derive(Debug)]
struct Register {
    temp: Temp,
    used_interval: Interval,
}

impl Register {
    fn new(temp: Temp, used_interval: Interval) -> Self {
        Self {
            temp,
            used_interval,
        }
    }

    fn assign(&mut self, interval: &Interval) {
        if self.used_interval.is_empty() {
            self.used_interval = interval.clone();
            return;
        }

        let mut new_used_interval = Interval::empty(self.used_interval.temp);
        let mut i = 0;
        let mut j = 0;
        while i < self.used_interval.len() && j < interval.len() {
            let used_first = self.used_interval.get_first(i);
            let used_last = self.used_interval.get_last(i);
            let interval_first = interval.get_first(j);
            let interval_last = interval.get_last(j);
            if used_first < interval_first || used_last < interval_last {
                new_used_interval.ranges.push((used_first, used_last));
                i += 1;
            }
            else if interval_first < used_first || interval_last < used_last {
                new_used_interval.ranges.push((interval_first, interval_last));
                j += 1;
            }
            else if used_first == interval_first && used_last == interval_last {
                // Since we always have the last range up to the max value, we do not consider this
                // last range.
                break;
            }
            else {
                unreachable!("{}, {} - {}, {}", used_first, used_last, interval_first, interval_last);
            }
        }
        if i < self.used_interval.len() {
            new_used_interval.ranges.extend(&self.used_interval.ranges[i..]);
        }
        else if j < interval.len() {
            new_used_interval.ranges.extend(&interval.ranges[j..]);
        }
        self.used_interval = new_used_interval;
    }

    fn conflict(&self, interval: &Interval) -> bool {
        if self.temp == interval.temp {
            // TODO: is that logical to do so?
            return false;
        }

        let mut i = 0;
        let mut j = 0;
        while i < self.used_interval.len() && j < interval.len() {
            let used_first = self.used_interval.get_first(i);
            let used_last = self.used_interval.get_last(i);
            let interval_first = interval.get_first(j);
            let interval_last = interval.get_last(j);
            if used_first > interval_first && used_first < interval_last ||
                interval_first > used_first && interval_first < used_last ||
                used_last > interval_first && used_last < interval_last ||
                interval_last > used_first && interval_last < used_last
            {
                return true;
            }
            else if used_first < interval_first {
                i += 1;
            }
            else if interval_first < used_first {
                j += 1;
            }
            else if used_first == interval_first {
                if self.used_interval.len() > interval.len() {
                    i += 1;
                }
                else {
                    j += 1;
                }
            }
            else {
                unreachable!("{}, {} - {}, {}", used_first, used_last, interval_first, interval_last);
            }
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::fs::File;
    use std::io::BufReader;
    use std::rc::Rc;

    use asm_gen::Gen;
    use canon::{basic_blocks, linearize, trace_schedule};
    use env::Env;
    use escape::find_escapes;
    use frame::{Fragment, Frame};
    use frame::x86_64::X86_64;
    use lexer::Lexer;
    use liveness::Interval;
    use parser::Parser;
    use semant::SemanticAnalyzer;
    use super::{Allocator, IntervalAnalysis, Register};
    use symbol::{Strings, Symbols};
    use temp::Temp;

    fn get_intervals(filename: &str) -> (Vec<(Temp, Interval)>, HashMap<Temp, Interval>) {
        let strings = Rc::new(Strings::new());
        let file = BufReader::new(File::open(filename).expect("file open"));
        let mut symbols = Symbols::new(Rc::clone(&strings));
        let file_symbol = symbols.symbol("no_file");
        let lexer = Lexer::new(file, file_symbol);
        let mut parser = Parser::new(lexer, &mut symbols);
        let ast = parser.parse().expect("parse");
        let escape_env = find_escapes(&ast, Rc::clone(&strings));
        let mut env = Env::<X86_64>::new(&strings, escape_env);
        {
            let semantic_analyzer = SemanticAnalyzer::new(&mut env, &mut symbols, Rc::clone(&strings));
            let fragments = semantic_analyzer.analyze(ast).expect("semantic analyze");

            for (_, fragment) in fragments.functions {
                match fragment {
                    Fragment::Function { body, escaping_vars, frame, temp_map } => {
                        let mut frame = frame.borrow_mut();
                        let body = frame.proc_entry_exit1(body);

                        let statements = linearize(body);
                        let (basic_blocks, done_label) = basic_blocks(statements);
                        let statements = trace_schedule(basic_blocks, done_label);

                        let mut generator = Gen::<X86_64>::new();
                        for statement in statements {
                            generator.munch_statement(statement);
                        }
                        let instructions = generator.get_result();
                        let instructions = frame.proc_entry_exit2(instructions, escaping_vars);

                        for (i, instruction) in instructions.iter().enumerate() {
                            println!("{}. {}", i, instruction.to_string::<X86_64>());
                        }

                        let mut allocator = Allocator::new::<X86_64>(instructions, temp_map);
                        let IntervalAnalysis { intervals, precolored_intervals, .. } = allocator.live_interval_analysis::<X86_64>();

                        return (intervals, precolored_intervals);
                    },
                    Fragment::Str(_, _) => (),
                }
            }
        }

        (vec![], HashMap::new())
    }

    #[test]
    fn interval() {
        let files = vec![
            "tests/hello.tig",
            "tests/integers.tig",
            "tests/conditions.tig",
        ];

        let mut expected_intervals = HashMap::new();
        let mut expected_precolored_intervals = HashMap::new();

        let mut intervals = HashMap::new();
        intervals.insert(29, vec![(12, 12), (23, usize::MAX)]);
        intervals.insert(30, vec![(8, 9), (23, usize::MAX)]);
        intervals.insert(31, vec![(13, 14), (23, usize::MAX)]);
        expected_intervals.insert("tests/hello.tig", intervals);
        let mut intervals = HashMap::new();
        intervals.insert(2, vec![(0, usize::MAX)]);
        expected_precolored_intervals.insert("tests/hello.tig", intervals);

        let mut intervals = HashMap::new();
        intervals.insert(66, vec![(26, 27), (65, usize::MAX)]);
        intervals.insert(65, vec![(24, 25), (65, usize::MAX)]);
        expected_intervals.insert("tests/integers.tig", intervals);

        let mut intervals = HashMap::new();
        intervals.insert(85, vec![(19, 20), (23, 26), (203, usize::MAX)]);
        expected_intervals.insert("tests/conditions.tig", intervals);
        let mut intervals = HashMap::new();
        intervals.insert(2, vec![(0, usize::MAX)]);
        expected_precolored_intervals.insert("tests/conditions.tig", intervals);

        for filename in files {
            let (intervals, precolored_intervals) = get_intervals(filename);
            let intervals: HashMap<_, _> = intervals.into_iter().collect();

            for (&temp, expected_interval) in &expected_intervals[filename] {
                assert!(intervals.contains_key(&Temp::from_num(temp)), "no temp {}", temp);
                assert_eq!(&intervals[&Temp::from_num(temp)].ranges, expected_interval);
            }

            if let Some(expected_precolored_intervals) = expected_precolored_intervals.get(filename) {
                for (&temp, expected_interval) in expected_precolored_intervals {
                    assert!(precolored_intervals.contains_key(&Temp::from_num(temp)), "no precolored temp {}", temp);
                    assert_eq!(&precolored_intervals[&Temp::from_num(temp)].ranges, expected_interval);
                }
            }
        }
    }

    #[test]
    fn register() {
        let mut interval = Interval::empty(Temp::from_num(6));
        interval.ranges = vec![(0, 0), (27, 27)];
        let mut register = Register::new(Temp::from_num(6), interval);
        let mut interval = Interval::empty(Temp::from_num(22));
        interval.ranges = vec![(9, 26)];
        register.assign(&interval);
        assert_eq!(register.used_interval.ranges, vec![(0, 0), (9, 26), (27, 27)]);

        let mut interval = Interval::empty(Temp::from_num(6));
        interval.ranges = vec![(0, 0), (27, 27), (42, usize::MAX)];
        let mut register = Register::new(Temp::from_num(6), interval);
        let mut interval = Interval::empty(Temp::from_num(22));
        interval.ranges = vec![(9, 26), (42, usize::MAX)];
        register.assign(&interval);
        assert_eq!(register.used_interval.ranges, vec![(0, 0), (9, 26), (27, 27), (42, usize::MAX)]);

        let mut interval = Interval::empty(Temp::from_num(6));
        interval.ranges = vec![(0, usize::MAX)];
        let register = Register::new(Temp::from_num(1), interval);
        let mut interval = Interval::empty(Temp::from_num(2));
        interval.ranges = vec![(0, 0), (3, 3), (8, 8), (14, 14), (20, 21), (21, 21), (27, 27), (35, usize::MAX)];
        assert!(register.conflict(&interval));
    }
}
