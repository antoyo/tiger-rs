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

use std::collections::{BTreeMap, BTreeSet, HashMap};

use asm::Instruction;
use asm_gen::Gen;
use color::color;
use flow::instructions_to_graph;
use frame::Frame;
use ir::{Exp, Statement};
use liveness::interference_graph;
use temp::Temp;

pub type Allocation = BTreeMap<Temp, Temp>; // Map temporaries to temporaries pre-assigned to machine registers.

pub fn alloc<F: Frame>(instructions: Vec<Instruction>, frame: &mut F) -> Vec<Instruction> {
    let precolored = F::temp_map();
    let mut initial = vec![];
    for instruction in &instructions {
        match instruction {
            Instruction::Label { .. } => (),
            Instruction::Move { ref destination, ref source, .. } |
                Instruction::Operation { ref destination, ref source, .. } =>
            {
                for destination in destination {
                    if !precolored.contains_key(destination) {
                        initial.push(destination.clone());
                    }
                }
                for source in source {
                    if !precolored.contains_key(source) {
                        initial.push(source.clone());
                    }
                }
            },
        }
    }

    allocate(instructions, initial, frame)
}

fn allocate<F: Frame>(instructions: Vec<Instruction>, initial: Vec<Temp>, frame: &mut F) -> Vec<Instruction> {
    let flow_graph = instructions_to_graph(&instructions);
    let interference_graph = interference_graph(flow_graph);
    let (allocation, spills, colored_nodes, coalesced_nodes) = color::<F>(interference_graph, initial);
    if spills.is_empty() {
        replace_allocation(instructions, allocation)
    }
    else {
        let (instructions, new_temps) = rewrite_program(instructions, spills, frame);
        let initial: Vec<_> = colored_nodes.union(&new_temps).cloned().collect::<BTreeSet<_>>()
            .union(&coalesced_nodes).cloned().collect();
        allocate(instructions, initial, frame)
    }
}

fn replace_allocation(mut instructions: Vec<Instruction>, allocation: Allocation) -> Vec<Instruction> {
    for instruction in &mut instructions {
        match *instruction {
            Instruction::Label { .. } => (),
            Instruction::Move { ref mut destination, ref mut source, .. } |
                Instruction::Operation { ref mut destination, ref mut source, .. } =>
            {
                for destination in destination {
                    if let Some(allocation) = allocation.get(destination) {
                        *destination = allocation.clone();
                    }
                }
                for source in source {
                    if let Some(allocation) = allocation.get(source) {
                        *source = allocation.clone();
                    }
                }
            },
        }
    }

    instructions.retain(|instruction| {
        match *instruction {
            Instruction::Move { ref assembly, ref destination, ref source, .. } =>
                !(assembly == "mov 'd0, 's0" && destination[0] == source[0]),
            _ => true,
        }
    });

    instructions
}

fn rewrite_program<F: Frame>(instructions: Vec<Instruction>, spills: Vec<Temp>, frame: &mut F) -> (Vec<Instruction>, BTreeSet<Temp>) {
    let mut memory = HashMap::new();
    let mut new_temps = BTreeSet::new();
    for spill in &spills {
        let local = frame.alloc_local(true);
        let exp = frame.exp(local, Exp::Temp(F::fp()));
        memory.insert(spill, exp);
        new_temps.insert(*spill);
    }
    let mut gen = Gen::new();

    for instruction in instructions {
        match instruction {
            Instruction::Move { ref destination, ref source, .. } | Instruction::Operation { ref destination, ref source, .. } => {
                if let Some(spill) = destination.iter().find(|destination| spills.contains(destination)) {
                    if let Some(spill) = source.iter().find(|source| spills.contains(source)) {
                        let temp = gen.munch_expression(memory[spill].clone());
                        gen.munch_statement(Statement::Move(Exp::Temp(*spill), Exp::Temp(temp)));
                        new_temps.insert(temp);
                    }
                    let spill = spill.clone();
                    gen.emit(instruction);
                    gen.munch_statement(Statement::Move(memory[&spill].clone(), Exp::Temp(spill)));
                }
                else if let Some(spill) = source.iter().find(|source| spills.contains(source)) {
                    let temp = gen.munch_expression(memory[spill].clone());
                    gen.munch_statement(Statement::Move(Exp::Temp(*spill), Exp::Temp(temp)));
                    new_temps.insert(temp);
                    gen.emit(instruction);
                }
                else {
                    gen.emit(instruction);
                }
            },
            Instruction::Label { .. } => gen.emit(instruction),
        }
    }

    (gen.get_result(), new_temps)
}
