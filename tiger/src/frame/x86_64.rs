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

use std::collections::HashMap;
use std::sync::Once;

use asm::{Instruction, Subroutine};
use ir::BinOp::Plus;
use ir::Exp:: {
    self,
    BinOp,
    Call,
    Const,
    Mem,
    Name,
};
use ir::Statement;
use super::Frame;
use temp::{Label, Temp};

use self::Access::{InFrame, InReg};

const POINTER_SIZE: i64 = 8;

#[derive(Clone)]
pub struct X86_64 {
    formals: Vec<Access>, // Representation of parameters.
    name: Label,
    pointer: i64,
}

impl PartialEq for X86_64 {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

#[derive(Clone, Debug)]
pub enum Access {
    InFrame(i64),
    InReg(Temp),
}

static mut RBP: Option<Temp> = None;
static mut RSP: Option<Temp> = None;
static mut RAX: Option<Temp> = None;
static mut RBX: Option<Temp> = None;
static mut RCX: Option<Temp> = None;
static mut RDX: Option<Temp> = None;
static mut RSI: Option<Temp> = None;
static mut RDI: Option<Temp> = None;
static mut R8: Option<Temp> = None;
static mut R9: Option<Temp> = None;
static mut R10: Option<Temp> = None;
static mut R11: Option<Temp> = None;
static mut R12: Option<Temp> = None;
static mut R13: Option<Temp> = None;
static mut R14: Option<Temp> = None;
static mut R15: Option<Temp> = None;
static ONCE: Once = Once::new();

fn initialize() {
    unsafe {
        RBP = Some(Temp::new());
        RSP = Some(Temp::new());
        RAX = Some(Temp::new());
        RBX = Some(Temp::new());
        RCX = Some(Temp::new());
        RDX = Some(Temp::new());
        RDI = Some(Temp::new());
        RSI = Some(Temp::new());
        R8 = Some(Temp::new());
        R9 = Some(Temp::new());
        R10 = Some(Temp::new());
        R11 = Some(Temp::new());
        R12 = Some(Temp::new());
        R13 = Some(Temp::new());
        R14 = Some(Temp::new());
        R15 = Some(Temp::new());
    }
}

impl X86_64 {
    pub fn arg_registers() -> Vec<Temp> {
        vec![Self::rdi(), Self::rsi(), Self::rdx(), Self::rcx(), Self::r8(), Self::r9()]
    }

    fn callee_saved_registers() -> Vec<Temp> {
        vec![Self::rbx(), Self::r12(), Self::r13(), Self::r14(), Self::r15()]
    }

    fn special_registers() -> Vec<Temp> {
        vec![Self::rax(), Self::rbp(), Self::rsp()]
    }

    fn caller_saved_registers() -> Vec<Temp> {
        vec![Self::r10(), Self::r11()]
    }

    pub fn calldefs() -> Vec<Temp> {
        let mut registers = Self::caller_saved_registers();
        registers.extend(Self::arg_registers());
        registers.push(Self::return_value());
        registers
    }

    pub fn rsp() -> Temp {
        ONCE.call_once(initialize);
        unsafe { RSP.expect("temp") }
    }

    fn rbp() -> Temp {
        ONCE.call_once(initialize);
        unsafe { RBP.expect("temp") }
    }

    fn rdi() -> Temp {
        ONCE.call_once(initialize);
        unsafe { RDI.expect("temp") }
    }

    fn rsi() -> Temp {
        ONCE.call_once(initialize);
        unsafe { RSI.expect("temp") }
    }

    pub fn rax() -> Temp {
        ONCE.call_once(initialize);
        unsafe { RAX.expect("temp") }
    }

    fn rbx() -> Temp {
        ONCE.call_once(initialize);
        unsafe { RBX.expect("temp") }
    }

    fn rcx() -> Temp {
        ONCE.call_once(initialize);
        unsafe { RCX.expect("temp") }
    }

    pub fn rdx() -> Temp {
        ONCE.call_once(initialize);
        unsafe { RDX.expect("temp") }
    }

    fn r8() -> Temp {
        ONCE.call_once(initialize);
        unsafe { R8.expect("temp") }
    }

    fn r9() -> Temp {
        ONCE.call_once(initialize);
        unsafe { R9.expect("temp") }
    }

    fn r10() -> Temp {
        ONCE.call_once(initialize);
        unsafe { R10.expect("temp") }
    }

    fn r11() -> Temp {
        ONCE.call_once(initialize);
        unsafe { R11.expect("temp") }
    }

    fn r12() -> Temp {
        ONCE.call_once(initialize);
        unsafe { R12.expect("temp") }
    }

    fn r13() -> Temp {
        ONCE.call_once(initialize);
        unsafe { R13.expect("temp") }
    }

    fn r14() -> Temp {
        ONCE.call_once(initialize);
        unsafe { R14.expect("temp") }
    }

    fn r15() -> Temp {
        ONCE.call_once(initialize);
        unsafe { R15.expect("temp") }
    }
}

impl Frame for X86_64 {
    type Access = Access;

    const WORD_SIZE: i64 = 8;

    fn registers() -> Vec<Temp> {
        let mut registers = Self::arg_registers();
        registers.extend(Self::callee_saved_registers());
        registers.extend(Self::special_registers());
        registers.extend(Self::caller_saved_registers());
        registers
    }

    fn register_count() -> usize {
        Self::registers().len() - [Self::rsp(), Self::rbp()].len()
    }

    fn temp_map() -> HashMap<Temp, &'static str> {
        let mut map = HashMap::new();
        map.insert(Self::rbp(), "rbp");
        map.insert(Self::rsp(), "rsp");
        map.insert(Self::return_value(), "rax");
        map.insert(Self::rbx(), "rbx");
        map.insert(Self::rdi(), "rdi");
        map.insert(Self::rsi(), "rsi");
        map.insert(Self::rdx(), "rdx");
        map.insert(Self::rcx(), "rcx");
        map.insert(Self::r8(), "r8");
        map.insert(Self::r9(), "r9");
        map.insert(Self::r10(), "r10");
        map.insert(Self::r11(), "r11");
        map.insert(Self::r12(), "r12");
        map.insert(Self::r13(), "r13");
        map.insert(Self::r14(), "r14");
        map.insert(Self::r15(), "r15");
        map
    }

    fn special_name(temp: Temp) -> Option<&'static str> {
        Self::temp_map().get(&temp).map(|&str| str)
    }

    fn fp() -> Temp {
        Self::rbp()
    }

    fn return_value() -> Temp {
        Self::rax()
    }

    fn new(name: Label, formals: Vec<bool>) -> Self {
        let mut frame = X86_64 {
            formals: vec![],
            name,
            pointer: 0,
        };
        let formals = formals.iter()
            .map(|&escape| frame.alloc_local(escape))
            .collect();
        frame.formals = formals;
        frame
    }

    fn name(&self) -> Label {
        self.name.clone()
    }

    fn formals(&self) -> &[Self::Access] {
        &self.formals
    }

    fn alloc_local(&mut self, escape: bool) -> Self::Access {
        if escape {
            self.pointer -= POINTER_SIZE;
            InFrame(self.pointer)
        }
        else {
            InReg(Temp::new())
        }
    }

    fn exp(&self, access: Self::Access, stack_frame: Exp) -> Exp {
        match access {
            InFrame(pos) => {
                Mem(Box::new(BinOp {
                    op: Plus,
                    left: Box::new(stack_frame),
                    right: Box::new(Const(pos)),
                }))
            },
            InReg(reg) => {
                Exp::Temp(reg)
            },
        }
    }

    fn external_call(name: &str, arguments: Vec<Exp>) -> Exp {
        Call {
            function_expr: Box::new(Name(Label::with_name(name))),
            arguments,
        }
    }

    fn proc_entry_exit1(&mut self, mut statement: Statement) -> Statement {
        let mut start_statements = vec![];
        let mut end_statements = vec![];

        let mut saved_register_locations = vec![];
        for register in Self::callee_saved_registers().into_iter() {
            let local = Temp::new();
            let memory = Exp::Temp(local);
            saved_register_locations.push(memory.clone());
            start_statements.push(Statement::Move(memory, Exp::Temp(register)));
        }

        let arg_registers = Self::arg_registers();
        let arg_registers_len = arg_registers.len();
        for (formal, arg_register) in self.formals.iter().zip(arg_registers) {
            let destination = self.exp(formal.clone(), Exp::Temp(Self::fp()));
            start_statements.push(Statement::Move(destination, Exp::Temp(arg_register)));
        }
        for (index, formal) in self.formals.iter().skip(arg_registers_len).enumerate() {
            let destination = self.exp(formal.clone(), Exp::Temp(Self::fp()));
            start_statements.push(Statement::Move(destination, Exp::Mem(Box::new(
                Exp::BinOp {
                    left: Box::new(Exp::Temp(Self::fp())),
                    op: Plus,
                    right: Box::new(Exp::Const(Self::WORD_SIZE * (index + 2) as i64)), // TODO: explain why + 2. Maybe because + 1 is the return address?
                }
            ))));
        }

        for (register, location) in Self::callee_saved_registers().into_iter().zip(saved_register_locations) {
            end_statements.push(Statement::Move(Exp::Temp(register), location));
        }

        let mut end_statement = Statement::Exp(Exp::Const(0));
        for statement in end_statements {
            end_statement = Statement::Sequence(Box::new(end_statement), Box::new(statement));
        }

        for new_statement in start_statements.into_iter().rev() {
            statement = Statement::Sequence(Box::new(new_statement), Box::new(statement));
        }

        Statement::Sequence(Box::new(statement), Box::new(end_statement))
    }

    fn proc_entry_exit2(&self, mut instructions: Vec<Instruction>) -> Vec<Instruction> {
        // TODO: explain why we push a new empty instruction with this source.
        let mut source = Self::callee_saved_registers();
        source.extend(Self::special_registers());
        let instruction = Instruction::Operation {
            assembly: String::new(),
            source,
            destination: vec![],
            jump: Some(vec![]),
        };
        instructions.push(instruction);

        // TODO: add comment to explain why rbp and rsp are added as destination.
        // Problably so that the register allocator does not use them.
        // FIXME: does not seem to work, though.
        let mut destination = vec![];
        destination.push(Self::rsp());
        destination.push(Self::rbp());
        destination.extend(Self::callee_saved_registers());
        destination.extend(Self::arg_registers());
        let instruction = Instruction::Operation {
            assembly: String::new(),
            source: vec![],
            destination,
            jump: Some(vec![]),
        };
        instructions.insert(0, instruction);

        for instruction in instructions.iter_mut().rev() {
            match *instruction {
                Instruction::Label { .. } => (),
                Instruction::Call { ref mut source, .. } |
                    Instruction::Move { ref mut source, .. } |
                    Instruction::Operation { ref mut source, .. } =>
                {
                    source.push(Self::rbp());
                    source.push(Self::rsp());
                    break;
                },
            }
        }

        instructions
    }

    fn proc_entry_exit3(&self, body: Vec<Instruction>) -> Subroutine {
        let mut stack_size = -self.pointer;
        if stack_size % 16 != 0 {
            // Align the stack of 16 bytes.
            stack_size = (stack_size & !0xF) + 0x10;
        }

        Subroutine { // FIXME: saving to rbp is apparently not needed in 64-bit.
            prolog: format!("{}:
    push rbp
    mov rbp, rsp
    sub rsp, {}", self.name(), stack_size),
            body,
            epilog: "leave
    ret".to_string(),
        }
    }
}
