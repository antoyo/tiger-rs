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

use asm::Instruction;
use frame::Frame;
use frame::x86_64::X86_64;
use ir::{
    BinOp,
    Exp,
    RelationalOp,
    Statement,
};
use temp::Temp;

pub struct Gen {
    instructions: Vec<Instruction>,
}

impl Gen {
    pub fn new() -> Self {
        Self {
            instructions: vec![],
        }
    }

    pub fn emit(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    fn munch_args(&mut self, arguments: Vec<Exp>) -> Vec<Temp> {
        let mut temps = vec![];

        let mut arguments = arguments.into_iter();

        for register in X86_64::arg_registers() {
            match arguments.next() {
                Some(argument) => {
                    let instruction = Instruction::Move {
                        assembly: "mov 'd0, 's0".to_string(),
                        source: vec![self.munch_expression(argument)],
                        destination: vec![register],
                    };
                    self.emit(instruction);
                    temps.push(register);
                },
                None => break,
            }
        }

        let instructions: Vec<_> = arguments.map(|argument| {
            Instruction::Operation {
                assembly: "push 's0".to_string(),
                source: vec![self.munch_expression(argument), X86_64::rsp()],
                destination: vec![X86_64::rsp()],
                jump: None,
            }
        })
            .rev() // Arguments are pushed backwards.
            .collect();

        for instruction in instructions {
            self.emit(instruction);
        }

        temps
    }

    pub fn munch_expression(&mut self, expr: Exp) -> Temp {
        let temp = Temp::new();
        match expr {
            // Error cases:
            Exp::Error | Exp::ExpSequence(_, _) | Exp::BinOp { left: box Exp::Error, .. }
                | Exp::BinOp { right: box Exp::Error, .. } | Exp::BinOp { left: box Exp::Name(_), .. }
                | Exp::BinOp { right: box Exp::Name(_), .. }
                => unreachable!(),

            Exp::Name(ref label) => {
                let instruction = Instruction::Move {
                    assembly: format!("mov 'd0, {}", label),
                    source: vec![],
                    destination: vec![temp],
                };
                self.emit(instruction)
            },
            Exp::Mem(box Exp::BinOp { op: BinOp::Plus, left: expr, right: box Exp::Const(num) }) |
                Exp::Mem(box Exp::BinOp { op: BinOp::Plus, left: box Exp::Const(num), right: expr }) => {
                let instruction = Instruction::Move {
                    assembly: format!("mov 'd0, ['s0 + {}]", num),
                    source: vec![self.munch_expression(*expr)],
                    destination: vec![temp],
                };
                self.emit(instruction);
            },
            Exp::Mem(box Exp::Const(num)) => {
                let instruction = Instruction::Move {
                    assembly: format!("mov 'd0, [{}]", num),
                    source: vec![],
                    destination: vec![temp],
                };
                self.emit(instruction);
            },
            Exp::Mem(expr) => {
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, ['s0]".to_string(),
                    source: vec![self.munch_expression(*expr)],
                    destination: vec![temp],
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::Plus, left: expr, right: box Exp::Const(num) } |
                Exp::BinOp { op: BinOp::Plus, left: box Exp::Const(num), right: expr } => {
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*expr)],
                    destination: vec![temp],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: format!("add 'd0, {}", num),
                    source: vec![temp],
                    destination: vec![temp],
                    jump: None,
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::Minus, left: expr, right: box Exp::Const(num) } => {
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*expr)],
                    destination: vec![temp],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: format!("sub 'd0, {}", num),
                    source: vec![temp],
                    destination: vec![temp],
                    jump: None,
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::Minus, left: box Exp::Const(num), right: expr } => {
                let instruction = Instruction::Move {
                    assembly: format!("mov 'd0, {}", num),
                    source: vec![],
                    destination: vec![temp],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: "sub 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*expr), temp],
                    destination: vec![temp],
                    jump: None,
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::Mul, left: expr, right: box Exp::Const(num) } |
                Exp::BinOp { op: BinOp::Mul, left: box Exp::Const(num), right: expr } => {
                let instruction = Instruction::Move {
                    assembly: format!("mov 'd0, {}", num),
                    source: vec![],
                    destination: vec![temp],
                };
                self.emit(instruction);
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*expr)],
                    destination: vec![X86_64::rax()],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: "mul 's0".to_string(),
                    source: vec![temp, X86_64::rax()],
                    destination: vec![X86_64::rax(), X86_64::rdx()],
                    jump: None,
                };
                self.emit(instruction);
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![X86_64::rax()],
                    destination: vec![temp],
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::Div, left: expr, right: box Exp::Const(num) } => {
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 0".to_string(),
                    source: vec![],
                    destination: vec![X86_64::rdx()],
                };
                self.emit(instruction);
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*expr)],
                    destination: vec![X86_64::rax()],
                };
                self.emit(instruction);
                let immediate = Temp::new();
                let instruction = Instruction::Move {
                    assembly: format!("mov 'd0, {}", num),
                    source: vec![],
                    destination: vec![immediate],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: "idiv 's0".to_string(),
                    source: vec![immediate, X86_64::rax(), X86_64::rdx()],
                    destination: vec![X86_64::rax(), X86_64::rdx()],
                    jump: None,
                };
                self.emit(instruction);
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![X86_64::rax()],
                    destination: vec![temp],
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::Div, left: box Exp::Const(num), right: expr } => {
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 0".to_string(),
                    source: vec![],
                    destination: vec![X86_64::rdx()],
                };
                self.emit(instruction);
                let instruction = Instruction::Move {
                    assembly: format!("mov 'd0, {}", num),
                    source: vec![],
                    destination: vec![X86_64::rax()],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: "idiv 's0".to_string(),
                    source: vec![self.munch_expression(*expr), X86_64::rax(), X86_64::rdx()],
                    destination: vec![X86_64::rax(), X86_64::rdx()],
                    jump: None,
                };
                self.emit(instruction);
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![X86_64::rax()],
                    destination: vec![temp],
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::And, left: expr, right: box Exp::Const(num) } |
                Exp::BinOp { op: BinOp::And, left: box Exp::Const(num), right: expr } => {
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*expr)],
                    destination: vec![temp],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: format!("and 'd0, {}", num),
                    source: vec![],
                    destination: vec![temp],
                    jump: None,
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::Or, left: expr, right: box Exp::Const(num) } |
                Exp::BinOp { op: BinOp::Or, left: box Exp::Const(num), right: expr } => {
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*expr)],
                    destination: vec![temp],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: format!("or 'd0, {}", num),
                    source: vec![],
                    destination: vec![temp],
                    jump: None,
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::ShiftLeft, left: expr, right: box Exp::Const(num) } |
                Exp::BinOp { op: BinOp::ShiftLeft, left: box Exp::Const(num), right: expr } => {
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*expr)],
                    destination: vec![temp],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: format!("sal 'd0, {}", num),
                    source: vec![],
                    destination: vec![temp],
                    jump: None,
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::ArithmeticShiftRight, left: expr, right: box Exp::Const(num) } |
                Exp::BinOp { op: BinOp::ArithmeticShiftRight, left: box Exp::Const(num), right: expr } => {
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*expr)],
                    destination: vec![temp],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: format!("sar 'd0, {}", num),
                    source: vec![],
                    destination: vec![temp],
                    jump: None,
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::ShiftRight, left: expr, right: box Exp::Const(num) } |
                Exp::BinOp { op: BinOp::ShiftRight, left: box Exp::Const(num), right: expr } => {
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*expr)],
                    destination: vec![temp],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: format!("shr 'd0, {}", num),
                    source: vec![],
                    destination: vec![temp],
                    jump: None,
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::Xor, left: expr, right: box Exp::Const(num) } |
                Exp::BinOp { op: BinOp::Xor, left: box Exp::Const(num), right: expr } => {
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*expr)],
                    destination: vec![temp],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: format!("xor 'd0, {}", num),
                    source: vec![],
                    destination: vec![temp],
                    jump: None,
                };
                self.emit(instruction);
            },
            Exp::Const(num) => {
                let instruction = Instruction::Move {
                    assembly: format!("mov 'd0, {}", num),
                    source: vec![],
                    destination: vec![temp],
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::Plus, left, right } => {
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*left)],
                    destination: vec![temp],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: "add 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*right), temp],
                    destination: vec![temp],
                    jump: None,
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::Minus, left, right } => {
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*left)],
                    destination: vec![temp],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: "sub 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*right), temp],
                    destination: vec![temp],
                    jump: None,
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::Mul, left, right } => {
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*left)],
                    destination: vec![temp],
                };
                self.emit(instruction);
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*right)],
                    destination: vec![X86_64::rax()],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: "mul 's0".to_string(),
                    source: vec![temp, X86_64::rax()],
                    destination: vec![X86_64::rax(), X86_64::rdx()],
                    jump: None,
                };
                self.emit(instruction);
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![X86_64::rax()],
                    destination: vec![temp],
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::Div, left, right } => {
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 0".to_string(),
                    source: vec![],
                    destination: vec![X86_64::rdx()],
                };
                self.emit(instruction);
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*left)],
                    destination: vec![X86_64::rax()],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: "idiv 's0".to_string(),
                    source: vec![self.munch_expression(*right), X86_64::rax(), X86_64::rdx()],
                    destination: vec![X86_64::rax(), X86_64::rdx()],
                    jump: None,
                };
                self.emit(instruction);
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![X86_64::rax()],
                    destination: vec![temp],
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::And, left, right } => {
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*left)],
                    destination: vec![temp],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: "and 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*right)],
                    destination: vec![temp],
                    jump: None,
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::Or, left, right } => {
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*left)],
                    destination: vec![temp],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: "or 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*right)],
                    destination: vec![temp],
                    jump: None,
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::ShiftLeft, left, right } => {
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*left)],
                    destination: vec![temp],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: "sal 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*right)],
                    destination: vec![temp],
                    jump: None,
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::ArithmeticShiftRight, left, right } => {
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*left)],
                    destination: vec![temp],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: "sar 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*right)],
                    destination: vec![temp],
                    jump: None,
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::ShiftRight, left, right } => {
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*left)],
                    destination: vec![temp],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: "shr 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*right)],
                    destination: vec![temp],
                    jump: None,
                };
                self.emit(instruction);
            },
            Exp::BinOp { op: BinOp::Xor, left, right } => {
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*left)],
                    destination: vec![temp],
                };
                self.emit(instruction);
                let instruction = Instruction::Operation {
                    assembly: "xor 'd0, 's0".to_string(),
                    source: vec![self.munch_expression(*right)],
                    destination: vec![temp],
                    jump: None,
                };
                self.emit(instruction);
            },
            Exp::Temp(temp) => return temp,
            Exp::Call(box Exp::Name(label), arguments) => {
                let argument_count = arguments.len();
                let source = self.munch_args(arguments);
                let instruction = Instruction::Operation {
                    assembly: format!("call {}", label),
                    source,
                    destination: X86_64::calldefs(),
                    jump: None,
                };
                self.emit(instruction);
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![X86_64::rax()],
                    destination: vec![temp],
                };
                self.emit(instruction);
                let arg_register_count = X86_64::arg_registers().len();
                if argument_count > arg_register_count {
                    let stack_arg_count = argument_count - X86_64::arg_registers().len();
                    let instruction = Instruction::Operation {
                        assembly: format!("add 'd0, {}", stack_arg_count as i64 * X86_64::WORD_SIZE),
                        source: vec![],
                        destination: vec![X86_64::rsp()],
                        jump: None,
                    };
                    self.emit(instruction);
                }
            },
            Exp::Call(function, arguments) => {
                let argument_count = arguments.len();
                let mut source = vec![self.munch_expression(*function)];
                source.extend(self.munch_args(arguments));
                let instruction = Instruction::Operation {
                    assembly: "call 's0".to_string(),
                    source,
                    destination: X86_64::calldefs(),
                    jump: None,
                };
                self.emit(instruction);
                let instruction = Instruction::Move {
                    assembly: "mov 'd0, 's0".to_string(),
                    source: vec![X86_64::rax()],
                    destination: vec![temp],
                };
                self.emit(instruction);
                let arg_register_count = X86_64::arg_registers().len();
                if argument_count > arg_register_count {
                    let stack_arg_count = argument_count - X86_64::arg_registers().len();
                    let instruction = Instruction::Operation {
                        assembly: format!("add 'd0, {}", stack_arg_count as i64 * X86_64::WORD_SIZE),
                        source: vec![],
                        destination: vec![X86_64::rsp()],
                        jump: None,
                    };
                    self.emit(instruction);
                }
            },
        }

        temp
    }

    pub fn munch_statement(&mut self, statement: Statement) {
        match statement {
            Statement::Sequence(statement1, statement2) => {
                self.munch_statement(*statement1);
                self.munch_statement(*statement2);
            },
            Statement::Move(Exp::Mem(box Exp::BinOp {
                op: BinOp::Plus,
                left: memory_destination,
                right: box Exp::Const(num),
            }), expr) |
                Statement::Move(Exp::Mem(box Exp::BinOp {
                    op: BinOp::Plus,
                    left: box Exp::Const(num),
                    right: memory_destination,
                }), expr) => {
                let instruction =
                    Instruction::Move {
                        assembly: format!("mov ['s0 + {}], 's1", num), // FIXME: might be wrong if expr is in memory as Intel might not allow a move from memory to memory.
                        source: vec![self.munch_expression(*memory_destination), self.munch_expression(expr)],
                        destination: vec![],
                    };
                self.emit(instruction);
            },
            /*Statement::Move(Exp::Mem(box Exp::BinOp {
                op: BinOp::Plus,
                left: memory_destination,
                right: offset,
            }), expr) => {
                let temp = Temp::new();
                let instruction =
                    Instruction::Move {
                        assembly: "mov 'd0, 's0".to_string(),
                        source: vec![self.munch_expression(*offset)],
                        destination: vec![temp],
                    };
                self.emit(instruction);
                let instruction =
                    Instruction::Move {
                        assembly: "mov ['s0 + 's1], 's2".to_string(),
                        source: vec![self.munch_expression(*memory_destination), temp, self.munch_expression(expr)],
                        destination: vec![],
                    };
                self.emit(instruction);
            },*/
            Statement::Move(Exp::Mem(box Exp::Const(num)), expr) => {
                let instruction =
                    Instruction::Move {
                        assembly: format!("mov [{}], ['s0]", num), // FIXME: not sure move from memory to memory is allowed.
                        source: vec![self.munch_expression(expr)],
                        destination: vec![],
                    };
                self.emit(instruction);
            },
            Statement::Move(Exp::Mem(destination), source) => {
                let instruction =
                    Instruction::Move {
                        assembly: "mov ['s0], 's1".to_string(),
                        source: vec![self.munch_expression(*destination), self.munch_expression(source)],
                        destination: vec![],
                    };
                self.emit(instruction);
            },
            Statement::Move(Exp::Temp(temp), source) => {
                let generate_instruction = {
                    let source = source.clone();
                    || Instruction::Move {
                        assembly: "mov 'd0, 's0".to_string(),
                        source: vec![self.munch_expression(source)],
                        destination: vec![temp],
                    }
                };
                let instruction =
                    if let Exp::Mem(box Exp::BinOp { op: BinOp::Plus, left: expr, right: box Exp::Const(num) }) = source {
                        // TODO: should that optimization be removed in favor of loophole optimization?
                        if <X86_64 as Frame>::registers().contains(&temp) {
                            Instruction::Move {
                                assembly: format!("mov 'd0, ['s0 + {}]", num),
                                source: vec![self.munch_expression(*expr)],
                                destination: vec![temp],
                            }
                        }
                        else {
                            generate_instruction()
                        }
                    }
                    else {
                        generate_instruction()
                    };

                self.emit(instruction);
            },
            Statement::Label(label) => {
                let instruction =
                    Instruction::Label {
                        assembly: format!("{}:", label),
                        label,
                    };
                self.emit(instruction);
            },
            Statement::Exp(Exp::Const(_)) => (), // Nop statement.
            Statement::Exp(exp) => {
                self.munch_expression(exp);
            },
            Statement::Jump(exp, labels) => {
                match exp {
                    Exp::Name(label) => {
                        let instruction =
                            Instruction::Operation {
                                assembly: format!("jmp {}", label),
                                source: vec![],
                                destination: vec![],
                                jump: Some(labels),
                            };
                        self.emit(instruction);
                    },
                    _ => panic!("Unexpected jump expression: {:?}", exp),
                }
            },
            Statement::CondJump { op, left, right, false_label, true_label } => {
                let instruction =
                    Instruction::Operation {
                        assembly: "cmp 's0, 's1".to_string(),
                        source: vec![self.munch_expression(left), self.munch_expression(right)],
                        destination: vec![],
                        jump: None,
                    };
                self.emit(instruction);

                let opcode =
                    match op {
                        RelationalOp::Equal => "je",
                        RelationalOp::NotEqual => "jne",
                        RelationalOp::LesserThan => "jl",
                        RelationalOp::GreaterThan => "jg",
                        RelationalOp::LesserOrEqual => "jle",
                        RelationalOp::GreaterOrEqual => "jge",
                        RelationalOp::UnsignedLesserThan => "jb",
                        RelationalOp::UnsignedLesserOrEqual => "jbe",
                        RelationalOp::UnsignedGreaterThan => "ja",
                        RelationalOp::UnsignedGreaterOrEqual => "jae",
                    };
                let instruction =
                    Instruction::Operation {
                        assembly: format!("{} {}", opcode, true_label),
                        source: vec![],
                        destination: vec![],
                        jump: Some(vec![false_label, true_label]),
                    };
                self.emit(instruction);
            },

            // Error cases:
            Statement::Move(Exp::Const(_), _) | Statement::Move(Exp::Error, _) | Statement::Move(Exp::Name(_), _) |
                Statement::Move(Exp::BinOp { .. }, _) | Statement::Move(Exp::Call(_, _), _) |
                Statement::Move(Exp::ExpSequence(_, _), _) => unreachable!("{:#?}", statement),
        }
    }

    pub fn get_result(self) -> Vec<Instruction> {
        self.instructions
    }
}
