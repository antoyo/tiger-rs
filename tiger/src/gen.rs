/*
 * Copyright (c) 2018 Boucher, Antoni <bouanto@zoho.com>
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

use ast::Operator;
use frame::{Fragment, Frame};
use ir;
use ir::BinOp::{
    And,
    Div,
    Minus,
    Mul,
    Or,
    Plus,
};
use ir::Exp::{
    self,
    BinOp,
    Call,
    Const,
    ExpSequence,
    Mem,
    Name,
};
use ir::RelationalOp::{
    self,
    Equal,
    NotEqual,
    GreaterThan,
    GreaterOrEqual,
    LesserThan,
    LesserOrEqual,
};
use ir::Statement::{
    self,
    CondJump,
    Jump,
    Move,
    Sequence,
};
use temp::{Label, Temp};

#[allow(type_alias_bounds)]
pub type Access<F: Frame> = (Level<F>, F::Access);

#[derive(Clone)]
pub struct Level<F: Clone> {
    current: F,
    parent: Option<Box<Level<F>>>,
}

impl<F: Clone + PartialEq> PartialEq for Level<F> {
    fn eq(&self, other: &Self) -> bool {
        self.current == other.current
    }
}

pub fn outermost<F: Clone + Frame>() -> Level<F> {
    Level {
        current: F::new(Label::new(), vec![]),
        parent: None,
    }
}

impl<F: Clone + Frame> Level<F> {
    pub fn new(parent: &Level<F>, name: Label, mut formals: Vec<bool>) -> Level<F> {
        formals.push(true); // for the static link.
        Level {
            current: F::new(name, formals),
            parent: Some(Box::new(parent.clone())),
        }
    }
}

fn formals<F: Clone + Frame>(level: Level<F>) -> Vec<Access<F>> {
    unimplemented!();
}

pub fn alloc_local<F: Clone + Frame>(level: &Level<F>, escape: bool) -> Access<F> {
    let mut level = level.clone();
    let frame_local = level.current.alloc_local(escape);
    (level, frame_local)
}

pub fn array_subscript<F: Frame>(var: Exp, subscript: Exp) -> Exp {
    BinOp {
        op: Plus,
        left: Box::new(Mem(Box::new(var))),
        right: Box::new(BinOp {
            op: Mul,
            left: Box::new(subscript),
            right: Box::new(Const(F::WORD_SIZE)),
        }),
    }
}

pub fn binary_oper(op: Operator, left: Exp, right: Exp) -> Exp {
    BinOp {
        op: to_ir_op(op),
        left: Box::new(left),
        right: Box::new(right),
    }
}

pub fn field_access<F: Frame>(var: Exp, field_index: usize) -> Exp {
    BinOp {
        op: Plus,
        left: Box::new(Mem(Box::new(var))),
        right: Box::new(Const(F::WORD_SIZE * field_index as i64)),
    }
}

pub fn function_call<F: Clone + Frame + PartialEq>(label: &Label, mut args: Vec<Exp>, parent_level: &Level<F>,
    current_level: &Level<F>) -> Exp
{
    if *parent_level == *current_level {
        // For a recursive call, we simply pass the current static link, which represents the stack
        // frame of the parent function.
        let frame = &current_level.current;
        args.push(frame.exp(frame.formals().last().expect("static link").clone(), Exp::Temp(frame.fp())));
    }
    else if current_level.parent.as_ref().map(|level| &**level) == Some(parent_level) {
        // When calling a function defined in the current frame, simply pass the current frame
        // pointer for the static link.
        args.push(Exp::Temp(parent_level.current.fp()));
    }
    else {
        // When calling a function defined in a parent frame, go up throught the static links.
        let mut function_level = current_level;
        let frame = &function_level.current;
        let mut var = frame.exp(frame.formals().last().expect("static link").clone(), Exp::Temp(frame.fp()));
        while function_level.parent.as_ref().map(|level| &**level) != Some(parent_level) {
            var = Mem(Box::new(BinOp {
                op: Plus,
                left: Box::new(frame.exp(function_level.current.formals().last().expect("static link").clone(),
                    Exp::Temp(function_level.current.fp()))),
                right: Box::new(var),
            }));
            function_level = function_level.parent.as_ref().unwrap_or_else(|| panic!("function level should have a parent"));
        }
        args.push(var);
    }
    Call(Box::new(Name(label.clone())), args)
}

pub fn goto(label: Label) -> Exp {
    ExpSequence(
        Box::new(Jump(Name(label.clone()), vec![label])),
        Box::new(unit()),
    )
}

pub fn if_expression<F: Clone + Frame>(test_expr: Exp, if_expr: Exp, else_expr: Option<Exp>, level: &Level<F>) -> Exp {
    let result = alloc_local(level, false);
    let true_label = Label::new();
    let false_label = Label::new();
    let frame = &level.current;
    let result = frame.exp(result.1, Exp::Temp(frame.fp()));
    ExpSequence(
        Box::new(Sequence(
            Box::new(CondJump {
                op: Equal,
                left: test_expr,
                right: Const(1),
                true_label: true_label.clone(),
                false_label: false_label.clone(),
            }),
            Box::new(Sequence(
                Box::new(Statement::Label(true_label)),
                Box::new(Sequence(
                    Box::new(Move(result.clone(), if_expr)),
                    Box::new(Sequence(
                        Box::new(Statement::Label(false_label)),
                        Box::new(Move(result.clone(), else_expr.unwrap_or(unit()))),
                    )),
                )),
            )),
        )),
        Box::new(result),
    )
}

pub fn num(number: i64) -> Exp {
    Const(number)
}

pub fn record_create<F: Frame>(fields: Vec<Exp>) -> Exp {
    if fields.is_empty() {
        return unit();
    }
    let result = Exp::Temp(Temp::new());
    let mut fields = fields.into_iter();
    let mut sequence = Sequence(
        Box::new(Move(result.clone(), F::external_call("malloc", vec![Const(fields.len() as i64 * F::WORD_SIZE)]))),
        Box::new(Move(Mem(Box::new(result.clone())), fields.next().expect("record first field"))),
    );
    for (index, field) in fields.enumerate() {
        sequence = Sequence(
            Box::new(sequence),
            Box::new(Move(Mem(Box::new(BinOp {
                op: Plus,
                left: Box::new(result.clone()),
                right: Box::new(Const(index as i64 * F::WORD_SIZE)),
            })), field))
        );
    }
    ExpSequence(
        Box::new(sequence),
        Box::new(result),
    )
}

pub fn relational_oper<F: Clone + Frame>(op: Operator, left: Exp, right: Exp, level: &Level<F>) -> Exp {
    let result = alloc_local(level, false);
    let true_label = Label::new();
    let false_label = Label::new();
    let frame = &level.current;
    let result = frame.exp(result.1, Exp::Temp(frame.fp()));
    ExpSequence(
        Box::new(Sequence(
            Box::new(CondJump {
                op: to_ir_rel_op(op),
                left: left,
                right: right,
                true_label: true_label.clone(),
                false_label: false_label.clone(),
            }),
            Box::new(Sequence(
                Box::new(Statement::Label(true_label)),
                Box::new(Sequence(
                    Box::new(Move(result.clone(), Const(1))),
                    Box::new(Sequence(
                        Box::new(Statement::Label(false_label)),
                        Box::new(Move(result.clone(), Const(0))),
                    )),
                )),
            )),
        )),
        Box::new(result),
    )
}

pub fn simple_var<F: Clone + Frame + PartialEq>(access: Access<F>, level: &Level<F>) -> Exp {
    let mut function_level = level;
    let var_level = access.0;
    let frame = &level.current;
    let mut var = frame.exp(access.1, Exp::Temp(frame.fp()));
    // Add the offset of each parent frames (static link).
    while function_level.current != var_level.current {
        var = Mem(Box::new(BinOp {
            op: Plus,
            left: Box::new(frame.exp(function_level.current.formals().last().expect("static link").clone(),
                Exp::Temp(function_level.current.fp()))),
            right: Box::new(var),
        }));
        function_level = function_level.parent.as_ref().unwrap_or_else(|| panic!("function level should have a parent"));
    }
    var
}

pub fn string_equality<F: Frame>(oper: Operator, left: Exp, right: Exp) -> Exp {
    let exp = F::external_call("stringEqual", vec![left, right]);
    match oper {
        Operator::Equal => exp,
        Operator::Neq => BinOp {
            op: Minus,
            left: Box::new(Const(1)),
            right: Box::new(exp),
        },
        _ => panic!("unexpected operator {:?}", oper),
    }
}

pub fn unit() -> Exp {
    Const(0)
}

pub fn var_dec<F: Frame>(access: &Access<F>, value: Exp) -> Statement {
    let var_level = &access.0;
    let frame = &var_level.current;
    Move(frame.exp(access.1.clone(), Exp::Temp(frame.fp())), value)
}

pub fn var_decs(variables: Vec<Statement>, body: Exp) -> Exp {
    let len = variables.len();
    let mut iter = variables.into_iter();
    let var1 = iter.next().expect("first variable declaration");
    if len < 2 {
        return ExpSequence(Box::new(var1), Box::new(body));
    }
    let var2 = iter.next().expect("second variable declaration");
    let mut statements = Sequence(Box::new(var1), Box::new(var2));
    for var in iter {
        statements = Sequence(
            Box::new(statements),
            Box::new(var),
        );
    }
    ExpSequence(Box::new(statements), Box::new(body))
}

pub fn while_loop(done_label: &Label, test_expr: Exp, body: Exp) -> Exp {
    let test_label = Label::new();
    let after_check_label = Label::new();
    ExpSequence(
        Box::new(Sequence(
            Box::new(Sequence(
                Box::new(Statement::Label(test_label.clone())),
                Box::new(Sequence(
                    Box::new(Sequence(
                        Box::new(CondJump {
                            op: NotEqual,
                            left: test_expr,
                            right: Const(1),
                            true_label: done_label.clone(),
                            false_label: after_check_label.clone(),
                        }),
                        Box::new(Statement::Label(after_check_label)),
                    )),
                    Box::new(Sequence(
                        Box::new(Statement::Exp(body)),
                        Box::new(Jump(Name(test_label.clone()), vec![test_label])),
                    )),
                )),
            )),
            Box::new(Statement::Label(done_label.clone())),
        )),
        Box::new(unit()),
    )
}

fn to_ir_op(op: Operator) -> ir::BinOp {
    match op {
        Operator::Plus => Plus,
        Operator::Minus => Minus,
        Operator::Times => Mul,
        Operator::And => And,
        Operator::Or => Or,
        Operator::Divide => Div,
        _ => panic!("{:?} is not a binary operator", op),
    }
}

fn to_ir_rel_op(op: Operator) -> RelationalOp {
    match op {
        Operator::Equal => Equal,
        Operator::Ge => GreaterOrEqual,
        Operator::Gt => GreaterThan,
        Operator::Le => LesserOrEqual,
        Operator::Lt => LesserThan,
        Operator::Neq => NotEqual,
        _ => panic!("{:?} is not a relational operator or is not used", op),
    }
}

pub struct Gen<F: Frame> {
    fragments: Vec<Fragment<F>>,
}

impl<F:Frame> Gen<F> {
    pub fn new() -> Self {
        Self {
            fragments: vec![],
        }
    }

    pub fn get_result(self) -> Vec<Fragment<F>> {
        self.fragments
    }

    pub fn proc_entry_exit(&mut self, level: &Level<F>, body: Exp) {
        let body = Move(Exp::Temp(level.current.return_value()), body);
        self.fragments.push(Fragment::Function {
            body,
            frame: level.current.clone(),
        });
    }

    pub fn string_literal(&mut self, string: String) -> Exp {
        let label = Label::new();
        self.fragments.push(Fragment::Str(label.clone(), string));
        Name(label)
    }
}
