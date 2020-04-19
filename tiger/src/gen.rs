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

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

use ast::Operator;
use data_layout::{
    ARRAY_DATA_LAYOUT_SIZE,
    RECORD_DATA_LAYOUT_SIZE,
};
use frame::{Fragment, Frame, Memory};
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
use ir::Statement;
use ir::_Statement::{
    self,
    CondJump,
    Jump,
    Move,
    Sequence,
};
use temp::{Label, Temp, TempMap};

#[allow(type_alias_bounds)]
pub type Access<F: Frame> = (Level<F>, F::Access);

pub struct IR<F: Frame> {
    pub data: Vec<Fragment<F>>,
    pub functions: BTreeMap<String, Fragment<F>>,
}

impl<F: Frame> IR<F> {
    fn new() -> Self {
        Self {
            data: vec![],
            functions: BTreeMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct Level<F> {
    pub current: Rc<RefCell<F>>,
    pub parent: Option<Box<Level<F>>>,
}

impl<F> Clone for Level<F> {
    fn clone(&self) -> Self {
        Self {
            current: self.current.clone(),
            parent: self.parent.clone(),
        }
    }
}

impl<F: PartialEq> PartialEq for Level<F> {
    fn eq(&self, other: &Self) -> bool {
        self.current == other.current
    }
}

pub fn outermost<F: Frame>() -> Level<F> {
    Level {
        current: Rc::new(RefCell::new(F::new(Label::new(), vec![]))),
        parent: None,
    }
}

impl<F: Frame> Level<F> {
    pub fn new(parent: &Level<F>, name: Label, mut formals: Vec<bool>) -> Level<F> {
        formals.push(true); // for the static link.
        Level {
            current: Rc::new(RefCell::new(F::new(name, formals))),
            parent: Some(Box::new(parent.clone())),
        }
    }

    pub fn formals(&self) -> Vec<Access<F>> {
        self.current.borrow().formals().iter()
            .map(|access| (self.clone(), access.clone()))
            .collect()
    }
}

pub fn alloc_local<F: Frame>(level: &Level<F>, escape: bool) -> Access<F> {
    let level = level.clone();
    let frame_local = level.current.borrow_mut().alloc_local(escape);
    (level, frame_local)
}

pub fn array_subscript<F: Frame>(var: Exp, subscript: Exp) -> Exp {
    Mem(Box::new(BinOp {
        op: Plus,
        left: Box::new(var),
        right: Box::new(BinOp {
            op: Mul,
            left: Box::new(
                BinOp {
                    op: Plus,
                    left: Box::new(subscript),
                    right: Box::new(num(ARRAY_DATA_LAYOUT_SIZE as i64)),
                }),
            right: Box::new(Const(F::WORD_SIZE)),
        }),
    }))
}

pub fn binary_oper(op: Operator, left: Exp, right: Exp) -> Exp {
    BinOp {
        op: to_ir_op(op),
        left: Box::new(left),
        right: Box::new(right),
    }
}

pub fn field_access<F: Frame>(var: Exp, field_index: usize) -> Exp {
    let offset = RECORD_DATA_LAYOUT_SIZE;
    Mem(Box::new(BinOp {
        op: Plus,
        left: Box::new(var),
        right: Box::new(Const(F::WORD_SIZE * (field_index + offset) as i64)),
    }))
}

pub fn function_call(label: &Label, arguments: Vec<Exp>, collectable_return_type: bool) -> Exp
{
    Call {
        arguments,
        collectable_return_type,
        function_expr: Box::new(Name(label.clone())),
        return_label: Label::new(),
    }
}

pub fn function_pointer_call(function_pointer: Exp, arguments: Vec<Exp>, collectable_return_type: bool) -> Exp {
    Call {
        arguments,
        collectable_return_type,
        function_expr: Box::new(function_pointer),
        return_label: Label::new(),
    }
}

pub fn goto(label: Label) -> Exp {
    ExpSequence(
        Box::new(Jump(Name(label.clone()), vec![label]).into()),
        Box::new(unit()),
    )
}

pub fn if_expression<F: Clone + Frame>(test_expr: Exp, if_expr: Exp, else_expr: Option<Exp>, level: &Level<F>) -> Exp {
    let result = alloc_local(level, false);
    let true_label = Label::new();
    let false_label = Label::new();
    let end_label = Label::new();
    let frame = level.current.borrow();
    let result = frame.exp(result.1, Exp::Temp(F::fp()));
    ExpSequence(
        Box::new(Sequence(
            Box::new(CondJump {
                op: Equal,
                left: test_expr,
                right: Const(1),
                true_label: true_label.clone(),
                false_label: false_label.clone(),
            }.into()),
            Box::new(Sequence(
                Box::new(_Statement::Label(true_label).into()),
                Box::new(Sequence(
                    Box::new(Move(result.clone(), if_expr).into()),
                    Box::new(Sequence(
                        Box::new(Jump(Name(end_label.clone()), vec![end_label.clone()]).into()),
                        Box::new(Sequence(
                            Box::new(_Statement::Label(false_label).into()),
                            Box::new(Sequence(
                                Box::new(Move(result.clone(), else_expr.unwrap_or_else(unit)).into()),
                                Box::new(_Statement::Label(end_label).into()),
                            ).into()),
                        ).into()),
                    ).into()),
                ).into()),
            ).into()),
        ).into()),
        Box::new(result),
    )
}

pub fn init_array<F: Clone + Frame + PartialEq>(var: Option<Access<F>>, size_expr: Exp, is_pointer: Exp, init_expr: Exp, level: &Level<F>) -> Exp {
    // FIXME: it does many allocations for a 2D array.
    let temp = Temp::new();
    let result =
        if let Some(var) = var.clone() {
            let level = var.0.clone();
            simple_var(var, &level)
        }
        else {
            Exp::Temp(temp)
        };
    let loop_var = Exp::Temp(Temp::new());
    let test_expr = relational_oper(Operator::Lt, loop_var.clone(), size_expr.clone(), level);
    let init_var = Exp::Temp(Temp::new());
    let body = Exp::ExpSequence(
        Box::new(_Statement::Sequence(
            Box::new(_Statement::Sequence(
                Box::new(_Statement::Move(init_var.clone(), init_expr).into()),
                Box::new(_Statement::Move(array_subscript::<F>(result.clone(), loop_var.clone()), init_var).into()),
            ).into()),
            Box::new(_Statement::Move(loop_var.clone(), BinOp {
                op: Plus,
                left: Box::new(loop_var.clone()),
                right: Box::new(Const(1)),
            }).into()),
        ).into()),
        Box::new(unit())
    );
    let init =
        if let Some(var) = var {
            var_dec(&var, F::external_call("initArray", vec![size_expr, is_pointer], true))
        }
        else {
            Move(result.clone(), F::external_call("initArray", vec![size_expr, is_pointer], true)).into()
        };
    let sequence = ExpSequence(
        Box::new(Sequence(
            Box::new(init),
            Box::new(Move(loop_var, Const(0)).into()),
        ).into()),
        Box::new(while_loop(&Label::new(), test_expr, body)),
    );
    ExpSequence(
        Box::new(_Statement::Exp(sequence).into()),
        Box::new(result),
    )
}

pub fn num(number: i64) -> Exp {
    Const(number)
}

pub fn record_create<F: Frame>(data_layout: Exp, fields: Vec<Exp>) -> Exp {
    if fields.is_empty() {
        return unit();
    }
    let temp = Temp::new();
    let result = Exp::Temp(temp);
    let mut sequence = Move(result.clone(), F::external_call("allocRecord", vec![data_layout], true)).into();
    for (index, field) in fields.into_iter().enumerate() {
        let index = index + RECORD_DATA_LAYOUT_SIZE;
        let temp = Exp::Temp(Temp::new());
        sequence = Sequence(
            Box::new(sequence),
            Box::new(Sequence(
                // NOTE: the field is move into a temp first to avoid GC issues.
                // The idea is that the record field's offset won't be computed and spilled before
                // the field is computed. If the offset is spilled, it won't be updated by the GC
                // in case of heap relocation because it's a derived pointer.
                Box::new(Move(temp.clone(), field).into()),
                Box::new(Move(Mem(Box::new(BinOp {
                    op: Plus,
                    left: Box::new(result.clone()),
                    right: Box::new(Const(index as i64 * F::WORD_SIZE)),
                })), temp).into()),
            ).into()),
        ).into();
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
    let end_label = Label::new();
    let frame = level.current.borrow();
    let result = frame.exp(result.1, Exp::Temp(F::fp()));
    ExpSequence(
        Box::new(Sequence(
            Box::new(CondJump {
                op: to_ir_rel_op(op),
                left,
                right,
                true_label: true_label.clone(),
                false_label: false_label.clone(),
            }.into()),
            Box::new(Sequence(
                Box::new(_Statement::Label(true_label).into()),
                Box::new(Sequence(
                    Box::new(Move(result.clone(), Const(1)).into()),
                    Box::new(Sequence(
                        Box::new(Jump(Name(end_label.clone()), vec![end_label.clone()]).into()),
                        Box::new(Sequence(
                            Box::new(_Statement::Label(false_label).into()),
                            Box::new(Sequence(
                                Box::new(Move(result.clone(), Const(0)).into()),
                                Box::new(_Statement::Label(end_label).into()),
                            ).into())
                        ).into()),
                    ).into()),
                ).into()),
            ).into()),
        ).into()),
        Box::new(result),
    )
}

pub fn simple_var<F: Clone + Frame + PartialEq>(access: Access<F>, level: &Level<F>) -> Exp {
    let frame = level.current.borrow();
    frame.exp(access.1, Exp::Temp(F::fp()))
}

pub fn string_equality<F: Frame>(oper: Operator, left: Exp, right: Exp) -> Exp {
    let exp = F::external_call("stringEqual", vec![left, right], false);
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
    let frame = var_level.current.borrow();
    let dec = Move(frame.exp(access.1.clone(), Exp::Temp(F::fp())), value);
    if let Some(pos) = access.1.as_stack() {
        // This is for the purpose of stack variable liveness analysis.
        Statement {
            stack_var: Some(pos),
            statement: dec,
        }
    }
    else {
        Statement {
            stack_var: None,
            statement: dec,
        }
    }
}

pub fn var_decs(variables: Vec<Statement>, body: Exp) -> Exp {
    if variables.is_empty() {
        return body;
    }

    let len = variables.len();
    let mut iter = variables.into_iter();
    let var1 = iter.next().expect("first variable declaration");
    if len < 2 {
        return ExpSequence(Box::new(var1), Box::new(body));
    }
    let var2 = iter.next().expect("second variable declaration");
    let mut statements = Sequence(Box::new(var1), Box::new(var2)).into();
    for var in iter {
        statements = Sequence(
            Box::new(statements),
            Box::new(var),
        ).into();
    }
    ExpSequence(Box::new(statements), Box::new(body))
}

pub fn while_loop(done_label: &Label, test_expr: Exp, body: Exp) -> Exp {
    let test_label = Label::new();
    let after_check_label = Label::new();
    ExpSequence(
        Box::new(Sequence(
            Box::new(Sequence(
                Box::new(_Statement::Label(test_label.clone()).into()),
                Box::new(Sequence(
                    Box::new(Sequence(
                        Box::new(CondJump {
                            op: NotEqual,
                            left: test_expr,
                            right: Const(1),
                            true_label: done_label.clone(),
                            false_label: after_check_label.clone(),
                        }.into()),
                        Box::new(_Statement::Label(after_check_label).into()),
                    ).into()),
                    Box::new(Sequence(
                        Box::new(_Statement::Exp(body).into()),
                        Box::new(Jump(Name(test_label.clone()), vec![test_label]).into()),
                    ).into()),
                ).into()),
            ).into()),
            Box::new(_Statement::Label(done_label.clone()).into()),
        ).into()),
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
    ir: IR<F>,
}

impl<F:Frame> Gen<F> {
    pub fn new() -> Self {
        Self {
            ir: IR::new(),
        }
    }

    pub fn get_result(self) -> IR<F> {
        self.ir
    }

    pub fn proc_entry_exit(&mut self, level: &Level<F>, body: Exp, temp_map: TempMap, escaping_vars: Vec<i64>) {
        let body = Move(Exp::Temp(F::return_value()), body).into();
        self.ir.functions.insert(level.current.borrow().name().to_string(), Fragment::Function {
            body,
            escaping_vars,
            frame: level.current.clone(),
            temp_map,
        });
    }

    pub fn string_literal(&mut self, string: String) -> Exp {
        let label = Label::new();
        self.ir.data.push(Fragment::Str(label.clone(), string));
        Name(label)
    }
}
