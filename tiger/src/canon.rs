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

use std::collections::{
    HashMap,
    HashSet,
    VecDeque,
};
use std::mem;

use ir::{
    Exp,
    RelationalOp,
    Statement,
};
use temp::{Label, Temp};

pub fn linearize(statement: Statement) -> Vec<Statement> {
    let statement = do_statement(statement);

    fn linear(statement: Statement, result: &mut Vec<Statement>) {
        if let Statement::Sequence(statement1, statement2) = statement {
            linear(*statement1, result);
            linear(*statement2, result);
        }
        else {
            result.push(statement);
        }
    }

    let mut result = vec![];
    linear(statement, &mut result);
    result
}

pub fn basic_blocks(statements: Vec<Statement>) -> (Vec<Vec<Statement>>, Label) {
    let done = Label::new();

    #[derive(PartialEq)]
    enum State {
        Label,
        InBlock,
    }

    let mut state = State::Label;
    let mut basic_blocks = vec![];
    for statement in statements {
        if state == State::Label {
            state = State::InBlock;
            match statement {
                Statement::Label(_) => {
                    basic_blocks.push(vec![statement]);
                    continue;
                },
                _ => basic_blocks.push(vec![Statement::Label(Label::new())]),
            }
        }

        if state == State::InBlock {
            match statement {
                Statement::Label(ref label) => {
                    basic_blocks.last_mut().expect("at least one basic block")
                        .push(Statement::Jump(Exp::Name(label.clone()), vec![label.clone()]));
                    basic_blocks.push(vec![statement]);
                },
                Statement::Jump(_, _) | Statement::CondJump { .. } => {
                    basic_blocks.last_mut().expect("at least one basic block for a jump").push(statement);
                    state = State::Label;
                },
                statement => basic_blocks.last_mut().expect("at least one basic block for a jump").push(statement),
            }
        }
    }

    if state == State::InBlock {
        basic_blocks.last_mut().expect("at least one basic block")
            .push(Statement::Jump(Exp::Name(done.clone()), vec![done.clone()]));
    }

    (basic_blocks, done)
}

pub fn trace_schedule(mut basic_blocks: Vec<Vec<Statement>>, done_label: Label) -> Vec<Statement> {
    let mut label_mapping = HashMap::new();
    label_mapping.insert(&done_label, usize::max_value());
    for (index, basic_block) in basic_blocks.iter().enumerate() {
        match basic_block.first().expect("at least one statement in basic block") {
            Statement::Label(label) => {
                let _ = label_mapping.insert(label, index);
            },
            _ => panic!("label as first statement of basic block"),
        }
    }

    let mut marks = HashSet::new();
    let mut traces = vec![];
    let mut current_trace;
    for (mut index, basic_block) in basic_blocks.iter().enumerate() {
        current_trace = vec![];
        while !marks.contains(&index) {
            marks.insert(index);
            current_trace.push(index);

            match basic_block.last().expect("at least one instruction in basic block") {
                Statement::CondJump { true_label, false_label, .. } => {
                    if let Some(&next_index) = marks.get(&label_mapping[false_label]) {
                        index = next_index;
                    }
                    else if let Some(&next_index) = marks.get(&label_mapping[true_label]) {
                        index = next_index;
                    }
                },
                Statement::Jump(_, labels) => {
                    for label in labels {
                        if let Some(&next_index) = marks.get(&label_mapping[label]) {
                            index = next_index;
                            break;
                        }
                    }
                },
                _ => panic!("Expected jump as last statement of basic blocks"),
            }
        }
        if !current_trace.is_empty() {
            traces.push(current_trace);
        }
    }

    let mut statements = VecDeque::new();

    for trace in traces {
        for index in trace {
            let trace_statements = mem::replace(&mut basic_blocks[index], vec![]);
            for statement in trace_statements {
                statements.push_back(statement);
            }
        }
    }

    let mut new_statements = vec![];

    let mut current = statements.pop_front();
    while let Some(statement) = current {
        match statement {
            Statement::CondJump { op, left, right, false_label, true_label } => {
                let next = statements.pop_front().expect("pop front label");
                if next == Statement::Label(true_label.clone()) {
                    // Put the false label right after the jump by negating the condition and
                    // switching the labels.
                    new_statements.push(Statement::CondJump {
                        op: negate_condition(op),
                        left,
                        right,
                        false_label: true_label,
                        true_label: false_label,
                    });
                }
                else if next == Statement::Label(false_label.clone()) {
                    new_statements.push(Statement::CondJump {
                        op,
                        left,
                        right,
                        false_label,
                        true_label,
                    });
                }
                else {
                    // If the condition is not followed by any of its labels, create a new label
                    // where the code will jump to the false label.
                    let new_false = Label::new();
                    new_statements.push(Statement::CondJump {
                        op,
                        left,
                        right,
                        false_label: new_false.clone(),
                        true_label,
                    });
                    new_statements.push(Statement::Label(new_false));
                    new_statements.push(Statement::Jump(Exp::Name(false_label.clone()), vec![false_label]));
                }
                new_statements.push(next);
            },
            Statement::Jump(expr, labels) => {
                match expr {
                    Exp::Name(label) => {
                        if labels.len() == 1 && labels[0] == label {
                            if let Some(ref statement) = statements.front() {
                                if let Statement::Label(ref next_label) = statement {
                                    if next_label == &label {
                                        // Remove unconditional jumps to next statement.
                                        current = statements.pop_front();
                                        continue;
                                    }
                                }
                            }
                        }

                        new_statements.push(Statement::Jump(Exp::Name(label), labels));
                    },
                    _ => new_statements.push(Statement::Jump(expr, labels))
                }
            },
            statement => new_statements.push(statement),
        }
        current = statements.pop_front();
    }

    new_statements.push(Statement::Label(done_label));

    new_statements
}

fn negate_condition(op: RelationalOp) -> RelationalOp {
    match op {
        RelationalOp::Equal => RelationalOp::NotEqual,
        RelationalOp::GreaterOrEqual => RelationalOp::LesserThan,
        RelationalOp::GreaterThan => RelationalOp::LesserOrEqual,
        RelationalOp::LesserOrEqual => RelationalOp::GreaterThan,
        RelationalOp::LesserThan => RelationalOp::GreaterOrEqual,
        RelationalOp::NotEqual => RelationalOp::Equal,
        RelationalOp::UnsignedGreaterOrEqual => RelationalOp::UnsignedLesserThan,
        RelationalOp::UnsignedGreaterThan => RelationalOp::UnsignedLesserOrEqual,
        RelationalOp::UnsignedLesserOrEqual => RelationalOp::UnsignedGreaterThan,
        RelationalOp::UnsignedLesserThan => RelationalOp::UnsignedGreaterOrEqual,
    }
}

fn commute(expr1: &Statement, expr2: &Exp) -> bool {
    match (expr1, expr2) {
        (Statement::Exp(Exp::Const(_)), _) => true,
        (_, Exp::Name(_)) => true,
        (_, Exp::Const(_)) => true,
        _ => false,
    }
}

fn reorder1(expr: Exp) -> (Statement, Exp) {
    do_expression(expr)
}

fn reorder2(expr1: Exp, expr2: Exp) -> (Statement, Exp, Exp) {
    if let Exp::Call(_, _) = expr1 {
        let temp = Temp::new();
        return reorder2(Exp::ExpSequence(
            Box::new(Statement::Move(Exp::Temp(temp), expr1)),
            Box::new(Exp::Temp(temp))),
            expr2
        );
    }

    let (statements, expr1) = do_expression(expr1);
    let (statements2, expr2) = do_expression(expr2);

    if commute(&statements2, &expr1) {
        (Statement::Sequence(Box::new(statements), Box::new(statements2)), expr1, expr2)
    }
    else {
        let temp = Temp::new();
        let statements = Statement::Sequence(
            Box::new(statements),
            Box::new(Statement::Sequence(
                Box::new(Statement::Move(Exp::Temp(temp), expr1)),
                Box::new(statements2),
            )),
        );
        (statements, Exp::Temp(temp), expr2)
    }
}

fn reorder(mut exprs: VecDeque<Exp>) -> (Statement, VecDeque<Exp>) {
    if exprs.is_empty() {
        return (Statement::Exp(Exp::Const(0)), VecDeque::new());
    }

    if let Exp::Call(_, _) = exprs.front().expect("front") {
        let temp = Temp::new();
        let function = exprs.pop_front().expect("pop front");
        exprs.push_front(Exp::ExpSequence(
            Box::new(Statement::Move(Exp::Temp(temp), function)),
            Box::new(Exp::Temp(temp))
        ));
        return reorder(exprs);
    }

    let (statements, expr1) = do_expression(exprs.pop_front().expect("pop front"));
    let (statements2, mut expr2) = reorder(exprs);

    if commute(&statements2, &expr1) {
        expr2.push_front(expr1);
        (append(statements, statements2), expr2)
    }
    else {
        let temp = Temp::new();
        let statements = append(statements, append(Statement::Move(Exp::Temp(temp), expr1), statements2));
        expr2.push_front(Exp::Temp(temp));
        (statements, expr2)
    }
}

fn reorder_statement1<F: FnOnce(Exp) -> Statement>(expr: Exp, builder: F) -> Statement {
    let (statements, expr) = reorder1(expr);
    Statement::Sequence(Box::new(statements), Box::new(builder(expr)))
}

fn reorder_statement2<F: FnOnce(Exp, Exp) -> Statement>(expr1: Exp, expr2: Exp, builder: F) -> Statement {
    let (statements, expr1, expr2) = reorder2(expr1, expr2);
    Statement::Sequence(Box::new(statements), Box::new(builder(expr1, expr2)))
}

fn reorder_statement<F: FnOnce(VecDeque<Exp>) -> Statement>(exprs: VecDeque<Exp>, builder: F) -> Statement {
    let (statements, exprs) = reorder(exprs);
    Statement::Sequence(Box::new(statements), Box::new(builder(exprs)))
}

fn reorder_expression1<F: FnOnce(Exp) -> Exp>(expr: Exp, builder: F) -> (Statement, Exp) {
    let (statements, expr) = reorder1(expr);
    (statements, builder(expr))
}

fn reorder_expression2<F: FnOnce(Exp, Exp) -> Exp>(expr1: Exp, expr2: Exp, builder: F) -> (Statement, Exp) {
    let (statements, expr1, expr2) = reorder2(expr1, expr2);
    (statements, builder(expr1, expr2))
}

fn reorder_expression<F: FnOnce(VecDeque<Exp>) -> Exp>(exprs: VecDeque<Exp>, builder: F) -> (Statement, Exp) {
    let (statements, exprs) = reorder(exprs);
    (statements, builder(exprs))
}

fn do_statement(statement: Statement) -> Statement {
    match statement {
        Statement::Sequence(statement1, statement2) =>
            append(do_statement(*statement1), do_statement(*statement2)),
        Statement::Jump(expr, labels) =>
            reorder_statement1(expr, |expr| Statement::Jump(expr, labels)),
        Statement::CondJump { op, left, right, true_label, false_label } =>
            reorder_statement2(left, right, |left, right| Statement::CondJump {
                op,
                left,
                right,
                true_label,
                false_label,
            }),
        Statement::Move(Exp::Temp(temp), Exp::Call(function, arguments)) => {
            let mut exprs = VecDeque::new();
            exprs.push_back(*function);
            exprs.extend(arguments);
            reorder_statement(exprs, |mut exprs| {
                let function = exprs.pop_front().expect("pop front");
                let exprs = exprs.into_iter().collect();
                Statement::Move(Exp::Temp(temp),
                    Exp::Call(Box::new(function), exprs)
                )
            })
        }
        Statement::Move(Exp::Temp(temp), expr) =>
            reorder_statement1(expr, |expr| Statement::Move(Exp::Temp(temp), expr)),
        Statement::Move(Exp::Mem(mem), expr) =>
            reorder_statement2(*mem, expr, |mem, expr| Statement::Move(Exp::Mem(Box::new(mem)), expr)),
        Statement::Move(Exp::ExpSequence(statement, expr1), expr2) =>
            do_statement(Statement::Sequence(statement, Box::new(Statement::Move(*expr1, expr2)))),
        Statement::Exp(Exp::Call(function, arguments)) => {
            let mut exprs = VecDeque::new();
            exprs.push_back(*function);
            exprs.extend(arguments);
            reorder_statement(exprs, |mut exprs| {
                let function = exprs.pop_front().expect("pop front");
                let exprs = exprs.into_iter().collect();
                Statement::Exp(Exp::Call(Box::new(function), exprs))
            })
        },
        Statement::Exp(expr) =>
            reorder_statement1(expr, |expr| Statement::Exp(expr)),
        _ => statement,
    }
}

fn do_expression(expr: Exp) -> (Statement, Exp) {
    match expr {
        Exp::BinOp { op, left, right } =>
            reorder_expression2(*left, *right, |left, right| Exp::BinOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            }),
        Exp::Mem(expr) =>
            reorder_expression1(*expr, |expr| Exp::Mem(Box::new(expr))),
        Exp::ExpSequence(statement, expr) => {
            let statements1 = do_statement(*statement);
            let (statements2, expr) = do_expression(*expr);
            (append(statements1, statements2), expr)
        },
        Exp::Call(function, arguments) => {
            let mut exprs = VecDeque::new();
            exprs.push_back(*function);
            exprs.extend(arguments);
            reorder_expression(exprs, |mut exprs| {
                let function = exprs.pop_front().expect("pop front");
                let exprs = exprs.into_iter().collect();
                Exp::Call(Box::new(function), exprs)
            })
        },
        _ => (Statement::Exp(Exp::Const(0)), expr),
    }
}

fn append(statement1: Statement, statement2: Statement) -> Statement {
    match (statement1, statement2) {
        (Statement::Exp(Exp::Const(_)), statement2) => statement2,
        (statement1, Statement::Exp(Exp::Const(_))) => statement1,
        (statement1, statement2) => Statement::Sequence(Box::new(statement1), Box::new(statement2)),
    }
}

#[cfg(test)]
mod tests {
    use canon::linearize;
    use ir::{Exp, Statement};

    #[test]
    fn test_rewrite_rules() {
        let expr = Exp::ExpSequence(Box::new(Statement::Exp(Exp::Const(1))),
            Box::new(Exp::ExpSequence(Box::new(Statement::Exp(Exp::Const(2))), Box::new(Exp::Const(3)))));

        let result = linearize(Statement::Exp(expr));

        println!("{:#?}", result);
    }
}
