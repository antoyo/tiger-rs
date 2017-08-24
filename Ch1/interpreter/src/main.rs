use std::cmp::max;
use std::collections::HashMap;

use BinOp::*;
use Expression::*;
use Statement::*;

type Id = String;

enum BinOp {
    Div,
    Minus,
    Plus,
    Times,
}

enum Expression {
    Id(Id),
    Num(i32),
    Operation(Box<Expression>, BinOp, Box<Expression>),
    Sequence(Statement, Box<Expression>),
}

enum Statement {
    Assign(Id, Box<Expression>),
    Compound(Box<Statement>, Box<Statement>),
    Print(Vec<Expression>),
}

fn main() {
    let program =
        Compound(Box::new(
            Assign("a".to_string(), Box::new(Operation(
                Box::new(Num(5)),
                Plus,
                Box::new(Num(3))
            ))),
        ), Box::new(Compound(
            Box::new(Assign("b".to_string(), Box::new(Sequence(
                Print(vec![Id("a".to_string()), Operation(
                    Box::new(Id("a".to_string())),
                    Minus,
                    Box::new(Num(1))
                )]),
                Box::new(Operation(Box::new(Num(10)), Times, Box::new(Id("a".to_string()))))
            )))),
            Box::new(Print(vec![Id("b".to_string())]))
        )));

    println!("{}", maxargs(&program));

    let mut interpreter = Interpreter::new();
    interpreter.statement(&program);
}

fn maxargs_expr(expr: &Expression) -> i32 {
    match *expr {
        Id(_) => 0,
        Num(_) => 0,
        Operation(ref expr1, _, ref expr2) => max(maxargs_expr(expr1), maxargs_expr(expr2)),
        Sequence(ref statement, ref expr) => max(maxargs(statement), maxargs_expr(expr)),
    }
}

fn maxargs(statement: &Statement) -> i32 {
    match *statement {
        Assign(_, ref expr) => maxargs_expr(expr),
        Compound(ref statement1, ref statement2) => max(maxargs(statement1), maxargs(statement2)),
        Print(ref exprs) => {
            let maximum = exprs.iter()
                .map(|expr| maxargs_expr(expr))
                .max()
                .unwrap_or(0);
            max(maximum, exprs.len() as i32)
        },
    }
}

struct Interpreter {
    env: HashMap<Id, i32>,
}

impl Interpreter {
    fn new() -> Self {
        Interpreter {
            env: HashMap::new(),
        }
    }

    fn expr(&mut self, expr: &Expression) -> i32 {
        match *expr {
            Id(ref ident) => self.env[ident],
            Num(num) => num,
            Operation(ref expr1, ref op, ref expr2) => {
                let val1 = self.expr(expr1);
                let val2 = self.expr(expr2);
                match *op {
                    Div => val1 / val2,
                    Minus => val1 - val2,
                    Plus => val1 + val2,
                    Times => val1 * val2,
                }
            },
            Sequence(ref statement, ref expr) => {
                self.statement(statement);
                self.expr(expr)
            },
        }
    }

    fn statement(&mut self, statement: &Statement) {
        match *statement {
            Assign(ref ident, ref expr) => {
                let value = self.expr(expr);
                let _ = self.env.insert(ident.clone(), value);
            },
            Compound(ref statement1, ref statement2) => {
                self.statement(statement1);
                self.statement(statement2);
            },
            Print(ref exprs) => {
                let exprs = exprs.iter()
                    .map(|expr| self.expr(expr));
                for expr in exprs {
                    print!("{} ", expr);
                }
                println!();
            },
        }
    }
}
