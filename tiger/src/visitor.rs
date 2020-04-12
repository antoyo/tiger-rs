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

use ast::{
    Declaration,
    DeclarationWithPos,
    Expr,
    ExprWithPos,
    FuncDeclaration,
    Operator,
};
use position::WithPos;
use symbol::SymbolWithPos;

pub trait Visitor {
    fn visit_binary_op(&mut self, left: &ExprWithPos, right: &ExprWithPos) {
        self.visit_exp(left);
        self.visit_exp(right);
    }

    fn visit_dec(&mut self, declaration: &DeclarationWithPos) {
        match declaration.node {
            Declaration::ClassDeclaration { ref declarations, .. } => {
                for declaration in declarations {
                    self.visit_dec(declaration);
                }
            },
            Declaration::Function(ref declarations) => {
                for &WithPos { node: FuncDeclaration { ref body, .. }, .. } in declarations {
                    self.visit_exp(body);
                }
            },
            Declaration::Type(_) => (),
            Declaration::VariableDeclaration { ref init, .. } => {
                self.visit_exp(init);
            },
        }
    }

    fn visit_call(&mut self, function: &ExprWithPos, args: &[ExprWithPos]) {
        self.visit_exp(function);
        for arg in args {
            self.visit_exp(arg);
        }
    }

    fn visit_exp(&mut self, expr: &ExprWithPos) {
        match expr.node {
            Expr::Array { ref init, ref size, .. } => {
                self.visit_exp(size);
                self.visit_exp(init);
            },
            Expr::Assign { ref expr, ref var } => {
                self.visit_exp(var);
                self.visit_exp(expr);
            },
            Expr::Break => {
            },
            Expr::Call { ref args, ref function } => {
                self.visit_call(function, args);
            },
            Expr::Closure { ref body, .. } => {
                self.visit_exp(body);
            },
            Expr::ClosureParamField { ref this, .. } =>
                self.visit_exp(this),
            Expr::Field { ref this, .. } =>
                self.visit_exp(this),
            Expr::FunctionPointer { .. } => (),
            Expr::FunctionPointerCall { ref args, ref function, .. } => {
                self.visit_exp(function);
                for arg in args {
                    self.visit_exp(arg);
                }
            },
            Expr::If { ref else_, ref test, ref then } => {
                self.visit_exp(test);
                self.visit_exp(then);
                if let Some(ref else_) = *else_ {
                    self.visit_exp(&else_);
                }
            },
            Expr::Int { .. } => (),
            Expr::Let { ref body, ref declarations } => {
                for declaration in declarations {
                    self.visit_dec(declaration);
                }
                self.visit_exp(body);
            },
            Expr::MethodCall { ref args, ref this, .. } => {
                self.visit_exp(this);
                for arg in args {
                    self.visit_exp(arg);
                }
            },
            Expr::New { .. } => (),
            Expr::Nil => (),
            Expr::Oper { ref left, oper: WithPos { node: Operator::Plus, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: Operator::Minus, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: Operator::Times, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: Operator::Lt, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: Operator::Gt, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: Operator::And, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: Operator::Or, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: Operator::Ge, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: Operator::Le, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: Operator::Divide, .. }, ref right } =>
                self.visit_binary_op(left, right),
            Expr::Oper { ref left, oper: WithPos { node: Operator::Equal, .. }, ref right }
            | Expr::Oper { ref left, oper: WithPos { node: Operator::Neq, .. }, ref right } => {
                self.visit_exp(left);
                self.visit_exp(right);
            },
            Expr::Record { ref fields, .. } => {
                for field in fields {
                    self.visit_exp(&field.node.expr);
                }
            },
            Expr::Sequence(ref exprs) => {
                if let Some((last_expr, exprs)) = exprs.split_last() {
                    for expr in exprs {
                        self.visit_exp(expr);
                    }
                    self.visit_exp(last_expr)
                }
            },
            Expr::Str { .. } => (),
            Expr::Subscript { ref expr, ref this } => {
                self.visit_exp(this);
                self.visit_exp(expr);
            },
            Expr::Variable(ref ident) => self.visit_var(ident),
            Expr::While { ref body, ref test } => {
                self.visit_exp(test);
                self.visit_exp(body);
            },
        }
    }

    fn visit_var(&mut self, _ident: &SymbolWithPos) {
    }
}
