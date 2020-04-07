/// Extract the values collectable by the GC as variables.
/// They need to be on the stack in order to be accessible by the GC.

use ast::{
    Declaration,
    DeclarationWithPos,
    Expr,
    ExprWithPos,
    RecordField,
};
use position::{Pos, WithPos};
use symbol::{Symbol, Symbols};

pub struct Rewriter<'a> {
    index: usize,
    symbols: &'a mut Symbols<()>,
}

impl<'a> Rewriter<'a> {
    pub fn new(symbols: &'a mut Symbols<()>) -> Self {
        Self {
            index: 0,
            symbols,
        }
    }

    fn extract(&mut self, expr: ExprWithPos) -> (Symbol, Declaration) {
        let name = self.symbols.symbol(&format!("__var_{}", self.index));
        self.index += 1;
        (name, Declaration::VariableDeclaration {
            escape: false,
            init: self.rewrite(expr),
            name,
            typ: None,
        })
    }

    pub fn rewrite(&mut self, expr: ExprWithPos) -> ExprWithPos {
        let pos = expr.pos;
        match expr.node {
            Expr::Array { init, size, typ } => {
                // NOTE: do not rewrite the initial element of the array because it's immediately assigned a memory location in the array.
                let init = self.rewrite(*init);
                WithPos::new(Expr::Array {
                    init: Box::new(init),
                    size: Box::new(self.rewrite(*size)),
                    typ,
                }, pos)
            },
            Expr::Assign { expr, var } => {
                WithPos::new(Expr::Assign {
                    expr: Box::new(self.rewrite(*expr)),
                    var: Box::new(self.rewrite(*var)),
                }, pos)
            },
            Expr::Break => WithPos::new(Expr::Break, pos),
            Expr::Call { args, function } => {
                let mut new_args = vec![];
                let mut declarations = vec![];
                for arg in args {
                    if can_extract(&arg) {
                        let (name, declaration) = self.extract(arg);
                        declarations.push(WithPos::new(declaration, pos));
                        new_args.push(variable(name, pos));
                    }
                    else {
                        new_args.push(self.rewrite(arg));
                    }
                }
                let call = WithPos::new(Expr::Call {
                    args: new_args,
                    function,
                }, pos);

                if declarations.is_empty() {
                    call
                }
                else {
                    add_declarations(call, declarations, pos)
                }
            },
            Expr::Field { ident, this } => {
                WithPos::new(Expr::Field {
                    ident,
                    this: Box::new(self.rewrite(*this)),
                }, pos)
            },
            Expr::If { else_, test, then } => {
                // TODO: extract then and else?
                let mut declarations = vec![];
                let test =
                    if can_extract(&test) {
                        let (name, declaration) = self.extract(*test);
                        declarations.push(WithPos::new(declaration, pos));
                        variable(name, pos)
                    }
                    else {
                        self.rewrite(*test)
                    };
                let else_ = else_.map(|else_| Box::new(self.rewrite(*else_)));
                let cond = WithPos::new(Expr::If {
                    else_,
                    test: Box::new(test),
                    then: Box::new(self.rewrite(*then)),
                }, pos);
                if declarations.is_empty() {
                    cond
                }
                else {
                    add_declarations(cond, declarations, pos)
                }
            },
            Expr::Int { value } => WithPos::new(Expr::Int { value }, pos),
            Expr::Let { body, declarations } => {
                let mut new_declarations = vec![];
                for declaration in declarations {
                    new_declarations.push(self.rewrite_dec(declaration));
                }
                WithPos::new(Expr::Let {
                    body: Box::new(self.rewrite(*body)),
                    declarations: new_declarations,
                }, pos)
            },
            Expr::MethodCall { args, method, this } => {
                let mut new_args = vec![];
                let mut declarations = vec![];
                for arg in args {
                    if can_extract(&arg) {
                        let (name, declaration) = self.extract(arg);
                        declarations.push(WithPos::new(declaration, pos));
                        new_args.push(variable(name, pos));
                    }
                    else {
                        new_args.push(self.rewrite(arg));
                    }
                }
                let call = WithPos::new(Expr::MethodCall {
                    args: new_args,
                    method,
                    this: Box::new(self.rewrite(*this)),
                }, pos);

                if declarations.is_empty() {
                    call
                }
                else {
                    add_declarations(call, declarations, pos)
                }
            },
            Expr::New { class_name } => WithPos::new(Expr::New { class_name }, pos),
            Expr::Nil => WithPos::new(Expr::Nil, pos),
            Expr::Oper { left, right, oper } => {
                let mut declarations = vec![];
                let left =
                    if can_extract(&left) {
                        let (name, declaration) = self.extract(*left);
                        declarations.push(WithPos::new(declaration, pos));
                        variable(name, pos)
                    }
                    else {
                        self.rewrite(*left)
                    };
                let right =
                    if can_extract(&right) {
                        let (name, declaration) = self.extract(*right);
                        declarations.push(WithPos::new(declaration, pos));
                        variable(name, pos)
                    }
                    else {
                        self.rewrite(*right)
                    };
                let oper = WithPos::new(Expr::Oper {
                    left: Box::new(left),
                    right: Box::new(right),
                    oper,
                }, pos);
                if declarations.is_empty() {
                    oper
                }
                else {
                    add_declarations(oper, declarations, pos)
                }
            },
            Expr::Record { fields, typ } => {
                let mut new_fields = vec![];
                let mut declarations = vec![];
                for mut field in fields {
                    if can_extract(&field.node.expr) {
                        let (name, declaration) = self.extract(field.node.expr);
                        declarations.push(WithPos::new(declaration, pos));
                        field.node = RecordField {
                            expr: variable(name, pos),
                            ident: field.node.ident,
                        };
                        new_fields.push(field);
                    }
                    else {
                        field.node.expr = self.rewrite(field.node.expr);
                        new_fields.push(field);
                    }
                }
                let record =
                    WithPos::new(Expr::Record {
                        fields: new_fields,
                        typ,
                    }, pos);
                if declarations.is_empty() {
                    record
                }
                else {
                    add_declarations(record, declarations, pos)
                }
            },
            Expr::Sequence(exprs) => {
                // TODO: extract.
                let mut new_exprs = vec![];
                for expr in exprs {
                    new_exprs.push(self.rewrite(expr));
                }
                WithPos::new(Expr::Sequence(new_exprs), pos)
            },
            Expr::Str { value } => WithPos::new(Expr::Str { value }, pos),
            Expr::Subscript { expr, this } => {
                WithPos::new(Expr::Subscript {
                    expr: Box::new(self.rewrite(*expr)),
                    this: Box::new(self.rewrite(*this)),
                }, pos)
            },
            Expr::Variable(var) => WithPos::new(Expr::Variable(var), pos),
            Expr::While { body, test } => {
                // TODO: extract.
                WithPos::new(Expr::While {
                    body: Box::new(self.rewrite(*body)),
                    test: Box::new(self.rewrite(*test)),
                }, pos)
            },
        }
    }

    fn rewrite_dec(&mut self, mut declaration: DeclarationWithPos) -> DeclarationWithPos {
        declaration.node =
            match declaration.node {
                Declaration::ClassDeclaration { declarations, name, parent_class } => {
                    let declarations =
                        declarations
                            .into_iter()
                            .map(|declaration| self.rewrite_dec(declaration))
                            .collect();
                    Declaration::ClassDeclaration {
                        declarations,
                        name,
                        parent_class,
                    }
                },
                Declaration::Function(functions) => {
                    let mut new_functions = vec![];
                    for mut function in functions {
                        function.node.body = self.rewrite(function.node.body);
                        let mut declarations = vec![];
                        for param in &function.node.params {
                            declarations.push(WithPos::new(Declaration::VariableDeclaration {
                                escape: false,
                                init: WithPos::new(Expr::Variable(WithPos::new(param.node.name, param.pos)), param.pos),
                                name: param.node.name,
                                typ: None,
                            }, param.pos));
                        }
                        function.node.body = WithPos::new(Expr::Let {
                            body: Box::new(function.node.body),
                            declarations,
                        }, function.pos);
                        new_functions.push(function);
                    }
                    Declaration::Function(new_functions)
                },
                Declaration::Type(types) => Declaration::Type(types),
                Declaration::VariableDeclaration { escape, init, name, typ } => {
                    Declaration::VariableDeclaration {
                        escape,
                        init: self.rewrite(init),
                        name,
                        typ,
                    }
                },
            };
        declaration
    }
}

fn add_declarations(body: ExprWithPos, declarations: Vec<DeclarationWithPos>, pos: Pos) -> ExprWithPos {
    WithPos::new(Expr::Let {
        body: Box::new(body),
        declarations,
    }, pos)
}

fn can_extract(expr: &ExprWithPos) -> bool {
    match expr.node {
        Expr::Nil => false,
        _ => true,
    }
}

fn variable(name: Symbol, pos: Pos) -> ExprWithPos {
    WithPos::new(Expr::Variable(WithPos::new(name, pos)), pos)
}
