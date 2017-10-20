/*
 * Copyright (c) 2017 Boucher, Antoni <bouanto@zoho.com>
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
 * Operator precedence:
 * -
 * * /
 * + -
 * = <> > < >= <=
 * &
 * |
 */

use std::io::Read;
use std::result;

use ast::{
    Declaration,
    Expr,
    Field,
    FuncDeclaration,
    Ident,
    Operator,
    RecordField,
    Ty,
    Var,
};
use ast::Declaration::VariableDeclaration;
use error::{Error, Result};
use error::Error::UnexpectedToken;
use lexer::Lexer;
use position::Pos;
use symbol::Symbol;
use token::{Tok, Token};
use token::Tok::*;

macro_rules! eat {
    ($_self:ident, $pat:ident, $var:ident) => {
        match $_self.token() {
            Ok(token) => {
                match token.token {
                    $pat(var) => {
                        $var = var;
                        token.start
                    },
                    tok => return Err(UnexpectedToken {
                        expected: stringify!($pat).to_lowercase(),
                        pos: token.start,
                        unexpected: tok,
                    }),
                }
            },
            Err(error) => return Err(error),
        }
    };
    ($_self:ident, $pat:ident) => {
        eat!($_self, $pat, stringify!($pat).to_lowercase())
    };
    ($_self:ident, $pat:ident, $expected:expr) => {
        match $_self.token() {
            Ok(token) => {
                match token.token {
                    $pat => token.start,
                    tok => return Err(UnexpectedToken {
                        expected: $expected,
                        pos: token.start,
                        unexpected: tok,
                    }),
                }
            },
            Err(error) => return Err(error),
        }
    };
}

macro_rules! fields {
    ($_self:ident, $pat:ident) => {
        match $_self.peek()?.token {
            $pat => vec![],
            _ => $_self.fields()?,
        }
    };
}

pub struct Parser<R: Read> {
    lexer: Lexer<R>,
    lookahead: Option<Result<Token>>,
}

impl<R: Read> Parser<R> {
    pub fn new(lexer: Lexer<R>) -> Self {
        Parser {
            lexer,
            lookahead: None,
        }
    }

    fn additive_expr(&mut self) -> Result<Expr> {
        let mut expr = self.multiplicative_expr()?;
        loop {
            let (oper_pos, oper) =
                match self.peek_token() {
                    Ok(&Minus) => (eat!(self, Minus), Operator::Minus),
                    Ok(&Plus) => (eat!(self, Plus), Operator::Plus),
                    _ => break,
                };
            let right = Box::new(self.multiplicative_expr()?);
            expr = Expr::Oper {
                left: Box::new(expr),
                oper,
                oper_pos,
                right,
            };
        }
        Ok(expr)
    }

    fn arr_ty(&mut self) -> Result<Ty> {
        let pos = eat!(self, Array);
        eat!(self, Of);
        let type_name;
        eat!(self, Ident, type_name);
        let ident = Symbol::new(&type_name);
        Ok(Ty::Array {
            ident,
            pos,
        })
    }

    fn break_(&mut self) -> Result<Expr> {
        let pos = eat!(self, Break);
        Ok(Expr::Break(pos))
    }

    fn call_expr_or_other(&mut self) -> Result<Expr> {
        let name;
        let pos = eat!(self, Ident, name);
        let symbol = Symbol::new(&name);
        if let OpenParen = self.peek()?.token {
            eat!(self, OpenParen);
            let mut args = vec![];
            loop {
                if let CloseParen = self.peek()?.token {
                    break;
                }
                let arg = self.expr()?;
                args.push(arg);
                match self.peek()?.token {
                    Comma => { self.token()?; },
                    _ => break,
                }
            }
            eat!(self, CloseParen);
            Ok(Expr::Call {
                args,
                function: symbol,
                pos,
            })
        }
        else {
            match self.peek()?.token {
                OpenCurly => self.rec_create(symbol, pos),
                _ => {
                    let var = Var::Simple {
                        ident: symbol,
                        pos,
                    };
                    self.lvalue_or_assign(var)
                }
            }
        }
    }

    fn dec(&mut self) -> Result<Declaration> {
        match self.peek()?.token {
            Function => self.fun_dec(),
            Type => self.ty_dec(),
            Var => self.var_dec(),
            _ => Err(self.unexpected_token("function, type or var")?),
        }
    }

    fn expr(&mut self) -> Result<Expr> {
        self.logical_or_expr()
    }

    fn field_dec(&mut self) -> Result<Field> {
        let field_name;
        let pos = eat!(self, Ident, field_name);
        let name = Symbol::new(&field_name);
        eat!(self, Colon);
        let type_name;
        eat!(self, Ident, type_name);
        let typ = Symbol::new(&type_name);
        Ok(Field {
            escape: false,
            name,
            pos,
            typ,
        })
    }

    fn field_exp(&mut self, var: Var) -> Result<Var> {
        eat!(self, Dot);
        let field_name;
        eat!(self, Ident, field_name);
        let var = Var::Field {
            ident: Symbol::new(&field_name),
            this: Box::new(var),
        };
        self.lvalue(var)
    }

    fn fields(&mut self) -> Result<Vec<Field>> {
        let field = self.field_dec()?;
        let mut fields = vec![field];
        while let Comma = self.peek()?.token {
            eat!(self, Comma);
            fields.push(self.field_dec()?)
        }
        Ok(fields)
    }

    fn field_create(&mut self) -> Result<RecordField> {
        let field_name;
        let pos = eat!(self, Ident, field_name);
        let ident = Symbol::new(&field_name);
        eat!(self, Equal);
        let expr = self.expr()?;
        Ok(RecordField {
            expr,
            ident,
            pos,
        })
    }

    fn for_loop(&mut self) -> Result<Expr> {
        let pos = eat!(self, For);
        let var_name;
        eat!(self, Ident, var_name);
        let var = Symbol::new(&var_name);
        eat!(self, ColonEqual);
        let start = Box::new(self.expr()?);
        eat!(self, To);
        let end = Box::new(self.expr()?);
        eat!(self, Do);
        let body = Box::new(self.expr()?);
        Ok(Expr::For {
            body,
            end,
            escape: false,
            pos,
            start,
            var,
        })
    }

    fn fun_dec(&mut self) -> Result<Declaration> {
        let pos = eat!(self, Function);
        let func_name;
        eat!(self, Ident, func_name);
        let name = Symbol::new(&func_name);
        eat!(self, OpenParen);
        let params = fields!(self, CloseParen);
        eat!(self, CloseParen);
        let result = self.optional_type()?;
        eat!(self, Equal);
        let body = self.expr()?;
        let func = FuncDeclaration {
            body,
            name,
            params,
            pos,
            result,
        };
        Ok(Declaration::Function(vec![func]))
    }

    fn if_then_else(&mut self) -> Result<Expr> {
        let pos = eat!(self, If);
        let test = Box::new(self.expr()?);
        eat!(self, Then);
        let then = Box::new(self.expr()?);
        let else_=
            if let Else = self.peek()?.token {
                eat!(self, Else);
                Some(Box::new(self.expr()?))
            }
            else {
                None
            };
        Ok(Expr::If {
            else_,
            pos,
            test,
            then,
        })
    }

    fn int_lit(&mut self) -> Result<Expr> {
        let value;
        let pos = eat!(self, Int, value);
        Ok(Expr::Int {
            pos,
            value,
        })
    }

    fn let_expr(&mut self) -> Result<Expr> {
        let pos = eat!(self, Let);
        let mut declarations = vec![self.dec()?];
        loop {
            match self.peek()?.token {
                Function | Type | Var => declarations.push(self.dec()?),
                _ => break,
            }
        }
        eat!(self, In, "function, in, type, var".to_string());
        let expr = self.expr()?;
        let mut exprs = vec![expr];
        while let Semicolon = self.peek()?.token {
            eat!(self, Semicolon);
            exprs.push(self.expr()?);
        }
        eat!(self, End);
        Ok(Expr::Let {
            body: Box::new(Expr::Sequence(exprs)),
            declarations,
            pos
        })
    }

    fn logical_and_expr(&mut self) -> Result<Expr> {
        let mut expr = self.relational_expr()?;
        while let Ok(&Ampersand) = self.peek_token() {
            let oper_pos = eat!(self, Ampersand);
            let right = Box::new(self.relational_expr()?);
            expr = Expr::Oper {
                left: Box::new(expr),
                oper: Operator::And,
                oper_pos,
                right,
            };
        }
        Ok(expr)
    }

    fn logical_or_expr(&mut self) -> Result<Expr> {
        let mut expr = self.logical_and_expr()?;
        while let Ok(&Pipe) = self.peek_token() {
            let oper_pos = eat!(self, Pipe);
            let right = Box::new(self.logical_and_expr()?);
            expr = Expr::Oper {
                left: Box::new(expr),
                oper: Operator::Or,
                oper_pos,
                right,
            };
        }
        Ok(expr)
    }

    fn lvalue(&mut self, var: Var) -> Result<Var> {
        match self.peek()?.token {
            OpenSquare => self.subscript(var),
            Dot => self.field_exp(var),
            _ => Ok(var),
        }
    }

    fn lvalue_or_assign(&mut self, var: Var) -> Result<Expr> {
        let var = self.lvalue(var)?;
        let value =
            if let Of = self.peek()?.token {
                match var {
                    Var::Subscript { expr, this } => {
                        let simple_var = *this;
                        if let Var::Simple { ident, pos } = simple_var {
                            eat!(self, Of);
                            let init = Box::new(self.expr()?);
                            return Ok(Expr::Array {
                                init,
                                size: expr,
                                pos,
                                typ: ident,
                            });
                        }
                        else {
                            return Err(self.unexpected_token("neither dot nor subscript")?);
                        }
                    },
                    _ => return Err(self.unexpected_token(":= or (nothing)")?),
                }
            }
            else {
                var
            };
        if let ColonEqual = self.peek()?.token {
            eat!(self, ColonEqual);
            let expr = Box::new(self.expr()?);
            Ok(Expr::Assign {
                expr,
                var: value,
            })
        }
        else {
            Ok(Expr::Variable(value))
        }
    }

    fn multiplicative_expr(&mut self) -> Result<Expr> {
        let mut expr = self.unary_expr()?;
        loop {
            let (oper_pos, oper) =
                match self.peek_token() {
                    Ok(&Slash) => (eat!(self, Slash), Operator::Divide),
                    Ok(&Star) => (eat!(self, Star), Operator::Times),
                    _ => break,
                };
            let right = Box::new(self.unary_expr()?);
            expr = Expr::Oper {
                left: Box::new(expr),
                oper,
                oper_pos,
                right,
            };
        }
        Ok(expr)
    }

    fn nil(&mut self) -> Result<Expr> {
        eat!(self, Nil);
        Ok(Expr::Nil)
    }

    fn optional_type(&mut self) -> Result<Option<Ident>> {
        let mut typ = None;
        if let Colon = self.peek()?.token {
            eat!(self, Colon);
            let type_name;
            let pos = eat!(self, Ident, type_name);
            let ident = Symbol::new(&type_name);
            typ = Some(Ident {
                ident,
                pos,
            });
        }
        Ok(typ)
    }

    fn primary_expr(&mut self) -> Result<Expr> {
        match self.peek()?.token {
            Break => self.break_(),
            For => self.for_loop(),
            If => self.if_then_else(),
            Ident(_) => self.call_expr_or_other(),
            Int(_) => self.int_lit(),
            Let => self.let_expr(),
            Nil => self.nil(),
            OpenParen => self.seq_exp(),
            Str(_) => self.string_lit(),
            While => self.while_loop(),
            _ => Err(self.unexpected_token("break, for, if, identifier, integer literal, let, nil, (, string literal, while")?),
        }
    }

    fn rec_create(&mut self, typ: Symbol, pos: Pos) -> Result<Expr> {
        eat!(self, OpenCurly);
        let field = self.field_create()?;
        let mut fields = vec![field];
        while let Comma = self.peek()?.token {
            eat!(self, Comma);
            fields.push(self.field_create()?)
        }
        eat!(self, CloseCurly);
        Ok(Expr::Record {
            fields,
            pos,
            typ,
        })
    }

    fn rec_ty(&mut self) -> Result<Ty> {
        let pos = eat!(self, OpenCurly);
        let fields = fields!(self, CloseCurly);
        eat!(self, CloseCurly);
        Ok(Ty::Record {
            fields,
            pos,
        })
    }

    fn relational_expr(&mut self) -> Result<Expr> {
        let mut expr = self.additive_expr()?;
        loop {
            let (oper_pos, oper) =
                match self.peek_token() {
                    Ok(&Equal) => (eat!(self, Equal), Operator::Equal),
                    Ok(&Greater) => (eat!(self, Greater), Operator::Gt),
                    Ok(&GreaterOrEqual) => (eat!(self, GreaterOrEqual), Operator::Ge),
                    Ok(&Lesser) => (eat!(self, Lesser), Operator::Lt),
                    Ok(&LesserOrEqual) => (eat!(self, LesserOrEqual), Operator::Le),
                    Ok(&NotEqual) => (eat!(self, NotEqual), Operator::Neq),
                    _ => break,
                };
            let right = Box::new(self.additive_expr()?);
            expr = Expr::Oper {
                left: Box::new(expr),
                oper,
                oper_pos,
                right,
            };
        }
        Ok(expr)
    }

    fn seq_exp(&mut self) -> Result<Expr> {
        eat!(self, OpenParen);
        let mut exprs = vec![self.expr()?];
        while let Semicolon = self.peek()?.token {
            eat!(self, Semicolon);
            exprs.push(self.expr()?);
        }
        eat!(self, CloseParen);
        Ok(Expr::Sequence(exprs))
    }

    fn string_lit(&mut self) -> Result<Expr> {
        let value;
        let pos = eat!(self, Str, value);
        Ok(Expr::Str {
            pos,
            value,
        })
    }

    fn subscript(&mut self, var: Var) -> Result<Var> {
        eat!(self, OpenSquare);
        let expr = Box::new(self.expr()?);
        eat!(self, CloseSquare);
        let var = Var::Subscript {
            expr,
            this: Box::new(var),
        };
        self.lvalue(var)
    }

    fn ty(&mut self) -> Result<Ty> {
        match self.peek()?.token {
            Array => self.arr_ty(),
            OpenCurly => self.rec_ty(),
            Ident(_) => {
                let type_name;
                let pos = eat!(self, Ident, type_name);
                let ident = Symbol::new(&type_name);
                Ok(Ty::Name {
                    ident,
                    pos,
                })
            },
            _ => Err(self.unexpected_token("array, { or identifier")?),
        }
    }

    fn ty_dec(&mut self) -> Result<Declaration> {
        let pos = eat!(self, Type);
        let type_name;
        eat!(self, Ident, type_name);
        let name = Symbol::new(&type_name);
        eat!(self, Equal);
        let ty = self.ty()?;
        Ok(Declaration::Type {
            name,
            pos,
            ty,
        })
    }

    fn unary_expr(&mut self) -> Result<Expr> {
        match self.peek()?.token {
            Minus => {
                let pos = eat!(self, Minus);
                let oper_pos = pos.clone();
                let expr = self.unary_expr()?;
                Ok(Expr::Oper {
                    left: Box::new(Expr::Int {
                        pos,
                        value: 0,
                    }),
                    oper: Operator::Minus,
                    oper_pos,
                    right: Box::new(expr),
                })
            },
            _ => self.primary_expr(),
        }
    }

    fn var_dec(&mut self) -> Result<Declaration> {
        let pos = eat!(self, Var);
        let var_name;
        eat!(self, Ident, var_name);
        let typ = self.optional_type()?;
        let name = Symbol::new(&var_name);
        eat!(self, ColonEqual);
        let init = self.expr()?;
        Ok(VariableDeclaration {
            escape: false,
            init,
            name,
            pos,
            typ,
        })
    }

    fn while_loop(&mut self) -> Result<Expr> {
        let pos = eat!(self, While);
        let test = Box::new(self.expr()?);
        eat!(self, Do);
        let body = Box::new(self.expr()?);
        Ok(Expr::While {
            body,
            pos,
            test,
        })
    }

    pub fn parse(&mut self) -> Result<Expr> {
        self.expr()
    }

    fn peek(&mut self) -> result::Result<&Token, &Error> {
        if self.lookahead.is_none() {
            self.lookahead = Some(self.lexer.token());
        }
        // NOTE: lookahead always contain a value, hence unwrap.
        self.lookahead.as_ref().unwrap()
            .as_ref()
    }

    fn peek_token(&mut self) -> result::Result<&Tok, &Error> {
        self.peek()
            .map(|token| &token.token)
    }

    fn token(&mut self) -> Result<Token> {
        if let Some(token) = self.lookahead.take() {
            return token;
        }
        self.lexer.token()
    }

    fn unexpected_token(&mut self, expected: &str) -> Result<Error> {
        let token = self.token()?;
        return Err(UnexpectedToken {
            expected: expected.to_string(),
            pos: token.start,
            unexpected: token.token,
        });
    }
}

        /*loop {
            match lexer.token() {
                Ok(token) => println!("{:?}", token),
                Err(Eof) => break,
                Err(error) => {
                    println!("{}", error);
                    break;
                },
            }
        }*/
