use lexer::{ Lexer, Token };
use compiler::{ CompileResult, CompileError };

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Rem,
    Div,
    Lt,
    LtEq,
    Gt,
    GtEq,
    Eq,
    NotEq,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOpKind {
    Neg,
}

#[derive(Debug)]
pub enum AssignKind {
    Let,
    LetAssign(Box<Expr>),
    Reassign(Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Atom {
    Ident(String),
    Number(f64),
    Bool(bool),
    Null,
    String(String)
}

#[derive(Debug)]
pub enum Expr {
    BinOp {
        kind: BinOpKind,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    UnaryOp {
        kind: UnaryOpKind,
        child: Box<Expr>,
    },
    Index {
        left: Box<Expr>,
        index: Box<Expr>,
    },
    Attr {
        left: Box<Expr>,
        ident: String,
    },
    FuncCall {
        left: Box<Expr>,
        args: Box<[Box<Expr>]>,
    },
    Atom(Atom),
}

#[derive(Debug)]
pub enum Stmt {
    If {
        cond: Box<Expr>,
        then: Block,
        els: Option<Box<Stmt>>, // Should be either `Block` or `If`
    },
    While {
        cond: Box<Expr>,
        body: Block,
    },
    Assign {
        kind: AssignKind,
        left: Box<Expr>,
    },
    Function {
        name: String,
        args: Box<[String]>,
        body: Block,
    },
    Block(Block),
    Expr(Box<Expr>),
    Print(Box<Expr>),
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Box<[Stmt]>,
}

pub struct Parser<'a> {
    pub lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
        Parser {
            lexer: lexer,
        }
    }

    pub fn parse_brace_block(&mut self) -> CompileResult<Block> {
        self.lexer.expect(Token::LBrace)?;
        let block = self.parse_block()?;
        self.lexer.expect(Token::RBrace)?;
        Ok(block)
    }

    fn parse_if_stmt(&mut self) -> CompileResult<Stmt> {
        let cond = self.parse_expr()?;
        let then = self.parse_brace_block()?;
        let els = if self.lexer.matches(Token::KElse) {
            if self.lexer.matches(Token::KIf) {
                Some(Box::new(self.parse_if_stmt()?))
            } else {
                Some(Box::new(Stmt::Block(self.parse_brace_block()?)))
            }
        } else {
            None
        };

        Ok(Stmt::If {
            cond: cond,
            then: then,
            els: els
        })
    }

    pub fn parse_block(&mut self) -> CompileResult<Block> {
        let mut result = Vec::<Stmt>::new();
        loop {
            if self.lexer.peek() == Ok(&Token::RBrace) { break; }
            match self.lexer.next() {
                Ok(Token::KLet) => {
                    let ident = self.lexer.expect_ident()?;

                    if self.lexer.matches(Token::Semicolon) {
                        result.push(Stmt::Assign {
                            left: Box::new(Expr::Atom(Atom::Ident(ident))),
                            kind: AssignKind::Let,
                        })
                    } else {
                        self.lexer.expect(Token::Assign)?;
                        let expr = self.parse_expr()?;
                        self.lexer.expect(Token::Semicolon)?;

                        result.push(Stmt::Assign {
                            left: Box::new(Expr::Atom(Atom::Ident(ident))),
                            kind: AssignKind::LetAssign(expr),
                        });
                    }
                }

                Ok(Token::Ident(ident)) => {
                    self.lexer.peeked = Some(Token::Ident(ident));
                    let left = self.parse_expr_until(0)?;
                    if self.lexer.matches(Token::Assign) {
                        let expr = self.parse_expr()?;
                        self.lexer.expect(Token::Semicolon)?;

                        result.push(Stmt::Assign {
                            left: left,
                            kind: AssignKind::Reassign(expr),
                        })
                    } else {
                        self.lexer.expect(Token::Semicolon)?;
                        result.push(Stmt::Expr(left));
                    }
                }

                Ok(Token::KIf) => {
                    result.push(self.parse_if_stmt()?);
                }

                Ok(Token::KWhile) => {
                    let cond = self.parse_expr()?;
                    let body = self.parse_brace_block()?;

                    result.push(Stmt::While {
                        cond: cond,
                        body: body,
                    });
                }

                Ok(Token::KFn) => {
                    let name = self.lexer.expect_ident()?;
                    self.lexer.expect(Token::LParen)?;
                    let mut args = Vec::<String>::new();

                    if !self.lexer.matches(Token::RParen) {
                        if let &Token::Ident(_) = self.lexer.peek()? {
                            args.push(self.lexer.expect_ident()?);
                            loop {
                                if self.lexer.matches(Token::RParen) {
                                    break;
                                } else {
                                    self.lexer.expect(Token::Comma)?;
                                    args.push(self.lexer.expect_ident()?);
                                }
                            }
                        }
                    }

                    let body = self.parse_brace_block()?;

                    result.push(Stmt::Function {
                        name: name,
                        args: args.into_boxed_slice(),
                        body: body,
                    });
                }

                Ok(Token::KPrint) => {
                    let expr = self.parse_expr()?;
                    self.lexer.expect(Token::Semicolon)?;

                    result.push(Stmt::Print(expr));
                }

                // Ok(Token::KReturn)

                Ok(other) => {
                    panic!("Wtf is {:?}", other);
                    // self.lexer.peeked = Some(other);
                    // let expr = self.parse_expr()?;
                    // self.lexer.expect(Token::Semicolon)?;

                    // result.push(Stmt::Expr(expr));
                }

                Err(CompileError::EndOfStream) => {
                    break;
                }

                Err(other) => {
                    return Err(other);
                }
            }
        }

        Ok(Block {
            stmts: result.into_boxed_slice()
        })
    }

    pub fn parse_expr_until(&mut self, min_rbp: i32) -> CompileResult<Box<Expr>> {
        ::pratt::parse_expr_until(self, min_rbp)
    }

    pub fn parse_expr(&mut self) -> CompileResult<Box<Expr>> {
        self.parse_expr_until(0)
    }

    pub fn parse(&mut self) {

    }
}
