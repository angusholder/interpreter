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
        ident: String,
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
                            ident: ident,
                            kind: AssignKind::Let,
                        })
                    } else {
                        self.lexer.expect(Token::Assign)?;
                        let expr = self.parse_expr()?;
                        self.lexer.expect(Token::Semicolon)?;

                        result.push(Stmt::Assign {
                            ident: ident,
                            kind: AssignKind::LetAssign(expr),
                        });
                    }
                }

                Ok(Token::Ident(ident)) => {
                    if self.lexer.matches(Token::Assign) {
                        let expr = self.parse_expr()?;
                        self.lexer.expect(Token::Semicolon)?;

                        result.push(Stmt::Assign {
                            ident: ident,
                            kind: AssignKind::Reassign(expr),
                        })
                    } else {
                        let node = Box::new(Expr::Atom(Atom::Ident(ident)));
                        self.parse_expr_with_initial_node(node)?;
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

                Ok(Token::KPrint) => {
                    let expr = self.parse_expr()?;
                    self.lexer.expect(Token::Semicolon)?;

                    result.push(Stmt::Print(expr));
                }

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

    fn parse_expr_with_initial_node(&mut self, initial: Box<Expr>) -> CompileResult<Box<Expr>> {
        use lexer::Token::*;
        let mut root = initial;

        loop {
            let kind = match self.lexer.peek() {
                Ok(&Plus) => BinOpKind::Add,
                Ok(&Minus) => BinOpKind::Sub,
                Ok(&Star) => BinOpKind::Mul,
                Ok(&Percent) => BinOpKind::Rem,
                Ok(&Slash) => BinOpKind::Div,

                Ok(&Lt) => BinOpKind::Lt,
                Ok(&LtEq) => BinOpKind::LtEq,
                Ok(&Gt) => BinOpKind::Gt,
                Ok(&GtEq) => BinOpKind::GtEq,
                Ok(&Eq) => BinOpKind::Eq,
                Ok(&NotEq) => BinOpKind::NotEq,

                Ok(_) => return Ok(root),
                Err(CompileError::EndOfStream) => return Ok(root),
                Err(other) => {
                    return Err(other);
                }
            };

            self.lexer.next()?;

            let right = self.parse_atom()?;

            root = Box::new(Expr::BinOp {
                kind: kind,
                left: root,
                right: right,
            });
        }
    }

    pub fn parse_expr(&mut self) -> CompileResult<Box<Expr>> {
        let initial = self.parse_atom()?;
        self.parse_expr_with_initial_node(initial)
    }

    pub fn parse_atom(&mut self) -> CompileResult<Box<Expr>> {
        let atom = match self.lexer.next()? {
            Token::KTrue => Atom::Bool(true),
            Token::KFalse => Atom::Bool(false),
            Token::KNull => Atom::Null,
            Token::Number(n) => Atom::Number(n),
            Token::Ident(i) => Atom::Ident(i),
            Token::StrLit(s) => Atom::String(s),

            Token::Minus => {
                let atom = self.parse_atom()?;
                return Ok(Box::new(Expr::UnaryOp {
                    kind: UnaryOpKind::Neg,
                    child: atom
                }));
            }

            Token::LParen => {
                let expr = self.parse_expr()?;
                self.lexer.expect(Token::RParen)?;
                return Ok(expr);
            }

            _ => panic!(),//return Err(CompileError::Unimplemented),
        };

        Ok(Box::new(Expr::Atom(atom)))
    }

    pub fn parse(&mut self) {

    }
}
