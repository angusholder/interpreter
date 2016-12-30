use lexer::{ Lexer, Token };

#[derive(Debug, PartialEq)]
pub enum ParseError {
    Expected {
        expected: Token,
        got: Token
    },
    EndOfStream,
    Unimplemented,
}

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub enum UnaryOpKind {
    Neg,
}

#[derive(Debug)]
pub enum AssignKind {
    Let,
    Reassign,
}

#[derive(Debug)]
pub enum Atom {
    Ident(String),
    Number(f64),
    Bool(bool),
    Null,
    // String(String)
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
    IfStmt {
        cond: Box<Expr>,
        then: Block,
        els: Option<Block>,
    },
    Assign {
        kind: AssignKind,
        ident: String,
        expr: Option<Box<Expr>>,

    },
    Expr(Box<Expr>),
}

#[derive(Debug)]
pub struct Block {
    stmts: Box<[Stmt]>,
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

    pub fn parse_brace_block(&mut self) -> ParseResult<Block> {
        self.lexer.expect(Token::LBrace)?;
        let block = self.parse_block()?;
        self.lexer.expect(Token::RBrace)?;
        Ok(block)
    }

    pub fn parse_block(&mut self) -> ParseResult<Block> {
        let mut result = Vec::<Stmt>::new();
        loop {
            match self.lexer.next() {
                Ok(Token::KLet) => {
                    let ident = self.lexer.expect_ident()?;

                    if self.lexer.matches(Token::Semicolon) {
                        result.push(Stmt::Assign {
                            ident: ident,
                            expr: None,
                            kind: AssignKind::Let,
                        })
                    } else {
                        self.lexer.expect(Token::Assign)?;
                        let expr = self.parse_expr()?;
                        self.lexer.expect(Token::Semicolon)?;

                        result.push(Stmt::Assign {
                            ident: ident,
                            expr: Some(expr),
                            kind: AssignKind::Let,
                        });
                    }
                }

                Ok(Token::Ident(ident)) => {
                    if self.lexer.matches(Token::Assign) {
                        let expr = self.parse_expr()?;
                        self.lexer.expect(Token::Semicolon)?;

                        result.push(Stmt::Assign {
                            ident: ident,
                            expr: Some(expr),
                            kind: AssignKind::Reassign,
                        })
                    } else {
                        let node = Box::new(Expr::Atom(Atom::Ident(ident)));
                        self.parse_expr_with_initial_node(node)?;
                    }
                }

                Ok(Token::KIf) => {
                    let cond = self.parse_expr()?;
                    let then = self.parse_brace_block()?;
                    let els = if self.lexer.matches(Token::KElse) {
                        Some(self.parse_brace_block()?)
                    } else {
                        None
                    };

                    result.push(Stmt::IfStmt {
                        cond: cond,
                        then: then,
                        els: els
                    });
                }

                Ok(other) => {
                    return Err(ParseError::Expected {
                        got: other,
                        expected: Token::KIf,
                    });
                }

                Err(ParseError::EndOfStream) => {
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

    fn parse_expr_with_initial_node(&mut self, initial: Box<Expr>) -> ParseResult<Box<Expr>> {
        use lexer::Token::*;
        let mut root = initial;

        loop {
            let kind = match self.lexer.peek() {
                Ok(&Plus) => BinOpKind::Add,
                Ok(&Minus) => BinOpKind::Sub,
                Ok(&Star) => BinOpKind::Mul,
                Ok(&Slash) => BinOpKind::Div,
                Ok(_) => return Ok(root),
                Err(ParseError::EndOfStream) => return Ok(root),
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

    pub fn parse_expr(&mut self) -> ParseResult<Box<Expr>> {
        let initial = self.parse_atom()?;
        self.parse_expr_with_initial_node(initial)
    }

    pub fn parse_atom(&mut self) -> ParseResult<Box<Expr>> {
        let atom = match self.lexer.next()? {
            Token::KTrue => Atom::Bool(true),
            Token::KFalse => Atom::Bool(false),
            Token::KNull => Atom::Null,
            Token::Number(n) => Atom::Number(n),
            Token::Ident(i) => Atom::Ident(i),

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

            _ => return Err(ParseError::Unimplemented),
        };

        Ok(Box::new(Expr::Atom(atom)))
    }

    pub fn parse(&mut self) {

    }
}