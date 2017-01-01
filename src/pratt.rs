use lexer::{ Token };
use parser::{ Expr, Parser, Atom, BinOpKind, UnaryOpKind };
use compiler::{ CompileResult, CompileError };

type NullDenotation = fn(p: &mut Parser, token: Token, bp: i32) -> CompileResult<Box<Expr>>;
type LeftDenotation = fn(p: &mut Parser, token: Token, left: Box<Expr>, rbp: i32) -> CompileResult<Box<Expr>>;

//
// Null Denotation -- token that takes nothing on the left
//

fn null_constant(_p: &mut Parser, token: Token, _bp: i32) -> CompileResult<Box<Expr>> {
    let atom = match token {
        Token::KTrue => Atom::Bool(true),
        Token::KFalse => Atom::Bool(false),
        Token::KNull => Atom::Null,
        Token::Number(n) => Atom::Number(n),
        Token::Ident(i) => Atom::Ident(i),
        Token::StrLit(s) => Atom::String(s),
        _ => unreachable!(),
    };

    Ok(Box::new(Expr::Atom(atom)))
}

fn null_paren(p: &mut Parser, _token: Token, bp: i32) -> CompileResult<Box<Expr>> {
    let result = p.parse_expr_until(bp)?;
    p.lexer.expect(Token::RParen)?;
    Ok(result)
}

// Prefix operator.
//
// Low precedence:  return, raise, etc.
//   return x+y is return (x+y), not (return x) + y
//
// High precedence: logical negation, bitwise complement, etc.
//   !x && y is (!x) && y, not !(x && y)
fn null_prefix_op(p: &mut Parser, token: Token, bp: i32) -> CompileResult<Box<Expr>> {
    let result = p.parse_expr_until(bp)?;
    let kind = match token {
        Token::Minus => UnaryOpKind::Neg,
        _ => unreachable!(),
    };

    Ok(Box::new(Expr::UnaryOp {
        kind: kind,
        child: result,
    }))
}

//
// Left Denotations -- token that takes an expression on the left
//

fn left_index(p: &mut Parser, _token: Token, left: Box<Expr>, _rbp: i32) -> CompileResult<Box<Expr>> {
    // TODO: Ensure left is indexable (ie: only Ident/Index/Attr)
    let index = p.parse_expr_until(0)?;
    p.lexer.expect(Token::RBracket)?;
    Ok(Box::new(Expr::Index{
        left: left,
        index: index
    }))
}

fn left_attr(p: &mut Parser, _token: Token, left: Box<Expr>, _rbp: i32) -> CompileResult<Box<Expr>> {
    // TODO: Ensure left is indexable (ie: only Ident/Index/Attr)
    let ident = p.lexer.expect_ident()?;
    Ok(Box::new(Expr::Attr {
        left: left,
        ident: ident
    }))
}

fn left_binary_op(p: &mut Parser, token: Token, left: Box<Expr>, rbp: i32) -> CompileResult<Box<Expr>> {
    let kind = match token {
        Token::Plus => BinOpKind::Add,
        Token::Minus => BinOpKind::Sub,
        Token::Star => BinOpKind::Mul,
        Token::Percent => BinOpKind::Rem,
        Token::Slash => BinOpKind::Div,

        Token::Lt => BinOpKind::Lt,
        Token::LtEq => BinOpKind::LtEq,
        Token::Gt => BinOpKind::Gt,
        Token::GtEq => BinOpKind::GtEq,
        Token::Eq => BinOpKind::Eq,
        Token::NotEq => BinOpKind::NotEq,
        _ => panic!("Unexpected token {:?}", token),
    };
    Ok(Box::new(Expr::BinOp {
        kind: kind,//token.into_binary()?,
        left: left,
        right: p.parse_expr_until(rbp)?,
    }))
}

const COMMA_PREC: i32 = 1;
fn left_func_call(p: &mut Parser, _token: Token, left: Box<Expr>, _rbp: i32) -> CompileResult<Box<Expr>> {
    // TODO: Ensure left is callable (eg: only Index/Ident/Attr/...?)
    let mut args = Vec::new();
    if p.lexer.peek() != Ok(&Token::RParen) {
        loop {
            args.push(p.parse_expr_until(COMMA_PREC)?);
            if p.lexer.matches(Token::RParen) {
                break;
            } else {
                p.lexer.expect(Token::Comma)?;
            }
        }
    }

    Ok(Box::new(Expr::FuncCall {
        left: left,
        args: args.into_boxed_slice(),
    }))
}

pub fn parse_expr_until(p: &mut Parser, min_rbp: i32) -> CompileResult<Box<Expr>> {
    use lexer::Token::*;
    let token = p.lexer.next()?;

    let (nud, bp): (NullDenotation, i32) = match token {
        Ident(_) | Number(_) | StrLit(_) | KTrue | KFalse | KNull => {
            (null_constant, -1)
        }
        LParen => {
            (null_paren, 0)
        }
        Minus => {
            (null_prefix_op, 29)
        }
        _ => {
            return Err(CompileError::UnexpectedNullToken(format!("{:?}", token)));
        }
    };

    let mut node = nud(p, token, bp)?;

    loop {
        let (led, lbp, rbp): (LeftDenotation, i32, i32) = match p.lexer.peek()? {
            &LParen => (left_func_call, 31, 31),
            &LBracket => (left_index, 31, 31),
            &Dot => (left_attr, 31, 31),

            &Star | &Slash | &Percent => (left_binary_op, 25, 25),
            &Plus | &Minus => (left_binary_op, 23, 23),
            &Eq | &NotEq | &Lt | &LtEq | &Gt | &GtEq => (left_binary_op, 21, 21),

            &KAnd => (left_binary_op, 19, 19),
            &KOr => (left_binary_op, 17, 17),

            _ => {
                // TODO: Work out which tokens are acceptable to terminate an
                // expression, and which (if any) should be errors here.
                // eg: `return Err(CompileError::UnexpectedLeftToken(format!("{:?}", token)));`
                return Ok(node);
            }
        };


        if min_rbp >= lbp {
            break;
        }

        let token = p.lexer.next()?; // .peek()? above should handle errors

        node = led(p, token, node, rbp)?;
    }

    Ok(node)
}
