use std::iter::Peekable;
use std::str::CharIndices;

use compiler::{ CompileResult, CompileError };

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    KIf,
    KElse,
    KLet,
    KTrue,
    KFalse,
    KNull,

    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,

    Plus,
    Minus,
    Star,
    Slash,

    Assign,
    Semicolon,
    Comma,

    Ident(String),
    Number(f64),
}

pub struct Lexer<'a> {
    iter: Peekable<CharIndices<'a>>,
    src: &'a str,
    pub peeked: Option<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Lexer<'a> {
        Lexer {
            iter: src.char_indices().peekable(),
            src: src,
            peeked: None,
        }
    }

    pub fn peek(&mut self) -> CompileResult<&Token> {
        if self.peeked.is_none() {
            let next = self.next()?;
            self.peeked = Some(next);
        }

        Ok(self.peeked.as_ref().unwrap())
    }

    pub fn next(&mut self) -> CompileResult<Token> {
        if let Some(tok) = self.peeked.take() {
            return Ok(tok);
        }

        while let Some(&(_, c)) = self.iter.peek() {
            if c.is_whitespace() {
                self.iter.next();
            } else {
                break;
            }
        }

        let (next_pos, next_char) = self.iter.next().ok_or(CompileError::EndOfStream)?;

        use self::Token::*;
        let token = match next_char {
            '[' => LBracket,
            ']' => RBracket,
            '{' => LBrace,
            '}' => RBrace,
            '(' => LParen,
            ')' => RParen,

            '+' => Plus,
            '-' => Minus,
            '*' => Star,
            '/' => Slash,

            '=' => Assign,
            ';' => Semicolon,
            ',' => Comma,

            _ if next_char.is_numeric() => {
                let start = next_pos;
                let mut end = start + 1; // TODO: Check this

                while let Some(&(pos, ch)) = self.iter.peek() {
                    if ch.is_numeric() {
                        self.iter.next();
                    } else {
                        end = pos;
                        break;
                    }
                }

                let num = self.src[start..end].parse::<f64>().unwrap();

                Number(num)
            }

            _ if next_char.is_alphabetic() || next_char == '_' => {
                let start = next_pos;
                let mut end = start + 1; // TODO: Check this

                while let Some(&(pos, ch)) = self.iter.peek() {
                    if ch.is_alphanumeric() || ch == '_' {
                        self.iter.next();
                    } else {
                        end = pos;
                        break;
                    }
                }

                match &self.src[start..end] {
                    "if" => KIf,
                    "else" => KElse,
                    "let" => KLet,
                    "true" => KTrue,
                    "false" => KFalse,
                    "null" => KNull,
                    ident => Ident(ident.to_string()),
                }
            }

            _ => panic!("Unhandled character {:?}", next_char),
        };

        Ok(token)
    }

    // If this fails it is a hard parse error, which is why we don't care about
    // consuming the next token with .next()
    pub fn expect(&mut self, token: Token) -> CompileResult<()> {
        if self.peek() == Ok(&token) {
            self.next().unwrap(); // The if statement should make this impossible
            Ok(())
        } else {
            Err(CompileError::Expected {
                expected: token,
                got: self.next()?
            })
        }
    }

    pub fn expect_ident(&mut self) -> CompileResult<String> {
        match self.next()? {
            Token::Ident(ident) => Ok(ident),
            got => Err(CompileError::Expected {
                expected: Token::Ident("_".to_string()),
                got: got
            })
        }
    }

    pub fn matches(&mut self, token: Token) -> bool {
        if self.peek() == Ok(&token) {
            self.next().unwrap(); // The if statement should make this impossible
            true
        } else {
            false
        }
    }
}

