#![allow(dead_code)]

mod lexer;
mod parser;

use lexer::Lexer;
use parser::Parser;

fn main() {
    let prog = "
        let a = 1;
        let b = 2;
        let answer = a + -(b * E);
    ";

    let mut parser = Parser::new(Lexer::new(prog));
    let block = parser.parse_block();
    println!("{:?}", block);
}
