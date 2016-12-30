#![allow(dead_code)]

mod lexer;
mod parser;
mod value;
mod compiler;
mod virtual_machine;

use lexer::Lexer;
use parser::Parser;
use virtual_machine::VirtualMachine;

fn main() {
    let prog = "
        let a = 1;
        let b = 2;
        let answer = a + -(2*b);
        2*b;
    ";

    let mut parser = Parser::new(Lexer::new(prog));
    let block = parser.parse_block().unwrap();
    println!("{:?}", block);

    let compiled = compiler::compile(&block, &mut VirtualMachine{}).unwrap();

    println!("locals: {:?}", compiled.local_names);
    println!("consts: {:?}", compiled.consts);
    println!("code:");
    for op in compiled.code.iter() {
        println!("  {:?}", op);
    }
}
