#![allow(dead_code)]

#[macro_use]
extern crate clap;

mod lexer;
mod parser;
mod value;
mod compiler;
mod virtual_machine;
mod pratt;

use lexer::Lexer;
use parser::Parser;
use compiler::CompileError;
use virtual_machine::VirtualMachine;

use clap::{ Arg, App };

use std::fs::File;
use std::io::{ Read, stdin };

fn main() {
    let matches = App::new("Interpreter")
        .version("0.1")
        .author("Angus Holder")
        .about("Interpreter for a WIP scripting language")
        .arg(Arg::with_name("mode")
             .short("m")
             .long("mode")
             .possible_values(&["tokens", "ast", "bytecode", "execute"])
             .default_value("execute"))
        .arg(Arg::with_name("INPUT_SOURCE")
             .help("Set the input source file to use. Defaults to stdin")
             .index(1))
        .get_matches();

    let mut program = String::new();
    if let Some(src) = matches.value_of("INPUT_SOURCE") {
        let mut file = File::open(src).expect("Invalid file path given");
        file.read_to_string(&mut program).unwrap();
    } else {
        stdin().read_to_string(&mut program).unwrap();
    }

    // Clap ensures this has a value
    match matches.value_of("mode").unwrap() {
        "tokens" => {
            let mut lexer = Lexer::new(&program);
            loop {
                let next = lexer.next();
                match next {
                    Ok(t) => println!("{:?}", t),
                    Err(CompileError::EndOfStream) => break,
                    Err(e) => println!("{:?}", e),
                }
            }
        }
        "ast" => {
            let mut parser = Parser::new(Lexer::new(&program));
            let block = parser.parse_block().unwrap();
            println!("{:?}", block);
        }
        "bytecode" => {
            let mut parser = Parser::new(Lexer::new(&program));
            let block = parser.parse_block().unwrap();

            let compiled = compiler::compile(&block, &mut VirtualMachine::new()).unwrap();

            println!("locals: {:?}", compiled.local_names);
            println!("consts: {:?}", compiled.consts);
            println!("code:");
            for op in compiled.code.iter() {
                println!("  {:?}", op);
            }
        }
        "execute" => {
            let mut parser = Parser::new(Lexer::new(&program));
            let block = parser.parse_block().unwrap();

            let mut vm = VirtualMachine::new();
            let compiled = compiler::compile(&block, &mut vm).unwrap();

            vm.execute(&compiled);
        }
        _ => unreachable!(), // Clap handles other cases
    }
}
