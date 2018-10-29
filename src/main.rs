#![recursion_limit = "1000"] 

extern crate regex;
#[macro_use]
extern crate combine;

mod compiler;
mod parser;
mod vm;

#[cfg(test)]
mod tests;

use std::fs;
use std::io::{self, Read};

use clap::{App, Arg};

fn main() {
    let matches = App::new("RL VM")
        .version("1.0")
        .author("Robin <robinjint@gmail.com>")
        .about("A stack based VM in rust")
        .arg(Arg::with_name("input").help("input file").index(1))
        .arg(
            Arg::with_name("config")
                .short("c")
                .long("config")
                .value_name("FILE")
                .help("Sets a custom config file")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("debug")
                .short("d")
                .long("debug")
                .help("debug mode, print VM state and wait for input on every loop")
                .requires("input"),
        )
        .arg(
            Arg::with_name("bytecode")
                .short("b")
                .long("bytecode")
                .help("compile bytecode instead of normal code"),
        )
        .arg(
            Arg::with_name("verbose")
                .short("v")
                .long("verbose")
                .multiple(true)
                .help("Sets the level of verbosity"),
        )
        .get_matches();

    let mut vm = vm::VM::default();
    vm.register_basic();
    vm.register_io();
    if matches.is_present("debug") {
        vm.debug = true;
    }

    let mut input = String::new();

    let input_file = matches.value_of("input").unwrap_or("/dev/stdin");
    fs::File::open(input_file)
        .unwrap()
        .read_to_string(&mut input)
        .unwrap();

    if matches.is_present("bytecode") {
        vm.parse_ir_block("main", &input).unwrap();
    } else {
        use combine::Parser;
        let i: &str = &input;
        let parsed = parser::code::parse_file()
            .easy_parse(i)
            .expect("failed to parse");
        add_main(&mut vm).expect("failed to add main");
        compiler::compile(&mut vm, &parsed.0).expect("failed to compile");
    }

    if matches.is_present("verbose") {
        println!("start state:\n{:?}", vm);
    }

    let mut f = vm.new_fiber("start").expect("failed to make fiber");
    let result = f.run();

    if matches.is_present("verbose") {
        println!("result: {:?}", result);
    }
}

fn add_main(vm: &mut vm::VM) -> Result<(), vm::VMError> {
    vm.parse_ir_block("start", ".data\n0\n.code\nconst 0\ncall main\nhalt")?;
    Ok(())
}
