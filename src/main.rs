extern crate regex;
#[macro_use]
extern crate combine;

mod bc_parser;
mod vm;

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
        ).arg(
            Arg::with_name("debug")
                .short("d")
                .long("debug")
                .help("debug mode, print VM state and wait for input on every loop")
                .requires("input"),
        ).arg(
            Arg::with_name("verbose")
                .short("v")
                .long("verbose")
                .multiple(true)
                .help("Sets the level of verbosity"),
        ).get_matches();

    let mut vm = vm::VM::default();
    if matches.is_present("debug") {
        vm.debug = true;
    }

    let mut input = String::new();

    let input_file = matches.value_of("input").unwrap_or("/dev/stdin");
    fs::File::open(input_file)
        .unwrap()
        .read_to_string(&mut input)
        .unwrap();
    vm.parse_ir(&input).unwrap();

    if matches.is_present("verbose") {
        println!("start state:\n{:?}", vm);
    }

    let result = vm.run();

    // if matches.is_present("verbose") {
    println!("result: {:?}", result);
    // }
}
