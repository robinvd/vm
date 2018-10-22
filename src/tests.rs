//! integration tests,
//! tests are found in tests/

use crate::compiler;
use crate::parser;
use crate::vm::{VM, VMError, value::Value};

use combine::Parser;
use std::io::Read;

fn add_main(vm: &mut VM) -> Result<(), VMError> {
    vm.parse_ir_block("start", "push 0\ncall main\nhalt")?;
    Ok(())
}

fn test_file(file: impl AsRef<std::path::Path>) -> (Vec<u8>, Result<Value, VMError>) {
    let mut buffer = Vec::new();
    let res = {
        let mut vm = VM::new(Box::new(&mut buffer));
        let mut input = String::new();

        std::fs::File::open(file)
            .unwrap()
            .read_to_string(&mut input)
            .unwrap();

        let i: &str = &input;
        let parsed = parser::code::parse_file()
            .easy_parse(i)
            .expect("failed to parse");

        add_main(&mut vm).expect("failed to add main");
        compiler::compile(&mut vm, &parsed.0).expect("failed to compile");

        vm.new_fiber("start").and_then(|mut f| f.run())
    };
    (buffer, res)
}

#[test]
fn test_while() {
    let (out, res) = test_file("tests/sabi/while.sabi");
    assert_eq!(res, Ok(Value::Number(0.)));
    assert_eq!(out, b"1\n2\n3\n");
}

#[test]
fn test_if() {
    let (out, res) = test_file("tests/sabi/if.sabi");
    assert_eq!(res, Ok(Value::Number(0.)));
    assert_eq!(out, b"3\n");
}

#[test]
fn test_sqrt() {
    let (out, res) = test_file("tests/sabi/sqrt.sabi");
    assert_eq!(res, Ok(Value::Number(0.)));
    assert_eq!(out, b"1024\n");
}