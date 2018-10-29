//! integration tests,
//! tests are found in tests/

use crate::compiler;
use crate::parser;
use crate::vm::{value::Value, VMError, VM};

use combine::Parser;
use std::io::Read;

const ENTRY: &'static str = r#"
    .data
    0
    .code
    call main
    const 0
    halt
"#;

pub fn add_main(vm: &mut VM) -> Result<(), VMError> {
    vm.parse_ir_block("start", ENTRY)?;
    Ok(())
}

pub fn test_file(file: impl AsRef<std::path::Path>) -> (Vec<u8>, Result<Value, VMError>) {
    let mut buffer = Vec::new();
    let res = {
        let mut vm = VM::new(Box::new(&mut buffer));
        vm.register_basic();
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
    assert_eq!(res, Ok(Value::number(0.)));
    assert_eq!(out, b"1\n2\n3\n");
}

#[test]
fn test_if() {
    let (out, res) = test_file("tests/sabi/if.sabi");
    assert_eq!(res, Ok(Value::number(0.)));
    assert_eq!(out, b"3\n");
}

#[test]
fn test_sqrt() {
    let (out, res) = test_file("tests/sabi/sqrt.sabi");
    assert_eq!(res, Ok(Value::number(0.)));
    assert_eq!(out, b"1024\n");
}

#[test]
fn test_function() {
    let (out, res) = test_file("tests/sabi/function.sabi");
    assert_eq!(res, Ok(Value::number(0.)));
    assert_eq!(out, b"1\n");
}

#[test]
fn test_literals() {
    let (out, res) = test_file("tests/sabi/literals.sabi");
    assert_eq!(res, Ok(Value::number(0.)));
    assert_eq!(out, b"[1, 2]\n{1 = 2}\n");
}

#[test]
fn test_index() {
    let (out, res) = test_file("tests/sabi/index.sabi");
    assert_eq!(res, Ok(Value::number(0.)));
    assert_eq!(out, b"1\n3\n2\n4\n");
}
