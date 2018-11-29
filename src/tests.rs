//! integration tests,
//! tests are found in tests/

use crate::{
    compiler, parser,
    vm::{value::Value, VMError, VM},
};

use combine::Parser;
use std::io::Read;

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

        compiler::compile(&mut vm, &parsed.0).expect("failed to compile");
        vm.add_start();

        vm.new_fiber("start").and_then(|mut f| f.run())
    };
    (buffer, res)
}

#[test]
fn test_while() {
    let (out, res) = test_file("tests/basic/while.sabi");
    assert_eq!(res, Ok(Value::nil()));
    assert_eq!(out, b"1\n2\n3\n4\n");
}

#[test]
fn test_if() {
    let (out, res) = test_file("tests/basic/if.sabi");
    assert_eq!(res, Ok(Value::nil()));
    assert_eq!(out, b"6\n");
}

#[test]
fn test_sqrt() {
    let (out, res) = test_file("tests/programs/sqrt.sabi");
    assert_eq!(res, Ok(Value::nil()));
    assert_eq!(out, b"1024\n");
}

#[test]
fn test_fib() {
    let (out, res) = test_file("tests/programs/fib.sabi");
    assert_eq!(res, Ok(Value::nil()));
    assert_eq!(out, b"10946\n");
}

#[test]
fn test_function() {
    let (out, res) = test_file("tests/basic/function.sabi");
    assert_eq!(res, Ok(Value::nil()));
    assert_eq!(out, b"1\n3\n1\n");
}

#[test]
fn test_literals() {
    let (out, res) = test_file("tests/basic/literals.sabi");
    assert_eq!(res, Ok(Value::nil()));
    assert_eq!(out, b"[1, 2]\n{1 = 2}\n");
}

#[test]
fn test_index() {
    let (out, res) = test_file("tests/basic/index.sabi");
    assert_eq!(res, Ok(Value::nil()));
    assert_eq!(out, b"1\n3\n2\n4\n");
}

#[test]
fn test_letorder() {
    let (out, res) = test_file("tests/basic/letorder.sabi");
    assert_eq!(res, Ok(Value::nil()));
    assert_eq!(out, b"2\n");
}

#[test]
fn test_access() {
    let (out, res) = test_file("tests/basic/access.sabi");
    assert_eq!(res, Ok(Value::nil()));
    assert_eq!(out, b"2\n5\n");
}
