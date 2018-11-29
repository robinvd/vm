#![recursion_limit = "1000"]

#[macro_use]
extern crate criterion;

use criterion::Criterion;

use sabi::vm::VM;

fn load_file(file: impl AsRef<std::path::Path>) -> String {
    use std::io::Read;

    let mut input = String::new();

    std::fs::File::open(file)
        .unwrap()
        .read_to_string(&mut input)
        .unwrap();

    input
}

fn load_vm<'a>(input: &'a str, buffer: Box<'a + std::io::Write + Sync>) -> VM<'a> {
    use combine::Parser;
    use sabi::{compiler, parser};

    let mut vm = VM::new(buffer);
    vm.jit = false;
    vm.register_basic();

    let parsed = parser::code::parse_file()
        .easy_parse(input)
        .expect("failed to parse");

    // add_main(&mut vm).expect("failed to add main");
    compiler::compile(&mut vm, &parsed.0).expect("failed to compile");
    vm.add_start();

    vm
}

fn fib_benchmark(c: &mut Criterion) {
    c.bench_function("fib 20", |b| {
        let file = load_file("tests/programs/fib.sabi");
        let mut buffer = Vec::new();
        let vm = load_vm(&file, Box::new(&mut buffer));

        b.iter(|| vm.new_fiber("start").unwrap().run())
    });
}

fn sqrt_benchmark(c: &mut Criterion) {
    c.bench_function("sqrt 1048576", |b| {
        let file = load_file("tests/programs/sqrt.sabi");
        let mut buffer = std::io::sink();
        let vm = load_vm(&file, Box::new(&mut buffer));

        b.iter(|| vm.new_fiber("start").unwrap().run())
    });
}

fn trace_jit(input: &str) {
    let mut buffer = std::io::sink();
    let mut vm = load_vm(input, Box::new(&mut buffer));
    vm.jit = true;

    vm.new_fiber("start").unwrap().run().unwrap();
}

fn trace_no_jit(input: &str) {
    let mut buffer = std::io::sink();
    let mut vm = load_vm(input, Box::new(&mut buffer));
    vm.jit = false;

    vm.new_fiber("start").unwrap().run().unwrap();
}

fn trace_jit_compare(c: &mut Criterion) {
    let file = load_file("tests/programs/trace.sabi");

    let jit = criterion::Fun::new("jit", |b, input: &String| b.iter(|| trace_jit(&*input)));
    let no_jit = criterion::Fun::new("no jit", |b, input: &String| b.iter(|| trace_no_jit(&*input)));

    let functions = vec![jit, no_jit];

    c.bench_functions("trace", functions, file);
}

criterion_group!(benches, sqrt_benchmark, fib_benchmark);
criterion_group!(jit, trace_jit_compare);
criterion_main!(benches, jit);
