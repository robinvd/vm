#![recursion_limit = "1000"]

#[macro_use]
extern crate criterion;

use criterion::Criterion;

use sabi::vm::VM;

const ENTRY: &'static str = r#"
    .data
    0
    .code
    const 0
    call main
    halt
"#;

pub fn add_main(vm: &mut VM) {
    vm.parse_ir_block("start", ENTRY)
        .expect("failed to add main");
}

fn load_file(file: impl AsRef<std::path::Path>) -> String {
    use std::io::Read;

    let mut input = String::new();

    std::fs::File::open(file)
        .unwrap()
        .read_to_string(&mut input)
        .unwrap();

    input
}

fn load_vm<'a>(input: &'a str, buffer: &'a mut Vec<u8>) -> VM<'a> {
    use combine::Parser;
    use sabi::compiler;
    use sabi::parser;

    let mut vm = VM::new(Box::new(buffer));
    vm.register_basic();

    let parsed = parser::code::parse_file()
        .easy_parse(input)
        .expect("failed to parse");

    // add_main(&mut vm).expect("failed to add main");
    compiler::compile(&mut vm, &parsed.0).expect("failed to compile");

    vm
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("fib 20", |b| {
        let file = load_file("tests/programs/fib.sabi");
        let mut buffer = Vec::new();
        let mut vm = load_vm(&file, &mut buffer);
        add_main(&mut vm);

        b.iter(|| vm.new_fiber("start").unwrap().run())
    });
    c.bench_function("sqrt 1048576", |b| {
        let file = load_file("tests/programs/sqrt.sabi");
        let mut buffer = Vec::new();
        let mut vm = load_vm(&file, &mut buffer);
        add_main(&mut vm);

        b.iter(|| vm.new_fiber("start").unwrap().run())
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
