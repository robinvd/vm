#![recursion_limit = "1000"]

use std::fs;
use std::io::Read;

use structopt::StructOpt;
use codespan::CodeMap;

use sabi::compiler;
use sabi::parser;

#[derive(Debug, StructOpt)]
#[structopt(name = "SABI VM", about = "A stack based VM in rust")]
struct Opt {
    // TODO use pathbuf
    #[structopt(short = "c", long = "config")]
    config: Option<String>,

    #[structopt()]
    input: Option<String>,

    #[structopt(short = "d", long = "debug")]
    debug: bool,

    #[structopt(short = "b", long = "bytecode")]
    bytecode: bool,

    #[structopt(short = "v", long = "verbose")]
    verbose: bool,
}

fn main() {
    let opt = Opt::from_args();

    let mut vm = vm::VM::default();
    vm.register_basic();
    vm.register_io();
    vm.register_internal();
    vm.debug = opt.debug;

    let mut codemap = CodeMap::new();

    let filemap = codemap.add_filemap_from_disk(opt.input.as_ref().map(|x|->&str {&*x}).unwrap_or("/dev/stdin")).expect("err loading file");

    let input = filemap.src();

    if opt.bytecode {
        panic!("not supported atm");
    }

    let parsed = parser::grammar::ModuleParser::new().parse(&input);

    if let Err(e) = &parsed {
        let (lloc, rloc, text) = match e {
            lalrpop_util::ParseError::InvalidToken{location} => {
                (*location, *location + 1, "".to_owned())
            }
            lalrpop_util::ParseError::UnrecognizedToken{token, expected} => {
                let t = token.clone().unwrap();
                (t.0, t.2, format!("expected one of: {}", expected.join(", ")))
            }

            _ => panic!(""),
        };

        use codespan::{Span, ByteIndex};

        let mut msg = codespan_reporting::Diagnostic::new_error("parse error");
        let span = Span::new(ByteIndex::from((lloc + 1) as u32) ,ByteIndex::from((rloc + 1) as u32));
        let label = codespan_reporting::Label::new_primary(span).with_message(text);
        msg.labels.push(label);

        codespan_reporting::emit(termcolor::Ansi::new(std::io::stdout()), &codemap, &msg).unwrap();

        panic!("err")
    }

    let parsed = parsed.unwrap();
    compiler::compile(&mut vm, parsed.as_slice()).expect("failed to compile");

    vm.add_start();

    if opt.verbose {
        println!("start state:\n{:?}", vm);
    }

    let mut f = vm.new_fiber("start").expect("failed to make fiber");
    let result = if vm.debug { f.debug_run() } else { f.run() };

    if opt.verbose {
        println!("result: {:?}", result);
    }
}
