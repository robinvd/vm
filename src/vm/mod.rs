use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::{fmt, io, mem};

use crate::parser::bytecode::{self, Arg, Instr};

use combine::Parser;

pub mod opcode;
pub mod value;

use self::value::*;

use self::opcode::Opcode;

#[derive(Clone, Debug, PartialEq)]
pub enum VMError {
    Halt,
    RuntimeError(String),
    ParseErr(String),
    WrongOpCode,
    EmptyPop,
    CombineErr(String),
    Msg(String),
    DifferentNArgs,
    NotIndexable,
    InvalidConversion,
}

// static (read only) and state part

// state:

/// a single thread
///
/// a fiber has its own stack
pub struct Fiber<'a, 'write> {
    base: &'a VM<'write>,
    f: Vec<FState<'a>>,
    value_stack: Vec<Value>,
}

/// a single function
/// it has its own labels
/// it shares a stack
pub struct FState<'a> {
    current_block: &'a Block,
    locals: Vec<Value>,
    code_ptr: usize,
}



impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}:", self.name);
        let mut i = 0;
        while i < self.code.len() {
            let op = Opcode::from_u8(self.code[i]).unwrap();
            if op == Opcode::Nop {
                i += 1;
                continue;
            }

            write!(f, "{:04x}: {:?}", i, op);
            if op.has_arg() {
                let slice = &self.code.as_slice()[i + 1..i + 1 + 8].as_ptr();
                let num: i64 = unsafe { *(*slice as *const i64) };
                i += 8;

                if let Some(s) = self.label_index.get(num as usize) {
                    write!(f, " {}/{}", num, s);
                } else {
                    write!(f, " {}", num);
                }
            }

            writeln!(f, "");
            i += 1;
        }
        writeln!(f, "{:?}", self.labels);
        writeln!(f, "{:?}", self.label_index);

        Ok(())
    }
}

impl<'a> FState<'a> {
    pub fn new(current_block: &'a Block) -> Self {
        Self {
            current_block,
            locals: vec![Value::Nil; current_block.local_n],
            code_ptr: 0,
        }
    }

    pub fn advance_opcode(&mut self) -> Opcode {
        let op = self.current_block.code[self.code_ptr].clone();
        self.code_ptr += 1;

        // internal opcodes should always be valid
        Opcode::from_u8(op).expect(&format!("{} not a valid opcode", op))
    }

    pub fn advance_u16(&mut self) -> u16 {
        // let slice = &self.current_block.code.as_slice()[self.code_ptr..self.code_ptr + 8].as_ptr();
        // let num: i64 = unsafe { *(*slice as *const i64) };
        let num = (self.current_block.code[self.code_ptr] as u16) << 4
            | self.current_block.code[self.code_ptr + 1] as u16;
        self.code_ptr += 2;

        num
    }

    pub fn jump_to_label_index(&mut self, index: usize) -> Result<(), VMError> {
        let label = &self.current_block.label_index[index];
        let new_code_ptr = self.current_block.labels[label];
        self.code_ptr = new_code_ptr;

        Ok(())
    }
}

impl<'a, 'write> Fiber<'a, 'write> {
    pub fn new(base: &'a VM<'write>, f: FState<'a>) -> Self {
        Self {
            base,
            f: vec![f],
            value_stack: Vec::default(),
        }
    }

    pub fn push(&mut self, x: Value) {
        self.value_stack.push(x)
    }

    pub fn pop(&mut self) -> Result<Value, VMError> {
        self.value_stack.pop().ok_or(VMError::EmptyPop)
    }

    pub fn current_f(&mut self) -> &mut FState<'a> {
        self.f.last_mut().expect("empty fiber")
    }

    pub fn push_frame(&mut self, f_index: usize) {
        let block_index = self.base.fn_labels[&self.base.fn_label_index[f_index]];
        let new_block = &self.base.blocks[block_index];
        let f = FState::new(new_block);

        self.f.push(f);
    }

    pub fn pop_frame(&mut self) {
        self.f.pop();
    }

    pub fn step(&mut self) -> Result<(), VMError> {
        match self.current_f().advance_opcode() {
            Opcode::Halt => return Err(VMError::Halt),
            Opcode::Const => {
                let x = self.current_f().advance_u16() as usize;
                let c = self.current_f().current_block.constants[x];
                self.push(c);
            }
            Opcode::Copy => {
                let x = self.current_f().advance_u16() as usize;
                self.push(self.value_stack[self.value_stack.len() - 1 - x]);
            }
            Opcode::Load => {
                let x = self.current_f().advance_u16() as usize;
                let val = self.current_f().locals[x];
                self.push(val);
            }
            Opcode::Store => {
                let x = self.current_f().advance_u16() as usize;
                self.current_f().locals[x] = self.pop()?
            }
            Opcode::Call => {
                let x = self.current_f().advance_u16();
                self.push_frame(x as usize)
            }
            Opcode::CallForeign => {
                let f_index = self.current_f().advance_u16();
                let f = self.base.fn_foreign[f_index as usize](self);
            }
            Opcode::Ret => self.pop_frame(),
            Opcode::Pop => {
                self.pop()?;
            }
            Opcode::Print => {
                let val = self.pop()?;
                let mut data = self.base.output.lock().unwrap();
                writeln!(data, "{}", val);
            }
            Opcode::Add => {
                let a = self.pop()?;
                let b = self.pop()?;
                self.push(Value::binary_op(a, b, |a, b| Value::Number(a + b))?);
            }
            Opcode::Mul => {
                let a = self.pop()?;
                let b = self.pop()?;
                self.push(Value::binary_op(a, b, |a, b| Value::Number(a * b))?);
            }
            Opcode::Div => {
                let a = self.pop()?;
                let b = self.pop()?;
                self.push(Value::binary_op(a, b, |a, b| Value::Number(a / b))?);
            }
            Opcode::Neg => {
                let a = self.pop()?;
                self.push(a.unary_op(|a| -a)?);
            }
            Opcode::Not => {
                let a = self.pop()?;
                self.push(a.not());
            }
            Opcode::LEQ => {
                let a = self.pop()?;
                let b = self.pop()?;
                self.push(Value::binary_op(a, b, |a, b| {
                    if a <= b {
                        Value::False
                    } else {
                        Value::True
                    }
                })?);
            }
            Opcode::Jmp => {
                let x = self.current_f().advance_u16();
                self.current_f().jump_to_label_index(x as usize)?;
            }
            Opcode::JmpT => {
                let label_index = self.current_f().advance_u16();
                let cond = self.pop()?;
                if cond.is_true() {
                    self.current_f().jump_to_label_index(label_index as usize)?;
                }
            }
            Opcode::New => {}
            Opcode::Index => {
                let index = self.pop()?;
                let val = self.pop()?;
                let result = val.index(index)?;
                self.push(result)
            }
            Opcode::Nop => {}
        };
        Ok(())
    }

    pub fn run(&mut self) -> Result<Value, VMError> {
        loop {
            // if self.base.debug {
            //     if self.current_f().current_block.code.get(self.current_f().code_ptr).and_then(|x| Opcode::from_u8(x.clone())) != Some(Opcode::Nop) {
            //         println!("{:?}", self.);
            //         let mut input = String::new();
            //         io::stdin().read_line(&mut input);
            //     }
            // }
            match self.step() {
                Ok(_) => continue,
                Err(VMError::Halt) => return Ok(self.pop()?),
                Err(e) => return Err(e),
            }
        }
    }
}

pub struct Block {
    pub name: String,
    pub code: Vec<u8>,

    pub labels: HashMap<String, usize>,
    pub label_index: Vec<String>,
    pub fresh_label: usize,

    pub constants: Vec<Value>,

    pub local_n: usize,
}

impl Block {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            code: Vec::default(),

            labels: Default::default(),
            label_index: Default::default(),
            fresh_label: 0,

            constants: Default::default(),

            local_n: 0,
        }
    }

    pub fn add_opcode(
        &mut self,
        vm: &mut VM,
        opcode: Opcode,
        arg: Option<&Arg>,
    ) -> Result<(), VMError> {
        if opcode.has_arg() && !arg.is_some() {
            return Err(VMError::Msg("opcode has no arg".to_owned()));
        }
        if !opcode.has_arg() && arg.is_some() {
            return Err(VMError::Msg("opcode has an arg".to_owned()));
        }

        // if opcode.has_arg() {
        //     while self.code.len() % 4 != 3 {
        //         self.code.push(Opcode::Nop as u8)
        //     }
        // }

        self.code.push(opcode.clone() as u8);

        if let Some(ref a) = arg {
            let num = match a {
                Arg::Text(t) => match opcode {
                    Opcode::Call => {
                        let i = vm.fn_label_index.len();
                        vm.fn_label_index.push((*t).to_owned());
                        i
                    }
                    _ => {
                        let i = self.label_index.len();
                        self.label_index.push((*t).to_owned());
                        i
                    }
                },
                Arg::Int(i) => *i as usize,
            };
            self.code.push((num >> 4) as u8);
            self.code.push(num as u8);

            // if self.debug {
            //     println!("conversion {} -> {:?}", num, raw_bytes);
            // }
        };

        Ok(())
    }

    /// Add a label to the current location
    /// a jump to this label jumps to the instruction
    /// after the label
    pub fn add_label(&mut self, label: &str) -> Result<(), VMError> {
        let opcode_index = self.code.len();
        self.labels.insert(label.to_owned(), opcode_index);

        Ok(())
    }

    /// Reserve a label,
    /// Like fresh_label, but dont add the label yet
    /// This has to be done later by add_label
    ///
    /// This is useful when you want to jump to a label that will be defined later
    pub fn reserve_fresh_label(&mut self, surgestion: &str) -> Result<String, VMError> {
        let mut name = surgestion.to_owned();
        while self.labels.contains_key(&name) {
            name = format!("{}-{}", surgestion, self.fresh_label);
            self.fresh_label += 1;
        }

        Ok(name)
    }

    // Get a label that is not used before, and add it to the current location
    pub fn fresh_label(&mut self, surgestion: &str) -> Result<String, VMError> {
        let name = self.reserve_fresh_label(surgestion)?;
        self.add_label(&name)?;

        Ok(name)
    }

    pub fn parse_instr(&mut self, vm: &mut VM, instr: &Instr) -> Result<(), VMError> {
        let opcode = match instr.instr {
            "const" => Opcode::Const,
            "copy" => Opcode::Copy,
            "load" => Opcode::Load,
            "store" => Opcode::Store,
            "pop" => Opcode::Pop,
            "halt" => Opcode::Halt,
            "print" => Opcode::Print,
            "call" => Opcode::Call,
            "ret" => Opcode::Ret,
            "add" => Opcode::Add,
            "mul" => Opcode::Mul,
            "div" => Opcode::Div,
            "neg" => Opcode::Neg,
            "not" => Opcode::Not,
            "leq" => Opcode::LEQ,
            "jmp" => Opcode::Jmp,
            "jmpt" => Opcode::JmpT,
            "nop" => Opcode::Nop,
            x => return Err(VMError::ParseErr(format!("err invalid instr: {:?}", x))),
        };

        if let Some(ref x) = instr.label {
            self.add_label(x)?;
        }
        self.add_opcode(vm, opcode.clone(), instr.arg.as_ref())?;

        Ok(())
    }

    pub fn parse_opcode_data(&mut self, data: &str) -> Result<(), VMError> {
        self.constants.push(Value::Number(data.parse().unwrap()));
        Ok(())
    }

    pub fn parse_ir(&mut self, vm: &mut VM, input: &str) -> Result<(), VMError> {
        #[derive(Debug)]
        enum Mode {
            Code,
            Data,
            Comment,
        };
        let mut mode = Mode::Code;
        for x in input.lines() {
            match x.trim() {
                ".code" => mode = Mode::Code,
                ".data" => mode = Mode::Data,
                ".comment" => mode = Mode::Comment,
                x => match mode {
                    Mode::Code => {
                        if x.is_empty() {
                            continue;
                        }
                        let (res, extra) = bytecode::parse_instr()
                            .easy_parse(x)
                            .map_err(|e| VMError::CombineErr(format!("{:?}", e)))?;

                        if !extra.is_empty() {
                            return Err(VMError::ParseErr("trailing char".to_owned()));
                        }

                        self.parse_instr(vm, &res)?
                    }
                    Mode::Data => self.parse_opcode_data(x)?,
                    Mode::Comment => {}
                },
            }
        }

        Ok(())
    }
}

impl<'a> Default for VM<'a> {
    fn default() -> Self {
        Self::new(Box::new(io::stdout()))
    }
}

impl<'a> fmt::Debug for VM<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // writeln!(f, "code:");
        writeln!(f, "{:?}", self.fn_labels)?;
        writeln!(f, "{:?}", self.fn_label_index)?;

        for b in self.blocks.iter() {
            writeln!(f, "{:?}", b)?;
        }

        Ok(())
    }
}

pub struct VM<'write> {
    blocks: Vec<Block>,
    fn_labels: HashMap<String, usize>,
    fn_label_index: Vec<String>,

    fn_foreign: Vec<fn(&mut Fiber)>,
    fn_foreign_labels: HashMap<String, usize>,

    data: Vec<&'static str>,

    output: Arc<Mutex<Box<io::Write + 'write>>>,

    pub debug: bool,
}

impl<'a> VM<'a> {
    pub fn new(output: Box<io::Write + 'a + Sync>) -> Self {
        Self {
            blocks: Default::default(),

            fn_labels: Default::default(),
            fn_label_index: Default::default(),
            fn_foreign: Default::default(),
            fn_foreign_labels: Default::default(),

            data: Default::default(),

            output: Arc::new(Mutex::new(output)),

            debug: false,
        }
    }

    pub fn new_fiber<'b>(&'b self, label: &str) -> Result<Fiber<'b, 'a>, VMError> {
        let fs = FState::new(&self.blocks[self.fn_labels[label]]);
        let fiber = Fiber::new(self, fs);

        Ok(fiber)
    }

    pub fn add_block(&mut self, block: Block) {
        let pos = self.blocks.len();
        let name = block.name.clone();
        self.blocks.push(block);
        self.fn_labels.insert(name, pos);
    }

    pub fn parse_ir_block(&mut self, name: impl Into<String>, input: &str) -> Result<(), VMError> {
        let mut bl = Block::new(name.into());

        bl.parse_ir(self, input)?;
        self.add_block(bl);

        Ok(())
    }

    pub fn register_foreign(&mut self, name: &str, f: fn(&mut Fiber)) {
        let index = self.fn_foreign.len();
        self.fn_foreign.push(f);
        self.fn_foreign_labels.insert(name.to_owned(), index);

        let mut bl = Block::new(name);
        bl.add_opcode(self, Opcode::CallForeign, Some(&Arg::Int(index as isize))).unwrap();
        bl.add_opcode(self, Opcode::Ret, None).unwrap();
        self.add_block(bl);
    }

    pub fn register_io(&mut self) {
        fn vm_read_line(fiber: &mut Fiber) {
            let mut buffer = String::new();
            std::io::stdin().read_line(&mut buffer).unwrap();
            fiber.push(Value::object(Object::String(buffer)));
        }
        fn vm_print_info(_fiber: &mut Fiber) {
            println!("sabi vm, version: 0.1");
        }

        self.register_foreign("read_line", vm_read_line);
        self.register_foreign("print_info", vm_print_info);
    }

    pub fn register_basic(&mut self) {
        fn register_basic_instr(vm: &mut VM, name: &str, op: Opcode) {
            let mut bl = Block::new(name);
            bl.add_opcode(vm, op, None).unwrap();
            bl.add_opcode(vm, Opcode::Ret, None).unwrap();
            vm.add_block(bl);
        }

        register_basic_instr(self, "add", Opcode::Add);
        register_basic_instr(self, "mul", Opcode::Mul);
        register_basic_instr(self, "div", Opcode::Div);
        register_basic_instr(self, "neg", Opcode::Neg);

        register_basic_instr(self, "not", Opcode::Not);
        register_basic_instr(self, "leq", Opcode::LEQ);

        register_basic_instr(self, "print", Opcode::Print);
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     fn test_file(vm: &mut VM, file: impl AsRef<std::path::Path>) -> Result<Value, VMError> {
//         use std::io::Read;

//         let mut input = String::new();

//         std::fs::File::open(file)
//             .unwrap()
//             .read_to_string(&mut input)
//             .unwrap();
//         vm.parse_ir_block("test", &input)?;
//         let mut f = vm.new_fiber("test")?;
//         f.run()
//     }

//     fn test_instr(instr: &str, vals: &[Value], result: Value) {
//         let mut vm = VM::default();
//         let start = vals
//             .iter()
//             .map(|x| format!("push {}\n", x))
//             .collect::<String>();
//         let p_res = vm.parse_ir_block("test", &format!("{}\n{}\nhalt", start, instr));
//         assert_eq!(p_res, Ok(()));
//         println!("{:?}", vm);
//         let mut f = vm.new_fiber("test").unwrap();
//         let vm_result = f.run();
//         assert_eq!(vm_result, Ok(result));
//     }

//     // #[test]
//     // fn test_push() {
//     //     let mut vm = VM::default();
//     //     vm.parse_ir(
//     //         r#".code
//     //         push 1
//     //         halt"#,
//     //     ).unwrap();
//     //     assert_eq!(vm.run(), Ok(1));
//     // }

//     #[test]
//     fn test_copy() {
//         test_instr("copy 1", &[Value::Number(10.), Value::Number(15.)], Value::Number(10.));
//     }

//     #[test]
//     fn test_add() {
//         test_instr("add", &[Value::Number(10.), Value::Number(15.),, 25);
//     }
//     #[test]
//     fn test_mul() {
//         test_instr("mul", &[Value::Number(10.), Value::Number(15.),, Value::Number(150.),);
//     }
//     #[test]
//     fn test_div() {
//         test_instr("div", &[Value::Number(10.), 2], 5);
//     }
//     #[test]
//     fn test_not() {
//         test_instr("not", &[1], 0);
//     }
//     #[test]
//     fn test_neg() {
//         test_instr("neg", &[10], Value::Number(-10.),;
//     }
//     #[test]
//     fn test_leq() {
//         test_instr("leq", &[Value::Number(10.), Value::Number(10.)], Value::Number(0.));
//         test_instr("leq", &[Value::Number(9.), Value::Number(10.)], Value::Number(1));
//         test_instr("leq", &[Value::Number(10.), Value::Number(9.)], Value::Number(0.));
//     }
//     #[test]
//     fn test_pop() {
//         test_instr("pop", &[Value::Number(10.), Value::Number(15.),, Value::Number(10.));
//     }
//     #[test]
//     // #[test]
//     // fn load_store_test() {
//     //     assert_eq!(test_file(&mut VM::default(), "tests/label.vmb"), Ok(2));
//     // }
//     #[test]
//     fn label_if_test() {
//         assert_eq!(test_file(&mut VM::default(), "tests/bc/label.vmb"), Ok(Value::Number(2.)));
//     }

//     #[test]
//     fn sqrt_test() {
//         assert_eq!(test_file(&mut VM::default(), "tests/bc/sqrt.vmb"), Ok(Value::Number(1024.)));
//     }

// }
