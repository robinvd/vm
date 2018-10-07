use std::collections::HashMap;
use std::{fmt, io, mem};

use crate::bc_parser::{self, Arg, Instr};

use combine::Parser;

#[derive(Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum Opcode {
    Halt,
    Push,
    Pop,
    Copy,
    Load,
    Store,
    Print,
    Add,
    Mul,
    Div,
    Neg,
    LEQ,
    If,
    Jmp,
    JmpR,
    Nop,
}

impl Opcode {
    pub fn from_u8(n: u8) -> Option<Opcode> {
        if n <= Opcode::Nop as u8 {
            Some(unsafe { mem::transmute(n) })
        } else {
            None
        }
    }

    pub fn has_arg(&self) -> bool {
        match self {
            Opcode::Push
            | Opcode::JmpR
            | Opcode::Jmp
            | Opcode::If
            | Opcode::Load
            | Opcode::Store
            | Opcode::Copy => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum VMError {
    Halt,
    ParseErr(String),
    WrongOpCode,
    EmptyPop,
    CombineErr(String),
}

pub struct VM<'a> {
    code: Vec<u8>,
    code_ptr: usize,
    stack: Vec<isize>,

    labels: HashMap<String, usize>,
    label_index: Vec<String>,

    output: Box<io::Write + 'a>,

    pub debug: bool,
}

impl<'a> Default for VM<'a> {
    fn default() -> Self {
        Self::new(Box::new(io::stdout()))
    }
}

impl<'a> fmt::Debug for VM<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "code:");
        let mut i = 0;
        while i < self.code.len() {
            let current = i == self.code_ptr;

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
                write!(f, " {:?}", num);
            }

            if current {
                write!(f, " <");
            }
            writeln!(f, "");
            i += 1;
        }
        writeln!(f, "code_ptr: {:04x}", self.code_ptr);
        self.stack.fmt(f)?;
        Ok(())
    }
}

impl<'a> VM<'a> {
    pub fn new(output: Box<io::Write + 'a>) -> Self {
        Self {
            code: Default::default(),
            code_ptr: Default::default(),
            stack: Default::default(),

            labels: Default::default(),
            label_index: Default::default(),

            output: output,

            debug: false,
        }
    }

    // pub fn output(&self) -> &Box<dyn io::Write> {
    //     &self.output
    // }

    pub fn parse_instr(&mut self, instr: &Instr) -> Result<(), VMError> {
        let opcode = match instr.instr {
            "push" => Opcode::Push,
            "copy" => Opcode::Copy,
            "load" => Opcode::Load,
            "store" => Opcode::Store,
            "pop" => Opcode::Pop,
            "halt" => Opcode::Halt,
            "print" => Opcode::Print,
            "add" => Opcode::Add,
            "mul" => Opcode::Mul,
            "div" => Opcode::Div,
            "neg" => Opcode::Neg,
            "leq" => Opcode::LEQ,
            "if" => Opcode::If,
            "jmp" => Opcode::Jmp,
            "jmpr" => Opcode::JmpR,
            "nop" => Opcode::Nop,
            x => return Err(VMError::ParseErr(format!("err invalid instr: {:?}", x))),
        };

        if instr.arg.is_some() && !opcode.has_arg() {
            return Err(VMError::WrongOpCode);
        }

        if let Some(ref _a) = &instr.arg {
            if opcode.has_arg() {
                while self.code.len() % 4 != 3 {
                    self.code.push(Opcode::Nop as u8)
                }
            } else {
                return Err(VMError::WrongOpCode);
            }
        }

        let opcode_index = self.code.len();
        self.code.push(opcode as u8);

        if let Some(ref a) = &instr.arg {
            let num = match a {
                Arg::Text(t) => {
                    let i = self.label_index.len();
                    self.label_index.push((*t).to_owned());
                    i
                }
                Arg::Int(i) => *i as usize,
            };
            let raw_bytes: [u8; 8] = unsafe { mem::transmute(num) };

            if self.debug {
                println!("conversion {} -> {:?}", num, raw_bytes);
            }
            self.code.extend_from_slice(&raw_bytes);
        };

        if let Some(label) = instr.label {
            self.labels.insert(label.to_owned(), opcode_index);
        }

        Ok(())

        // match data.parse::<i64>() {
        //     Ok(num) => {
        //         let raw_bytes: [u8; 8] = unsafe { mem::transmute(num) };
        //         self.code.extend_from_slice(&raw_bytes);
        //         Ok(())
        //     },
        //     Err(e) => {
        //         let opcode = match data {
        //             "push" => Opcode::Push,
        //             "copy" => Opcode::Copy,
        //             "load" => Opcode::Load,
        //             "store" => Opcode::Store,
        //             "pop" => Opcode::Pop,
        //             "halt" => Opcode::Halt,
        //             "print" => Opcode::Print,
        //             "add" => Opcode::Add,
        //             "mul" => Opcode::Mul,
        //             "neg" => Opcode::Neg,
        //             "leq" => Opcode::LEQ,
        //             "if" => Opcode::If,
        //             "jmp" => Opcode::Jmp,
        //             "jmpr" => Opcode::JmpR,
        //             x => return Err(VMError::ParseErr(format!("{}: {}", x, e)))
        //         };
        //         if opcode.has_arg() {
        //             while self.code.len() % 4 != 3 {
        //                 self.code.push(Opcode::Nop as u8)
        //             }
        //         }
        //         self.code.push(opcode as u8);
        //         Ok(())

        //     }
        // }
    }

    pub fn parse_opcode_data(&mut self, _data: &str) -> Result<(), VMError> {
        Ok(())
    }

    pub fn parse_ir(&mut self, input: &str) -> Result<(), VMError> {
        // lazy_static! {
        //     static ref split_regex: regex::Regex = regex::Regex::new(r"\s+").expect("invalid regex");
        //     static ref line_regex: regex::Regex = regex::Regex::new(r"\s+\n\s+").expect("invalid regex");
        // }

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
                        let (res, extra) = bc_parser::parse_instr()
                            .easy_parse(x)
                            .map_err(|e| VMError::CombineErr(format!("{:?}", e)))?;

                        if !extra.is_empty() {
                            return Err(VMError::ParseErr("trailing char".to_owned()));
                        }

                        self.parse_instr(&res)?
                    }
                    Mode::Data => self.parse_opcode_data(x)?,
                    Mode::Comment => {}
                },
            }
        }

        Ok(())
    }

    fn advance_opcode(&mut self) -> Opcode {
        let op = self.code[self.code_ptr].clone();
        self.code_ptr += 1;

        // internal opcodes should always be valid
        Opcode::from_u8(op).unwrap()
    }

    fn advance_lit(&mut self) -> i64 {
        let slice = &self.code.as_slice()[self.code_ptr..self.code_ptr + 8].as_ptr();
        let num: i64 = unsafe { *(*slice as *const i64) };
        self.code_ptr += 8;

        num
    }

    pub fn peek(&self) -> isize {
        self.stack[self.stack.len() - 1]
    }

    pub fn push(&mut self, x: isize) {
        self.stack.push(x)
    }

    pub fn pop(&mut self) -> Result<isize, VMError> {
        self.stack.pop().ok_or(VMError::EmptyPop)
    }

    pub fn jump_to_label_index(&mut self, index: usize) -> Result<(), VMError> {
        let label = &self.label_index[index];
        let new_code_ptr = self.labels[label];
        self.code_ptr = new_code_ptr;

        Ok(())
    }

    pub fn step(&mut self) -> Result<(), VMError> {
        match self.advance_opcode() {
            Opcode::Halt => return Err(VMError::Halt),
            Opcode::Push => {
                let x = self.advance_lit() as isize;
                self.push(x);
            }
            Opcode::Copy => {
                let x = self.advance_lit() as usize;
                self.push(self.stack[self.stack.len() - 1 - x]);
            }
            Opcode::Load => {
                let x = self.advance_lit() as usize;
                self.push(self.stack[x]);
            }
            Opcode::Store => {
                let x = self.advance_lit() as usize;
                self.stack[x] = self.pop()?;
            }
            Opcode::Pop => {
                self.pop()?;
            }
            Opcode::Print => {
                let val = self.pop()?;
                writeln!(self.output, "{}", val);
            }
            Opcode::Add => {
                let a = self.pop()?;
                let b = self.pop()?;
                self.push(a + b);
            }
            Opcode::Mul => {
                let a = self.pop()?;
                let b = self.pop()?;
                self.push(a * b);
            }
            Opcode::Div => {
                let a = self.pop()?;
                let b = self.pop()?;
                self.push(b/a);
            }
            Opcode::Neg => {
                let a = self.pop()?;
                self.push(-a);
            }
            Opcode::LEQ => {
                let a = self.pop()?;
                let b = self.pop()?;
                let val = if b < a { 1 } else { -1 };
                self.push(val);
            }
            Opcode::Jmp => {
                // let x = self.pop()?;
                let x = self.advance_lit();
                self.jump_to_label_index(x as usize)?;
            }
            Opcode::JmpR => {
                let x = self.advance_lit();
                self.code_ptr = (x as isize + self.code_ptr as isize) as usize;
            }
            Opcode::If => {
                let label_index = self.advance_lit();
                let cond = self.pop()?;
                if !(cond > 0) {
                    self.jump_to_label_index(label_index as usize)?;
                }
            }
            Opcode::Nop => {}
        };
        Ok(())
    }

    pub fn run(&mut self) -> Result<isize, VMError> {
        loop {
            if self.debug {
                if self.code.get(self.code_ptr).and_then(|x| Opcode::from_u8(x.clone())) != Some(Opcode::Nop) {
                    println!("{:?}", self);
                    let mut input = String::new();
                    io::stdin().read_line(&mut input);
                }
            }
            match self.step() {
                Ok(_) => continue,
                Err(VMError::Halt) => return Ok(self.pop()?),
                Err(e) => return Err(e),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_file(vm: &mut VM, file: impl AsRef<std::path::Path>) -> Result<isize, VMError> {
        use std::io::Read;

        let mut input = String::new();

        std::fs::File::open(file)
            .unwrap()
            .read_to_string(&mut input)
            .unwrap();
        vm.parse_ir(&input).unwrap();
        vm.run()
    }

    fn test_instr(instr: &str, vals: &[isize], result: isize) {
        let mut vm = VM::default();
        let start = vals.iter().map(|x| format!("push {}\n", x)).collect::<String>();
        let p_res = vm.parse_ir(&format!("{}\n{}\nhalt", start, instr));
        assert_eq!(p_res, Ok(()));
        let vm_result = vm.run();
        assert_eq!(vm_result, Ok(result));
    }

    #[test]
    fn test_push() {
        let mut vm = VM::default();
        vm.parse_ir(
            r#".code
            push 1
            halt"#,
        ).unwrap();
        assert_eq!(vm.run(), Ok(1));
    }

    #[test]
    fn test_copy() {
       test_instr("copy 1", &[10, 15], 10);
    }

   #[test]
   fn test_add() {
       test_instr("add", &[10, 15], 25);
   }
   #[test]
   fn test_mul() {
       test_instr("mul", &[10, 15], 150);
   }
   #[test]
   fn test_div() {
       test_instr("div", &[10, 2], 5);
   }
   #[test]
   fn test_neg() {
       test_instr("neg", &[10], -10);
   }
   #[test]
   fn test_leq() {
       test_instr("leq", &[10,10], 1);
       test_instr("leq", &[9,10], 1);
       test_instr("leq", &[10,9], 0);
   }
   #[test]
   fn test_pop() {
       test_instr("pop", &[10, 15], 10);
   }

   #[test]
    fn test_math() {
        let mut buffer = Vec::new();
        {
            let mut vm = VM::new(Box::new(&mut buffer));
            let result = test_file(&mut vm, "tests/math.vmb");
            assert_eq!(result, Ok(0));
        };
        assert_eq!(buffer.as_slice(), &b"7\n10\n-10\n5\n"[..]);
    }

    #[test]
    fn vm_test() {
        let mut vm = VM::default();
        vm.parse_ir(
            r#".code
            start: push 1
            copy 0
            add
            push 2
            mul
            halt"#,
        ).unwrap();
        assert_eq!(vm.run(), Ok(4));
    }

    #[test]
    fn label_if_test() {
        assert_eq!(test_file(&mut VM::default(), "tests/label.vmb"), Ok(2));
    }

    #[test]
    fn sqrt_test() {
        assert_eq!(test_file(&mut VM::default(), "tests/sqrt.vmb"), Ok(1024));
    }

}
