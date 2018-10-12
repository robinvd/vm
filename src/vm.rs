use std::collections::HashMap;
use std::{fmt, io, mem};
use std::sync::{Arc, Mutex};

use crate::parser::bytecode::{self, Arg, Instr};

use combine::Parser;

#[derive(Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum Opcode {
    // general
    Halt,
    Push,
    Pop,
    Copy,
    Load,
    Store,
    Print,

    // fn
    Call,
    Ret,

    // math
    Add,
    Mul,
    Div,
    Neg,

    // bool
    Not,
    LEQ,

    // jmp
    JmpZ,
    Jmp,
    JmpR,

    // async:
    // asyncjmp
    // await

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
            | Opcode::JmpZ
            | Opcode::Load
            | Opcode::Store
            | Opcode::Call
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
    Msg(String),
}

// static (read only) and state part

// state:

/// a single thread
/// 
/// a fiber has its own stack
pub struct Fiber<'a, 'write> {
    base: &'a VM<'write>,
    f: Vec<FState<'a>>,
    value_stack: Vec<isize>,
}

/// a single function
/// it has its own labels
/// it shares a stack
pub struct FState<'a> {
    current_block: &'a Block,
    code_ptr: usize,
}

// static
pub struct VM<'write> {
    blocks: Vec<Block>,
    fn_labels: HashMap<String, usize>,
    fn_label_index: Vec<String>,

    data: Vec<&'static str>,

    output: Arc<Mutex<Box<io::Write + 'write>>>,

    pub debug: bool,
}

pub struct Block {
    pub name: String,
    pub code: Vec<u8>,

    pub labels: HashMap<String, usize>,
    pub label_index: Vec<String>,
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
                    write!(f, " {}/{}", num,s);
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
            code_ptr: 0,
        }
    }

    pub fn advance_opcode(&mut self) -> Opcode {
        let op = self.current_block.code[self.code_ptr].clone();
        self.code_ptr += 1;

        // internal opcodes should always be valid
        Opcode::from_u8(op).expect(&format!("{} not a valid opcode", op))
    }

    pub fn advance_lit(&mut self) -> i64 {
        let slice = &self.current_block.code.as_slice()[self.code_ptr..self.code_ptr + 8].as_ptr();
        let num: i64 = unsafe { *(*slice as *const i64) };
        self.code_ptr += 8;

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

    pub fn push(&mut self, x: isize) {
        self.value_stack.push(x)
    }

    pub fn pop(&mut self) -> Result<isize, VMError> {
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
            Opcode::Push => {
                let x = self.current_f().advance_lit() as isize;
                self.push(x);
            }
            Opcode::Copy => {
                let x = self.current_f().advance_lit() as usize;
                self.push(self.value_stack[self.value_stack.len() - 1 - x]);
            }
            Opcode::Load => {
                // let x = self.advance_lit() as usize;
                // self.push(self.stack[x]);
            }
            Opcode::Store => {
                // let x = self.advance_lit() as usize;
                // self.stack[x] = self.pop()?;
            }
            Opcode::Call => {
                let x = self.current_f().advance_lit();
                self.push_frame(x as usize)
            }
            Opcode::Ret => {
                self.pop_frame()
            }
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
            Opcode::Not => {
                let a = self.pop()?;
                self.push(-(a - 1));
            }
            Opcode::LEQ => {
                // push a, push b, leq == a <= b
                let a = self.pop()?;
                let b = self.pop()?;
                let val = if a <= b { 0 } else { 1 };
                self.push(val);
            }
            Opcode::Jmp => {
                // let x = self.pop()?;
                let x = self.current_f().advance_lit();
                self.current_f().jump_to_label_index(x as usize)?;
            }
            Opcode::JmpR => {
                // let x = self.advance_lit();
                // self.code_ptr = (x as isize + self.code_ptr as isize) as usize;
            }
            Opcode::JmpZ => {
                let label_index = self.current_f().advance_lit();
                let cond = self.pop()?;
                if cond == 0 {
                    self.current_f().jump_to_label_index(label_index as usize)?;
                }
            }
            Opcode::Nop => {}
        };
        Ok(())
    }

    pub fn run(&mut self) -> Result<isize, VMError> {
        loop {
            // if self.base.debug {
            //     if self.code.get(self.code_ptr).and_then(|x| Opcode::from_u8(x.clone())) != Some(Opcode::Nop) {
            //         println!("{:?}", self);
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

impl Block {
    pub fn new(name: String) -> Self {
        Self {
            name,
            code: Vec::default(),
            labels: Default::default(),
            label_index: Default::default(),
        }
    }

    pub fn add_opcode(&mut self, vm: &mut VM, opcode: Opcode, arg: Option<&Arg>) -> Result<(), VMError> {
        if opcode.has_arg() && !arg.is_some() {
            return Err(VMError::Msg("opcode has no arg".to_owned()))
        }
        if !opcode.has_arg() && arg.is_some() {
            return Err(VMError::Msg("opcode has an arg".to_owned()))
        }

        if opcode.has_arg() {
            while self.code.len() % 4 != 3 {
                self.code.push(Opcode::Nop as u8)
            }
        }

        self.code.push(opcode.clone() as u8);

        if let Some(ref a) = arg {
            let num = match a {
                Arg::Text(t) => {
                    if opcode == Opcode::Call {
                        let i = vm.fn_label_index.len();
                        vm.fn_label_index.push((*t).to_owned());
                        i
                    } else {
                        let i = self.label_index.len();
                        self.label_index.push((*t).to_owned());
                        i
                    }
                }
                Arg::Int(i) => *i as usize,
            };
            let raw_bytes: [u8; 8] = unsafe { mem::transmute(num) };

            // if self.debug {
            //     println!("conversion {} -> {:?}", num, raw_bytes);
            // }
            self.code.extend_from_slice(&raw_bytes);
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

    pub fn parse_instr(&mut self, vm: &mut VM, instr: &Instr) -> Result<(), VMError> {
        let opcode = match instr.instr {
            "push" => Opcode::Push,
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
            "jmpz" => Opcode::JmpZ,
            "jmp" => Opcode::Jmp,
            "jmpr" => Opcode::JmpR,
            "nop" => Opcode::Nop,
            x => return Err(VMError::ParseErr(format!("err invalid instr: {:?}", x))),
        };

        if let Some(ref x) = instr.label {
            self.add_label(x)?;
        }
        self.add_opcode(vm, opcode.clone(), instr.arg.as_ref())?;

        Ok(())
    }

    pub fn parse_opcode_data(&mut self, _data: &str) -> Result<(), VMError> {
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

// pub struct VM<'a> {
//     code: Vec<u8>,
//     code_ptr: usize,
//     stack: Vec<isize>,

//     labels: HashMap<String, usize>,
//     label_index: Vec<String>,

//     output: Box<io::Write + 'a>,

//     pub debug: bool,
// }

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

impl<'a> VM<'a> {
    pub fn new(output: Box<io::Write + 'a + Sync>) -> Self {
        Self {
            blocks: Default::default(),

            fn_labels: Default::default(),
            fn_label_index: Default::default(),

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
        vm.parse_ir_block("test", &input)?;
        let mut f = vm.new_fiber("test")?;
        f.run()
    }

    fn test_instr(instr: &str, vals: &[isize], result: isize) {
        let mut vm = VM::default();
        let start = vals.iter().map(|x| format!("push {}\n", x)).collect::<String>();
        let p_res = vm.parse_ir_block("test", &format!("{}\n{}\nhalt", start, instr));
        assert_eq!(p_res, Ok(()));
        println!("{:?}", vm);
        let mut f = vm.new_fiber("test").unwrap();
        let vm_result = f.run();
        assert_eq!(vm_result, Ok(result));
    }

    // #[test]
    // fn test_push() {
    //     let mut vm = VM::default();
    //     vm.parse_ir(
    //         r#".code
    //         push 1
    //         halt"#,
    //     ).unwrap();
    //     assert_eq!(vm.run(), Ok(1));
    // }

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
   fn test_not() {
       test_instr("not", &[1], 0);
   }
   #[test]
   fn test_neg() {
       test_instr("neg", &[10], -10);
   }
   #[test]
   fn test_leq() {
       test_instr("leq", &[10,10], 0);
       test_instr("leq", &[9,10], 1);
       test_instr("leq", &[10,9], 0);
   }
   #[test]
   fn test_pop() {
       test_instr("pop", &[10, 15], 10);
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
