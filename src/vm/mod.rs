use std::collections::{HashMap, LinkedList};
use std::sync::{Arc, Mutex};
use std::{fmt, io};

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
    FunctionNotFound(String),
    WrongOpCode(u8),
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
    all_gc: LinkedList<GCObject>,
}

/// a single function
/// it has its own labels
/// it shares a stack
pub struct FState<'a> {
    current_block: &'a Block,
    locals: Vec<Value>,
    code_ptr: usize,
    stack_start: usize,
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
                let num = (self.code[i + 1] as u16) << 4 | self.code[i + 2] as u16;
                i += 2;

                // if let Some(s) = self.label_index.get(num as usize) {
                //     write!(f, " {}/{}", num, s);
                // } else {
                write!(f, " {}", num);
                // }
            }

            writeln!(f);
            i += 1;
        }
        writeln!(f, "labels {:?}", self.labels);
        writeln!(f, "labeli {:?}", self.label_index);
        writeln!(f, "const  {:?}", self.constants);

        Ok(())
    }
}

impl<'a> FState<'a> {
    pub fn new(current_block: &'a Block, stack_start: usize) -> Self {
        Self {
            current_block,
            locals: vec![Value::Nil; current_block.local_n],
            code_ptr: 0,
            stack_start,
        }
    }

    pub fn advance_opcode(&mut self) -> Opcode {
        let op = self.current_block.code[self.code_ptr];
        self.code_ptr += 1;

        // internal opcodes should always be valid
        Opcode::from_u8(op)
            .ok_or_else(|| VMError::WrongOpCode(op))
            .unwrap()
    }

    pub fn advance_u16(&mut self) -> u16 {
        // let slice = &self.current_block.code.as_slice()[self.code_ptr..self.code_ptr + 8].as_ptr();
        // let num: i64 = unsafe { *(*slice as *const i64) };
        let num = (self.current_block.code[self.code_ptr] as u16) << 8
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

impl<'a, 'write> Drop for Fiber<'a, 'write> {
    fn drop(&mut self) {
        for o in &self.all_gc {
            unsafe {
                Box::from_raw(o.val.as_ptr());
            }
        }
    }
}

impl<'a, 'write> Fiber<'a, 'write> {
    pub fn new(base: &'a VM<'write>, f: FState<'a>) -> Self {
        Self {
            base,
            f: vec![f],
            value_stack: Vec::default(),
            all_gc: LinkedList::new(),
        }
    }

    pub fn deep_clone(&mut self, val: Value) -> Value {
        let new = val.deep_clone();

        let mut f = |val: &Value| {
            if let Some(gc) = val.get_object_gcobj() {
                self.all_gc.push_front(gc);
            }
        };
        new.traverse(&mut f);
        new
    }

    pub fn gc_len(&self) -> usize {
        self.all_gc.len()
    }

    pub fn collect(&mut self) {
        let color = true;
        let mut f = |v: &Value| {
            if let Some(info) = v.get_object_info() {
                info.mark.set(color);
            }
        };

        for v in &self.value_stack {
            v.traverse(&mut f)
        }

        for fst in &self.f {
            for v in &fst.locals {
                v.traverse(&mut f)
            }
        }

        let mut new = LinkedList::new();
        while let Some(o) = self.all_gc.pop_back() {
            if o.object_info().mark.get() {
                new.push_front(o);
            } else {
                unsafe {
                    Box::from_raw(o.val.as_ptr());
                }
            }
        }
        self.all_gc = new;

        for v in &self.value_stack {
            v.traverse(&mut f)
        }
    }

    pub fn push(&mut self, x: Value) {
        self.value_stack.push(x)
    }

    pub fn pop(&mut self) -> Result<Value, VMError> {
        if self.value_stack.len() <= self.current_f().stack_start {
            return Err(VMError::EmptyPop);
        }
        self.value_stack.pop().ok_or(VMError::EmptyPop)
    }

    pub fn current_f(&mut self) -> &mut FState<'a> {
        self.f.last_mut().expect("empty fiber")
    }

    pub fn push_frame(&mut self, f_index: usize) -> Result<(), VMError> {
        let block_name = &self.base.fn_label_index[f_index];
        let block_index = *self
            .base
            .fn_labels
            .get(block_name)
            .ok_or_else(|| VMError::FunctionNotFound(block_name.to_owned()))?;
        let new_block = &self.base.blocks[block_index];
        let f = FState::new(new_block, self.value_stack.len() - new_block.n_args);

        self.f.push(f);

        Ok(())
    }

    pub fn pop_frame(&mut self) {
        let old = self.f.pop().unwrap();
        let ret_val = if old.stack_start < self.value_stack.len() {
            *self.value_stack.last().unwrap()
        } else {
            Value::Nil
        };
        self.value_stack.truncate(old.stack_start);
        self.value_stack.push(ret_val);
    }

    pub fn step(&mut self) -> Result<(), VMError> {
        let instr = self.current_f().advance_opcode();
        if self.base.debug {
            println!("{:?} {:?}", instr, self.value_stack);
        }
        match instr {
            Opcode::Halt => return Err(VMError::Halt),
            Opcode::Const => {
                let x = self.current_f().advance_u16() as usize;
                let c = self.current_f().current_block.constants[x];
                let new = self.deep_clone(c);
                self.push(new);
            }
            Opcode::Nil => {
                self.push(Value::Nil);
            }
            Opcode::Num => {
                let x = self.current_f().advance_u16() as f64;
                self.push(Value::number(x));
            }
            Opcode::True => {
                self.push(Value::True);
            }
            Opcode::False => {
                self.push(Value::False);
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
                self.current_f().locals[x] = self.pop()?;
            }
            Opcode::Call => {
                let x = self.current_f().advance_u16();
                self.push_frame(x as usize)?;
            }
            Opcode::CallForeign => {
                let f_index = self.current_f().advance_u16();
                self.base.fn_foreign[f_index as usize](self);
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
                self.push(Value::binary_op(a, b, |a, b| Value::number(a + b))?);
            }
            Opcode::Mul => {
                let a = self.pop()?;
                let b = self.pop()?;
                self.push(Value::binary_op(a, b, |a, b| Value::number(a * b))?);
            }
            Opcode::Div => {
                let b = self.pop()?;
                let a = self.pop()?;
                self.push(Value::binary_op(a, b, |a, b| Value::number(a / b))?);
            }
            Opcode::Neg => {
                let a = self.pop()?;
                self.push(a.unary_op(|a| -a)?);
            }
            Opcode::Not => {
                let a = self.pop()?;
                self.push(!a);
            }
            Opcode::LEQ => {
                // leq(a,b) then the code will be (push not actual opcode)
                // push a, push b, leq
                // thus b will be popped first
                let b = self.pop()?;
                let a = self.pop()?;
                self.push(Value::binary_op(a, b, |a, b| {
                    if a <= b {
                        Value::True
                    } else {
                        Value::False
                    }
                })?);
            }
            Opcode::GEQ => {
                // see LEQ
                let b = self.pop()?;
                let a = self.pop()?;
                self.push(Value::binary_op(a, b, |a, b| {
                    if a >= b {
                        Value::True
                    } else {
                        Value::False
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
                self.push(result);
            }
            Opcode::Nop => {}
        };
        Ok(())
    }

    pub fn run(&mut self) -> Result<Value, VMError> {
        loop {
            if self.base.debug {
                println!("stack: {:?}", self.value_stack);
                println!("locals: {:?}", self.current_f().locals);
                println!(
                    "code: {} {:04x}",
                    self.current_f().current_block.name,
                    self.current_f().code_ptr
                );
                let mut input = String::new();
                io::stdin().read_line(&mut input);
            }
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

    pub n_args: usize,
    pub local_n: usize,
}

impl Block {
    pub fn new(name: impl Into<String>, n_args: usize) -> Self {
        Self {
            name: name.into(),
            code: Vec::default(),

            labels: Default::default(),
            label_index: Default::default(),
            fresh_label: 0,

            constants: Default::default(),

            n_args,
            local_n: 0,
        }
    }

    pub fn add_opcode(
        &mut self,
        vm: &mut VM,
        opcode: Opcode,
        arg: Option<&Arg>,
    ) -> Result<(), VMError> {
        if opcode.has_arg() && arg.is_none() {
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

        self.code.push(opcode as u8);

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
            self.code.push((num >> 8) as u8);
            self.code.push(num as u8);

            // if self.debug {
            //     println!("conversion {} -> {:?}", num, raw_bytes);
            // }
        };

        Ok(())
    }

    pub fn add_data(&mut self, data: Value) -> usize {
        let i = self.constants.len();
        self.constants.push(data);
        i
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
        self.add_opcode(vm, opcode, instr.arg.as_ref())?;

        Ok(())
    }

    pub fn parse_opcode_data(&mut self, data: &str) -> Result<(), VMError> {
        self.add_data(Value::number(data.parse().unwrap()));
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

        for b in &self.blocks {
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

            output: Arc::new(Mutex::new(output)),

            debug: false,
        }
    }

    pub fn new_fiber<'b>(&'b self, label: &str) -> Result<Fiber<'b, 'a>, VMError> {
        // todo hangle args
        let fs = FState::new(&self.blocks[self.fn_labels[label]], 0);
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
        // todo hangle args
        let mut bl = Block::new(name.into(), 0);

        bl.parse_ir(self, input)?;
        self.add_block(bl);

        Ok(())
    }

    pub fn register_foreign(&mut self, name: &str, f: fn(&mut Fiber), n_args: usize) {
        let index = self.fn_foreign.len();
        self.fn_foreign.push(f);
        self.fn_foreign_labels.insert(name.to_owned(), index);

        let mut bl = Block::new(name, n_args);
        bl.add_opcode(self, Opcode::CallForeign, Some(&Arg::Int(index as isize)))
            .unwrap();
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

        self.register_foreign("read_line", vm_read_line, 0);
        self.register_foreign("print_info", vm_print_info, 0);
    }

    pub fn register_basic(&mut self) {
        fn register_basic_instr(vm: &mut VM, name: &str, n_args: usize, ops: &[Opcode]) {
            let mut bl = Block::new(name, n_args);
            ops.iter()
                .for_each(|op| bl.add_opcode(vm, *op, None).unwrap());
            bl.add_opcode(vm, Opcode::Ret, None).unwrap();
            vm.add_block(bl);
        }

        register_basic_instr(self, "add", 2, &[Opcode::Add]);
        register_basic_instr(self, "mul", 2, &[Opcode::Mul]);
        register_basic_instr(self, "div", 2, &[Opcode::Div]);
        register_basic_instr(self, "neg", 1, &[Opcode::Neg]);

        register_basic_instr(self, "not", 1, &[Opcode::Not]);
        register_basic_instr(self, "leq", 2, &[Opcode::LEQ]);
        register_basic_instr(self, "geq", 2, &[Opcode::GEQ]);

        register_basic_instr(self, "print", 1, &[Opcode::Print]);

        fn vm_floor(fiber: &mut Fiber) {
            let val = fiber.pop().unwrap();
            fiber.push(val.unary_op(|x| x.floor()).unwrap());
        }
        self.register_foreign("floor", vm_floor, 1);

        fn vm_ceil(fiber: &mut Fiber) {
            let val = fiber.pop().unwrap();
            fiber.push(val.unary_op(|x| x.ceil()).unwrap());
        }
        self.register_foreign("ceil", vm_ceil, 1);

        fn vm_pow(fiber: &mut Fiber) {
            let b = fiber.pop().unwrap();
            let a = fiber.pop().unwrap();
            fiber.push(Value::binary_op(a, b, |a, b| Value::number(a.powf(b))).unwrap());
        }
        self.register_foreign("pow", vm_pow, 2);
    }

    pub fn register_internal(&mut self) {
        fn vm_collect(fiber: &mut Fiber) {
            fiber.collect();
        }
        fn vm_count(fiber: &mut Fiber) {
            fiber.push(Value::number(fiber.gc_len() as f64));
        }

        self.register_foreign("collect_garbage", vm_collect, 0);
        self.register_foreign("count_garbage", vm_count, 0);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::Opcode::*;

    const ENTRY: &'static str = r#"
        .data
        0
        .code
        const 0
        call main
        halt
    "#;

    pub fn add_main(vm: &mut VM) -> Result<(), VMError> {
        vm.parse_ir_block("start", ENTRY)?;
        Ok(())
    }

    fn load_file(file: impl AsRef<std::path::Path>) -> Result<String, VMError> {
        use std::io::Read;

        let mut input = String::new();

        std::fs::File::open(file)
            .unwrap()
            .read_to_string(&mut input)
            .unwrap();

        Ok(input)
    }

    fn load_vm<'a>(input: &'a str, buffer: &'a mut Vec<u8>) -> VM<'a> {
        use crate::compiler;
        use crate::parser;

        let mut vm = VM::new(Box::new(buffer));
        vm.register_basic();

        let parsed = parser::code::parse_file()
            .easy_parse(input)
            .expect("failed to parse");

        add_main(&mut vm).expect("failed to add main");
        compiler::compile(&mut vm, &parsed.0).expect("failed to compile");

        vm
    }

    #[test]
    fn test_gc() {
        let input = load_file("tests/basic/gc.sabi").unwrap();
        let mut buffer = Vec::new();
        let mut vm = load_vm(&input, &mut buffer);
        vm.register_internal();

        let mut f = vm.new_fiber("start").unwrap();
        let res = f.run();
        assert_eq!(res, Ok(Value::True));
    }

    fn test_instr(instr: Opcode, arg: Option<Arg>, vals: &[Value], result: Value) {
        let buffer = Vec::new();
        let mut vm = VM::new(Box::new(buffer));
        let mut block = Block::new("test", 0);
        vals.iter().for_each(|x| {
            let i = block.add_data(*x);
            block
                .add_opcode(&mut vm, Opcode::Const, Some(&Arg::Int(i as isize)))
                .unwrap();
            block
                .add_opcode(&mut vm, Opcode::Copy, Some(&Arg::Int(0)))
                .unwrap();
            block.add_opcode(&mut vm, Opcode::Print, None).unwrap();
        });

        block.add_opcode(&mut vm, instr, arg.as_ref()).unwrap();
        block.add_opcode(&mut vm, Halt, None).unwrap();

        vm.add_block(block);
        println!("{:?}", vm);
        let mut f = vm.new_fiber("test").unwrap();
        let vm_result = f.run();
        assert_eq!(vm_result, Ok(result));
    }

    #[test]
    fn test_copy() {
        test_instr(
            Copy,
            Some(Arg::Int(0)),
            &[Value::number(10.)],
            Value::number(10.),
        );
    }
    #[test]
    fn test_not() {
        test_instr(Not, None, &[Value::true_val()], Value::false_val());
        test_instr(Not, None, &[Value::false_val()], Value::true_val());
        test_instr(Not, None, &[Value::nil()], Value::true_val());
        test_instr(Not, None, &[Value::number(10.)], Value::false_val());
    }
    #[test]
    fn test_add() {
        test_instr(
            Add,
            None,
            &[Value::number(10.), Value::number(2.)],
            Value::number(12.),
        );
    }
    #[test]
    fn test_mul() {
        test_instr(
            Mul,
            None,
            &[Value::number(10.), Value::number(2.)],
            Value::number(20.),
        );
    }
    #[test]
    fn test_div() {
        test_instr(
            Div,
            None,
            &[Value::number(10.), Value::number(2.)],
            Value::number(5.),
        );
    }
    #[test]
    fn test_neg() {
        test_instr(Neg, None, &[Value::number(10.)], Value::number(-10.));
    }
    #[test]
    fn test_leq() {
        test_instr(
            LEQ,
            None,
            &[Value::number(10.), Value::number(10.)],
            Value::True,
        );
        test_instr(
            LEQ,
            None,
            &[Value::number(9.), Value::number(10.)],
            Value::True,
        );
        test_instr(
            LEQ,
            None,
            &[Value::number(10.), Value::number(9.)],
            Value::False,
        );
    }

    #[test]
    fn test_empty_push() {
        let buffer = Vec::new();
        let mut vm = VM::new(Box::new(buffer));

        let mut main = Block::new("main", 0);
        main.add_opcode(&mut vm, Num, Some(&Arg::Int(21))).unwrap();
        main.add_opcode(&mut vm, Call, Some(&Arg::Text("wrong")))
            .unwrap();
        main.add_opcode(&mut vm, Ret, None).unwrap();

        let mut wrong = Block::new("wrong", 0);
        wrong.add_opcode(&mut vm, Pop, None).unwrap();
        wrong.add_opcode(&mut vm, Ret, None).unwrap();

        vm.add_block(main);
        vm.add_block(wrong);

        let res = vm.new_fiber("main").unwrap().run();

        assert_eq!(res, Err(VMError::EmptyPop))
    }
}
