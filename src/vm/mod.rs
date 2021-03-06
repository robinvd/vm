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
    current_f: FState<'a>,
    value_stack: Vec<Value>,
    local_stack: Vec<Value>,
    all_gc: LinkedList<GCObject>,
}

/// a single function,
/// it has its own labels,
/// it shares a stack
///
/// The struct only contains 'stack' values
/// so that no allocations have to happen by creating a new stack frame
/// (except for growing both stacks)
#[derive(Clone, Debug)]
pub struct FState<'a> {
    current_block: &'a Block,
    code_ptr: usize,
    stack_start: usize,
    local_stack_start: usize,
}

impl<'a> FState<'a> {
    /// Create a new FState
    ///
    /// The last two opcodes in the code block should both be Ret or Halt
    /// This makes sure we dont access the code buffer out of bounds
    /// One would also be enough if we know that the second last opcode
    ///     is not an argument
    ///
    /// This is always the case in a valid block
    /// This function assumes it is given a valid block
    pub fn new(current_block: &'a Block, stack_start: usize, local_stack_start: usize) -> Self {
        Self {
            current_block,
            code_ptr: 0,
            stack_start,
            local_stack_start,
        }
    }

    /// Get the next opcode.
    ///
    /// This function is safe because we check at jumps that it is a valid loc
    /// And (TODO) we check that a module always ends with a halt/ret instruction
    ///
    /// The unchecked conversion into an opcode should be safe as it is al internal
    pub fn advance_opcode(&mut self) -> Opcode {
        unsafe {
            let op = *self.current_block.code.get_unchecked(self.code_ptr);
            self.code_ptr += 1;
            Opcode::from_u8_unchecked(op)
        }
    }

    /// Get a u16 literal from the code buffer
    ///
    /// See advance_opcode and new for safety explanation
    pub fn advance_u16(&mut self) -> u16 {
        let num = unsafe {
            (*self.current_block.code.get_unchecked(self.code_ptr) as u16) << 8
                | *self.current_block.code.get_unchecked(self.code_ptr + 1) as u16
        };
        self.code_ptr += 2;

        num
    }

    pub fn jump_to_label_index(&mut self, index: usize) -> Result<(), VMError> {
        let label = &self.current_block.label_index[index];
        let new_code_ptr = self.current_block.labels[label];
        if new_code_ptr >= self.current_block.code.len() {
            return Err(VMError::RuntimeError("jmp outside of code".to_owned()));
        }
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
        let local_n = f.current_block.local_n;
        Self {
            base,
            f: Vec::new(),
            current_f: f,
            value_stack: Vec::default(),
            local_stack: vec![Value::nil(); local_n],
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

        for v in &self.local_stack {
            v.traverse(&mut f)
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
        if self.value_stack.len() <= self.current_f.stack_start {
            return Err(VMError::EmptyPop);
        }
        self.value_stack.pop().ok_or(VMError::EmptyPop)
    }

    pub fn push_frame(&mut self, f_index: usize) -> Result<(), VMError> {
        let block_name = &self.base.fn_label_index[f_index];
        let block_index = *self
            .base
            .fn_labels
            .get(block_name)
            .ok_or_else(|| VMError::FunctionNotFound(block_name.to_owned()))?;
        let new_block = &self.base.blocks[block_index];
        let f = FState::new(
            new_block,
            self.value_stack.len() - new_block.n_args,
            self.local_stack.len(),
        );

        let old_len = self.local_stack.len();
        self.local_stack
            .resize(old_len + new_block.local_n, Value::nil());

        self.f.push(self.current_f.clone());
        self.current_f = f;

        Ok(())
    }

    pub fn pop_frame(&mut self) {
        let old = &self.current_f;
        let ret_val = if old.stack_start < self.value_stack.len() {
            *self.value_stack.last().unwrap()
        } else {
            Value::Nil
        };
        self.value_stack.truncate(old.stack_start);
        self.local_stack.truncate(old.local_stack_start);
        self.value_stack.push(ret_val);

        self.current_f = self.f.pop().unwrap();
    }

    pub fn step(&mut self) -> Result<(), VMError> {
        let instr = self.current_f.advance_opcode();
        match instr {
            Opcode::Halt => return Err(VMError::Halt),
            Opcode::Const => {
                let x = self.current_f.advance_u16() as usize;
                let c = self.current_f.current_block.constants[x];
                let new = self.deep_clone(c);
                self.push(new);
            }
            Opcode::Nil => {
                self.push(Value::Nil);
            }
            Opcode::Num => {
                let x = self.current_f.advance_u16() as f64;
                self.push(Value::number(x));
            }
            Opcode::True => {
                self.push(Value::True);
            }
            Opcode::False => {
                self.push(Value::False);
            }
            Opcode::Copy => {
                let x = self.current_f.advance_u16() as usize;
                self.push(self.value_stack[self.value_stack.len() - 1 - x]);
            }
            Opcode::Load => {
                let x = self.current_f.advance_u16() as usize;
                // let val = self.current_f().locals[x];
                let start = self.current_f.local_stack_start;
                let val = self.local_stack[start + x];
                self.push(val);
            }
            Opcode::Store => {
                let x = self.current_f.advance_u16() as usize;
                // self.current_f().locals[x] = self.pop()?;
                let start = self.current_f.local_stack_start;
                self.local_stack[start + x] = self.pop()?;
            }
            Opcode::Call => {
                let x = self.current_f.advance_u16();
                self.push_frame(x as usize)?;
            }
            Opcode::CallForeign => {
                let f_index = self.current_f.advance_u16();
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
                let x = self.current_f.advance_u16();
                self.current_f.jump_to_label_index(x as usize)?;
            }
            Opcode::JmpT => {
                let label_index = self.current_f.advance_u16();
                let cond = self.pop()?;
                if cond.is_true() {
                    self.current_f.jump_to_label_index(label_index as usize)?;
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
            Opcode::End => panic!("reached end of block"),
        };
        Ok(())
    }

    pub fn trace(&mut self) -> Result<(), VMError> {
        Ok(())
    }

    pub fn debug_run(&mut self) -> Result<Value, VMError> {
        loop {
            println!("stack: {:?}", self.value_stack);
            println!("locals: {:?}", self.local_stack);
            println!(
                "code: {} {:04x}",
                self.current_f.current_block.name, self.current_f.code_ptr
            );
            let mut input = String::new();
            io::stdin().read_line(&mut input).unwrap();
            match self.step() {
                Ok(_) => continue,
                Err(VMError::Halt) => return Ok(self.pop()?),
                Err(e) => return Err(e),
            }
        }
    }

    pub fn run(&mut self) -> Result<Value, VMError> {
        loop {
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
        if opcode.has_arg() != arg.is_some() {
            return Err(VMError::Msg("opcode/arg do not match".to_owned()));
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

    pub fn add_finishing_opcode(&mut self, vm: &mut VM) {
        self.add_opcode(vm, Opcode::End, None)
            .expect("internal error");
        self.add_opcode(vm, Opcode::End, None)
            .expect("internal error");
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
            blocks: Vec::default(),

            fn_labels: HashMap::new(),
            fn_label_index: Vec::new(),
            fn_foreign: Vec::new(),
            fn_foreign_labels: HashMap::new(),

            output: Arc::new(Mutex::new(output)),

            debug: false,
        }
    }

    pub fn new_fiber<'b>(&'b self, label: &str) -> Result<Fiber<'b, 'a>, VMError> {
        // todo hangle args
        let fs = FState::new(&self.blocks[self.fn_labels[label]], 0, 0);
        let fiber = Fiber::new(self, fs);

        Ok(fiber)
    }

    pub fn add_block(&mut self, mut block: Block) {
        block.add_finishing_opcode(self);

        unsafe { self.add_block_unchecked(block) };
    }

    pub unsafe fn add_block_unchecked(&mut self, block: Block) {
        let pos = self.blocks.len();
        let name = block.name.clone();
        self.blocks.push(block);
        self.fn_labels.insert(name, pos);
    }

    pub fn add_start(&mut self) {
        let mut block = Block::new("start", 0);
        block
            .add_opcode(self, Opcode::Num, Some(&Arg::Int(0)))
            .expect("internal error");
        block
            .add_opcode(self, Opcode::Call, Some(&Arg::Text("main")))
            .expect("internal error");
        block
            .add_opcode(self, Opcode::Halt, None)
            .expect("internal error");

        self.add_block(block);
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
        unsafe { self.add_block_unchecked(bl) };
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
            unsafe { vm.add_block_unchecked(bl) };
        }

        fn vm_floor(fiber: &mut Fiber) {
            let val = fiber.pop().unwrap();
            fiber.push(val.unary_op(|x| x.floor()).unwrap());
        }

        fn vm_ceil(fiber: &mut Fiber) {
            let val = fiber.pop().unwrap();
            fiber.push(val.unary_op(|x| x.ceil()).unwrap());
        }

        fn vm_pow(fiber: &mut Fiber) {
            let b = fiber.pop().unwrap();
            let a = fiber.pop().unwrap();
            fiber.push(Value::binary_op(a, b, |a, b| Value::number(a.powf(b))).unwrap());
        }

        register_basic_instr(self, "add", 2, &[Opcode::Add]);
        register_basic_instr(self, "mul", 2, &[Opcode::Mul]);
        register_basic_instr(self, "div", 2, &[Opcode::Div]);
        register_basic_instr(self, "neg", 1, &[Opcode::Neg]);

        register_basic_instr(self, "not", 1, &[Opcode::Not]);
        register_basic_instr(self, "leq", 2, &[Opcode::LEQ]);
        register_basic_instr(self, "geq", 2, &[Opcode::GEQ]);

        register_basic_instr(self, "print", 1, &[Opcode::Print]);

        self.register_foreign("floor", vm_floor, 1);
        self.register_foreign("ceil", vm_ceil, 1);
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

        compiler::compile(&mut vm, &parsed.0).expect("failed to compile");
        vm.add_start();

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
        block.add_finishing_opcode(&mut vm);

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

    #[test]
    fn test_fiber_local_stack() {
        let buffer = Vec::new();
        let mut vm = VM::new(Box::new(buffer));

        let mut main = Block::new("main", 0);
        main.local_n = 1;
        main.add_opcode(&mut vm, Opcode::Num, Some(&Arg::Int(5)))
            .unwrap();
        main.add_opcode(&mut vm, Opcode::Store, Some(&Arg::Int(0)))
            .unwrap();
        main.add_opcode(&mut vm, Opcode::Call, Some(&Arg::Text("f")))
            .unwrap();
        main.add_opcode(&mut vm, Opcode::Halt, None).unwrap();
        main.add_finishing_opcode(&mut vm);

        let mut f = Block::new("f", 0);
        f.local_n = 1;
        f.add_opcode(&mut vm, Opcode::Nil, None).unwrap();
        f.add_opcode(&mut vm, Opcode::Num, Some(&Arg::Int(9)))
            .unwrap();
        f.add_opcode(&mut vm, Opcode::Store, Some(&Arg::Int(0)))
            .unwrap();
        f.add_opcode(&mut vm, Opcode::Ret, None).unwrap();
        f.add_finishing_opcode(&mut vm);

        vm.add_block(f);
        vm.add_block(main);

        let mut fiber = vm.new_fiber("main").unwrap();
        let res = fiber.run();
        assert_eq!(res, Ok(Value::nil()));

        // the main function has 1 local so when that function halts there should again be 1 local
        // as the locals from f should be popped off
        assert_eq!(fiber.local_stack.len(), 1);
    }

}
