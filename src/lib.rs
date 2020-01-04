use std::collections::{HashMap, HashSet, LinkedList};
use std::sync::{Arc, Mutex};
use std::{fmt, io};

pub mod opcode;
pub mod value;

use self::opcode::{Instruction, Opcode};
use self::value::*;

#[derive(Clone, Debug, PartialEq)]
pub enum VMError {
    Halt,
    RuntimeError(String),
    ParseErr(String),
    FunctionNotFound(String),
    WrongOpCode(u8),
    EmptyPop,
    CombineErr(String),
    TypeError(String),
    StructNotFound(u16),
    Msg(String),
    DifferentNArgs,
    NotIndexable,
    InvalidConversion,
}

pub struct VMStruct {
    pub items: Vec<String>,
}

pub struct VMStructId(pub u32);

// static (read only) and state part

// state:

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
    fn new(current_block: &'a Block, stack_start: usize, local_stack_start: usize) -> Self {
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
    fn advance_opcode(&mut self) -> Opcode {
        unsafe {
            let op = *self.current_block.code.get_unchecked(self.code_ptr);
            self.code_ptr += 1;
            Opcode::from_u8_unchecked(op)
        }
    }

    /// Get a u16 literal from the code buffer
    ///
    /// See advance_opcode and new for safety explanation
    fn advance_u16(&mut self) -> u16 {
        let num = unsafe {
            (*self.current_block.code.get_unchecked(self.code_ptr) as u16) << 8
                | *self.current_block.code.get_unchecked(self.code_ptr + 1) as u16
        };
        self.code_ptr += 2;

        num
    }

    fn jump_to_label_index(&mut self, index: usize) -> Result<(), VMError> {
        let new_code_ptr = self.current_block.label_index[index];

        if new_code_ptr as usize >= self.current_block.code.len() {
            return Err(VMError::RuntimeError("jmp outside of code".to_owned()));
        }
        self.code_ptr = new_code_ptr as usize;

        Ok(())
    }

    fn jump_to_addr(&mut self, index: usize) {
        self.code_ptr = index;
    }
}

/// a single thread
///
/// a fiber has its own stack
pub struct Fiber<'a, 'write> {
    base: &'a VM<'write>,
    f: Vec<FState<'a>>,
    current_f: FState<'a>,

    value_stack: Vec<Value>,
    local_stack: Vec<Value>,
    heap: Vec<ObjectInfo>,
    free: Vec<usize>,

    // TODO remove
    all_gc: LinkedList<GCObject>,
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
    fn new(base: &'a VM<'write>, f: FState<'a>) -> Self {
        let local_n = f.current_block.local_n;
        Self {
            base,
            f: Vec::new(),
            current_f: f,
            value_stack: Vec::default(),
            local_stack: vec![Value::nil(); local_n],
            heap: Vec::new(),
            free: Vec::new(),
            all_gc: LinkedList::new(),
        }
    }

    fn heap_alloc(&mut self) -> HeapRef {
        let loc = if let Some(loc) = self.free.pop() {
            self.heap[loc].alive = true;
            loc
        } else {
            let loc = self.heap.len();
            let mut new = ObjectInfo::new(Object::Empty);
            new.alive = true;
            self.heap.push(new);

            loc
        };

        HeapRef { loc }
    }

    fn heap_mut_ref(&mut self, heap_ref: HeapRef) -> &mut Object {
        &mut self.heap[heap_ref.loc].val
    }

    fn deep_clone(&mut self, val: Value) -> Value {
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
        // self.all_gc.len()
        self.heap.len() - self.free.len()
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

        for (i, item) in self.heap.iter_mut().enumerate() {
            if !item.mark.get() {
                item.alive = false;
                item.val = Object::Empty;
                self.free.push(i);
            }
        }

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

    pub fn peek(&mut self) -> Result<Value, VMError> {
        if self.value_stack.len() <= self.current_f.stack_start {
            return Err(VMError::EmptyPop);
        }
        self.value_stack.last().ok_or(VMError::EmptyPop).map(|x| *x)
    }

    pub fn push_frame(&mut self, f_index: usize) -> Result<(), VMError> {
        let new_block = &self.base.blocks[f_index];
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

    fn pop_frame(&mut self) {
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

    fn step(&mut self) -> Result<(), VMError> {
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
            Opcode::Sub => {
                let a = self.pop()?;
                let b = self.pop()?;
                self.push(Value::binary_op(a, b, |a, b| Value::number(b - a))?);
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
            Opcode::JmpDirect => {
                let x = self.current_f.advance_u16();
                self.current_f.jump_to_addr(x as usize);
            }
            Opcode::JmpTDirect => {
                let label_index = self.current_f.advance_u16();
                let cond = self.pop()?;
                if cond.is_true() {
                    self.current_f.jump_to_addr(label_index as usize);
                }
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
            Opcode::EmptyMap => unimplemented!(),
            Opcode::EmptyList => {
                let heap_ref = self.heap_alloc();
                let ptr = self.heap_mut_ref(heap_ref);

                *ptr = Object::List(Vec::new());
                self.push(Value::Heap(heap_ref));
            }
            Opcode::PushList => {
                let val = self.pop()?;
                let list_ref = self
                    .peek()?
                    .as_object()
                    .ok_or_else(|| VMError::TypeError("List/Object".to_owned()))?;

                let obj = self.heap_mut_ref(list_ref);

                if let Object::List(vec) = obj {
                    vec.push(val);
                } else {
                    return Err(VMError::TypeError("List".to_owned()));
                }
            }
            Opcode::PopList => {
                let list_ref = self
                    .peek()?
                    .as_object()
                    .ok_or_else(|| VMError::TypeError("List/Object".to_owned()))?;

                let obj = self.heap_mut_ref(list_ref);

                if let Object::List(vec) = obj {
                    let val = vec.pop().unwrap_or(Value::nil());
                    self.push(val);
                } else {
                    return Err(VMError::TypeError("List".to_owned()));
                }
            }
            Opcode::New => {
                let heap_ref = self.heap_alloc();
                let struct_id = self.current_f.advance_u16();
                let struct_info = self
                    .base
                    .structs
                    .get(struct_id as usize)
                    .ok_or(VMError::StructNotFound(struct_id))?;

                let ptr = self.heap_mut_ref(heap_ref);
                *ptr = Object::Struct(vec![Value::nil(); struct_info.items.len()]);

                self.push(Value::Heap(heap_ref));
            }
            Opcode::SetAttr => {
                let item_loc = self.current_f.advance_u16();

                let item = self.pop()?;
                let struct_ref = self
                    .peek()?
                    .as_object()
                    .ok_or_else(|| VMError::TypeError("Struct/Object".to_owned()))?;

                let obj = self.heap_mut_ref(struct_ref);

                if let Object::Struct(vec) = obj {
                    vec[item_loc as usize] = item
                } else {
                    return Err(VMError::TypeError("Struct".to_owned()));
                }
            }
            Opcode::GetAttr => {
                let item_loc = self.current_f.advance_u16();

                let struct_ref = self
                    .peek()?
                    .as_object()
                    .ok_or_else(|| VMError::TypeError("Struct/Object".to_owned()))?;

                let obj = self.heap_mut_ref(struct_ref);

                if let Object::Struct(vec) = obj {
                    let item = vec[item_loc as usize];
                    self.push(item);
                } else {
                    return Err(VMError::TypeError("Struct".to_owned()));
                }
            }
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
    name: String,
    code: Vec<u8>,

    label_index: Vec<u16>,

    constants: Vec<Value>,

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
        writeln!(f, "labeli {:?}", self.label_index);
        writeln!(f, "const  {:?}", self.constants);

        Ok(())
    }
}

pub struct Label(pub u16);

struct CodeTraverse {
    code_ptr: usize,
    last_opcode: usize,
}

impl CodeTraverse {
    fn new() -> Self {
        Self {
            code_ptr: 0,
            last_opcode: 0,
        }
    }

    fn advance_opcode(&mut self, block: &Block) -> Opcode {
        let op = block.code[self.code_ptr];
        self.last_opcode = self.code_ptr;
        self.code_ptr += 1;
        Opcode::from_u8(op).expect("internal error")
    }

    fn advance_u16(&mut self, block: &Block) -> u16 {
        let num = (block.code[self.code_ptr] as u16) << 8 | block.code[self.code_ptr + 1] as u16;
        self.code_ptr += 2;

        num
    }

    fn set_last_opcode(&mut self, block: &mut Block, op: Opcode) {
        block.code[self.last_opcode] = op as u8;
    }

    fn set_last_arg(&mut self, block: &mut Block, arg: u16) {
        if self.code_ptr == self.last_opcode {
            panic!("current addr is not an argument");
        }
        block.code[self.code_ptr - 2] = (arg >> 8) as u8;
        block.code[self.code_ptr - 1] = arg as u8;
    }
}

impl Block {
    pub fn new(name: impl Into<String>, n_args: usize) -> Self {
        Self {
            name: name.into(),
            code: Vec::default(),

            label_index: Default::default(),

            constants: Default::default(),

            n_args,
            local_n: 0,
        }
    }

    fn add_opcode(&mut self, opcode: Opcode, arg: Option<u16>) -> Result<(), VMError> {
        if opcode.has_arg() != arg.is_some() {
            return Err(VMError::Msg("opcode/arg do not match".to_owned()));
        }

        self.code.push(opcode as u8);

        if let Some(a) = arg {
            let num = a;
            self.code.push((num >> 8) as u8);
            self.code.push(num as u8);
        };

        Ok(())
    }

    pub fn add_instruction(&mut self, instr: Instruction) {
        let (opcode, arg) = instr.to_opcode();
        self.add_opcode(opcode, arg).unwrap();
    }

    /// Finish this block
    fn finish(&mut self, vm: &mut VM) {
        self.add_opcode(Opcode::End, None).expect("internal error");
        self.add_opcode(Opcode::End, None).expect("internal error");

        let mut f = CodeTraverse::new();

        loop {
            let op = f.advance_opcode(self);
            match op {
                Opcode::End => break,
                Opcode::Jmp => {
                    let label_index = f.advance_u16(self);
                    let absolute_addr = self.label_index[label_index as usize];
                    f.set_last_opcode(self, Opcode::JmpDirect);
                    f.set_last_arg(self, absolute_addr);
                }
                Opcode::JmpT => {
                    let label_index = f.advance_u16(self);
                    let absolute_addr = self.label_index[label_index as usize];
                    f.set_last_opcode(self, Opcode::JmpTDirect);
                    f.set_last_arg(self, absolute_addr);
                }
                op if op.has_arg() => {
                    f.advance_u16(self);
                }
                _ => continue,
            };
        }
    }

    pub fn add_data(&mut self, data: Value) -> usize {
        let i = self.constants.len();
        self.constants.push(data);
        i
    }

    /// Add a label to the current location
    /// a jump to this label jumps to the instruction
    /// after the label
    pub fn add_label(&mut self) -> Label {
        let opcode_index = self.code.len();
        let new_label_index = self.label_index.len();
        self.label_index.push(opcode_index as u16);

        Label(new_label_index as u16)
    }

    /// Reserve a label,
    /// Like add_label, but dont add the label yet
    /// This has to be done later by place_label
    ///
    /// This is useful when you want to jump to a label that will be defined later
    pub fn reserve_label(&mut self) -> Label {
        let new_label_index = self.label_index.len();
        self.label_index.push(1 << 15);

        Label(new_label_index as u16)
    }

    pub fn place_label(&mut self, l: Label) {
        if self.label_index[l.0 as usize] != 1 << 15 {
            panic!("place_label: label already placed");
        }

        self.label_index[l.0 as usize] = self.code.len() as u16;

        // explicitly drop the label for clippy lints
        drop(l)
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

        for b in &self.blocks {
            writeln!(f, "{:?}", b)?;
        }

        Ok(())
    }
}

pub struct VM<'write> {
    blocks: Vec<Block>,
    fn_labels: HashMap<String, u16>,
    structs: Vec<VMStruct>,

    fn_foreign: Vec<fn(&mut Fiber)>,

    output: Arc<Mutex<Box<io::Write + 'write>>>,

    pub debug: bool,
}

impl<'a> VM<'a> {
    pub fn new(output: Box<io::Write + 'a + Sync>) -> Self {
        Self {
            blocks: Vec::default(),

            fn_labels: HashMap::new(),
            fn_foreign: Vec::new(),
            structs: Vec::new(),

            output: Arc::new(Mutex::new(output)),

            debug: false,
        }
    }

    pub fn new_fiber<'b>(&'b self, label: &str) -> Result<Fiber<'b, 'a>, VMError> {
        // todo hangle args
        let fs = FState::new(&self.blocks[self.fn_labels[label] as usize], 0, 0);
        let fiber = Fiber::new(self, fs);

        Ok(fiber)
    }

    pub fn get_block_loc(&self, n: &str) -> Option<u16> {
        self.fn_labels.get(n).cloned()
    }

    pub fn add_block(&mut self, mut block: Block) -> u16 {
        block.finish(self);

        let pos = self.blocks.len();
        let name = block.name.clone();
        self.blocks.push(block);
        self.fn_labels.insert(name, pos as u16);
        pos as u16
    }

    pub fn reserve_block(&mut self, n: &str) -> u16 {
        let pos = self.blocks.len();
        self.blocks.push(Block::new(n, 0));
        self.fn_labels.insert(n.to_owned(), pos as u16);

        pos as u16
    }

    pub fn place_block(&mut self, index: u16, mut block: Block) {
        block.finish(self);
        self.blocks[index as usize] = block;
    }

    pub fn add_struct(&mut self, item: VMStruct) -> VMStructId {
        let id = self.structs.len();
        self.structs.push(item);

        VMStructId(id as u32)
    }

    pub fn add_start(&mut self) {
        let mut block = Block::new("start", 0);
        block
            .add_opcode(Opcode::Num, Some(0))
            .expect("internal error");

        let main_loc = *self
            .fn_labels
            .get("main")
            .expect("could not find 'main' function");
        block
            .add_opcode(Opcode::Call, Some(main_loc as u16))
            .expect("internal error");
        block
            .add_opcode(Opcode::Halt, None)
            .expect("internal error");

        self.add_block(block);
    }

    pub fn register_foreign(&mut self, name: &str, f: fn(&mut Fiber), n_args: usize) {
        let index = self.fn_foreign.len();
        self.fn_foreign.push(f);
        // self.fn_foreign_labels.insert(name.to_owned(), index);

        let mut bl = Block::new(name, n_args);
        bl.add_opcode(Opcode::CallForeign, Some(index as u16))
            .unwrap();
        bl.add_opcode(Opcode::Ret, None).unwrap();
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
            ops.iter().for_each(|op| bl.add_opcode(*op, None).unwrap());
            bl.add_opcode(Opcode::Ret, None).unwrap();
            vm.add_block(bl);
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

        fn vm_push(fiber: &mut Fiber) {
            unimplemented!()
            // let val = fiber.pop().unwrap();
            // let list = fiber.pop().unwrap();

            // if let Some(Object::List(lvec)) = list.get_object() {
            //     let vec = lvec.lock().unwrap();
            //     vec.push(val)
            // }
        }

        register_basic_instr(self, "add", 2, &[Opcode::Add]);
        register_basic_instr(self, "sub", 2, &[Opcode::Sub]);
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
    use crate::Opcode::*;

    fn test_program(vm: &mut VM, instrs: &[Instruction]) -> Result<Value, VMError> {
        let mut block = Block::new("start", 0);

        for instr in instrs.iter() {
            block.add_instruction(*instr);
        }

        block.add_instruction(Instruction::Halt);
        block.finish(vm);
        vm.add_block(block);

        let mut f = vm.new_fiber("start").unwrap();
        let res = f.run();
        res
    }

    #[test]
    fn test_struct() {
        use Instruction::*;
        let mut buffer = Vec::new();
        let mut vm = VM::new(Box::new(buffer));

        let struct_id = vm.add_struct(VMStruct {
            items: vec!["x".to_owned()],
        });

        let res = test_program(
            &mut vm,
            &[New(struct_id.0 as u16), Num(42), SetAttr(0), GetAttr(0)],
        );

        assert_eq!(res, Ok(Value::number(42.0)))
    }

    #[test]
    fn test_list() {
        use Instruction::*;

        let mut buffer = Vec::new();
        let mut vm = VM::new(Box::new(buffer));

        let res = test_program(
            &mut vm,
            &[
                EmptyList,
                Num(42),
                PushList,
                Num(43),
                PushList,
                PopList,
                Pop,
                PopList,
            ],
        );

        assert_eq!(res, Ok(Value::number(42.0)))
    }

    #[test]
    fn test_gc() {
        let mut buffer = Vec::new();
        let mut vm = VM::new(Box::new(buffer));
        let mut block = Block::new("start", 0);

        block.add_opcode(Opcode::EmptyList, None).unwrap();
        block.add_opcode(Opcode::EmptyList, None).unwrap();
        block.add_opcode(Opcode::Pop, None).unwrap();
        block.add_opcode(Opcode::Halt, None).unwrap();

        block.finish(&mut vm);
        vm.add_block(block);

        let mut f = vm.new_fiber("start").unwrap();
        let res = f.run();
        assert_eq!(res.is_ok(), true);

        let start = f.gc_len();
        f.collect();
        let end = f.gc_len();
        assert_eq!(start > end, true);
    }

    fn test_instr(instr: Opcode, arg: Option<u16>, vals: &[Value], result: Value) {
        let buffer = Vec::new();
        let mut vm = VM::new(Box::new(buffer));
        let mut block = Block::new("test", 0);
        vals.iter().for_each(|x| {
            let i = block.add_data(*x);
            block.add_opcode(Opcode::Const, Some(i as u16)).unwrap();
            block.add_opcode(Opcode::Copy, Some(0)).unwrap();
            block.add_opcode(Opcode::Print, None).unwrap();
        });

        block.add_opcode(instr, arg).unwrap();
        block.add_opcode(Halt, None).unwrap();
        block.finish(&mut vm);

        vm.add_block(block);
        println!("{:?}", vm);
        let mut f = vm.new_fiber("test").unwrap();
        let vm_result = f.run();
        assert_eq!(vm_result, Ok(result));
    }

    #[test]
    fn test_copy() {
        test_instr(Copy, Some(0), &[Value::number(10.)], Value::number(10.));
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

        let mut wrong = Block::new("wrong", 0);
        wrong.add_opcode(Pop, None).unwrap();
        wrong.add_opcode(Ret, None).unwrap();
        let wrong_loc = vm.add_block(wrong);

        let mut main = Block::new("main", 0);
        main.add_opcode(Num, Some(21)).unwrap();
        main.add_opcode(Call, Some(wrong_loc)).unwrap();
        main.add_opcode(Ret, None).unwrap();

        vm.add_block(main);

        let res = vm.new_fiber("main").unwrap().run();

        assert_eq!(res, Err(VMError::EmptyPop))
    }

    #[test]
    fn test_fiber_local_stack() {
        let buffer = Vec::new();
        let mut vm = VM::new(Box::new(buffer));

        let mut f = Block::new("f", 0);
        f.local_n = 1;
        f.add_opcode(Opcode::Nil, None).unwrap();
        f.add_opcode(Opcode::Num, Some(9)).unwrap();
        f.add_opcode(Opcode::Store, Some(0)).unwrap();
        f.add_opcode(Opcode::Ret, None).unwrap();
        f.finish(&mut vm);

        let f_loc = vm.add_block(f);

        let mut main = Block::new("main", 0);
        main.local_n = 1;
        main.add_opcode(Opcode::Num, Some(5)).unwrap();
        main.add_opcode(Opcode::Store, Some(0)).unwrap();
        main.add_opcode(Opcode::Call, Some(f_loc)).unwrap();
        main.add_opcode(Opcode::Halt, None).unwrap();
        main.finish(&mut vm);

        vm.add_block(main);

        let mut fiber = vm.new_fiber("main").unwrap();
        let res = fiber.run();
        assert_eq!(res, Ok(Value::nil()));

        // the main function has 1 local so when that function halts there should again be 1 local
        // as the locals from f should be popped off
        assert_eq!(fiber.local_stack.len(), 1);
    }
}
