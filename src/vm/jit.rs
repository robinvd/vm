use std::collections::HashMap;

use cranelift::prelude::*;
use cranelift_codegen::ir::entities::{FuncRef, SigRef};
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};
use std::slice;

use crate::vm::{
    trace::{Trace, TraceItem},
    VMError,
};

#[derive(Debug, Clone, Copy)]
enum StructVal {
    Untyped(Value, Value),
    Typed(Value, Type),
    Nil,
}

pub extern "C" fn as_number(val_mem: (i64,i64)) -> (i64, f64) {
    let val = unsafe { std::mem::transmute::<_, super::Value>(val_mem) };
    // println!("as_number arg: {:?}", unsafe {std::mem::transmute::<_, (u64, f64)>(val)});
    if let Some(v) = val.as_number() {
        (true as i64, v)
    } else {
        panic!("TODO handle wrong conversions");
        (false as i64, 0.0)
    }
}

pub extern "C" fn to_number(val: f64) -> (u64, u64) {
    // println!("to_number arg: {}", val);
    let val = super::Value::number(val);
    unsafe { std::mem::transmute(val) }
}

pub extern "C" fn is_number(val_mem: (u64, u64)) -> u64 {
    // println!("is_number start");
    let val = unsafe { std::mem::transmute::<_, super::Value>(val_mem) };
    // println!("is_number arg: {}", val);
    if val.as_number().is_some() {
        1
    } else {
        0
    }
}

pub extern "C" fn print_val(val_mem: (u64, u64)) {
    // println!("print_val start {:?}", val_mem);
    let val = unsafe { std::mem::transmute::<_, super::Value>(val_mem) };
    println!("print_val: {}", val);
}

pub extern "C" fn fiber_push(fiber: &mut super::Fiber, val_a: i64, val_b: i64) {
    // println!(
    //     "fiber_push({:04x},{:04x},{:04x}), old_size: {}",
    //     fiber as *const _ as usize, val_a, val_b, fiber.value_stack.len()
    // );
    let val = unsafe { std::mem::transmute::<_, super::Value>((val_a, val_b)) };
    // println!("val: {:?}", val);
    fiber.push(val);
}

pub extern "C" fn fiber_pop(fiber: &mut super::Fiber) -> (u64, u64) {
    // println!(
    //     "fiber_pop, stack_size before: {} - {}",
    //     fiber.value_stack.len(),
    //     fiber.current_f.stack_start
    // );
    let val = fiber.pop().expect("fiber_pop jit failed");
    unsafe { std::mem::transmute::<_, (u64, u64)>(val) }
}

pub extern "C" fn fiber_set_instr(fiber: &mut super::Fiber, instr_n: u64) {

}

pub extern "C" fn as_bool(val_mem: (u64, u64)) -> (i64, bool) {
    let val = unsafe { std::mem::transmute::<_, super::Value>(val_mem) };
    // println!("as_number arg: {:?}:{}", val as *const _, val);
    if let Some(v) = val.as_bool() {
        (true as i64, v)
    } else {
        (false as i64, false)
    }
}

pub extern "C" fn call_foreign(
    fiber: &mut super::Fiber,
    f: extern "C" fn(&mut super::Fiber),
    n_args: usize,
) {
    fiber.call_foreign(f, n_args);
}

/// The basic JIT class.
pub(crate) struct JIT {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The data context, which is to data objects what `ctx` is to functions.
    data_ctx: DataContext,

    /// The module, with the simplejit backend, which manages the JIT'd
    /// functions.
    module: Module<SimpleJITBackend>,
}

impl JIT {
    pub fn new() -> JIT {
        // Windows calling conventions are not supported yet.
        if cfg!(windows) {
            unimplemented!();
        }

        let mut builder = SimpleJITBuilder::new();
        let external_fn = &[
            ("as_number", as_number as *const u8),
            ("to_number", to_number as *const u8),
            ("is_number", is_number as *const u8),
            ("as_bool", as_bool as *const u8),
            ("print_value", print_val as *const u8),
            ("fiber_push", fiber_push as *const u8),
            ("fiber_pop", fiber_pop as *const u8),
            ("call_foreign", call_foreign as *const u8),
        ];
        for (n, f) in external_fn {
            builder.symbol(*n, *f);
        }

        let module = Module::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
        }
    }

    fn foreign(
        module: &mut Module<SimpleJITBackend>,
        builder: &mut FunctionBuilder,
        name: &str,
        args: &[Type],
        ret: &[Type],
    ) -> FuncRef {
        let mut sig = module.make_signature();
        for a in args {
            sig.params.push(AbiParam::new(*a));
        }
        for t in ret {
            sig.returns.push(AbiParam::new(*t));
        }

        let callee = module
            .declare_function(name, Linkage::Import, &sig)
            .expect("problem declaring function");
        module.declare_func_in_func(callee, builder.func)
    }

    pub fn translate(&mut self, trace: &Trace) -> Result<usize, VMError> {
        let ptr_t = self.module.pointer_type();
        let bool_t = cranelift_codegen::ir::types::I32;
        let number_t = cranelift_codegen::ir::types::F64;

        self.ctx.func.signature.params.push(AbiParam::new(ptr_t));
        self.ctx.func.signature.params.push(AbiParam::new(ptr_t));
        self.ctx.func.signature.params.push(AbiParam::new(ptr_t));
        self.ctx.func.signature.returns.push(AbiParam::new(ptr_t));

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let entry_ebb = builder.create_ebb();
        builder.append_ebb_params_for_function_params(entry_ebb);
        builder.switch_to_block(entry_ebb);
        builder.seal_block(entry_ebb);

        let loop_ebb = builder.create_ebb();
        builder.ins().jump(loop_ebb, &[]);
        builder.switch_to_block(loop_ebb);

        let mut ft = FunctionTranslator {
            number_t,
            ptr_t,
            bool_t,

            as_number: Self::foreign(
                &mut self.module,
                &mut builder,
                "as_number",
                &[ptr_t, ptr_t],
                &[ptr_t, number_t],
            ),
            to_number: Self::foreign(
                &mut self.module,
                &mut builder,
                "to_number",
                &[number_t],
                &[ptr_t, ptr_t],
            ),
            is_number: Self::foreign(
                &mut self.module,
                &mut builder,
                "is_number",
                &[ptr_t, ptr_t],
                &[bool_t],
            ),
            as_bool: Self::foreign(
                &mut self.module,
                &mut builder,
                "as_bool",
                &[ptr_t, ptr_t],
                &[ptr_t, bool_t],
            ),
            print_val: Self::foreign(
                &mut self.module,
                &mut builder,
                "print_value",
                &[ptr_t, ptr_t],
                &[],
            ),
            fiber_push: Self::foreign(
                &mut self.module,
                &mut builder,
                "fiber_push",
                &[ptr_t, ptr_t, ptr_t],
                &[],
            ),
            fiber_pop: Self::foreign(
                &mut self.module,
                &mut builder,
                "fiber_pop",
                &[ptr_t],
                &[ptr_t, ptr_t],
            ),
            call_foreign: Self::foreign(
                &mut self.module,
                &mut builder,
                "call_foreign",
                &[ptr_t, ptr_t, ptr_t],
                &[],
            ),

            value_stack: builder.ebb_params(entry_ebb)[0],
            local_stack: builder.ebb_params(entry_ebb)[1],
            fiber: builder.ebb_params(entry_ebb)[2],
            trace: trace,
            builder,
            stack: Vec::new(),
            module: &mut self.module,
            highest_local: 0,
        };

        for instr in &trace.buf {
            ft.translate_instr(instr.clone())
        }

        ft.builder.ins().jump(loop_ebb, &[]);

        ft.builder.seal_block(loop_ebb);

        ft.builder.finalize();

        if cfg!(debug_assertions) {
            println!("trace: {:?}", trace);
            println!("jit: {}", ft.builder.display(None));
        }

        Ok(ft.highest_local)
    }

    pub fn compile(&mut self, input: &Trace) -> Result<JitFunction, VMError> {
        let highest_local = self.translate(&input)?;
        let name = format!("{}:{}", input.block.name, input.loc);

        let id = self
            .module
            .declare_function(&name, Linkage::Export, &self.ctx.func.signature)
            .unwrap();

        self.module.define_function(id, &mut self.ctx).unwrap();
        // panic!("succes!");

        if cfg!(debug_assertions) {
            // println!("jit: {}", self.ctx.func.display(None));
        }

        self.module.clear_context(&mut self.ctx);

        self.module.finalize_definitions();

        let code = self.module.get_finalized_function(id);

        Ok(JitFunction{
            code_ptr: code,
            local_stack_space: highest_local,
        })
    }
}

pub struct JitFunction{
    pub code_ptr: *const u8,
    pub local_stack_space: usize,
}

struct FunctionTranslator<'a, 'write> {
    // types
    number_t: types::Type,
    ptr_t: types::Type,
    bool_t: types::Type,

    // host functions
    as_number: FuncRef,
    to_number: FuncRef,
    is_number: FuncRef,
    as_bool: FuncRef,
    print_val: FuncRef,
    fiber_push: FuncRef,
    fiber_pop: FuncRef,
    call_foreign: FuncRef,

    // host structs
    local_stack: Value,
    value_stack: Value,
    fiber: Value,

    trace: &'a Trace<'a, 'write>,

    // builders
    stack: Vec<StructVal>,
    builder: FunctionBuilder<'a>,
    module: &'a mut Module<SimpleJITBackend>,

    // stats
    highest_local: usize,
}

impl<'a, 'write> FunctionTranslator<'a, 'write> {
    fn foreign_signature(&mut self) -> SigRef {
        let mut sig = self.module.make_signature();
        // sig.params.push(AbiParam::new(self.ptr_t));
        // sig.params.push(AbiParam::new(self.ptr_t));
        sig.params.push(AbiParam::new(self.ptr_t));

        self.builder.import_signature(sig)
    }

    fn to_value(&mut self, s_val: StructVal) -> (Value, Value) {
        match s_val {
            StructVal::Typed(un_val, ty) => {
                let f = if ty == self.number_t {
                    self.to_number
                } else {
                    println!("not implemented {}", ty);
                    unimplemented!()
                };

                let call = self.builder.ins().call(f, &[un_val]);
                (
                    self.builder.inst_results(call)[0],
                    self.builder.inst_results(call)[1],
                )
            }
            StructVal::Untyped(a, b) => (a, b),
            StructVal::Nil => {
                println!("experimental nil get");
                // let call = self.builder.ins().call(self.get_nil, &[]);
                // (
                //     self.builder.inst_results(call)[0],
                //     self.builder.inst_results(call)[1],
                // )
                let (a, b): (i64, i64) = unsafe { std::mem::transmute(super::Value::nil()) };
                (
                    self.builder.ins().iconst(self.ptr_t, a),
                    self.builder.ins().iconst(self.ptr_t, b),
                )
            }
        }
    }

    pub fn get_typed(&mut self, s_val: StructVal, ty1: Type) -> Value {
        match s_val {
            StructVal::Typed(v, ty2) if ty1 == ty2 => v,
            StructVal::Untyped(n, v) => {
                let f = if ty1 == self.number_t {
                    self.as_number
                } else if ty1 == self.bool_t {
                    self.as_bool
                } else {
                    unimplemented!()
                };
                let call = self.builder.ins().call(f, &[n, v]);
                let success = self.builder.inst_results(call)[0];
                let val = self.builder.inst_results(call)[1];

                // TODO! check succes with a guard
                val
            }
            StructVal::Typed(_, ty2) => panic!(format!(
                "(internal) typed mismatch, expected {}, but got {}",
                ty1, ty2
            )),
            StructVal::Nil => panic!("get_typed: cannot get typed version of nil"),
        }
    }

    fn translate_instr(&mut self, instr: TraceItem) {
        match instr {
            TraceItem::FAdd => {
                let b_s = self.stack.pop().unwrap();
                let b = self.get_typed(b_s, self.number_t);
                let a_s = self.stack.pop().unwrap();
                let a = self.get_typed(a_s, self.number_t);

                let new = self.builder.ins().fadd(a, b);
                self.stack.push(StructVal::Typed(new, self.number_t));
            }
            TraceItem::FSub => unimplemented!(),
            TraceItem::FDiv => {
                let b_s = self.stack.pop().unwrap();
                let b = self.get_typed(b_s, self.number_t);
                let a_s = self.stack.pop().unwrap();
                let a = self.get_typed(a_s, self.number_t);
                let new = self.builder.ins().fdiv(a, b);
                self.stack.push(StructVal::Typed(new, self.number_t));
            }
            TraceItem::FMul => unimplemented!(),
            TraceItem::FNeg => {
                let a_s = self.stack.pop().unwrap();
                let a = self.get_typed(a_s, self.number_t);

                let new = self.builder.ins().fneg(a);
                self.stack.push(StructVal::Typed(new, self.number_t));
            }
            TraceItem::LEQ => {
                let b_s = self.stack.pop().unwrap();
                let b = self.get_typed(b_s, self.number_t);
                let a_s = self.stack.pop().unwrap();
                let a = self.get_typed(a_s, self.number_t);

                let new = self.builder.ins().fcmp(FloatCC::LessThanOrEqual, a, b);
                // let new = self.builder.ins().bextend(cranelift_codegen::ir::types::B8, new);
                let new = self.builder.ins().bint(self.bool_t, new);
                self.stack.push(StructVal::Typed(new, self.bool_t));
            }
            TraceItem::GEQ => {
                let b_s = self.stack.pop().unwrap();
                let b = self.get_typed(b_s, self.number_t);
                let a_s = self.stack.pop().unwrap();
                let a = self.get_typed(a_s, self.number_t);

                let new = self.builder.ins().fcmp(FloatCC::GreaterThanOrEqual, a, b);
                let new = self.builder.ins().bint(self.bool_t, new);
                self.stack.push(StructVal::Typed(new, self.bool_t));
            }
            TraceItem::Not => {
                let a_s = self.stack.pop().unwrap();
                let a = self.get_typed(a_s, self.bool_t);
                let new = self.builder.ins().bxor_imm(a, 1);
                // // let new_int = self.builder.ins().bint(self.bool_t, new);
                self.stack.push(StructVal::Typed(new, self.bool_t));
            }
            TraceItem::Print => {
                let s_val = self.stack.pop().unwrap();
                let (a, b) = self.to_value(s_val);
                self.builder.ins().call(self.print_val, &[a, b]);
            }
            TraceItem::Guard(b, f_states) => {
                // create new blocks
                let exit_ebb = self.builder.create_ebb();
                let cond_ebb = self.builder.create_ebb();

                // get the cond, and jump to the correct block

                // pop or last?
                let cond_val = self.stack.pop().unwrap();
                let cond_val = self.get_typed(cond_val, self.bool_t);
                let cond_val = self.builder.ins().icmp_imm(IntCC::Equal, cond_val, 0);
                if b {
                    self.builder.ins().brnz(cond_val, exit_ebb, &[]);
                } else {
                    self.builder.ins().brz(cond_val, exit_ebb, &[]);
                }
                self.builder.ins().jump(cond_ebb, &[]);

                // exit block
                self.builder.switch_to_block(exit_ebb);
                self.builder.seal_block(exit_ebb);

                // push the stack to the actual stack
                for stack_val in self.stack.clone().into_iter() {
                    let (a, b) = self.to_value(stack_val);
                    self.builder
                        .ins()
                        .call(self.fiber_push, &[self.fiber, a, b]);
                }

                // return the f stack, to be restored
                let ret_states: &mut Vec<super::FState> = Box::leak(Box::new(f_states));
                let ret_value = self.builder.ins().iconst(self.ptr_t, ret_states as *const _ as i64);

                self.builder.ins().return_(&[ret_value]);

                // continue with normal execution
                self.builder.switch_to_block(cond_ebb);
                self.builder.seal_block(cond_ebb);
            }
            TraceItem::IsNumber(n) => {
                let s_val = &self.stack[self.stack.len() - 1 - n as usize];
                match s_val {
                    StructVal::Typed(_val, ty) => assert!(ty == &self.number_t),
                    StructVal::Untyped(a, b) => {
                        let call = self.builder.ins().call(self.is_number, &[*a, *b]);
                        let res = self.builder.inst_results(call)[0];
                        self.stack.push(StructVal::Typed(res, self.bool_t))
                    }
                    StructVal::Nil => self.stack.push(StructVal::Typed(
                        self.builder.ins().iconst(self.bool_t, 0),
                        self.bool_t,
                    )),
                }
            }
            TraceItem::Foreign(x) => {
                // plan
                //
                // push only required args in the stack
                // call ptr x with fiber (that we dont have yet!) as args

                let f = self.trace.vm.fn_foreign[x as usize];
                let f_loc = self.builder.ins().iconst(self.ptr_t, f.1 as i64);
                let f_n_args = self.builder.ins().iconst(self.ptr_t, f.0 as i64);

                for _ in 0..(f.0) {
                    let stack_val = self.stack.pop().unwrap();
                    let (a, b) = self.to_value(stack_val);
                    self.builder
                        .ins()
                        .call(self.fiber_push, &[self.fiber, a, b]);
                }
                // self.builder.ins().call_indirect(sig, f_loc, &[self.fiber]);
                self.builder
                    .ins()
                    .call(self.call_foreign, &[self.fiber, f_loc, f_n_args]);
                let ret_val_call = self.builder.ins().call(self.fiber_pop, &[self.fiber]);
                let a = self.builder.inst_results(ret_val_call)[0];
                let b = self.builder.inst_results(ret_val_call)[1];
                // self.builder.ins().call(self.print_val, &[a,b]);
                self.stack.push(StructVal::Untyped(a, b));

                // unimplemented!()
            }
            TraceItem::Load(i) => {
                // let new = builder.ins().load(number_t, MemFlags::new(), local_stack, *i as i32);
                let f_off = i as usize * std::mem::size_of::<super::Value>();
                // let i_ins = self.builder.ins().iconst(
                //     self.ptr_t,
                //     i as i64 * std::mem::size_of::<super::Value>() as i64,
                // );
                // let loc = self.builder.ins().iadd(i_ins, self.local_stack);

                // let value = self.builder.ins().call(self.as_number, &[loc]);
                let value = self.builder.ins().load(
                    self.number_t,
                    MemFlags::new(),
                    self.local_stack,
                    f_off as i32 + 8,
                );

                // let new = self.builder.inst_results(value)[0];
                self.stack.push(StructVal::Typed(value, self.number_t));

                self.highest_local = self.highest_local.max(i);
            }
            TraceItem::Store(i) => {
                let val_s = self.stack.pop().unwrap();
                let (a, b) = self.to_value(val_s);

                let base_addr = (i as i32) * std::mem::size_of::<super::Value>() as i32;
                self.builder
                    .ins()
                    .store(MemFlags::new(), a, self.local_stack, base_addr);
                self.builder
                    .ins()
                    .store(MemFlags::new(), b, self.local_stack, base_addr + 8);

                self.highest_local = self.highest_local.max(i);
            }
            TraceItem::Const(val) => {
                if let Some(float) = val.as_number() {
                    let new = self.builder.ins().f64const(Ieee64::with_float(float));
                    self.stack.push(StructVal::Typed(new, self.number_t));
                } else if let Some(b) = val.as_bool() {
                    let new = self.builder.ins().iconst(self.bool_t, b as i64);
                    self.stack.push(StructVal::Typed(new, self.bool_t));
                } else {
                    unimplemented!()
                }
            }
            TraceItem::Nil => {
                // let (a, b): (i64, i64) = unsafe {std::mem::transmute(super::Value::nil())};
                self.stack.push(StructVal::Nil);
            }
            TraceItem::False => {
                self.stack.push(StructVal::Typed(
                    self.builder.ins().iconst(self.bool_t, 1),
                    self.bool_t,
                ));
            }
            TraceItem::True => {
                self.stack.push(StructVal::Typed(
                    self.builder.ins().iconst(self.bool_t, 0),
                    self.bool_t,
                ));
            }
        }
    }
}
