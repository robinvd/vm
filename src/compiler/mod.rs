use std::collections::HashMap;

use crate::parser::code::{Expr, Function, TopLevel};
use crate::vm::{opcode::Opcode, Block, VMError, VM};

#[derive(Debug)]
struct Context<'a> {
    names: HashMap<&'a str, u16>,
    pub block: Block,
}

impl<'a> Context<'a> {
    pub fn new(name: impl Into<String>, n_args: usize) -> Self {
        Self {
            names: HashMap::default(),
            block: Block::new(name, n_args),
        }
    }

    pub fn insert_name(&mut self, name: &'a str) -> u16 {
        let n = self.block.local_n;
        self.names.insert(name, n as u16);
        self.block.local_n += 1;

        n as u16
    }

    pub fn add_opcode(
        &mut self,
        vm: &mut VM,
        opcode: Opcode,
        arg: Option<u16>,
    ) -> Result<(), VMError> {
        self.block.add_opcode(vm, opcode, arg)
    }

    pub fn compile_expr(&mut self, vm: &mut VM, expr: &'a Expr) -> Result<(), VMError> {
        match expr {
            Expr::Call(n, args) => {
                for a in args.iter() {
                    self.compile_expr(vm, a)?;
                }

                let call_loc = vm.get_block_loc(n).expect("could not find function");
                self.add_opcode(vm, Opcode::Call, Some(call_loc))?;
            }
            Expr::Lit(i) => {
                let loc = self.block.constants.len();
                self.block.constants.push(*i);
                self.add_opcode(vm, Opcode::Const, Some(loc as u16))?;
            }
            Expr::While(p, body) => {
                let while_label = self.block.add_label();
                let done_label = self.block.reserve_label();
                self.compile_expr(vm, p)?;
                self.add_opcode(vm, Opcode::Not, None)?;
                self.add_opcode(vm, Opcode::JmpT, Some(done_label.0))?;

                for a in body {
                    self.compile_expr(vm, &a)?;
                }

                self.add_opcode(vm, Opcode::Jmp, Some(while_label.0))?;
                self.block.place_label(done_label);
            }
            Expr::If(p, t, maybe_f) => {
                let f_label = self.block.reserve_label();
                let end_label = self.block.reserve_label();

                self.compile_expr(vm, p)?;
                self.add_opcode(vm, Opcode::Not, None)?;
                self.add_opcode(vm, Opcode::JmpT, Some(f_label.0))?;

                for a in t {
                    self.compile_expr(vm, &a)?;
                }

                self.add_opcode(vm, Opcode::Jmp, Some(end_label.0))?;
                self.block.place_label(f_label);

                if let Some(f) = maybe_f {
                    for a in f {
                        self.compile_expr(vm, &a)?;
                    }
                }

                self.block.place_label(end_label);
            }
            Expr::Let(names, exprs) => {
                if names.len() != exprs.len() {
                    return Err(VMError::DifferentNArgs);
                }

                for (name, expr) in names.iter().zip(exprs.iter()) {
                    self.compile_expr(vm, expr)?;
                    let loc = self.insert_name(name);
                    self.add_opcode(vm, Opcode::Store, Some(loc))?;
                }
            }
            Expr::Var(name) => {
                let loc = self
                    .names
                    .get(name)
                    .ok_or_else(|| VMError::Msg("local var not found".to_owned()))?;
                self.add_opcode(vm, Opcode::Load, Some(*loc))?;
            }
            Expr::Assign(name, e) => {
                let loc = *self
                    .names
                    .get(name)
                    .ok_or_else(|| VMError::Msg("local assign not found".to_owned()))?;
                self.compile_expr(vm, e)?;
                self.add_opcode(vm, Opcode::Store, Some(loc))?;
            }
            Expr::Return(vals) => {
                for v in vals.iter() {
                    self.compile_expr(vm, v)?;
                }
                self.add_opcode(vm, Opcode::Ret, None)?;
            }
            Expr::Tuple(vals) => {
                for v in vals.iter() {
                    self.compile_expr(vm, v)?;
                }
            }
            Expr::Index(val, index) => {
                self.compile_expr(vm, val)?;
                self.compile_expr(vm, index)?;
                self.add_opcode(vm, Opcode::Index, None)?;
            }
        }

        Ok(())
    }
}

pub fn compile_f(vm: &mut VM, f: &Function) -> Result<(), VMError> {
    let block_loc = vm.reserve_block(f.name);
    let mut context = Context::new(f.name, f.args.len());

    for a in &f.args {
        let i = context.insert_name(&a);
        context.block.add_opcode(vm, Opcode::Store, Some(i))?;
    }

    for a in &f.body {
        context.compile_expr(vm, &a)?;
    }
    context.block.add_opcode(vm, Opcode::Nil, None)?;
    context.block.add_opcode(vm, Opcode::Ret, None)?;

    vm.place_block(block_loc, context.block);

    Ok(())
}

pub fn compile(vm: &mut VM, fs: &[TopLevel]) -> Result<(), VMError> {
    for f in fs.iter() {
        match f {
            TopLevel::Function(f) => compile_f(vm, f),
            TopLevel::Use(_us) => unimplemented!(),
        }?;
    }

    Ok(())
}
