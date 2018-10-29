use std::collections::HashMap;

use crate::parser::bytecode::Arg;
use crate::parser::code::{BcFunction, Expr, Function, TopLevel};
use crate::vm::{opcode::Opcode, Block, VMError, VM};

#[derive(Debug)]
struct Context<'a> {
    names: HashMap<&'a str, usize>,
    pub block: Block,
}

impl<'a> Context<'a> {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            names: HashMap::default(),
            block: Block::new(name),
        }
    }

    pub fn insert_name(&mut self, name: &'a str) -> usize {
        let n = self.block.local_n;
        self.names.insert(name, n);
        self.block.local_n += 1;

        n
    }

    pub fn add_opcode(
        &mut self,
        vm: &mut VM,
        opcode: Opcode,
        arg: Option<&Arg>,
    ) -> Result<(), VMError> {
        self.block.add_opcode(vm, opcode, arg)
    }

    pub fn compile_expr(&mut self, vm: &mut VM, expr: &'a Expr) -> Result<(), VMError> {
        match expr {
            Expr::Call(n, args) => {
                for a in args.iter() {
                    self.compile_expr(vm, a)?;
                }

                self.add_opcode(vm, Opcode::Call, Some(&Arg::Text(n)))?;
            }
            Expr::Lit(i) => {
                let loc = self.block.constants.len();
                self.block.constants.push(*i);
                self.add_opcode(vm, Opcode::Const, Some(&Arg::Int(loc as isize)))?;
            }
            Expr::While(p, body) => {
                let while_label = self.block.fresh_label("while")?;
                let done_label = self.block.reserve_fresh_label("done")?;
                self.compile_expr(vm, p)?;
                self.add_opcode(vm, Opcode::Not, None)?;
                self.add_opcode(vm, Opcode::JmpT, Some(&Arg::Text(&done_label)))?;

                for ref a in body {
                    self.compile_expr(vm, &a)?;
                }

                self.add_opcode(vm, Opcode::Jmp, Some(&Arg::Text(&while_label)))?;
                self.block.add_label(&done_label)?;
            }
            Expr::If(p, t, maybe_f) => {
                let f_label = self.block.reserve_fresh_label("false")?;
                let end_label = self.block.reserve_fresh_label("endif")?;

                self.compile_expr(vm, p)?;
                self.add_opcode(vm, Opcode::JmpT, Some(&Arg::Text(&f_label)))?;

                for ref a in t {
                    self.compile_expr(vm, &a)?;
                }

                self.add_opcode(vm, Opcode::Jmp, Some(&Arg::Text(&end_label)))?;
                self.block.add_label(&f_label)?;

                if let Some(f) = maybe_f {
                    for ref a in f {
                        self.compile_expr(vm, &a)?;
                    }
                }

                self.block.add_label(&end_label)?;
            }
            Expr::Let(names, exprs) => {
                if names.len() != exprs.len() {
                    return Err(VMError::DifferentNArgs);
                }

                for (name, expr) in names.iter().zip(exprs.iter()) {
                    let loc = self.insert_name(name);
                    self.compile_expr(vm, expr)?;
                    self.add_opcode(vm, Opcode::Store, Some(&Arg::Int(loc as isize)))?;
                }
            }
            Expr::Var(name) => {
                let loc = self
                    .names
                    .get(name)
                    .ok_or(VMError::Msg("local not found".to_owned()))?;
                self.add_opcode(vm, Opcode::Load, Some(&Arg::Int(*loc as isize)))?;
            }
            Expr::Assign(name, e) => {
                let loc = *self
                    .names
                    .get(name)
                    .ok_or(VMError::Msg("local not found".to_owned()))?;
                self.compile_expr(vm, e)?;
                self.add_opcode(vm, Opcode::Store, Some(&Arg::Int(loc as isize)))?;
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
    let mut context = Context::new(f.name);

    for ref a in f.args.iter() {
        let i = context.insert_name(&a);
        context
            .block
            .add_opcode(vm, Opcode::Store, Some(&Arg::Int(i as isize)))?;
    }

    for ref a in &f.body {
        context.compile_expr(vm, &a)?;
    }
    context.block.add_opcode(vm, Opcode::Ret, None)?;

    vm.add_block(context.block);

    Ok(())
}
pub fn compile_bc_f(vm: &mut VM, f: &BcFunction) -> Result<(), VMError> {
    let mut bl = Block::new(f.name.to_owned());

    for ref a in &f.body {
        bl.parse_instr(vm, a)?;
    }

    vm.add_block(bl);
    Ok(())
}

pub fn compile(vm: &mut VM, fs: &Vec<TopLevel>) -> Result<(), VMError> {
    for f in fs.iter() {
        match f {
            TopLevel::Function(f) => compile_f(vm, f),
            TopLevel::BcFunction(f) => compile_bc_f(vm, f),
            TopLevel::Use(us) => Ok(()),
        }?;
    }

    Ok(())
}
