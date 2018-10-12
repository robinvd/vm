use crate::parser::code::{Expr, Function, BcFunction, TopLevel};
use crate::parser::bytecode::{Arg};
use crate::vm::{VM, Opcode, VMError, Block};

pub fn compile_expr(block: &mut Block, vm: &mut VM, expr: &Expr) -> Result<(), VMError> {
    match expr {
        Expr::Call(n, args) => {
            for a in args.iter() {
                compile_expr(block, vm, a)?;
            }

            block.add_opcode(vm, Opcode::Call, Some(&Arg::Text(n)))?;
        },
        Expr::Lit(i) => {
            block.add_opcode(vm, Opcode::Push, Some(&Arg::Int(*i)))?;
        }
        Expr::While(p, body) => {
            block.add_label("while")?;
            compile_expr(block, vm, p)?;
            block.add_opcode(vm, Opcode::JmpZ, Some(&Arg::Text("done")))?;


            for ref a in body {
                compile_expr(block, vm, &a)?;
            }

            block.add_label("done")?;
        }
        _ => {},
    }

    Ok(())
}

pub fn compile_f(vm: &mut VM, f: &Function) -> Result<(), VMError> {
    let mut bl = Block::new(f.name.to_owned());

    for ref a in &f.body {
        compile_expr(&mut bl, vm, &a)?;
    }
    bl.add_opcode(vm, Opcode::Ret, None)?;

    vm.add_block(bl);


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
        }?;
    }

    Ok(())
}