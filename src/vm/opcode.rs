use std::mem;

#[derive(Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum Opcode {
    // general
    Halt,
    Const,
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
    JmpT,
    Jmp,

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
            Opcode::Const
            | Opcode::Jmp
            | Opcode::JmpT
            | Opcode::Load
            | Opcode::Store
            | Opcode::Call
            | Opcode::Copy => true,
            _ => false,
        }
    }

    pub fn stack_effect(&self) -> Option<isize> {
        use self::Opcode::*;
        let x = match *self {
            // general
            Halt | Pop | Print | Add | Mul | Div | Neg | LEQ | JmpT => -1,
            Const | Copy => 1,
            Not | Jmp | Nop => 0,
            Load | Store | Call | Ret => return None,
        };
        Some(x)
    }
}
