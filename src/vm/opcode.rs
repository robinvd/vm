use std::mem;

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(u8)]
pub enum Opcode {
    // general
    Halt,
    Pop,
    Copy,
    Load,
    Store,
    Print,

    // fn
    Call,
    CallForeign,
    Ret,

    // vals
    Const,
    Num,
    Nil,
    True,
    False,

    // math
    Add,
    Mul,
    Div,
    Neg,

    // bool
    Not,
    LEQ,
    GEQ,

    // jmp
    JmpT,
    Jmp,

    // objects
    New,
    Index,

    // async:
    // asyncjmp
    // await
    Nop,
    End,
}

impl Opcode {
    pub fn from_u8(n: u8) -> Option<Opcode> {
        if n <= Opcode::End as u8 {
            Some(unsafe { Self::from_u8_unchecked(n) })
        } else {
            None
        }
    }

    pub unsafe fn from_u8_unchecked(n: u8) -> Opcode {
        mem::transmute(n)
    }

    pub fn has_arg(self) -> bool {
        match self {
            Opcode::Const
            | Opcode::Jmp
            | Opcode::JmpT
            | Opcode::Load
            | Opcode::Store
            | Opcode::Call
            | Opcode::CallForeign
            | Opcode::Num
            | Opcode::Copy => true,
            _ => false,
        }
    }

    // pub fn stack_effect(&self) -> Option<isize> {
    //     use self::Opcode::*;
    //     let x = match *self {
    //         // general
    //         Halt | Pop | Print | Add | Mul | Div | Neg | LEQ | JmpT | Store | Index | True
    //         | False | Nil => -1,
    //         Const | Copy | New | Load => 1,
    //         Not | Jmp | Nop => 0,
    //         Call | CallForeign | Ret => return None,
    //     };
    //     Some(x)
    // }
}
