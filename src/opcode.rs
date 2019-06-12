use std::mem;

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    // general
    Halt,
    Pop,
    Copy(u16),
    Load(u16),
    Store(u16),
    Print,

    // fn
    Call(u16),
    CallForeign(u16),
    Ret,

    // vals
    Const(u16),
    Num(u16),
    Nil,
    True,
    False,

    // math
    Add,
    Sub,
    Mul,
    Div,
    Neg,

    // bool
    Not,
    LEQ,
    GEQ,

    // jmp
    JmpT(u16),
    Jmp(u16),

    // objects
    New,
    Index,

    EmptyMap,
    EmptyList,

    // async:
    // asyncjmp
    // await
    Nop,
}

impl Instruction {
    pub(crate) fn to_opcode(self) -> (Opcode, Option<u16>) {
        match self {
            // general
            Instruction::Halt => (Opcode::Halt, None),
            Instruction::Pop => (Opcode::Pop, None),
            Instruction::Copy(x) => (Opcode::Copy, Some(x)),
            Instruction::Load(x) => (Opcode::Load, Some(x)),
            Instruction::Store(x) => (Opcode::Store, Some(x)),
            Instruction::Print => (Opcode::Print, None),

            // fn
            Instruction::Call(x) => (Opcode::Call, Some(x)),
            Instruction::CallForeign(x) => (Opcode::CallForeign, Some(x)),
            Instruction::Ret => (Opcode::Ret, None),

            // vals
            Instruction::Const(x) => (Opcode::Const, Some(x)),
            Instruction::Num(x) => (Opcode::Num, Some(x)),
            Instruction::Nil => (Opcode::Nil, None),
            Instruction::True => (Opcode::True, None),
            Instruction::False => (Opcode::False, None),

            // math
            Instruction::Add => (Opcode::Add, None),
            Instruction::Sub => (Opcode::Sub, None),
            Instruction::Mul => (Opcode::Mul, None),
            Instruction::Div => (Opcode::Div, None),
            Instruction::Neg => (Opcode::Neg, None),

            // bool
            Instruction::Not => (Opcode::Not, None),
            Instruction::LEQ => (Opcode::LEQ, None),
            Instruction::GEQ => (Opcode::GEQ, None),

            // jmp
            Instruction::JmpT(x) => (Opcode::JmpT, Some(x)),
            Instruction::Jmp(x) => (Opcode::Jmp, Some(x)),

            // objects
            Instruction::New => (Opcode::New, None),
            Instruction::Index => (Opcode::Index, None),

            Instruction::EmptyMap => (Opcode::EmptyMap, None),
            Instruction::EmptyList => (Opcode::EmptyList, None),

            // async:
            // asyncjmp
            // await
            Instruction::Nop => (Opcode::Nop, None),
        }
    }
}

#[allow(dead_code)]
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
    Sub,
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
    JmpTDirect,
    JmpDirect,

    // objects
    New,
    Index,

    EmptyMap,
    EmptyList,

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
            | Opcode::JmpDirect
            | Opcode::JmpT
            | Opcode::JmpTDirect
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
