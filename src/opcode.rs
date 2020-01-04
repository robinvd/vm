pub trait InstructionExt {
    type OpcodeType;
    fn to_opcode(self) -> Self::OpcodeType;
    fn to_bytes(self, sink: &mut Vec<u8>);
}

pub trait OpcodeExt: Sized {
    fn from_u8(value: u8) -> Option<Self>;

    unsafe fn from_u8_unchecked(value: u8) -> Self;

    fn has_arg(self) -> bool {
        self.arg_count() > 0
    }
    fn arg_count(self) -> usize;
}

macro_rules! flat_pattern {
    ($field:ident, , $a:ident) => {
        Self::$field
    };
    ($field:ident, $ty:ty, $a:ident) => {
        Self::$field($a)
    };
}

macro_rules! flat_arm {
    ($field:ident, $ty:ty, $flat_name:ident, $output:ident, $a:ident) => {{
        $output.push($flat_name::$field as u8);
        $output.push(($a >> 8) as u8);
        $output.push($a as u8);
    }};
    ($field:ident, , $flat_name:ident, $output:ident, $a:ident) => {{
        $output.push($flat_name::$field as u8);
    }};
}

macro_rules! count {
    () => {
        0
    };
    ($x:tt, $($xs:tt, )*) => {count!($($xs)*) + 1};
}

macro_rules! derive_flat {
    (
        $(#[$($derives:meta)+])? $vis:vis enum $name:ident { $( $field:ident $(( $ty:ty ))? ),* }
        $(#[$($flat_derives:meta)+])? $flat_vis:vis enum $flat_name:ident { $( $flat_field:ident ),* }
    ) => {
        $(#[$( $derives )*])*
        #[repr(C)]
        $vis enum $name {
            $(
                $field $(($ty))* ,
            )*
        }

        impl InstructionExt for $name {
            type OpcodeType = $flat_name;
            fn to_opcode(self) -> Self::OpcodeType {
                match self {
                    $(
                        flat_pattern!($field, $($ty)*, _x) => $flat_name::$field,
                    )*
                }
            }
            fn to_bytes(self, out: &mut Vec<u8>) {
                match self {
                    $(
                        flat_pattern!($field, $($ty)*, x) => flat_arm!($field, $($ty)*, $flat_name, out, x),
                    )*
                }
            }
        }

        $(#[$( $flat_derives )*])*
        #[repr(u8)]
        $flat_vis enum $flat_name {
            $(
                $field,
            )*
            $(
                $flat_field,
            )*
        }

        impl OpcodeExt for $flat_name {
            unsafe fn from_u8_unchecked(value: u8) -> Self {
                std::mem::transmute(value)
            }

            fn from_u8(value: u8) -> Option<Self> {
                let all_opcodes = &[$(Self::$field ,)* $(Self::$flat_field ,)*];
                let last_opcode = *all_opcodes.last().unwrap();
                let last_value = last_opcode as u8;

                if value <= last_value {
                    Some(unsafe{Self::from_u8_unchecked(value)})
                } else {
                    None
                }

            }

            fn arg_count(self) -> usize {
                match self {
                    $(
                        Self::$field => count!($($ty ,)*),
                    )*
                    _ => 0,
                }
            }
        }
    };
}

derive_flat! {
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instruction {
    // general
    Halt,
    Pop,
    Copy(u16),
    Load(u16),
    Upvalue(u16),
    Store(u16),
    Print,

    // fn
    Call(u16),
    CallObj,
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
    New(u16),
    GetAttr(u16),
    SetAttr(u16),
    Index,

    EmptyList,
    PushList,
    PopList,

    EmptyMap,

    NewClosure(u16),

    // async:
    // asyncjmp
    // await
    Nop
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Opcode {
    JmpDirect,
    JmpTDirect,
    End
}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arg_count() {
        assert_eq!(Opcode::Nop.arg_count(), 0);
        assert_eq!(Opcode::Num.arg_count(), 1);
    }

    #[test]
    fn test_from_u8() {
        assert_eq!(Opcode::from_u8(Opcode::Halt as u8), Some(Opcode::Halt));
        assert_eq!(Opcode::from_u8(Opcode::End as u8 + 1), None);
    }

    #[test]
    fn test_from_instr() {
        assert_eq!(Instruction::Halt.to_opcode(), Opcode::Halt);

        let mut buffer = Vec::new();

        Instruction::Num(5).to_bytes(&mut buffer);
        assert_eq!(buffer, vec![Opcode::Num as u8, 0, 5]);
    }
}
