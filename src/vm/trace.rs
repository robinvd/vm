use super::{Block, Value, VM, FState};

#[derive(Debug, Clone)]
pub(crate) enum TraceItem<'a> {
    // basic
    FAdd,
    FSub,
    FDiv,
    FMul,
    FNeg,
    GEQ,
    LEQ,
    Not,
    Print,

    // guards
    Guard(bool, Vec<FState<'a>>),
    IsNumber(u16),

    // mem
    Foreign(u16),
    Store(usize),
    Load(usize),

    // const
    Const(Value),
    Nil,
    True,
    False,
}

pub(crate) struct Trace<'a, 'write> {
    pub buf: Vec<TraceItem<'a>>,
    pub block: &'a Block,
    pub vm: &'a VM<'write>,

    pub start_fstate: usize,
    pub start_local_stack: usize,
    pub loc: usize,
}

impl<'a, 'write> std::fmt::Debug for Trace<'a, 'write> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "Trace {:?}:", self.buf)
    }
}

impl<'a, 'write> Trace<'a, 'write> {
    pub fn push(&mut self, item: TraceItem<'a>) {
        self.buf.push(item);
    }
}
