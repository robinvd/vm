use super::*;

use std::{fmt};
use ordered_float::OrderedFloat;

use crate::vm::{
    trace::{Trace, TraceItem},
    VMError,
};


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Value {
    Nil,
    False,
    True,
    Number(OrderedFloat<f64>),

    Object(GCObject),
}

impl Traverse for Value {
    fn traverse<F>(&self, f: &mut F)
    where
        F: FnMut(&Value),
    {
        if let Value::Object(o) = self {
            f(self);
            o.traverse(f);
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::ops::Deref;
        match self {
            Value::Nil => write!(f, "Nil"),
            Value::False => write!(f, "False"),
            Value::True => write!(f, "True"),
            Value::Number(n) => write!(f, "{}", n),
            Value::Object(gc) => write!(f, "{}", gc.deref()),
        }
    }
}

impl std::ops::Not for Value {
    type Output = Value;

    fn not(self) -> Self {
        if self.is_true() {
            Value::False
        } else {
            Value::True
        }
    }
}

impl Value {
    pub fn and(self, other: Self) -> Self {
        if self.is_true() {
            other
        } else {
            Value::False
        }
    }
}

impl Value {
    pub fn is_true(self) -> bool {
        match self {
            Value::False | Value::Nil => false,
            _ => true,
        }
    }

    pub fn as_number(self) -> Option<f64> {
        if let Value::Number(of) = self {
            Some(*of)
        } else {
            None
        }
    }

    pub fn as_bool(self) -> Option<bool> {
        match self {
            Value::True => Some(true),
            Value::False => Some(false),
            _ => None,
        }
    }

    pub fn as_nil(self) -> bool {
        if let Value::Nil = self {
            true
        } else {
            false
        }
    }

    pub fn to_number(self) -> Result<f64, VMError> {
        match self {
            Value::Number(n) => Ok(*n),
            _ => Err(VMError::InvalidConversion),
        }
    }

    pub fn to_int(self) -> Result<isize, VMError> {
        self.to_number().map(|x| x.round() as isize)
    }

    pub fn index(&self, i: Value) -> Result<Value, VMError> {
        match self {
            Value::Object(o) => o.index(i),
            _ => Err(VMError::NotIndexable),
        }
    }

    pub fn true_val() -> Value {
        Value::True
    }

    pub fn false_val() -> Value {
        Value::False
    }

    pub fn nil() -> Value {
        Value::Nil
    }

    pub fn number(f: f64) -> Value {
        Value::Number(OrderedFloat(f))
    }

    pub fn object(o: Object) -> Value {
        Value::Object(GCObject::new(ObjectInfo::new(o)))
    }

    pub fn get_object(&self) -> Option<&Object> {
        match self {
            Value::Object(o) => Some(o),
            _ => None,
        }
    }

    pub fn get_object_info(&self) -> Option<&ObjectInfo> {
        match self {
            Value::Object(o) => Some(o.object_info()),
            _ => None,
        }
    }

    pub fn get_object_gcobj(self) -> Option<GCObject> {
        match self {
            Value::Object(o) => Some(o),
            _ => None,
        }
    }
}
