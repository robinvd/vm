// use std::collections::HashMap;
use std::collections::HashMap;
use std::fmt;
use std::ptr::NonNull;

use ordered_float::OrderedFloat;

use crate::vm::VMError;

#[derive(Debug, Clone, Copy, Hash, Eq)]
pub struct GCObject {
    val: NonNull<Object>,
}

impl GCObject {
    pub fn new(val: Object) -> Self {
        let b = Box::new(val);
        unsafe {
            Self {
                val: NonNull::new_unchecked(Box::leak(b)),
            }
        }
    }
}

impl std::ops::Deref for GCObject {
    type Target = Object;

    fn deref(&self) -> &Object {
        unsafe { self.val.as_ref() }
    }
}

impl std::cmp::PartialEq for GCObject {
    fn eq(&self, other: &Self) -> bool {
        unsafe { self.val.as_ref() == other.val.as_ref() }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Value {
    Nil,
    False,
    True,
    Number(f64),

    Object(GCObject),
}

impl std::hash::Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let len = std::mem::size_of::<Self>();
        let byteptr = self as *const _ as *const u8;
        for i in 0..len as isize {
            unsafe {
                (*byteptr.offset(i)).hash(state);
            }
        }
    }
}

impl std::cmp::Eq for Value {}

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

impl Value {
    pub fn is_true(self) -> bool {
        if self == Value::False || self == Value::Nil {
            false
        } else {
            true
        }
    }

    pub fn binary_op(self, v: Value, f: impl FnOnce(f64, f64) -> Value) -> Result<Value, VMError> {
        if let Value::Number(a) = self {
            if let Value::Number(b) = v {
                Ok(f(a, b))
            } else {
                Err(VMError::RuntimeError("combine not numbers".to_owned()))
            }
        } else {
            Err(VMError::RuntimeError("combine not numbers".to_owned()))
        }
    }

    pub fn unary_op(self, f: impl FnOnce(f64) -> f64) -> Result<Value, VMError> {
        if let Value::Number(a) = self {
            Ok(Value::Number(f(a)))
        } else {
            Err(VMError::RuntimeError("combine not numbers".to_owned()))
        }
    }

    pub fn not(self) -> Self {
        if self.is_true() {
            Value::False
        } else {
            Value::True
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    String(String),
    Map(HashMap<Value, Value>),
    List(Vec<Value>),

    Closure(usize),
    Fiber(usize),
    Foreign(usize),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::String(s) => write!(f, "{}", s),
            Object::List(l) => {
                write!(f, "[")?;
                let mut iter = l.iter();
                if let Some(x) = iter.next() {
                    write!(f, "{}", x)?;
                }
                for x in iter {
                    write!(f, ", {}", x)?;
                }
                write!(f, "]")?;

                Ok(())
            }
            Object::Map(m) => {
                write!(f, "{{")?;
                let mut iter = m.iter();
                if let Some((k, x)) = iter.next() {
                    write!(f, "{} = {}", k, x)?;
                }
                for (k, x) in iter {
                    write!(f, ", {} = {}", k, x)?;
                }
                write!(f, "}}")?;

                Ok(())
            }
            Object::Closure(_) => write!(f, "<closure>"),
            Object::Fiber(_) => write!(f, "<fiber>"),
            Object::Foreign(_) => write!(f, "<foreign>"),
        }
    }
}
