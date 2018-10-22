// use std::collections::HashMap;

use crate::vm::VMError;

#[derive(Debug, Clone, Copy)]
pub struct GCObject {
    val: *mut Object
}

impl GCObject {
    pub fn new(mut val: Object) -> Self {
        Self {
            val: &mut val
        }
    }
}

impl std::ops::Deref for GCObject {
    type Target = Object;

    fn deref(&self) -> &Object {
        unsafe {
            &*self.val
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Value {
    Nil,
    False,
    True,
    Number(f64),

    GCObject,
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
            if let Value::Number(b) = v{
                Ok(f(a,b))
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
    // Map(HashMap<Value, Value>),
    List(Vec<Object>),

    Closure(usize),
    Fiber(usize),
    Foreign(usize),
}