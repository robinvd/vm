use super::*;

use std::{fmt};
use ordered_float::OrderedFloat;

use crate::vm::{
    trace::{Trace, TraceItem},
    VMError,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value(usize);

impl Value {

    pub fn is_true(self) -> bool {}

    pub(crate) fn binary_op(
        self,
        v: Value,
        trace: Option<&mut Trace>,
        op: TraceItem,
        f: impl FnOnce(f64, f64) -> Value,
    ) -> Result<Value, VMError> {
    }

    pub fn unary_op(self, f: impl FnOnce(f64) -> f64) -> Result<Value, VMError> {
        if let Some(a) = self.as_number() {
            Ok(Value::number(f(a)))
        } else {
            Err(VMError::RuntimeError("combine not numbers".to_owned()))
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