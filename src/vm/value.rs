// use std::collections::HashMap;
use std::cell::Cell;
use std::collections::HashMap;
use std::fmt;
use std::ptr::NonNull;

use ordered_float::OrderedFloat;

use crate::vm::VMError;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct GCObject {
    pub val: NonNull<ObjectInfo>,
}

impl GCObject {
    pub fn new(val: ObjectInfo) -> Self {
        let b = Box::new(val);
        unsafe {
            Self {
                val: NonNull::new_unchecked(Box::leak(b)),
            }
        }
    }

    pub fn deep_clone(self) -> GCObject {
        let val = unsafe { self.val.as_ref() };
        Self::new((*val).clone())
    }

    pub fn object_info(&self) -> &ObjectInfo {
        unsafe { self.val.as_ref() }
    }
}

impl std::ops::Deref for GCObject {
    type Target = Object;

    fn deref(&self) -> &Object {
        unsafe { self.val.as_ref().val() }
    }
}

pub trait Traverse {
    fn traverse<F>(&self, f: &mut F)
    where
        F: FnMut(&Value);
}

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
    pub fn deep_clone(self) -> Self {
        match self {
            Value::Object(o) => Value::Object(o.deep_clone()),
            x => x,
        }
    }

    pub fn is_true(self) -> bool {
        match self {
            Value::False | Value::Nil => false,
            _ => true,
        }
    }

    pub fn binary_op(self, v: Value, f: impl FnOnce(f64, f64) -> Value) -> Result<Value, VMError> {
        if let Some(a) = self.as_number() {
            if let Some(b) = v.as_number() {
                Ok(f(a, b))
            } else {
                Err(VMError::RuntimeError("combine not numbers".to_owned()))
            }
        } else {
            Err(VMError::RuntimeError("combine not numbers".to_owned()))
        }
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

#[derive(Debug, Clone)]
pub struct ObjectInfo {
    pub mark: Cell<bool>,
    val: Object,
}

impl ObjectInfo {
    pub fn new(val: Object) -> Self {
        Self {
            mark: Cell::new(false),
            val,
        }
    }

    pub fn val(&self) -> &Object {
        &self.val
    }
}

impl std::cmp::PartialEq for ObjectInfo {
    fn eq(&self, other: &Self) -> bool {
        self.val.eq(&other.val)
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

impl Traverse for Object {
    fn traverse<F>(&self, f: &mut F)
    where
        F: FnMut(&Value),
    {
        match self {
            Object::Map(hm) => {
                for (k, v) in hm.iter() {
                    k.traverse(f);
                    v.traverse(f);
                }
            }
            Object::List(vec) => {
                for x in vec.iter() {
                    x.traverse(f)
                }
            }
            _ => (),
        }
    }
}

impl Object {
    pub fn index(&self, i: Value) -> Result<Value, VMError> {
        let val = match self {
            Object::List(vec) => vec.get(i.to_int()? as usize),
            Object::Map(map) => map.get(&i),
            _ => return Err(VMError::NotIndexable),
        };

        Ok(val.cloned().unwrap_or(Value::Nil))
    }
}
