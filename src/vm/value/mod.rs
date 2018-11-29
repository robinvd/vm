use std::{cell::Cell, collections::HashMap, fmt, ptr::NonNull};

use crate::vm::{
    trace::{Trace, TraceItem},
    VMError,
};

mod normal;
pub use self::normal::*;

impl Value {
    pub fn deep_clone(self) -> Self {
        if let Some(obj) = self.get_object_gcobj() {
            Value::Object(obj.deep_clone())
        } else {
            self
        }
    }

    pub(crate) fn binary_op<'a, 'write>(
        self,
        v: Value,
        trace: Option<&mut Trace<'a, 'write>>,
        op: TraceItem<'a>,
        f: impl FnOnce(f64, f64) -> Value,
    ) -> Result<Value, VMError> {
        if let Some(a) = self.as_number() {
            if let Some(b) = v.as_number() {
                if let Some(t) = trace {
                    // t.push(TraceItem::IsNumber(0));
                    // t.push(TraceItem::IsNumber(1));
                    t.push(op);
                }
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

    pub fn as_number_unchecked(self) -> f64 {
        self.as_number().unwrap()
    }
}

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
