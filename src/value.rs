use std::collections::HashMap;
use std::hash::{ Hash, Hasher };
use std::mem;
use std::fmt;

use compiler::CompiledBlock;

pub enum HeapObjectKind {
    String(Box<str>),
    HashMap(HashMap<Value, Value>),
    List(Vec<Value>),
    Function(CompiledBlock),
}

pub struct HeapObject {
    pub next: *mut HeapObject,
    pub marked: bool,
    pub kind: HeapObjectKind,
}

#[derive(Clone, Copy, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Number(f64),
    HeapObject(*mut HeapObject),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Null => write!(f, "null")?,
            Value::Bool(true) => write!(f, "true")?,
            Value::Bool(false) => write!(f, "false")?,
            Value::Number(n) => write!(f, "{}", n)?,
            Value::HeapObject(p) => {
                let obj = unsafe { &*p };
                match obj.kind {
                    HeapObjectKind::String(ref s) => write!(f, "String(\"{}\")", s)?,
                    HeapObjectKind::Function(ref func) => write!(f, "Function({}:{:p})", func.name, p)?,
                    HeapObjectKind::List(_) => write!(f, "List({:p})", p)?,
                    HeapObjectKind::HashMap(_) => write!(f, "HashMap({:p})", p)?,
                }
            }
        }

        Ok(())
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Null => write!(f, "null")?,
            Value::Bool(true) => write!(f, "true")?,
            Value::Bool(false) => write!(f, "false")?,
            Value::Number(n) => write!(f, "{}", n)?,
            Value::HeapObject(p) => {
                let obj = unsafe { &*p };
                match obj.kind {
                    HeapObjectKind::String(ref s) => write!(f, "{}", s)?,
                    HeapObjectKind::Function(ref func) => write!(f, "Function({}:{:p})", func.name, p)?,
                    HeapObjectKind::List(_) => write!(f, "List({:p})", p)?,
                    HeapObjectKind::HashMap(_) => write!(f, "HashMap({:p})", p)?,
                }
            }
        }

        Ok(())
    }
}

impl Default for Value {
    fn default() -> Value {
        Value::Null
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Value::Null => {
                state.write_u8(0);
            }
            Value::Bool(b) => {
                state.write_u8(1);
                state.write_u8(b as u8);
            }
            Value::Number(n) => {
                state.write_u8(2);
                state.write_u64(unsafe { mem::transmute(n) });
            }
            Value::HeapObject(p) => {
                state.write_u8(3);
                state.write_usize(p as usize);
            }
        }
    }
}

impl Value {
    pub fn as_bool(self) -> bool {
        self != Value::Null && self != Value::Bool(false)
    }
}
