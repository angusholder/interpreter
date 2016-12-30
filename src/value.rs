use std::collections::HashMap;
use std::hash::{ Hash, Hasher };
use std::mem;

pub enum HeapObjectKind {
    String(String),
    HashMap(HashMap<Value, Value>),
    List(Vec<Value>),
}

pub struct HeapObject {
    next: *mut HeapObject,
    marked: bool,
    kind: HeapObjectKind,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Value {
    Null,
    Bool(bool),
    Number(f64),
    HeapObject(*mut HeapObject),
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
