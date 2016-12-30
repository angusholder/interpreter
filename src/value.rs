struct HeapObject {
    next: *mut HeapObject,
    marked: bool,
    kind: HeapObjectKind,
}

struct HeapObjectWrapper<T> {
    ptr: T,
}

enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(Box<str>),
    HashMap(HashMap<Value, Value>),
    List(Vec<Value>),
}
