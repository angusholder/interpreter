use std::ptr;
use std::mem;

use value::{ Value, HeapObject, HeapObjectKind };
use compiler::CompiledBlock;

pub struct CallInfo {
    locals: Box<[Value]>,
    pc: usize,
    func: *const CompiledBlock,
}

pub struct VirtualMachine {
    value_stack: Vec<Value>,
    call_stack: Vec<CallInfo>,
    pub next_object: *mut HeapObject,
}

#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    Add,
    Sub,
    Mul,
    Rem,
    Div,

    Neg,

    Lt,
    LtEq,
    Gt,
    GtEq,
    Eq,
    NotEq,

    LoadConst(u16),
    LoadLocal(u16),
    StoreLocal(u16),

    BranchTrue(i16),
    BranchFalse(i16),
    Jump(i16),

    Pop,
    Return,
    Print,
    Call(u8),
}

impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            value_stack: Vec::new(),
            call_stack: Vec::new(),
            next_object: ptr::null_mut(),
        }
    }

    pub fn execute(&mut self, initial_block: *const CompiledBlock) {
        use self::Opcode::*;
        use value::Value::*;

        let mut pc = 0usize;
        let mut func: &CompiledBlock = unsafe { &*initial_block };
        let mut locals = vec![Value::Null; func.local_names.len()].into_boxed_slice();

        macro_rules! match_binop {
            ($($pat:pat => $block:block)+) => {{
                let _a = self.value_stack.pop().unwrap();
                let _b = self.value_stack.pop().unwrap();
                let _result = match (_b, _a) {
                    $($pat => $block)+,
                    _ => panic!("Invalid operands"),
                };
                self.value_stack.push(_result);
            }}
        }

        loop {
            let op = func.code[pc];

            // println!("    {:?}", self.value_stack);
            // println!("{:?}", op);

            match op {
                LoadConst(idx) => {
                    self.value_stack.push(func.consts[idx as usize]);
                }
                LoadLocal(idx) => {
                    self.value_stack.push(locals[idx as usize]);
                }
                StoreLocal(idx) => {
                    locals[idx as usize] = self.value_stack.pop().unwrap();
                }

                BranchTrue(diff) => {
                    if self.value_stack.pop().unwrap().as_bool() {
                        pc = pc.wrapping_add((diff as isize) as usize);
                    } else {
                        pc = pc.wrapping_add(1);
                    }
                    continue;
                }
                BranchFalse(diff) => {
                    if !self.value_stack.pop().unwrap().as_bool() {
                        pc = pc.wrapping_add((diff as isize) as usize);
                    } else {
                        pc = pc.wrapping_add(1);
                    }
                    continue;
                }

                Jump(diff) => {
                    pc = pc.wrapping_add((diff as isize) as usize);
                    continue;
                }

                Pop => {
                    self.value_stack.pop().unwrap();
                }

                Add => match_binop! {
                    (Number(a), Number(b)) => { Number(a + b) }
                },
                Sub => match_binop! {
                    (Number(a), Number(b)) => { Number(a - b) }
                },
                Mul => match_binop! {
                    (Number(a), Number(b)) => { Number(a * b) }
                },
                Rem => match_binop! {
                    (Number(a), Number(b)) => {
                        assert!(b != 0.0);
                        Number(a % b)
                    }
                },
                Div => match_binop! {
                    (Number(a), Number(b)) => {
                        assert!(b != 0.0);
                        Number(a / b)
                    }
                },

                Lt =>   match_binop! { (Number(a), Number(b)) => { Bool(a < b)  } },
                LtEq => match_binop! { (Number(a), Number(b)) => { Bool(a <= b) } },
                Gt =>   match_binop! { (Number(a), Number(b)) => { Bool(a > b)  } },
                GtEq => match_binop! { (Number(a), Number(b)) => { Bool(a >= b) } },
                Eq => {
                    let a = self.value_stack.pop().unwrap();
                    let b = self.value_stack.pop().unwrap();
                    self.value_stack.push(Bool(a == b));
                }
                NotEq => {
                    let a = self.value_stack.pop().unwrap();
                    let b = self.value_stack.pop().unwrap();
                    self.value_stack.push(Bool(a != b));
                }

                Neg => {
                    if let Number(n) = self.value_stack.pop().unwrap() {
                        self.value_stack.push(Number(-n));
                    } else {
                        panic!("Invalid operand type");
                    }
                }

                Print => {
                    println!("{}", self.value_stack.pop().unwrap());
                }

                Return => {
                    if let Some(call_info) = self.call_stack.pop() {
                        func = unsafe { &*call_info.func };
                        locals = call_info.locals;
                        pc = call_info.pc;
                    } else {
                        break;
                    }
                }

                Call(arg_count) => {
                    let arg_count = arg_count as usize;

                    let func_idx = self.value_stack.len() - arg_count - 1;
                    let func_val = self.value_stack[func_idx];

                    let old_func = func;

                    func = if let Value::HeapObject(p) = func_val {
                        let obj = unsafe { &*p };
                        if let HeapObjectKind::Function(ref func) = obj.kind {
                            func
                        } else {
                            panic!("Tried to call uncallable value {:?}", func_val);
                        }
                    } else {
                        panic!("Tried to call uncallable value {:?}", func_val);
                    };

                    let mut new_locals = vec![Value::Null; func.local_names.len()].into_boxed_slice();

                    for i in 0..arg_count {
                        new_locals[i] = self.value_stack[func_idx + 1 + i];
                    }

                    for _ in 0..arg_count+1 {
                        self.value_stack.pop();
                    }

                    let old_locals = mem::replace(&mut locals, new_locals);

                    self.call_stack.push(CallInfo {
                        pc: pc,
                        locals: old_locals,
                        func: old_func,
                    });

                    pc = 0;
                    continue;
                }
            }

            pc = pc.wrapping_add(1);
        }
    }

    pub fn allocate_object(&mut self, obj_kind: HeapObjectKind) -> Value {
        let obj = Box::into_raw(Box::new(HeapObject {
            next: self.next_object,
            marked: false,
            kind: obj_kind
        }));
        self.next_object = obj;
        Value::HeapObject(obj)
    }
}
