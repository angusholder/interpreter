use std::ptr;

use value::{ Value, HeapObject, HeapObjectKind };
use compiler::CompiledBlock;

pub struct VirtualMachine {
    value_stack: Vec<Value>,
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
}

impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            value_stack: Vec::new(),
            next_object: ptr::null_mut(),
        }
    }

    pub fn execute(&mut self, block: &CompiledBlock) {
        use self::Opcode::*;
        use value::Value::*;

        let mut pc = 0usize;
        let mut locals = vec![Value::Null; block.local_names.len()];

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
            let op = block.code[pc];

            match op {
                LoadConst(idx) => {
                    self.value_stack.push(block.consts[idx as usize]);
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

                Lt =>   match_binop! { (Number(a), Number(b)) => { Bool(a < b) } },
                LtEq => match_binop! { (Number(a), Number(b)) => { Bool(a <= b) } },
                Gt =>   match_binop! { (Number(a), Number(b)) => { Bool(a > b) } },
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

                Return => break,
            }

            // println!("{:?}", op);
            // println!("{:?}", self.value_stack);

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
