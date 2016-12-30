use std::collections::HashMap;
use std::mem;

use lexer::{ Token };
use parser::{ Block, Stmt, Expr, AssignKind, BinOpKind, UnaryOpKind, Atom };
use value::{ Value };
use virtual_machine::{ Opcode, VirtualMachine };

#[derive(Debug, PartialEq)]
pub enum CompileError {
    Expected {
        expected: Token,
        got: Token
    },
    EndOfStream,
    Unimplemented,
    TooManyLocals,
    RedeclaredLocal,
    UndeclaredLocal,
    BranchTooFar,
    TooManyConstants,
}

pub type CompileResult<T> = Result<T, CompileError>;

#[derive(Clone, Copy)]
struct JumpPatch(usize);

pub struct Compiler {
    locals: HashMap<String, u16>,
    code: Vec<Opcode>,
    consts: Vec<Value>,
}

pub struct CompiledBlock {
    pub code: Box<[Opcode]>,
    pub consts: Box<[Value]>,
    pub local_names: Box<[String]>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            locals: HashMap::new(),
            code: Vec::new(),
            consts: Vec::new(),
        }
    }

    fn declare_local(&mut self, name: &str) -> CompileResult<u16> {
        use std::collections::hash_map::Entry;

        let idx = self.locals.len();
        if idx > (u16::max_value() as usize) {
            return Err(CompileError::TooManyLocals);
        }

        let idx = idx as u16;
        let e = self.locals.entry(name.to_string());

        match e {
            Entry::Occupied(_) => {
                Err(CompileError::RedeclaredLocal)
            }
            Entry::Vacant(v) => {
                v.insert(idx);
                Ok(idx)
            }
        }
    }

    fn fetch_local(&mut self, name: &str) -> CompileResult<u16> {
        self.locals.get(name).map(|i| *i).ok_or(CompileError::UndeclaredLocal)
    }

    fn emit(&mut self, op: Opcode) {
        self.code.push(op);
    }

    fn emit_load_const(&mut self, value: Value) -> CompileResult<()> {
        let idx = self.consts.len();
        if idx > (u16::max_value() as usize) {
            Err(CompileError::TooManyConstants)
        } else {
            let idx = idx as u16;
            self.consts.push(value);
            self.emit(Opcode::LoadConst(idx));
            Ok(())
        }
    }

    fn emit_branch_false(&mut self) -> JumpPatch {
        let result = JumpPatch(self.code.len());
        self.emit(Opcode::BranchFalse(0));
        result
    }

    fn emit_branch_true(&mut self) -> JumpPatch {
        let result = JumpPatch(self.code.len());
        self.emit(Opcode::BranchTrue(0));
        result
    }

    fn emit_jump(&mut self) -> JumpPatch {
        let result = JumpPatch(self.code.len());
        self.emit(Opcode::Jump(0));
        result
    }

    fn patch_jump(&mut self, patch: JumpPatch) -> CompileResult<()> {
        let cur = self.code.len();
        let branch_loc = patch.0;
        let diff = (cur as isize) - (branch_loc as isize);

        if diff > (i16::max_value() as isize) || diff < (i16::min_value() as isize) {
            Err(CompileError::BranchTooFar)
        } else {
            let diff = diff as i16;

            match self.code[branch_loc] {
                Opcode::Jump(_) => self.code[branch_loc] = Opcode::Jump(diff),
                Opcode::BranchTrue(_) => self.code[branch_loc] = Opcode::BranchTrue(diff),
                Opcode::BranchFalse(_) => self.code[branch_loc] = Opcode::BranchFalse(diff),
                _ => unreachable!(),
            }

            Ok(())
        }
    }

    fn compile_block(&mut self, block: &Block) -> CompileResult<()> {
        for stmt in block.stmts.iter() {
            match stmt {
                &Stmt::Assign { ref kind, ref ident } => {
                    match kind {
                        &AssignKind::Let => {
                            self.declare_local(ident)?;
                        }
                        &AssignKind::LetAssign(ref expr) => {
                            self.compile_expr(expr)?;
                            let idx = self.declare_local(ident)?;
                            self.emit(Opcode::StoreLocal(idx));
                        }
                        &AssignKind::Reassign(ref expr) => {
                            self.compile_expr(expr)?;
                            let idx = self.fetch_local(ident)?;
                            self.emit(Opcode::StoreLocal(idx));
                        }
                    }
                }

                &Stmt::IfStmt { ref cond, ref then, ref els } => {
                    self.compile_expr(cond)?;
                    let if_not = self.emit_branch_false();
                    self.compile_block(then)?;

                    if let &Some(ref els) = els {
                        let jump_to_end = self.emit_jump();
                        self.patch_jump(if_not)?;
                        self.compile_block(els)?;
                        self.patch_jump(jump_to_end)?;
                    } else {
                        self.patch_jump(if_not)?;
                    }
                }

                &Stmt::Expr(ref expr) => {
                    self.compile_expr(expr)?;
                    self.emit(Opcode::Pop);
                }

                &Stmt::Print(ref expr) => {
                    self.compile_expr(expr)?;
                    self.emit(Opcode::Print);
                }
            }
        }

        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expr) -> CompileResult<()> {
        match expr {
            &Expr::BinOp { kind, ref left, ref right } => {
                self.compile_expr(left)?;
                self.compile_expr(right)?;

                match kind {
                    BinOpKind::Add => self.emit(Opcode::Add),
                    BinOpKind::Sub => self.emit(Opcode::Sub),
                    BinOpKind::Mul => self.emit(Opcode::Mul),
                    BinOpKind::Div => self.emit(Opcode::Div),
                }
            }
            &Expr::UnaryOp { kind, ref child } => {
                self.compile_expr(child)?;
                match kind {
                    UnaryOpKind::Neg => self.emit(Opcode::Neg),
                }
            }
            &Expr::Atom(ref atom) => {
                match atom {
                    &Atom::Null => {
                        self.emit_load_const(Value::Null)?;
                    }
                    &Atom::Bool(b) => {
                        self.emit_load_const(Value::Bool(b))?;
                    }
                    &Atom::Number(n) => {
                        self.emit_load_const(Value::Number(n))?;
                    }
                    &Atom::Ident(ref ident) => {
                        let idx = self.fetch_local(ident)?;
                        self.emit(Opcode::LoadLocal(idx));
                    }
                }
            }
        }

        Ok(())
    }

    fn compile_main(&mut self, block: &Block) -> CompileResult<CompiledBlock> {
        self.compile_block(block)?;
        self.code.push(Opcode::Return);

        let mut local_names = vec![String::new(); self.locals.len()];
        for (name, idx) in self.locals.drain() {
            local_names[idx as usize] = name;
        }

        Ok(CompiledBlock {
            code: mem::replace(&mut self.code, Vec::new()).into_boxed_slice(),
            consts: mem::replace(&mut self.consts, Vec::new()).into_boxed_slice(),
            local_names: local_names.into_boxed_slice(),
        })
    }
}

pub fn compile(block: &Block, vm: &mut VirtualMachine) -> CompileResult<CompiledBlock> {
    Compiler::new().compile_main(block)
}
