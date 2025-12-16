//! ARMLite code generation from a `Program`.

use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    ops::Not,
    rc::Rc,
    sync::LazyLock,
};

use itertools::Itertools;
use phf::phf_map;

use crate::{
    codegen::{
        arm::{file_builder::FileBuilder, func_builder, Address, Inst, Reg, RegOrImmediate},
        func_builder::FuncBuilder,
    },
    context::Context,
    parser::{
        program::{
            expr::{call::Call, BinaryOp, Expr, UnaryOp},
            statement::{Block, Statement, Variable},
            types::{CBuiltinType, CConcreteType, CFunc, CFuncType, CType},
            Program, Symbol,
        },
        TodoType,
    },
};

mod arm;

const WORD_SIZE: u32 = 4;

static BUILTIN_FUNCS: phf::Map<&str, fn(&mut FuncBuilder)> = phf_map! {
    "WriteChar" => |b| {
        b.ldrb(Reg::R0, Reg::Sp+0);
        b.asm("STRB R0, .WriteChar");
        b.add(Reg::Sp, Reg::Sp, 4);
        b.ret();
    },
    "WriteString" => |b| {
        b.pop(&[Reg::R0]);
        b.asm("STR R0, .WriteString");
        b.ret();
    },
    "WriteSignedNum" => |b| {
        b.pop(&[Reg::R0]);
        b.asm("STR R0, .WriteSignedNum");
        b.ret();
    },
    "WriteUnsignedNum" => |b| {
        b.pop(&[Reg::R0]);
        b.asm("STR R0, .WriteUnsignedNum");
        b.ret();
    },
    "ReadString" => |b| {
        b.pop(&[Reg::R0]);
        b.asm("STR R0, .ReadString");
        b.ret();
    },
    "add" => |b| {
        b.pop(&[Reg::R0, Reg::R1]);
        b.add(Reg::R0, Reg::R0, Reg::R1);
        b.ret();
    },
    "if_" => |b| {
        b.pop(&[Reg::R0, Reg::R1, Reg::R2]);

        b.cmp(Reg::R2, 0);
        b.beq("if_false");
        b.mov(Reg::ProgCounter, Reg::R1);

        b.label("if_false");
        b.mov(Reg::ProgCounter, Reg::R0);
    },
};

pub struct Generator {
    program: Program,
    file_builder: FileBuilder,
    ctype_size_cache: RefCell<HashMap<CType, u32>>,
}

impl Generator {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            file_builder: FileBuilder::default(),
            ctype_size_cache: HashMap::default().into(),
        }
    }

    pub fn generate(self) -> String {
        for (name, cfunc) in self.program.get_defined_functions() {
            let sig = self.program.get_func_type(cfunc.sig_id);

            self.file_builder.create_function(name, sig, |b| {
                // Hack to allow built-ins (inline asm) to work with the existing system :)
                if let Some(func) = BUILTIN_FUNCS.get(name) {
                    return func(b);
                }

                GenScope::new(&self, b).generate_func(cfunc);
            });
        }

        self.file_builder.build()
    }

    fn sizeof_ctype(&self, ctype: CType) -> u32 {
        if let Some(size) = self.ctype_size_cache.borrow().get(&ctype) {
            return *size;
        }

        let size = match ctype {
            CType::PointerTo(_) => WORD_SIZE,

            CType::ArrayOf(inner_id, element_count) => {
                self.sizeof_ctype(self.program.get_ctype(inner_id)) * element_count
            }

            CType::AsIs(concrete) => match concrete {
                CConcreteType::Struct(cstruct_id) => self
                    .program
                    .get_struct(cstruct_id)
                    .members
                    .as_ref()
                    .map(|members| {
                        members
                            .iter()
                            .map(|member| self.sizeof_ctype(member.ctype))
                            .sum()
                    })
                    .unwrap_or(0),

                CConcreteType::Enum(cenum_id) => WORD_SIZE,
                CConcreteType::Func(_) => WORD_SIZE,

                CConcreteType::Builtin(builtin) => match builtin {
                    CBuiltinType::Void => 0,
                    CBuiltinType::Bool => 1,
                    CBuiltinType::Char => 1,
                    CBuiltinType::SignedChar => 1,
                    CBuiltinType::UnsignedChar => 1,
                    CBuiltinType::Short => 2,
                    CBuiltinType::UnsignedShort => 2,
                    CBuiltinType::Int => WORD_SIZE,
                    CBuiltinType::UnsignedInt => WORD_SIZE,
                    CBuiltinType::Long => WORD_SIZE,
                    CBuiltinType::UnsignedLong => WORD_SIZE,
                    CBuiltinType::LongLong => 8,
                    CBuiltinType::UnsignedLongLong => 8,
                    CBuiltinType::Float => 2,
                    CBuiltinType::Double => 4,
                    CBuiltinType::LongDouble => 4,
                },
            },
        };

        self.ctype_size_cache.borrow_mut().insert(ctype, size);
        size
    }
}

struct GenScope<'a, 'b> {
    generator: &'b Generator,
    b: &'b mut FuncBuilder<'a>,
    stack_top_pos: i32,
    named_vars: HashMap<String, StackLocal>,
    frame: Vec<(u32, bool)>,
}

impl<'a, 'b> GenScope<'a, 'b> {
    pub fn new(generator: &'b Generator, builder: &'b mut FuncBuilder<'a>) -> Self {
        Self {
            generator,
            b: builder,
            stack_top_pos: 0,
            named_vars: HashMap::default(),
            frame: Vec::default(),
        }
    }

    /// Allocate space for a local variable, return its stack frame offset.
    fn allocate_var(&mut self, name: String, ctype: CType) -> i32 {
        self.b.comment(format!("Declare `{name}` ({ctype:?})"));
        let offset = self.allocate_anon(self.generator.sizeof_ctype(ctype));
        self.named_vars.insert(name, StackLocal { offset, ctype });

        offset
    }

    /// Allocate space for an anonymous variable, return its stack frame offset.
    fn allocate_anon(&mut self, size: u32) -> i32 {
        let mut actual_size = (size >> 2) << 2;

        if actual_size < 4 {
            actual_size = 4;
        }

        // look for a free spot first before allocing.
        let mut offset = 0;

        for (spot_size, in_use) in self.frame.iter_mut() {
            offset += *spot_size;

            if *spot_size == actual_size && !*in_use {
                *in_use = true;
                return offset as i32;
            }
        }

        self.stack_top_pos += actual_size as i32;
        self.frame.push((actual_size, true));

        self.b.inline_comment(format!(
            "Allocate {size} bytes @ [R11-{}]",
            self.stack_top_pos
        ));
        self.b.sub(Reg::Sp, Reg::Sp, actual_size as i32);

        self.stack_top_pos
    }

    /// Free a local variable on the stack. Its spot can now be recycled.
    fn free_local(&mut self, offset: i32) {
        if offset == self.stack_top_pos {
            let Some(top_slot) = self.frame.last_mut() else {
                panic!("Can't call `forget` with an empty stack!");
            };
            top_slot.1 = false;

            let old_stack_top_pos = self.stack_top_pos;

            // cascade downward and clear out all the unused vars before us.
            while let Some((size, _)) = self.frame.pop_if(|(_, in_use)| !*in_use) {
                self.stack_top_pos -= size as i32;
            }

            println!(
                "frame after pop: {:?}, top = {}",
                self.frame, self.stack_top_pos
            );

            self.b
                .add(Reg::Sp, Reg::Sp, old_stack_top_pos - self.stack_top_pos);

            return;
        }

        // stack surgery time (aka me writing horribly inefficient code that's Good Enough(tm) for learning)
        let mut total_offset = 0;

        for (size, in_use) in self.frame.iter_mut() {
            total_offset += *size;

            // if this is the target, mark as free.
            if total_offset == offset as u32 {
                *in_use = false;
                break;
            }
        }
    }

    fn forget_stack_locals(&mut self, count: usize) {
        for _ in 0..count {
            self.stack_top_pos -= self.frame.pop().unwrap().0 as i32;
        }
    }

    /// Get the offset of a previously defined local variable, if it exists.
    fn get_localvar(&self, name: &str) -> Option<StackLocal> {
        if let Some(offset) = self.named_vars.get(name) {
            return Some(*offset);
        }

        let mut offset: i32 = -4;

        if self.generator.sizeof_ctype(self.b.sig.returns) > WORD_SIZE {
            // 0th implicit argument is the return address.
            offset -= 4;
        }

        for arg in self.b.sig.args.iter() {
            offset -= self.generator.sizeof_ctype(arg.ctype) as i32;

            if arg.name.as_deref() == Some(name) {
                return Some(StackLocal {
                    offset,
                    ctype: arg.ctype,
                });
            }
        }

        None
    }

    pub(super) fn generate_func(&mut self, cfunc: &CFunc) {
        let Some(block) = &cfunc.body else {
            panic!("Function {cfunc:?} left undefined!");
        };

        self.b.push(&[Reg::R11, Reg::LinkReg]);
        self.b.mov(Reg::R11, Reg::Sp);

        self.generate_block(block);

        self.b.label("done");
        self.b.mov(Reg::Sp, Reg::R11);
        self.b.pop(&[Reg::R11, Reg::LinkReg]);

        if !self.b.sig.args.is_empty() {
            self.b.inline_comment("Eat the caller's args.");
            self.b.add(
                Reg::Sp,
                Reg::Sp,
                self.b
                    .sig
                    .args
                    .iter()
                    .map(|arg| self.generator.sizeof_ctype(arg.ctype))
                    .sum::<u32>() as i32,
            );
        }

        self.b.ret();
    }

    fn generate_block(&mut self, block: &Block) {
        for stmt in &block.statements {
            self.generate_stmt(stmt);
        }
    }

    fn generate_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Declare(variable) => {
                let variable_offset = self.allocate_var(variable.name.clone(), variable.ctype);

                if let Some(expr) = &variable.value {
                    // eval the initial val.
                    self.generate_expr(expr, Some((variable_offset, variable.ctype)));
                }
            }

            Statement::Expr(expr) => {
                self.generate_expr(expr, None);
            }

            Statement::Return(expr) => {
                let return_type_size = self.generator.sizeof_ctype(self.b.sig.returns);
                let temp_storage = self.allocate_anon(return_type_size);

                self.b.comment("<return>");

                self.generate_expr(expr, Some((temp_storage, self.b.sig.returns)));

                if return_type_size > WORD_SIZE {
                    // the caller pushes an address where they want us to copy the returned
                    // (presumably) struct to to return it to them, rather than splitting it
                    // across registers or something.
                    self.b.inline_comment("Grab the return storage address.");
                    self.b.ldr(Reg::R0, stack_offset(4));
                    self.b.ldr(Reg::R1, stack_offset(temp_storage));
                    self.b.str(Reg::R1, Address::at(Reg::R0));
                } else {
                    // we can neatly return the value in one register. yay!
                    self.b.inline_comment("Put the returned value in R0.");
                    self.b.ldr(Reg::R0, stack_offset(temp_storage));
                }

                self.free_local(temp_storage);

                self.b.inline_comment("Perform the cleanup.");
                self.b.b("done");

                self.b.comment("</return>");
            }

            Statement::If {
                condition,
                if_true,
                if_false,
            } => {
                let if_false_label = self.b.anonymise("If__else");
                let done_label = self.b.anonymise("If__done");

                self.b.comment(format!("if ({condition})"));

                let temp_condition_storage = self.allocate_anon(4);
                self.generate_expr(
                    condition,
                    Some((
                        temp_condition_storage,
                        CType::AsIs(CConcreteType::Builtin(CBuiltinType::Bool)),
                    )),
                );

                self.b.ldr(Reg::R0, stack_offset(temp_condition_storage));
                self.free_local(temp_condition_storage);

                self.b.cmp(Reg::R0, 0);

                if if_false.is_some() {
                    self.b.beq(if_false_label.clone());
                } else {
                    self.b.beq(done_label.clone());
                }

                self.b.comment("then");
                self.generate_stmt(if_true);

                if let Some(if_false) = if_false {
                    self.b.b(done_label.clone());

                    self.b.comment("else");
                    self.b.label(if_false_label);
                    self.generate_stmt(if_false);
                }

                self.b.comment(format!("endif ({condition})"));
                self.b.label(done_label);
            }

            Statement::Block(block) => self.generate_block(block),

            _ => {
                self.b.asm(format!("\tUnhandled statement: {stmt:?}\n"));
            }
        };
    }

    /// Generate instructions to perform the given operation, returning the instructions required to
    /// perform it, and place the output at `[R11-#result_offset]`.
    fn generate_expr(&mut self, expr: &Expr, result_info: Option<(i32, CType)>) {
        match expr {
            Expr::StringLiteral(value) => {
                let Some((result_offset, ctype)) = result_info else {
                    // pointless no-op.
                    return;
                };

                let string_id = self.generator.file_builder.create_string(value);

                self.b.mov(Reg::R0, string_id);
                self.b.str(Reg::R0, stack_offset(result_offset));
            }

            Expr::IntLiteral(value) => {
                let Some((result_offset, ctype)) = result_info else {
                    // pointless no-op.
                    return;
                };

                // ints are nice and relaxed :)
                self.b.mov(Reg::R0, *value);
                self.b.str(Reg::R0, stack_offset(result_offset));
            }

            Expr::Reference(name) => {
                let Some((result_offset, ctype)) = result_info else {
                    // pointless no-op.
                    return;
                };

                if let Some(source) = self.get_localvar(name) {
                    self.b.comment(format!(
                        "=== query localvar `{name}` ({:?}) as a {:?} ===",
                        source.ctype, ctype
                    ));

                    match source.ctype {
                        CType::AsIs(cconcrete_type) => {
                            self.b.ldr(Reg::R0, source);
                            self.b.str(Reg::R0, stack_offset(result_offset));
                        }

                        CType::PointerTo(ctype_id) => match ctype {
                            CType::AsIs(cconcrete_type) => {
                                panic!("can't cast a pointer to a value type")
                            }

                            CType::PointerTo(_) => {
                                self.b.ldr(Reg::R0, source);
                                self.b.str(Reg::R0, stack_offset(result_offset));
                            }

                            CType::ArrayOf(ctype_id, size) => todo!("load array from pointer"),
                        },

                        CType::ArrayOf(ctype_id, _) => match ctype {
                            CType::AsIs(cconcrete_type) => {
                                panic!("can't cast an array to a value type")
                            }

                            CType::PointerTo(ctype_id) => {
                                self.b.sub(Reg::R0, Reg::R11, source.offset);
                                self.b.str(Reg::R0, stack_offset(result_offset));
                            }

                            CType::ArrayOf(ctype_id, _) => {
                                todo!("array copying!")
                            }
                        },
                    }

                    self.b.comment(format!("=== done query `{name}` ==="));
                    return;
                }

                if let Some(symbol) = self.generator.program.get_symbol(name) {
                    return match symbol {
                        Symbol::Func(cfunc) => {
                            self.b.mov(Reg::R0, format!("fn_{name}"));
                            self.b.str(Reg::R0, stack_offset(result_offset));
                        }

                        Symbol::Var(var_ctype, _) => {
                            match var_ctype {
                                // FIXME FIXME FIXME: This is a copy-pasted impl of the above with
                                // tiny changes. Really, we need a unified `Assign` implementation
                                // which can go between *any* storage source and destinations, with
                                // *any* types, and use it instead rather than bespoke solutions
                                // everywhere for every single operation.
                                CType::AsIs(cconcrete_type) => {
                                    self.b.mov(Reg::R0, format!("var_{name}"));
                                    self.b.ldr(Reg::R0, Reg::R0 + 0);
                                    self.b.str(Reg::R0, stack_offset(result_offset));
                                }

                                CType::PointerTo(ctype_id) => match ctype {
                                    CType::AsIs(cconcrete_type) => {
                                        panic!("can't cast a pointer to a value type")
                                    }

                                    CType::PointerTo(ctype_id) => {
                                        self.b.mov(Reg::R0, format!("var_{name}"));
                                        self.b.ldr(Reg::R0, Reg::R0 + 0);
                                        self.b.str(Reg::R0, stack_offset(result_offset));
                                    }

                                    CType::ArrayOf(ctype_id, size) => {
                                        todo!("load array from pointer")
                                    }
                                },

                                CType::ArrayOf(ctype_id, _) => match ctype {
                                    CType::AsIs(cconcrete_type) => {
                                        panic!("can't cast an array to a value type")
                                    }

                                    CType::PointerTo(ctype_id) => {
                                        self.b.mov(Reg::R0, format!("var_{name}"));
                                        self.b.ldr(Reg::R0, Reg::R0 + 0);
                                        self.b.str(Reg::R0, stack_offset(result_offset));
                                    }

                                    CType::ArrayOf(ctype_id, _) => {
                                        todo!("array copying!")
                                    }
                                },
                            }
                        }
                    };
                }

                panic!("Reference to undefined variable `{name}`");
            }

            Expr::Call(Call {
                target,
                args,
                sig_id,
            }) => {
                let sig = self.generator.program.get_func_type(*sig_id);

                // push args to stack first. caller pushes args, callee expected to pop em. args
                // pushed last first, so top of stack is the first argument.
                let initial_frame_top = self.stack_top_pos;

                let arg_target_spots = sig
                    .args
                    .iter()
                    .rev()
                    .map(|member| {
                        (
                            self.allocate_anon(self.generator.sizeof_ctype(member.ctype)),
                            member.ctype,
                        )
                    })
                    .collect_vec();

                assert_eq!(
                    arg_target_spots.len(),
                    args.len(),
                    "must pass the expected arg count in calling {target:?} with signature {sig_id:?}"
                );

                let new_frame_top = self.stack_top_pos;
                let stack_pointer_adjustment = new_frame_top - initial_frame_top;

                self.b.comment(format!(
                    "=== Call {target}({}) [{stack_pointer_adjustment} arg bytes] ===",
                    args.iter().join(", ")
                ));

                for (arg, out_info) in args.iter().zip(&arg_target_spots) {
                    self.generate_expr(arg, Some(*out_info));
                }

                match &**target {
                    Expr::Reference(name) if self.get_localvar(name).is_none() => {
                        // Direct reference to a function by name. sane common case! we can just BL
                        // there using its label name.
                        self.b.call(name);
                    }

                    expr => {
                        // FIXME: we should support type coersion rules. ugghhhhh
                        let temp_fn_ptr_var = self.allocate_anon(WORD_SIZE);

                        // Resolve the function pointer.
                        self.generate_expr(
                            expr,
                            Some((temp_fn_ptr_var, CType::AsIs(CConcreteType::Func(*sig_id)))),
                        );

                        self.b.ldr(Reg::R0, stack_offset(temp_fn_ptr_var));
                        self.free_local(temp_fn_ptr_var);

                        self.b
                            .inline_comment("Manually set LR (TODO: shouldn't this be +4??)");
                        self.b.mov(Reg::LinkReg, Reg::ProgCounter);
                        self.b.ldr(Reg::ProgCounter, Reg::R0 + 0);
                    }
                };

                self.forget_stack_locals(arg_target_spots.len());

                let Some((out_pos, out_ctype)) = result_info else {
                    self.b.comment(format!("=== END Call {target} => void ==="));
                    return;
                };

                if self.generator.sizeof_ctype(sig.returns) <= WORD_SIZE {
                    self.b.str(Reg::R0, stack_offset(out_pos));
                    self.b.comment(format!(
                        "=== END Call {target} => R0 => {} ===",
                        stack_offset(out_pos)
                    ));
                } else {
                    // no-op, we should've passed this info earlier.
                }
            }

            Expr::BinaryOp(op, left, right) => {
                match op {
                    BinaryOp::AndThen => {
                        self.generate_expr(left, None);
                        self.generate_expr(right, result_info);
                    }

                    BinaryOp::Assign => todo!(),

                    BinaryOp::BooleanEqual => {
                        let left_ctype = self.type_of_expr(left);
                        let left_temp = self.allocate_anon(self.generator.sizeof_ctype(left_ctype));

                        let right_ctype = self.type_of_expr(right);
                        let right_temp =
                            self.allocate_anon(self.generator.sizeof_ctype(right_ctype));

                        self.generate_expr(left, Some((left_temp, self.type_of_expr(left))));

                        self.generate_expr(right, Some((right_temp, self.type_of_expr(right))));

                        let Some((result_offset, ctype)) = result_info else {
                            // discard the result.
                            return;
                        };

                        self.b.ldr(Reg::R0, stack_offset(left_temp));
                        self.b.ldr(Reg::R1, stack_offset(right_temp));
                        self.free_local(left_temp);
                        self.free_local(right_temp);

                        self.b.cmp(Reg::R0, Reg::R1);
                        self.b.bne(3);
                        self.b.mov(Reg::R0, 1);
                        self.b.b(2);
                        self.b.mov(Reg::R0, 0);
                        self.b.str(Reg::R0, stack_offset(result_offset));
                    }

                    BinaryOp::LessThan => todo!(),

                    BinaryOp::Plus => {
                        let Some((result_offset, ctype)) = result_info else {
                            // discard the result.
                            return;
                        };

                        let ctype_size = self.generator.sizeof_ctype(ctype);

                        self.b.comment(format!("=== binop({left} + {right}) ==="));

                        let left_temp = self.allocate_anon(ctype_size);
                        let right_temp = self.allocate_anon(ctype_size);

                        self.generate_expr(left, Some((left_temp, ctype)));
                        self.generate_expr(right, Some((right_temp, ctype)));
                        self.b.ldr(Reg::R0, stack_offset(left_temp));
                        self.b.ldr(Reg::R1, stack_offset(right_temp));

                        self.free_local(left_temp);
                        self.free_local(right_temp);

                        self.b.add(Reg::R0, Reg::R0, Reg::R1);
                        self.b.str(Reg::R0, stack_offset(result_offset));

                        self.b
                            .comment(format!("=== END binop({left} + {right}) ==="));
                    }

                    BinaryOp::ArrayIndex => {
                        let Some((result_offset, ctype)) = result_info else {
                            // pointless no-op.
                            return;
                        };

                        let element_ctype = self.type_of_expr(expr);
                        let element_size = self.generator.sizeof_ctype(element_ctype);

                        let temp_index_storage = self.allocate_anon(WORD_SIZE);
                        let temp_ptr_storage = self.allocate_anon(WORD_SIZE);

                        self.generate_expr(
                            right,
                            Some((
                                temp_index_storage,
                                CType::AsIs(CConcreteType::Builtin(CBuiltinType::Int)),
                            )),
                        );

                        self.generate_expr(
                            left,
                            Some((
                                temp_ptr_storage,
                                CType::PointerTo(
                                    self.generator
                                        .program
                                        .ctype_id_of(CConcreteType::Builtin(CBuiltinType::Void)),
                                ),
                            )),
                        );

                        let result_size = self.generator.sizeof_ctype(ctype);

                        self.b.ldr(Reg::R0, stack_offset(temp_ptr_storage));
                        self.b.ldr(Reg::R1, stack_offset(temp_index_storage));

                        match element_size {
                            1 => {
                                self.b.ldrb(Reg::R0, Address::relative(Reg::R0, Reg::R1));

                                if result_size == 1 {
                                    self.b.strb(Reg::R0, stack_offset(result_offset));
                                } else {
                                    self.b.str(Reg::R0, stack_offset(result_offset));
                                }
                            }

                            2 => {
                                // FIXME: I'm probably doing this big-endian by accident!!
                                self.b.shl(Reg::R2, Reg::R1, 1);
                                self.b.add(Reg::R2, Reg::R0, Reg::R2);
                                self.b.ldrb(Reg::R0, Reg::R2 + 0);
                                self.b.ldrb(Reg::R1, Reg::R2 + 1);
                                self.b.shl(Reg::R0, Reg::R0, 8);
                                self.b.or(Reg::R0, Reg::R0, Reg::R1);
                                self.b.str(Reg::R0, stack_offset(result_offset));
                            }

                            3 => {
                                // FIXME: I'm probably doing this big-endian by accident!!
                                self.b.shl(Reg::R2, Reg::R1, 1);
                                self.b.add(Reg::R2, Reg::R2, Reg::R1);
                                self.b.add(Reg::R2, Reg::R0, Reg::R2);
                                self.b.ldrb(Reg::R0, Reg::R2 + 0);
                                self.b.ldrb(Reg::R1, Reg::R2 + 1);
                                self.b.shl(Reg::R0, Reg::R0, 8);
                                self.b.or(Reg::R0, Reg::R0, Reg::R1);
                                self.b.ldrb(Reg::R1, Reg::R2 + 2);
                                self.b.shl(Reg::R0, Reg::R0, 8);
                                self.b.or(Reg::R0, Reg::R0, Reg::R1);
                                self.b.str(Reg::R0, stack_offset(result_offset));
                            }

                            4 => {
                                self.b.shl(Reg::R1, Reg::R1, 2);
                                self.b.ldr(Reg::R0, Reg::R0 + Reg::R1);
                                self.b.str(Reg::R0, stack_offset(result_offset));
                            }

                            _ => todo!(
                                "{element_ctype:?} ({element_size} bytes) can't fit in a register"
                            ),
                        }

                        self.free_local(temp_index_storage);
                        self.free_local(temp_ptr_storage);
                    }
                }
            }

            Expr::UnaryOp(op, expr) => match op {
                UnaryOp::IncrementThenGet => todo!(),
                UnaryOp::GetThenIncrement => todo!(),
                UnaryOp::DecrementThenGet => todo!(),
                UnaryOp::GetThenDecrement => todo!(),

                UnaryOp::SizeOf => {
                    let Some((result_offset, ctype)) = result_info else {
                        // pointless no-op.
                        return;
                    };

                    self.b.mov(
                        Reg::R0,
                        self.generator.sizeof_ctype(self.type_of_expr(expr)) as i32,
                    );
                    self.b.str(Reg::R0, stack_offset(result_offset));
                }

                UnaryOp::BooleanNot => {
                    self.generate_expr(expr, result_info);

                    if let Some((result_offset, ctype)) = result_info {
                        self.b.ldr(Reg::R0, stack_offset(result_offset));
                        self.b.cmp(Reg::R0, 0);
                        self.b.bne(3);
                        self.b.mov(Reg::R0, 1);
                        self.b.b(2);
                        self.b.mov(Reg::R0, 0);
                        self.b.str(Reg::R0, stack_offset(result_offset));
                    }
                }

                UnaryOp::Negative => todo!(),
                UnaryOp::AddressOf => todo!(),
                UnaryOp::Dereference => todo!(),
            },

            Expr::Cast(expr, _) => todo!(),
        }
    }

    fn type_of_expr(&self, expr: &Expr) -> CType {
        match expr {
            Expr::StringLiteral(_) => CType::PointerTo(
                self.generator
                    .program
                    .ctype_id_of(CConcreteType::Builtin(CBuiltinType::Char)),
            ),

            Expr::IntLiteral(_) => CConcreteType::Builtin(CBuiltinType::Int).into(),

            Expr::Reference(name) => {
                if let Some(local) = self.get_localvar(name) {
                    return local.ctype;
                }

                if let Some(symbol) = self.generator.program.get_symbol(name) {
                    return match symbol {
                        Symbol::Func(cfunc) => CType::AsIs(cfunc.sig_id.into()),
                        Symbol::Var(ctype, _) => *ctype,
                    };
                }

                panic!("Can't get the ctype of the undefined variable `{name}`")
            }

            Expr::Call(call) => CType::AsIs(CConcreteType::Func(call.sig_id)),

            Expr::BinaryOp(op, left, right) => match op {
                BinaryOp::AndThen => self.type_of_expr(&right),
                BinaryOp::Assign => self.type_of_expr(&left),
                BinaryOp::BooleanEqual => CType::AsIs(CBuiltinType::Bool.into()),
                BinaryOp::LessThan => CType::AsIs(CBuiltinType::Bool.into()),
                BinaryOp::Plus => todo!("numeric type implicit conversion rules"),

                BinaryOp::ArrayIndex => match self.type_of_expr(&left) {
                    CType::AsIs(_) => panic!("Can't index a concrete type as an array!"),
                    CType::PointerTo(inner) | CType::ArrayOf(inner, _) => {
                        self.generator.program.get_ctype(inner)
                    }
                },
            },

            Expr::UnaryOp(op, expr) => match op {
                UnaryOp::IncrementThenGet => self.type_of_expr(expr),
                UnaryOp::GetThenIncrement => self.type_of_expr(expr),
                UnaryOp::DecrementThenGet => self.type_of_expr(expr),
                UnaryOp::GetThenDecrement => self.type_of_expr(expr),
                UnaryOp::SizeOf => CType::AsIs(CBuiltinType::Int.into()),
                UnaryOp::BooleanNot => CType::AsIs(CBuiltinType::Bool.into()),
                UnaryOp::Negative => self.type_of_expr(expr),

                UnaryOp::AddressOf => {
                    CType::PointerTo(self.generator.program.ctype_id_of(self.type_of_expr(expr)))
                }

                UnaryOp::Dereference => match self.type_of_expr(expr) {
                    CType::AsIs(_) => panic!("Can't dereference a concrete type!"),
                    CType::PointerTo(inner) | CType::ArrayOf(inner, _) => {
                        self.generator.program.get_ctype(inner)
                    }
                },
            },

            Expr::Cast(expr, ctype) => *ctype,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct StackLocal {
    offset: i32,
    ctype: CType,
}

impl From<StackLocal> for Address {
    fn from(value: StackLocal) -> Self {
        stack_offset(value.offset)
    }
}

fn stack_offset(offset: i32) -> Address {
    Address {
        base: Reg::R11,
        offset: offset.abs().into(),
        negate_offset: (offset >= 0),
    }
}
