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

static BUILTIN_FUNCS: phf::Map<&str, fn(&mut FuncBuilder)> = phf_map! {
    "WriteChar" => |b| {
        b.pop(&[Reg::R0]);
        b.asm("STRB R0, .WriteChar");
        b.ret();
    },
    "WriteString" => |b| {
        b.pop(&[Reg::R0]);
        b.asm("STR R0, .WriteChar");
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

                self.generate_func(b, cfunc)
            });
        }

        self.file_builder.build()
    }

    fn generate_func(&self, b: &mut FuncBuilder, cfunc: &CFunc) {
        let mut scope = GenScope::new(self, b.sig);

        let Some(Block { statements, vars }) = &cfunc.body else {
            panic!("Function {cfunc:?} left undefined!");
        };

        b.push(&[Reg::R11, Reg::LinkReg]);
        b.mov(Reg::R11, Reg::Sp);

        self.generate_block(b, &mut scope, statements);

        // b.comment("# Named Stack Variables");

        // for (name, StackLocal { offset, ctype }) in scope
        //     .named_vars
        //     .iter()
        //     .sorted_by(|&a, &b| a.1.offset.cmp(&b.1.offset))
        // {
        //     b.comment(format!("- {ctype:?} {name}: {}\n", stack_offset(*offset)));
        // }

        // b.comment("");
        // b.comment(format!("Stack allocated size = {}", scope.stack_top_pos));
        // b.sub(Reg::Sp, Reg::Sp, scope.stack_top_pos);

        // output += &body_content;

        b.label("done");
        b.mov(Reg::Sp, Reg::R11);
        b.pop(&[Reg::R11, Reg::LinkReg]);

        if !scope.cfunc_type.args.is_empty() {
            b.inline_comment("Eat the caller's args.");
            b.add(
                Reg::Sp,
                Reg::Sp,
                scope
                    .cfunc_type
                    .args
                    .iter()
                    .map(|arg| self.sizeof_ctype(arg.ctype))
                    .sum::<u32>() as i32,
            );
        }

        b.ret();
    }

    fn generate_block(
        &self,
        b: &mut FuncBuilder,
        scope: &mut GenScope,
        statements: &Vec<Statement>,
    ) {
        for stmt in statements {
            self.generate_stmt(b, scope, stmt);
        }
    }

    fn generate_stmt(&self, b: &mut FuncBuilder, scope: &mut GenScope, stmt: &Statement) {
        match stmt {
            Statement::Declare(variable) => {
                let variable_offset = scope.allocate_var(variable.name.clone(), variable.ctype);

                if let Some(expr) = &variable.value {
                    // eval the initial val.
                    self.generate_expr(b, scope, expr, Some((variable_offset, variable.ctype)));
                }
            }

            Statement::Expr(expr) => {
                self.generate_expr(b, scope, expr, None);
            }

            Statement::Return(expr) => {
                let return_type_size = self.sizeof_ctype(scope.cfunc_type.returns);
                let temp_storage = scope.allocate_anon(return_type_size);

                b.comment("<return>");

                self.generate_expr(
                    b,
                    scope,
                    expr,
                    Some((temp_storage, scope.cfunc_type.returns)),
                );

                if return_type_size > Self::WORD_SIZE as u32 {
                    // the caller pushes an address where they want us to copy the returned
                    // (presumably) struct to to return it to them, rather than splitting it
                    // across registers or something.
                    b.inline_comment("Grab the return storage address.");
                    b.ldr(Reg::R0, stack_offset(4));
                    b.ldr(Reg::R1, stack_offset(temp_storage));
                    b.str(Reg::R1, Address::at(Reg::R0));
                } else {
                    // we can neatly return the value in one register. yay!
                    b.inline_comment("Put the returned value in R0.");
                    b.ldr(Reg::R0, stack_offset(temp_storage));
                }

                scope.forget(temp_storage);

                b.inline_comment("Perform the cleanup.");
                b.b("done");

                b.comment("</return>");
            }

            Statement::If {
                condition,
                if_true,
                if_false,
            } => {
                let if_false_label = b.anonymise("If__else");
                let done_label = b.anonymise("If__done");

                b.comment(format!("if ({condition})"));

                let temp_condition_storage = scope.allocate_anon(4);
                self.generate_expr(
                    b,
                    scope,
                    condition,
                    Some((
                        temp_condition_storage,
                        CType::AsIs(CConcreteType::Builtin(CBuiltinType::Bool)),
                    )),
                );

                b.ldr(Reg::R0, stack_offset(temp_condition_storage));
                scope.forget(temp_condition_storage);

                b.cmp(Reg::R0, 0);

                if if_false.is_some() {
                    b.beq(if_false_label.clone());
                } else {
                    b.beq(done_label.clone());
                }

                b.comment("then");
                self.generate_stmt(b, scope, if_true);

                if let Some(if_false) = if_false {
                    b.b(done_label.clone());

                    b.comment("else");
                    b.label(if_false_label);
                    self.generate_stmt(b, scope, if_false);
                }

                b.comment(format!("endif ({condition})"));
                b.label(done_label);
            }

            Statement::Block(Block { statements, vars }) => {
                self.generate_block(b, scope, statements)
            }

            _ => {
                b.asm(format!("\tUnhandled statement: {stmt:?}\n"));
            }
        };
    }

    /// Generate instructions to perform the given operation, returning the instructions required to
    /// perform it, and place the output at `[R11-#result_offset]`.
    fn generate_expr(
        &self,
        b: &mut FuncBuilder,
        scope: &mut GenScope,
        expr: &Expr,
        result_info: Option<(i32, CType)>,
    ) {
        match expr {
            Expr::StringLiteral(value) => {
                let Some((result_offset, ctype)) = result_info else {
                    // pointless no-op.
                    return;
                };

                let string_id = self.file_builder.create_string(value);

                b.mov(Reg::R0, string_id);
                b.str(Reg::R0, stack_offset(result_offset));
            }

            Expr::IntLiteral(value) => {
                let Some((result_offset, ctype)) = result_info else {
                    // pointless no-op.
                    return;
                };

                // ints are nice and relaxed :)
                b.mov(Reg::R0, *value);
                b.str(Reg::R0, stack_offset(result_offset));
            }

            Expr::Reference(name) => {
                let Some((result_offset, ctype)) = result_info else {
                    // pointless no-op.
                    return;
                };

                if let Some(source) = scope.get_localvar(name) {
                    b.comment(format!(
                        "=== query localvar `{name}` ({:?}) as a {:?} ===",
                        source.ctype, ctype
                    ));

                    match source.ctype {
                        CType::AsIs(cconcrete_type) => {
                            b.ldr(Reg::R0, source);
                            b.str(Reg::R0, stack_offset(result_offset));
                        }

                        CType::PointerTo(ctype_id) => match ctype {
                            CType::AsIs(cconcrete_type) => {
                                panic!("can't cast a pointer to a value type")
                            }

                            CType::PointerTo(_) => {
                                b.ldr(Reg::R0, source);
                                b.str(Reg::R0, stack_offset(result_offset));
                            }

                            CType::ArrayOf(ctype_id, size) => todo!("load array from pointer"),
                        },

                        CType::ArrayOf(ctype_id, _) => match ctype {
                            CType::AsIs(cconcrete_type) => {
                                panic!("can't cast an array to a value type")
                            }

                            CType::PointerTo(ctype_id) => {
                                b.sub(Reg::R0, Reg::R11, source.offset);
                                b.str(Reg::R0, stack_offset(result_offset));
                            }

                            CType::ArrayOf(ctype_id, _) => {
                                todo!("array copying!")
                            }
                        },
                    }

                    b.comment("=== done query `{name}` ===");
                    return;
                }

                if let Some(symbol) = self.program.get_symbol(name) {
                    return match symbol {
                        Symbol::Func(cfunc) => {
                            b.mov(Reg::R0, format!("fn_{name}"));
                            b.str(Reg::R0, stack_offset(result_offset));
                        }

                        Symbol::Var(var_ctype, _) => {
                            match var_ctype {
                                // FIXME FIXME FIXME: This is a copy-pasted impl of the above with
                                // tiny changes. Really, we need a unified `Assign` implementation
                                // which can go between *any* storage source and destinations, with
                                // *any* types, and use it instead rather than bespoke solutions
                                // everywhere for every single operation.
                                CType::AsIs(cconcrete_type) => {
                                    b.mov(Reg::R0, format!("var_{name}"));
                                    b.ldr(Reg::R0, Reg::R0 + 0);
                                    b.str(Reg::R0, stack_offset(result_offset));
                                }

                                CType::PointerTo(ctype_id) => match ctype {
                                    CType::AsIs(cconcrete_type) => {
                                        panic!("can't cast a pointer to a value type")
                                    }

                                    CType::PointerTo(ctype_id) => {
                                        b.mov(Reg::R0, format!("var_{name}"));
                                        b.ldr(Reg::R0, Reg::R0 + 0);
                                        b.str(Reg::R0, stack_offset(result_offset));
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
                                        b.mov(Reg::R0, format!("var_{name}"));
                                        b.ldr(Reg::R0, Reg::R0 + 0);
                                        b.str(Reg::R0, stack_offset(result_offset));
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
                let sig = self.program.get_func_type(*sig_id);

                // push args to stack first. caller pushes args, callee expected to pop em. args
                // pushed last first, so top of stack is the first argument.
                let initial_frame_top = scope.stack_top_pos;

                let arg_target_spots = sig
                    .args
                    .iter()
                    .rev()
                    .map(|member| {
                        (
                            scope.allocate_anon(self.sizeof_ctype(member.ctype)),
                            member.ctype,
                        )
                    })
                    .collect_vec();

                assert_eq!(
                    arg_target_spots.len(),
                    args.len(),
                    "must pass the expected arg count in calling {target:?} with signature {sig_id:?}"
                );

                let new_frame_top = scope.stack_top_pos;
                let stack_pointer_adjustment = new_frame_top - initial_frame_top;

                b.comment(format!(
                    "=== Call {target}({}) [{stack_pointer_adjustment} arg bytes] ===",
                    args.iter().join(", ")
                ));

                b.inline_comment("Adjust SP to point at 1st call argument!");
                b.sub(Reg::Sp, Reg::Sp, stack_pointer_adjustment);

                for (arg, out_info) in args.iter().zip(&arg_target_spots) {
                    self.generate_expr(b, scope, arg, Some(*out_info));
                }

                match &**target {
                    Expr::Reference(name) if scope.get_localvar(name).is_none() => {
                        // Direct reference to a function by name. sane common case! we can just BL
                        // there using its label name.
                        b.call(name);
                    }

                    expr => {
                        // FIXME: we should support type coersion rules. ugghhhhh
                        let temp_fn_ptr_var = scope.allocate_anon(Self::WORD_SIZE.into());

                        // Resolve the function pointer.
                        self.generate_expr(
                            b,
                            scope,
                            expr,
                            Some((temp_fn_ptr_var, CType::AsIs(CConcreteType::Func(*sig_id)))),
                        );

                        b.ldr(Reg::R0, stack_offset(temp_fn_ptr_var));
                        scope.forget(temp_fn_ptr_var);

                        b.inline_comment("Manually set LR (TODO: shouldn't this be +4??)");
                        b.mov(Reg::LinkReg, Reg::ProgCounter);
                        b.ldr(Reg::ProgCounter, Reg::R0 + 0);
                    }
                };

                for (spot, _) in arg_target_spots {
                    scope.forget(spot);
                }

                let Some((out_pos, out_ctype)) = result_info else {
                    b.comment(format!("=== END Call {target} => void ==="));
                    return;
                };

                if self.sizeof_ctype(sig.returns) <= Self::WORD_SIZE.into() {
                    b.str(Reg::R0, stack_offset(out_pos));
                    b.comment(format!(
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
                        self.generate_expr(b, scope, left, None);
                        self.generate_expr(b, scope, right, result_info);
                    }

                    BinaryOp::Assign => todo!(),

                    BinaryOp::BooleanEqual => {
                        let left_ctype = scope.type_of_expr(left);
                        let left_temp = scope.allocate_anon(self.sizeof_ctype(left_ctype));

                        let right_ctype = scope.type_of_expr(right);
                        let right_temp = scope.allocate_anon(self.sizeof_ctype(right_ctype));

                        self.generate_expr(
                            b,
                            scope,
                            left,
                            Some((left_temp, scope.type_of_expr(left))),
                        );

                        self.generate_expr(
                            b,
                            scope,
                            right,
                            Some((right_temp, scope.type_of_expr(right))),
                        );

                        let Some((result_offset, ctype)) = result_info else {
                            // discard the result.
                            return;
                        };

                        b.ldr(Reg::R0, stack_offset(left_temp));
                        b.ldr(Reg::R1, stack_offset(right_temp));
                        scope.forget(left_temp);
                        scope.forget(right_temp);

                        b.cmp(Reg::R0, Reg::R1);
                        b.bne(3);
                        b.mov(Reg::R0, 1);
                        b.b(2);
                        b.mov(Reg::R0, 0);
                        b.str(Reg::R0, stack_offset(result_offset));
                    }

                    BinaryOp::LessThan => todo!(),

                    BinaryOp::Plus => {
                        let Some((result_offset, ctype)) = result_info else {
                            // discard the result.
                            return;
                        };

                        let ctype_size = self.sizeof_ctype(ctype);

                        b.comment(format!("=== binop({left} + {right}) ==="));

                        b.sub(Reg::Sp, Reg::Sp, ctype_size * 2);
                        let left_temp = scope.allocate_anon(ctype_size);
                        let right_temp = scope.allocate_anon(ctype_size);

                        self.generate_expr(b, scope, left, Some((left_temp, ctype)));
                        self.generate_expr(b, scope, right, Some((right_temp, ctype)));
                        b.ldr(Reg::R0, stack_offset(left_temp));
                        b.ldr(Reg::R1, stack_offset(right_temp));

                        scope.forget(left_temp);
                        scope.forget(right_temp);
                        b.add(Reg::Sp, Reg::Sp, ctype_size * 2);

                        b.add(Reg::R0, Reg::R0, Reg::R1);
                        b.str(Reg::R0, stack_offset(result_offset));

                        b.comment(format!("=== END binop({left} + {right}) ==="));
                    }

                    BinaryOp::ArrayIndex => {
                        let Some((result_offset, ctype)) = result_info else {
                            // pointless no-op.
                            return;
                        };

                        let element_ctype = scope.type_of_expr(expr);
                        let element_size = self.sizeof_ctype(element_ctype);

                        let temp_index_storage = scope.allocate_anon(Self::WORD_SIZE as u32);
                        let temp_ptr_storage = scope.allocate_anon(Self::WORD_SIZE as u32);

                        self.generate_expr(
                            b,
                            scope,
                            right,
                            Some((
                                temp_index_storage,
                                CType::AsIs(CConcreteType::Builtin(CBuiltinType::Int)),
                            )),
                        );

                        self.generate_expr(
                            b,
                            scope,
                            left,
                            Some((
                                temp_ptr_storage,
                                CType::PointerTo(
                                    self.program
                                        .ctype_id_of(CConcreteType::Builtin(CBuiltinType::Void)),
                                ),
                            )),
                        );

                        let result_size = self.sizeof_ctype(ctype);

                        b.ldr(Reg::R0, stack_offset(temp_ptr_storage));
                        b.ldr(Reg::R1, stack_offset(temp_index_storage));

                        match element_size {
                            1 => {
                                b.ldrb(Reg::R1, Address::relative(Reg::R0, Reg::R1));

                                if result_size == 1 {
                                    b.strb(Reg::R0, stack_offset(result_offset));
                                } else {
                                    b.str(Reg::R0, stack_offset(result_offset));
                                }
                            }

                            2 => {
                                // FIXME: I'm probably doing this big-endian by accident!!
                                b.shl(Reg::R2, Reg::R1, 1);
                                b.add(Reg::R2, Reg::R0, Reg::R2);
                                b.ldrb(Reg::R0, Reg::R2 + 0);
                                b.ldrb(Reg::R1, Reg::R2 + 1);
                                b.shl(Reg::R0, Reg::R0, 8);
                                b.or(Reg::R0, Reg::R0, Reg::R1);
                                b.str(Reg::R0, stack_offset(result_offset));
                            }

                            3 => {
                                // FIXME: I'm probably doing this big-endian by accident!!
                                b.shl(Reg::R2, Reg::R1, 1);
                                b.add(Reg::R2, Reg::R2, Reg::R1);
                                b.add(Reg::R2, Reg::R0, Reg::R2);
                                b.ldrb(Reg::R0, Reg::R2 + 0);
                                b.ldrb(Reg::R1, Reg::R2 + 1);
                                b.shl(Reg::R0, Reg::R0, 8);
                                b.or(Reg::R0, Reg::R0, Reg::R1);
                                b.ldrb(Reg::R1, Reg::R2 + 2);
                                b.shl(Reg::R0, Reg::R0, 8);
                                b.or(Reg::R0, Reg::R0, Reg::R1);
                                b.str(Reg::R0, stack_offset(result_offset));
                            }

                            4 => {
                                b.shl(Reg::R1, Reg::R1, 2);
                                b.ldr(Reg::R0, Reg::R0 + Reg::R1);
                                b.str(Reg::R0, stack_offset(result_offset));
                            }

                            _ => todo!(
                                "{element_ctype:?} ({element_size} bytes) can't fit in a register"
                            ),
                        }

                        scope.forget(temp_index_storage);
                        scope.forget(temp_ptr_storage);
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

                    b.mov(Reg::R0, self.sizeof_ctype(scope.type_of_expr(expr)) as i32);
                    b.str(Reg::R0, stack_offset(result_offset));
                }

                UnaryOp::BooleanNot => {
                    self.generate_expr(b, scope, expr, result_info);

                    if let Some((result_offset, ctype)) = result_info {
                        b.ldr(Reg::R0, stack_offset(result_offset));
                        b.cmp(Reg::R0, 0);
                        b.bne(3);
                        b.mov(Reg::R0, 1);
                        b.b(2);
                        b.mov(Reg::R0, 0);
                        b.str(Reg::R0, stack_offset(result_offset));
                    }
                }

                UnaryOp::Negative => todo!(),
                UnaryOp::AddressOf => todo!(),
                UnaryOp::Dereference => todo!(),
            },

            Expr::Cast(expr, _) => todo!(),
        }
    }

    const WORD_SIZE: u16 = 4;

    fn sizeof_ctype(&self, ctype: CType) -> u32 {
        if let Some(size) = self.ctype_size_cache.borrow().get(&ctype) {
            return *size;
        }

        let size = match ctype {
            CType::PointerTo(_) => Self::WORD_SIZE.into(),

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

                CConcreteType::Enum(cenum_id) => Self::WORD_SIZE.into(),
                CConcreteType::Func(_) => Self::WORD_SIZE.into(),

                CConcreteType::Builtin(builtin) => match builtin {
                    CBuiltinType::Void => 0,
                    CBuiltinType::Bool => 1,
                    CBuiltinType::Char => 1,
                    CBuiltinType::SignedChar => 1,
                    CBuiltinType::UnsignedChar => 1,
                    CBuiltinType::Short => 2,
                    CBuiltinType::UnsignedShort => 2,
                    CBuiltinType::Int => Self::WORD_SIZE.into(),
                    CBuiltinType::UnsignedInt => Self::WORD_SIZE.into(),
                    CBuiltinType::Long => Self::WORD_SIZE.into(),
                    CBuiltinType::UnsignedLong => Self::WORD_SIZE.into(),
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

struct GenScope<'a> {
    generator: &'a Generator,
    cfunc_type: &'a CFuncType,
    stack_top_pos: i32,
    named_vars: HashMap<String, StackLocal>,
    frame: Vec<(u32, bool)>,
}

impl<'a> GenScope<'a> {
    pub fn new(generator: &'a Generator, cfunc_type: &'a CFuncType) -> Self {
        Self {
            generator,
            cfunc_type,
            stack_top_pos: 0,
            named_vars: HashMap::default(),
            frame: Vec::default(),
        }
    }

    /// Allocate space for a local variable, return its stack frame offset.
    pub fn allocate_var(&mut self, name: String, ctype: CType) -> i32 {
        let offset = self.allocate_anon(self.generator.sizeof_ctype(ctype));
        self.named_vars.insert(name, StackLocal { offset, ctype });

        offset
    }

    /// Allocate space for an anonymous variable, return its stack frame offset.
    pub fn allocate_anon(&mut self, size: u32) -> i32 {
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

        self.stack_top_pos
    }

    /// Forget about a specific variable or anon var in the stack. its spot can now get recycled.
    pub fn forget(&mut self, offset: i32) {
        if offset == self.stack_top_pos {
            self.frame.last_mut().unwrap().1 = false;

            // cascade downward and clear out all the unused vars before us.
            while let Some((size, _)) = self.frame.pop_if(|(_, in_use)| !*in_use) {
                self.stack_top_pos -= size as i32;
            }

            println!(
                "frame after pop: {:?}, top = {}",
                self.frame, self.stack_top_pos
            );

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

    /// Get the offset of a previously defined local variable, if it exists.
    pub fn get_localvar(&self, name: &str) -> Option<StackLocal> {
        if let Some(offset) = self.named_vars.get(name) {
            return Some(*offset);
        }

        let mut offset: i32 = -4;

        if self.generator.sizeof_ctype(self.cfunc_type.returns) > Generator::WORD_SIZE as u32 {
            // 0th implicit argument is the return address.
            offset -= 4;
        }

        for arg in self.cfunc_type.args.iter() {
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
