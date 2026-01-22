use std::{
    collections::{HashMap, HashSet, VecDeque},
    ops::{Not, Shl, Shr},
};

use anyhow::{bail, Context};
use indexmap::IndexMap;
use itertools::Itertools;

use crate::{
    codegen::{
        arm::{LiteralIndexAddress, OneOrMoreRegisters},
        func_builder::{FuncBuilder, LabelId},
        Address, Generator, Reg, RegOrImmediate, WORD_SIZE,
    },
    parser::program::{
        expr::{call::Call, BinaryOp, CompareMode, Expr, OrderMode, UnaryOp},
        statement::{Block, Statement},
        types::{CConcreteType, CFunc, CFuncBody, CPrimitive, CType},
        Symbol,
    },
};

#[derive(Clone, Copy)]
struct Breakable {
    loop_label: LabelId,
    done_label: LabelId,
    stack_top_pos: i32,
}

pub(super) struct FuncGenerator<'a, 'b> {
    generator: &'b Generator,
    b: &'b mut FuncBuilder<'a>,
    stack_top_pos: i32,
    named_vars: HashMap<String, StackLocal>,
    frame: Vec<(u32, bool)>,
    breakable_stack: VecDeque<Breakable>,
    done_label: LabelId,
}

impl<'a, 'b> FuncGenerator<'a, 'b> {
    pub fn new(generator: &'b Generator, builder: &'b mut FuncBuilder<'a>) -> Self {
        let done_label = builder.create_label("done");

        Self {
            generator,
            b: builder,
            stack_top_pos: 0,
            named_vars: HashMap::default(),
            frame: Vec::default(),
            breakable_stack: VecDeque::default(),
            done_label,
        }
    }

    /// Allocate space for a local variable, return its stack frame offset.
    fn allocate_var(&mut self, name: String, ctype: CType) -> StackLocal {
        self.b.header(format!(
            "{} {name};",
            self.generator.program.format_ctype(ctype)
        ));

        let var = self.allocate_anon(ctype);
        self.named_vars.insert(name, var);

        var
    }

    /// Allocate space for an anonymous variable, return its stack frame offset.
    fn allocate_anon(&mut self, ctype: impl Into<CType>) -> StackLocal {
        let ctype = ctype.into();

        let size = self.generator.sizeof_ctype(ctype);
        let actual_size = Self::align(size);

        // look for a free spot first before allocing.
        let mut offset = 0;

        for (spot_size, in_use) in self.frame.iter_mut() {
            offset += *spot_size;

            if *spot_size == actual_size && !*in_use {
                *in_use = true;

                return StackLocal {
                    offset: offset as i32,
                    ctype,
                };
            }
        }

        self.stack_top_pos -= actual_size as i32;
        self.frame.push((actual_size, true));

        let local = StackLocal {
            offset: self.stack_top_pos,
            ctype,
        };

        self.b.inline_comment(format!(
            "ALLOC {typename} ({bytes} bytes) at {location} (Expecting SP = {sp:#010x})",
            typename = self.generator.program.format_ctype(ctype),
            bytes = actual_size,
            location = self.b.format_address(&Address::from(local)),
            sp = 0x00100000 + self.stack_top_pos - 0x8
        ));

        self.b.sub(Reg::Sp, Reg::Sp, actual_size as i32);

        local
    }

    fn align<Scalar>(offset: Scalar) -> Scalar
    where
        Scalar: Shl<Output = Scalar>,
        Scalar: Shr<Output = Scalar>,
        Scalar: Ord,
        Scalar: From<u8>,
    {
        let mut actual_offset: Scalar = (offset >> 2.into()) << 2.into();

        if actual_offset < 4.into() {
            actual_offset = 4.into();
        }

        actual_offset
    }

    /// Free a local variable on the stack. Its spot can now be recycled.
    fn free_local(&mut self, var: StackLocal) {
        let offset = var.offset;

        if offset == self.stack_top_pos {
            #[cfg(debug_assertions)]
            println!("Freeing top of stack ({var:?})");

            let Some(top_slot) = self.frame.last_mut() else {
                panic!("Can't call `forget` with an empty stack!");
            };
            top_slot.1 = false;

            let old_stack_top_pos = self.stack_top_pos;

            // cascade downward and clear out all the unused vars before us.
            while let Some((size, _)) = self.frame.pop_if(|(_, in_use)| !*in_use) {
                self.stack_top_pos += size as i32;
            }

            #[cfg(debug_assertions)]
            println!(
                "frame after pop: {:?}, top = {}",
                self.frame, self.stack_top_pos
            );

            self.b
                .add(Reg::Sp, Reg::Sp, self.stack_top_pos - old_stack_top_pos);

            return;
        }

        #[cfg(debug_assertions)]
        println!("=> Freeing {var:?} via stack surgery");

        // stack surgery time (aka me writing horribly inefficient code that's Good Enough(tm) for learning)
        let mut total_offset = 0;

        for (size, in_use) in self.frame.iter_mut() {
            total_offset -= *size as i32;

            // if this is the target, mark as free.
            if total_offset == offset {
                *in_use = false;
                break;
            }
        }
    }

    fn forget_stack_locals(&mut self, count: usize) {
        for _ in 0..count {
            self.stack_top_pos += self.frame.pop().unwrap().0 as i32;
        }
    }

    /// Get the offset of a previously defined local variable, if it exists.
    fn get_localvar(&self, name: &str) -> Option<StackLocal> {
        if let Some(offset) = self.named_vars.get(name) {
            return Some(*offset);
        }

        let mut offset: i32 = 4;

        if self.generator.sizeof_ctype(self.b.sig.returns) > WORD_SIZE {
            // 0th implicit argument is the return address.
            offset += 4;
        }

        for arg in self.b.sig.args.iter() {
            offset += Self::align(self.generator.sizeof_ctype(arg.ctype)) as i32;

            if arg.name.as_deref() == Some(name) {
                return Some(StackLocal {
                    offset,
                    ctype: arg.ctype,
                });
            }
        }

        None
    }

    pub fn reg(&mut self) -> Reg {
        self.b.reg()
    }

    pub fn regs<const N: usize>(&mut self) -> [Reg; N] {
        self.b.regs()
    }

    pub fn release_reg<const N: usize>(&mut self, reg: impl Into<[Reg; N]>) {
        self.b.release_reg(reg);
    }

    pub(super) fn generate_func(mut self, cfunc: &CFunc) -> anyhow::Result<()> {
        let block = match &cfunc.body {
            CFuncBody::Defined(block) => block,
            CFuncBody::Extern => return Ok(()),
            CFuncBody::None => bail!("Function {cfunc:?} left undefined!"),
        };

        if cfunc.is_raw_assembly {
            self.generate_block(block)?;
            return Ok(());
        }

        self.b.push([Reg::R11, Reg::LinkReg]);
        self.b.mov(Reg::R11, Reg::Sp);

        self.generate_block(block)?;

        if self
            .generator
            .program
            .get_cfunc_sig(cfunc.sig_id)
            .is_noreturn
        {
            return Ok(());
        }

        self.b.label(self.done_label);
        self.b.mov(Reg::Sp, Reg::R11);
        self.b.pop([Reg::R11, Reg::LinkReg]);

        if !self.b.sig.args.is_empty() {
            self.b.inline_comment("Eat the caller's args.");
            self.b.add(
                Reg::Sp,
                Reg::Sp,
                self.b
                    .sig
                    .args
                    .iter()
                    .map(|arg| Self::align(self.generator.sizeof_ctype(arg.ctype)))
                    .sum::<u32>(),
            );
        }

        self.b.ret();

        Ok(())
    }

    fn generate_block(&mut self, block: &Block) -> anyhow::Result<()> {
        for stmt in &block.statements {
            self.generate_stmt(stmt)?;
        }

        Ok(())
    }

    fn generate_stmt(&mut self, stmt: &Statement) -> anyhow::Result<()> {
        match stmt {
            Statement::Declare(variable) => {
                let var = self.allocate_var(variable.name.clone(), variable.ctype);

                if let Some(expr) = &variable.value {
                    // eval the initial val.
                    self.generate_expr(expr, var)?;
                }
            }

            Statement::Expr(expr) => self.generate_expr(expr, NOWHERE)?,

            Statement::Return(expr) => {
                let temp_storage = self.allocate_anon(self.b.sig.returns);

                self.b.header("<return>");

                self.generate_expr(expr, temp_storage)?;

                if self.generator.sizeof_ctype(temp_storage.ctype) > WORD_SIZE {
                    // the caller pushes an address where they want us to copy the returned
                    // (presumably) struct to to return it to them, rather than splitting it
                    // across registers or something.
                    let address_holder = self.reg();
                    let value_holder = self.reg();

                    self.b.inline_comment("Grab the return storage address.");
                    self.b.ldr(address_holder, stack_offset(4));
                    self.b.ldr(value_holder, temp_storage);
                    self.b.str(value_holder, Address::at(address_holder));

                    self.release_reg(value_holder);
                    self.release_reg(address_holder);
                } else {
                    // we can neatly return the value in one register. yay!
                    self.b.inline_comment("Put the returned value in R0.");
                    self.b.ldr(Reg::R0, temp_storage);
                }

                self.free_local(temp_storage);

                self.b.inline_comment("Perform the cleanup.");
                self.b.b(self.done_label);

                self.b.footer("</return>");
            }

            Statement::Break => {
                let breakable = *self
                    .breakable_stack
                    .back()
                    .context("break can't be used outside a breakable")?;

                self.b.header("<break>");

                self.b.add(
                    Reg::Sp,
                    Reg::Sp,
                    breakable.stack_top_pos - self.stack_top_pos,
                );

                self.b.b(breakable.done_label);

                self.b.footer("</break>");
            }

            Statement::Continue => self.generate_continue_stmt()?,

            Statement::If {
                condition,
                if_true,
                if_false,
            } => {
                let if_false_label = self.b.create_label("If__else");
                let done_label = self.b.create_label("If__done");

                self.b.header(format!("## if ({condition})"));

                let temp_condition_storage = self.allocate_anon(CPrimitive::Bool);
                self.generate_expr(condition, temp_condition_storage)?;

                let condition_holder = self.reg();

                self.b.ldr(condition_holder, temp_condition_storage);
                self.free_local(temp_condition_storage);

                self.b.cmp(condition_holder, 0);

                self.release_reg(condition_holder);

                if if_false.is_some() {
                    self.b.beq(if_false_label);
                } else {
                    self.b.beq(done_label);
                }

                self.b.header("### then");
                self.generate_stmt(if_true)?;

                if let Some(if_false) = if_false {
                    self.b.b(done_label);

                    self.b.header("### else");
                    self.b.label(if_false_label);
                    self.generate_stmt(if_false)?;
                }

                self.b.footer(format!("## endif ({condition})"));
                self.b.label(done_label);
            }

            Statement::While { condition, block } => {
                let loop_label = self.b.create_label("While__loop");
                let done_label = self.b.create_label("While__done");

                self.b.header(format!("while ({condition})"));
                let temp_condition_storage = self.allocate_anon(CPrimitive::Bool);

                let breakable = Breakable {
                    loop_label,
                    done_label,
                    stack_top_pos: self.stack_top_pos,
                };

                self.breakable_stack.push_back(breakable);

                self.b.label(loop_label);
                self.generate_expr(condition, temp_condition_storage)?;

                let condition_holder = self.reg();

                self.b.ldr(condition_holder, temp_condition_storage);
                self.b.cmp(condition_holder, 0);

                self.release_reg(condition_holder);

                self.b.beq(done_label);

                self.b.header("do");
                self.generate_stmt(block)?;

                self.generate_continue_stmt()?;

                self.b.footer(format!("endwhile ({condition})"));
                self.b.label(done_label);
                self.free_local(temp_condition_storage);

                self.breakable_stack.pop_back();
            }

            Statement::Block(block) => self.generate_block(block)?,

            Statement::Asm(content) => self.b.asm(content),
        };

        Ok(())
    }

    fn generate_continue_stmt(&mut self) -> anyhow::Result<()> {
        let breakable = *self
            .breakable_stack
            .back()
            .context("continue can't be used outside a breakable")?;

        self.b.header("<continue>");

        self.b.add(
            Reg::Sp,
            Reg::Sp,
            breakable.stack_top_pos - self.stack_top_pos,
        );

        self.b.b(breakable.loop_label);

        self.b.footer("</continue>");

        Ok(())
    }

    /// Generate instructions to perform the given operation, returning the instructions required to
    /// perform it, and place the output at `[R11-#result_offset]`.
    fn generate_expr(
        &mut self,
        expr: &Expr,
        destination: impl Into<TypedLocation>,
    ) -> anyhow::Result<()> {
        let destination = destination.into();

        self.b.header(format!("EVALUATE `{expr}`"));

        match expr {
            Expr::StringLiteral(value) => {
                let string_id = self.generator.file_builder.create_string(value);
                let string_address_holder = self.reg();

                self.b.mov(string_address_holder, string_id);
                self.b.str(string_address_holder, destination.address);

                self.release_reg(string_address_holder);
            }

            Expr::IntLiteral(value) => {
                // ints are nice and relaxed :)
                let int_holder = self.reg();

                self.b.mov(int_holder, *value);
                self.b.str(int_holder, destination.address);

                self.release_reg(int_holder);
            }

            Expr::Reference(name) => {
                if let Some(source) = self.get_localvar(name) {
                    self.b.header(format!(
                        "=== query localvar `{name}` ({}) as a {} ===",
                        self.generator.program.format_ctype(source.ctype),
                        self.generator.program.format_ctype(destination.ctype)
                    ));

                    match (source.ctype, destination.ctype) {
                        (CType::AsIs(_), CType::AsIs(_))
                        | (CType::PointerTo(_), CType::PointerTo(_)) => {
                            let value_holder = self.reg();

                            self.b.ldr(value_holder, source);
                            self.b.str(value_holder, destination.address);

                            self.release_reg(value_holder);
                        }

                        (CType::ArrayOf(_, _), CType::ArrayOf(_, _)) => {
                            todo!("array copying!");
                        }

                        (CType::ArrayOf(_, _), CType::PointerTo(_)) => {
                            let address_holder = self.reg();

                            self.b.load_address(address_holder, source);
                            self.b.str(address_holder, destination.address);

                            self.release_reg(address_holder);
                        }

                        _ => bail!(
                            "Can't cast type `{}` to `{}`",
                            self.generator.program.format_ctype(source.ctype),
                            self.generator.program.format_ctype(destination.ctype)
                        ),
                    }

                    self.b.footer(format!("=== done query `{name}` ==="));
                    return Ok(());
                }

                let Some(symbol) = self.generator.program.get_symbol(name) else {
                    bail!("Reference to undefined variable `{name}`");
                };

                match symbol {
                    Symbol::Func(cfunc) => {
                        let reg = self.reg();

                        self.b.asm(format!("MOV {reg}, fn_{name}"));
                        self.b.str(reg, destination.address);

                        self.release_reg(reg);
                    }

                    Symbol::Var(var_ctype, _) => {
                        match var_ctype {
                            // FIXME FIXME FIXME: This is a copy-pasted impl of the above with
                            // tiny changes. Really, we need a unified `Assign` implementation
                            // which can go between *any* storage source and destinations, with
                            // *any* types, and use it instead rather than bespoke solutions
                            // everywhere for every single operation.
                            CType::AsIs(cconcrete_type) => {
                                let reg = self.reg();

                                self.b.asm(format!("MOV {reg}, var_{name}"));
                                self.b.ldr(reg, reg + 0);
                                self.b.str(reg, destination.address);

                                self.release_reg(reg);
                            }

                            CType::PointerTo(ctype_id) => match destination.ctype {
                                CType::AsIs(cconcrete_type) => {
                                    panic!("can't cast a pointer to a value type")
                                }

                                CType::PointerTo(ctype_id) => {
                                    let reg = self.reg();

                                    self.b.asm(format!("MOV {reg}, var_{name}"));
                                    self.b.ldr(reg, reg + 0);
                                    self.b.str(reg, destination.address);

                                    self.release_reg(reg);
                                }

                                CType::ArrayOf(ctype_id, size) => {
                                    todo!("load array from pointer")
                                }
                            },

                            CType::ArrayOf(ctype_id, _) => match destination.ctype {
                                CType::AsIs(cconcrete_type) => {
                                    panic!("can't cast an array to a value type")
                                }

                                CType::PointerTo(ctype_id) => {
                                    let reg = self.reg();

                                    self.b.asm(format!("MOV {reg}, var_{name}"));
                                    self.b.ldr(reg, reg + 0);
                                    self.b.str(reg, destination.address);

                                    self.release_reg(reg);
                                }

                                CType::ArrayOf(ctype_id, _) => {
                                    todo!("array copying!")
                                }
                            },
                        }
                    }
                };
            }

            Expr::Call(call) => self.generate_call(&call.target, &call.args, destination)?,

            Expr::BinaryOp(op, left, right) => {
                self.generate_binop(destination, *op, left, right)?
            }

            Expr::UnaryOp(op, expr) => match op {
                UnaryOp::IncrementThenGet => todo!(),
                UnaryOp::GetThenIncrement => todo!(),
                UnaryOp::DecrementThenGet => todo!(),
                UnaryOp::GetThenDecrement => todo!(),

                UnaryOp::SizeOf => {
                    let size = self.generator.sizeof_ctype(self.type_of_expr(expr)?);

                    match destination.ctype {
                        CType::AsIs(cconcrete_type) => match cconcrete_type {
                            CConcreteType::Struct(_) => {
                                bail!("A struct is not a valid assignment target for a number")
                            }
                            CConcreteType::Func(_) => {
                                bail!("A function pointer is not a valid assignment target for a number")
                            }
                            CConcreteType::Enum(_) => todo!(),

                            CConcreteType::Primitive(primitive) => match primitive {
                                CPrimitive::Void => (),

                                CPrimitive::Bool | CPrimitive::Char | CPrimitive::UnsignedChar => {
                                    let reg = self.reg();

                                    self.b.mov(reg, size & 0xff);
                                    self.b.strb(reg, destination.address);

                                    self.release_reg(reg);
                                }

                                CPrimitive::SignedChar => {
                                    let reg = self.reg();

                                    self.b.mov(reg, size & 0x7f);
                                    self.b.strb(reg, destination.address);

                                    self.release_reg(reg);
                                }

                                CPrimitive::Short | CPrimitive::UnsignedShort => {
                                    let reg = self.reg();

                                    self.b.ldr(reg, destination.address);
                                    self.b.shr(reg, reg, 16);
                                    self.b.shl(reg, reg, 16);
                                    self.b.or(reg, reg, size & 0xffff);
                                    self.b.str(reg, destination.address);

                                    self.release_reg(reg);
                                }

                                CPrimitive::Int
                                | CPrimitive::UnsignedInt
                                | CPrimitive::Long
                                | CPrimitive::UnsignedLong => {
                                    let reg = self.reg();

                                    self.b.mov(reg, size);
                                    self.b.str(reg, destination.address);

                                    self.release_reg(reg);
                                }

                                CPrimitive::LongLong | CPrimitive::UnsignedLongLong => {}

                                CPrimitive::Float | CPrimitive::Double | CPrimitive::LongDouble => {
                                    let reg = self.reg();

                                    self.b.mov(reg, size as f32);
                                    self.b.str(reg, destination.address);

                                    self.release_reg(reg);
                                }
                            },
                        },

                        CType::PointerTo(_) => {
                            todo!("A pointer is not a valid assignment target for a number")
                        }

                        CType::ArrayOf(_, _) => {
                            bail!("An array is not a valid assignment target for a number")
                        }
                    }
                }

                UnaryOp::BooleanNot => {
                    self.generate_expr(expr, destination)?;

                    if destination.is_somewhere() {
                        let upon_truthy_input = self.b.create_label("BooleanNot__uponTruthyInput");
                        let done = self.b.create_label("BooleanNot__done");

                        let reg = self.reg();

                        self.b.ldr(reg, destination.address);
                        self.b.cmp(reg, 0);
                        self.b.bne(upon_truthy_input);

                        self.b.mov(reg, 1);
                        self.b.b(done);

                        self.b.label(upon_truthy_input);
                        self.b.mov(reg, 0);

                        self.b.label(done);
                        self.b.str(reg, destination.address);

                        self.release_reg(reg);
                    }
                }

                UnaryOp::Negative => todo!(),

                UnaryOp::AddressOf => match &**expr {
                    Expr::StringLiteral(_) => todo!(),
                    Expr::IntLiteral(_) => todo!(),

                    Expr::Reference(name) => {
                        if let Some(var) = self.get_localvar(name) {
                            let reg = self.reg();

                            self.b.load_address(reg, var);
                            self.b.str(reg, destination.address);

                            self.release_reg(reg);

                            return Ok(());
                        }

                        todo!()
                    }

                    Expr::Call(call) => todo!(),
                    Expr::BinaryOp(binary_op, expr, expr1) => todo!(),
                    Expr::UnaryOp(unary_op, expr) => todo!(),
                    Expr::Cast(expr, ctype) => todo!(),
                },

                UnaryOp::Dereference => todo!(),
            },

            Expr::Cast(expr, _) => todo!(),
        };

        Ok(())
    }

    fn generate_call(
        &mut self,
        call_target: &Expr,
        call_args: &[Expr],
        destination: TypedLocation,
    ) -> anyhow::Result<()> {
        let CType::AsIs(CConcreteType::Func(sig_id)) = self.type_of_expr(call_target)? else {
            bail!("bad call target!");
        };

        let sig = self.generator.program.get_cfunc_sig(sig_id);

        // push args to stack first. caller pushes args, callee expected to pop em. args
        // pushed last first, so top of stack is the first argument.
        let initial_frame_top = self.stack_top_pos;

        let mut arg_target_spots = sig
            .args
            .iter()
            .rev()
            .map(|member| self.allocate_anon(member.ctype))
            .collect_vec();

        arg_target_spots.reverse();

        if arg_target_spots.len() != call_args.len() {
            bail!(
                "must pass the expected arg count in calling {call_target:?} with signature {sig_id:?}"
            );
        }

        let stack_pointer_adjustment = initial_frame_top - self.stack_top_pos;

        self.b.header(format!(
            "=== Call {1}({}) [{stack_pointer_adjustment} arg bytes] ===",
            call_args.iter().join(", "),
            call_target
        ));

        for (arg, out_info) in call_args.iter().zip(&arg_target_spots) {
            self.generate_expr(arg, *out_info)?;
        }

        match call_target {
            Expr::Reference(name) if self.get_localvar(name).is_none() => {
                // Direct reference to a function by name. sane common case! we can just BL
                // there using its label name.
                self.b.call(name);
            }

            expr => {
                // FIXME: we should support type coersion rules. ugghhhhh
                let temp_fn_ptr_var = self.allocate_anon(CType::AsIs(CConcreteType::Func(sig_id)));

                // Resolve the function pointer.
                self.generate_expr(expr, temp_fn_ptr_var)?;

                let reg = self.reg();
                self.b.ldr(reg, temp_fn_ptr_var);
                self.free_local(temp_fn_ptr_var);

                self.b
                    .inline_comment("Manually set LR (TODO: shouldn't this be +4??)");
                self.b.mov(Reg::LinkReg, Reg::ProgCounter);
                self.b.ldr(Reg::ProgCounter, Address::at(reg));

                self.release_reg(reg);
            }
        };

        self.forget_stack_locals(arg_target_spots.len());

        if destination.is_nowhere() {
            self.b
                .footer(format!("=== END Call {0} => void ===", call_target));
            return Ok(());
        };

        if self.generator.sizeof_ctype(sig.returns) <= WORD_SIZE {
            // Expecting callee to place the result in R0.
            self.b.str(Reg::R0, destination.address);

            self.b.footer(format!(
                "=== END Call {1} => R0 => {} ===",
                self.b.format_address(&destination.address),
                call_target
            ));
        } else {
            // no-op, we should've passed this info earlier.
        }

        Ok(())
    }

    fn generate_binop(
        &mut self,
        destination: TypedLocation,
        op: BinaryOp,
        left: &Expr,
        right: &Expr,
    ) -> anyhow::Result<()> {
        match op {
            BinaryOp::AndThen => {
                self.generate_expr(left, NOWHERE)?;
                self.generate_expr(right, destination)?;
            }
            BinaryOp::Assign => match left {
                Expr::StringLiteral(_) => bail!("A string is not an assignment target"),
                Expr::IntLiteral(_) => bail!("A number is not an assignment target"),
                Expr::Call(_) => bail!("A function call is not an assignment target"),

                Expr::Reference(name) => {
                    if let Some(local) = self.get_localvar(name) {
                        self.generate_expr(right, local)?;

                        if destination.is_somewhere() {
                            todo!("assign-and-return")
                        }

                        return Ok(());
                    }

                    bail!("Bad assignment target: undeclared variable `{name}`")
                }

                Expr::BinaryOp(binary_op, expr, expr1) => {
                    todo!("Assignment target result of BinaryOp")
                }
                Expr::UnaryOp(unary_op, expr) => {
                    todo!("Assignment target result of UnaryOp")
                }
                Expr::Cast(expr, ctype) => todo!("Cast assignment target >:("),
            },

            BinaryOp::PlusAssign => {
                // self.generate_binop(destination, BinaryOp::Plus, left, right)?;
                // self.generate_binop(destination, BinaryOp::Assign, left, right)?;
            }

            BinaryOp::MinusAssign => {}

            BinaryOp::LogicEqual(mode) => {
                let left_ctype = self.type_of_expr(left)?;
                let left_temp = self.allocate_anon(left_ctype);

                let right_ctype = self.type_of_expr(right)?;
                let right_temp = self.allocate_anon(right_ctype);

                self.generate_expr(left, left_temp)?;
                self.generate_expr(right, right_temp)?;

                if destination.is_nowhere() {
                    return Ok(());
                };

                let lhs_reg = self.reg();
                let rhs_reg = self.reg();

                self.b.ldr(lhs_reg, left_temp);
                self.b.ldr(rhs_reg, right_temp);

                self.free_local(left_temp);
                self.free_local(right_temp);

                let is_false = self.b.create_label("LogicEqual__isFalse");
                let done = self.b.create_label("LogicEqual__done");

                self.b.cmp(lhs_reg, rhs_reg);

                self.release_reg(lhs_reg);
                self.release_reg(rhs_reg);

                match mode {
                    CompareMode::Equal => self.b.bne(is_false),
                    CompareMode::NotEqual => self.b.beq(is_false),
                };

                let truthiness_reg = self.reg();

                self.b.mov(truthiness_reg, 1);
                self.b.b(done);

                self.b.label(is_false);
                self.b.mov(truthiness_reg, 0);

                self.b.label(done);
                self.b.str(truthiness_reg, destination.address);

                self.release_reg(truthiness_reg);
            }

            BinaryOp::Plus | BinaryOp::Minus => {
                if destination.is_nowhere() {
                    // discard the result.
                    return Ok(());
                };

                let opname = if op == BinaryOp::Plus { '+' } else { '-' };
                self.b
                    .header(format!("=== binop({left} {opname} {right}) ==="));

                let left_temp = self.allocate_anon(destination.ctype);
                let right_temp = self.allocate_anon(destination.ctype);

                self.generate_expr(left, left_temp)?;
                self.generate_expr(right, right_temp)?;

                let [left_reg, right_reg] = self.regs();

                self.b.ldr(left_reg, left_temp);
                self.b.ldr(right_reg, right_temp);

                self.free_local(left_temp);
                self.free_local(right_temp);

                match op {
                    BinaryOp::Plus => self.b.add(left_reg, left_reg, right_reg),
                    BinaryOp::Minus => self.b.sub(left_reg, left_reg, right_reg),
                    _ => unreachable!(),
                };

                self.release_reg(right_reg);
                self.b.str(left_reg, destination.address);
                self.release_reg(left_reg);

                self.b
                    .footer(format!("=== END binop({left} {opname} {right}) ==="));
            }

            BinaryOp::ArrayIndex => {
                if destination.is_nowhere() {
                    // discard the result.
                    return Ok(());
                };

                let (CType::ArrayOf(element_ctypeid, _) | CType::PointerTo(element_ctypeid)) =
                    self.type_of_expr(left)?
                else {
                    bail!("{left} has a non-indexable type");
                };

                let element_ctype = self.generator.program.get_ctype(element_ctypeid);
                let element_size = self.generator.sizeof_ctype(element_ctype);

                let temp_index_storage = self.allocate_anon(CPrimitive::Int);
                let temp_ptr_storage =
                    self.allocate_anon(self.generator.program.pointer_to(CPrimitive::Void));

                self.generate_expr(right, temp_index_storage)?;
                self.generate_expr(left, temp_ptr_storage)?;

                let result_size = self.generator.sizeof_ctype(destination.ctype);

                let ele_pointer = self.reg();
                let ele_index = self.reg();

                self.b.ldr(ele_pointer, temp_ptr_storage);
                self.b.ldr(ele_index, temp_index_storage);

                self.b.header("freeing index & ptr");
                self.free_local(temp_index_storage);
                self.free_local(temp_ptr_storage);

                match element_size {
                    1 => {
                        self.b.ldrb(ele_pointer, ele_pointer + ele_index);

                        if result_size == 1 {
                            self.b.strb(ele_pointer, destination.address);
                        } else {
                            self.b.str(ele_pointer, destination.address);
                        }
                    }

                    2 => {
                        // FIXME: I'm probably doing this big-endian by accident!!
                        let temp = self.reg();

                        self.b.shl(temp, ele_index, 1);
                        self.b.add(temp, ele_pointer, temp);
                        self.b.ldrb(ele_pointer, temp + 0);
                        self.b.ldrb(ele_index, temp + 1);

                        self.release_reg(temp);

                        self.b.shl(ele_pointer, ele_pointer, 8);
                        self.b.or(ele_pointer, ele_pointer, ele_index);
                        self.b.str(ele_pointer, destination.address);
                    }

                    3 => {
                        // FIXME: I'm probably doing this big-endian by accident!!
                        let temp = self.reg();

                        self.b.shl(temp, ele_index, 1);
                        self.b.add(temp, temp, ele_index);
                        self.b.add(temp, ele_pointer, temp);
                        self.b.ldrb(ele_pointer, temp + 0);
                        self.b.ldrb(ele_index, temp + 1);
                        self.b.shl(ele_pointer, ele_pointer, 8);
                        self.b.or(ele_pointer, ele_pointer, ele_index);
                        self.b.ldrb(ele_index, temp + 2);

                        self.release_reg(temp);

                        self.b.shl(ele_pointer, ele_pointer, 8);
                        self.b.or(ele_pointer, ele_pointer, ele_index);
                        self.b.str(ele_pointer, destination.address);
                    }

                    4 => {
                        self.b.shl(ele_index, ele_index, 2);
                        self.b.ldr(ele_pointer, ele_pointer + ele_index);
                        self.b.str(ele_pointer, destination.address);
                    }

                    _ => todo!("{element_ctype:?} ({element_size} bytes) can't fit in a register"),
                }

                self.release_reg(ele_pointer);
                self.release_reg(ele_index);
            }
            BinaryOp::BitwiseLeftShift => todo!(),
            BinaryOp::BitwiseRightShift => todo!(),

            BinaryOp::LogicOrdering(mode) => {
                let left_ctype = self.type_of_expr(left)?;
                let right_ctype = self.type_of_expr(right)?;

                let left_temp = self.allocate_anon(left_ctype);
                let right_temp = self.allocate_anon(right_ctype);

                self.generate_expr(left, left_temp)?;
                self.generate_expr(right, right_temp)?;

                if destination.is_nowhere() {
                    // discard the result.
                    return Ok(());
                }

                let [left_reg, right_reg] = self.regs();

                self.b.ldr(left_reg, left_temp);
                self.b.ldr(right_reg, right_temp);

                self.free_local(left_temp);
                self.free_local(right_temp);

                let upon_true = self.b.create_label("LogicOrdering__uponTrue");
                let done = self.b.create_label("LogicOrdering__done");

                self.b.cmp(left_reg, right_reg);

                self.release_reg([left_reg, right_reg]);

                match mode {
                    OrderMode::LessThan => self.b.blt(upon_true),
                    OrderMode::GreaterThan => self.b.bgt(upon_true),
                    _ => todo!(),
                };

                let condition_reg = self.reg();

                self.b.mov(condition_reg, 0);
                self.b.b(done);

                self.b.label(upon_true);
                self.b.mov(condition_reg, 1);

                self.b.label(done);
                self.b.str(condition_reg, destination.address);

                self.release_reg(condition_reg);
            }

            BinaryOp::BitwiseXor => todo!(),
            BinaryOp::BitwiseAnd => todo!(),
            BinaryOp::BitwiseOr => todo!(),
            BinaryOp::LogicAnd | BinaryOp::LogicOr => {
                let is_and = matches!(op, BinaryOp::LogicAnd);

                let condition_storage = self.allocate_anon(CPrimitive::Bool);

                let done_label = self.b.create_label(if is_and {
                    "LogicAnd__done"
                } else {
                    "LogicOr__done"
                });

                self.b.header("Evaluate LHS");
                self.generate_expr(left, condition_storage)?;

                self.b.header("Test if LHS is truthy");

                let condition_reg = self.reg();

                self.b.ldr(condition_reg, condition_storage);
                self.b.cmp(condition_reg, 0);

                self.release_reg(condition_reg);

                if is_and {
                    self.b.beq(done_label);
                    self.b.header("LHS is truthy, evaluate RHS");
                } else {
                    self.b.bne(done_label);
                    self.b.header("LHS is falsey, evaluate RHS");
                }

                self.generate_expr(right, condition_storage)?;

                self.b.label(done_label);

                if destination.is_somewhere() {
                    if destination.ctype != CPrimitive::Bool.into() {
                        todo!(
                            "Cast bool result to {}",
                            self.generator.program.format_ctype(destination.ctype)
                        )
                    }

                    let condition_reg = self.reg();

                    self.b.ldr(condition_reg, condition_storage);
                    self.b.str(condition_reg, destination.address);

                    self.release_reg(condition_reg);
                }

                self.free_local(condition_storage);
            }
        };

        Ok(())
    }

    fn type_of_expr(&self, expr: &Expr) -> anyhow::Result<CType> {
        match expr {
            Expr::StringLiteral(_) => Ok(CType::PointerTo(
                self.generator.program.ctype_id_of(CPrimitive::Char),
            )),

            Expr::IntLiteral(_) => Ok(CPrimitive::Int.into()),

            Expr::Reference(name) => {
                if let Some(local) = self.get_localvar(name) {
                    return Ok(local.ctype);
                }

                for arg in &self.b.sig.args {
                    if arg.name.as_ref() == Some(name) {
                        return Ok(arg.ctype);
                    }
                }

                if let Some(symbol) = self.generator.program.get_symbol(name) {
                    return match symbol {
                        Symbol::Func(cfunc) => Ok(CType::AsIs(cfunc.sig_id.into())),
                        Symbol::Var(ctype, _) => Ok(*ctype),
                    };
                }

                bail!("Can't get the ctype of the undefined variable `{name}`")
            }

            Expr::Call(call) => match self.type_of_expr(&call.target)? {
                CType::AsIs(CConcreteType::Func(sig_id)) => Ok(sig_id.into()),
                CType::AsIs(inner) => bail!(
                    "{} is not a valid call target",
                    self.generator.program.format_ctype(inner)
                ),
                CType::PointerTo(_) => {
                    bail!("Non-function pointer is not a valid call target")
                }
                CType::ArrayOf(_, _) => bail!("An array is not a valid call target"),
            },

            Expr::BinaryOp(op, left, right) => match op {
                BinaryOp::AndThen => self.type_of_expr(right),

                BinaryOp::Assign | BinaryOp::PlusAssign | BinaryOp::MinusAssign => {
                    self.type_of_expr(left)
                }

                BinaryOp::LogicEqual(_)
                | BinaryOp::LogicOrdering(_)
                | BinaryOp::LogicAnd
                | BinaryOp::LogicOr => Ok(CPrimitive::Bool.into()),

                BinaryOp::Plus | BinaryOp::Minus => {
                    let left_ctype = self.type_of_expr(left);
                    let right_ctype = self.type_of_expr(right);

                    todo!("numeric type implicit conversion rules")
                }

                BinaryOp::ArrayIndex => match self.type_of_expr(left)? {
                    CType::AsIs(_) => bail!("Can't index a concrete type as an array!"),
                    CType::PointerTo(inner) | CType::ArrayOf(inner, _) => {
                        Ok(self.generator.program.get_ctype(inner))
                    }
                },

                BinaryOp::BitwiseLeftShift => todo!(),
                BinaryOp::BitwiseRightShift => todo!(),
                BinaryOp::BitwiseXor => todo!(),
                BinaryOp::BitwiseAnd => todo!(),
                BinaryOp::BitwiseOr => todo!(),
            },

            Expr::UnaryOp(op, expr) => match op {
                UnaryOp::IncrementThenGet
                | UnaryOp::GetThenIncrement
                | UnaryOp::DecrementThenGet
                | UnaryOp::GetThenDecrement => self.type_of_expr(expr),

                UnaryOp::SizeOf => Ok(CPrimitive::Int.into()),
                UnaryOp::BooleanNot => Ok(CPrimitive::Bool.into()),
                UnaryOp::Negative => self.type_of_expr(expr),

                UnaryOp::AddressOf => Ok(CType::PointerTo(
                    self.generator.program.ctype_id_of(self.type_of_expr(expr)?),
                )),

                UnaryOp::Dereference => match self.type_of_expr(expr)? {
                    CType::AsIs(_) => bail!("Can't dereference a concrete type!"),
                    CType::PointerTo(inner) | CType::ArrayOf(inner, _) => {
                        Ok(self.generator.program.get_ctype(inner))
                    }
                },
            },

            // fixme: blindly accepting whatever the programmer is saying here right now! we really
            // should define a stricter ast that simply doesn't allow invalid constructs, like
            // casting a struct to a number, for instance.
            Expr::Cast(expr, ctype) => Ok(*ctype),
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
    Address::offset(Reg::R11, offset)
}

#[derive(Clone, Copy)]
struct TypedLocation {
    ctype: CType,
    address: Address,
}

impl TypedLocation {
    const fn new(ctype: CType, address: Address) -> Self {
        Self { ctype, address }
    }

    const fn is_nowhere(&self) -> bool {
        matches!(
            self.ctype,
            CType::AsIs(CConcreteType::Primitive(CPrimitive::Void))
        )
    }

    const fn is_somewhere(&self) -> bool {
        !self.is_nowhere()
    }
}

impl From<StackLocal> for TypedLocation {
    fn from(value: StackLocal) -> Self {
        Self::new(value.ctype, stack_offset(value.offset))
    }
}

const NOWHERE: TypedLocation = TypedLocation::new(
    CType::AsIs(CConcreteType::Primitive(CPrimitive::Void)),
    Address::LiteralIndex(LiteralIndexAddress {
        base: Reg::LinkReg,
        offset: 0,
    }),
);
