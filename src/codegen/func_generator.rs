use std::{
    array,
    cmp::Ordering,
    collections::{HashMap, HashSet, VecDeque},
    ops::{Add, Not, Shl, Shr},
};

use anyhow::{bail, Context};
use indexmap::IndexMap;
use itertools::Itertools;

use crate::{
    codegen::{
        arm::{
            address::{Address, LiteralIndexAddress},
            location::Location,
            value::ValueWidth,
        },
        func_builder::{FuncBuilder, LabelId},
        Generator, Reg, WORD_SIZE,
    },
    parser::program::{
        ctype::{self, CConcreteType, CFunc, CFuncBody, CPrimitive, CType, CTypeId, Member},
        expr::{self, call::Call, BinaryOp, CompareMode, Expr, OrderMode, UnaryOp},
        statement::{Block, Statement, Variable},
        ExecutionScope, Symbol,
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
        self.b
            .header(format!("{} {name};", self.format_ctype(ctype)));

        let var = self.allocate_anon(ctype);
        self.named_vars.insert(name, var);

        var
    }

    /// Allocate space for an anonymous variable, return its stack frame offset.
    fn allocate_anon(&mut self, ctype: impl Into<CType>) -> StackLocal {
        let ctype = ctype.into();

        let size = self.generator.sizeof_ctype(ctype);
        let actual_size = self.generator.align(size);

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
            typename = self.format_ctype(ctype),
            bytes = actual_size,
            location = self.b.format_address(&Address::from(local)),
            sp = 0x00100000 + self.stack_top_pos - 0x8
        ));

        self.b.sub(Reg::Sp, Reg::Sp, actual_size as i32);

        local
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
            offset += self.generator.align(self.generator.sizeof_ctype(arg.ctype)) as i32;

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
        self.b.move_dword(Reg::R11, Reg::Sp);

        self.generate_block(block)?;

        if self
            .generator
            .program
            .get_signature(cfunc.sig_id)
            .is_noreturn
        {
            return Ok(());
        }

        self.b.label(self.done_label);
        self.b.move_dword(Reg::Sp, Reg::R11);
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
                    .map(|arg| self.generator.align(self.generator.sizeof_ctype(arg.ctype)))
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
                    self.evaluate_expression(expr, var)?;
                }
            }

            Statement::Expr(expr) => self.evaluate_expression(expr, NOWHERE)?,

            Statement::Return(expr) => {
                let temp_storage = self.allocate_anon(self.b.sig.returns);

                self.b.header("<return>");

                self.evaluate_expression(expr, temp_storage)?;

                if self.generator.sizeof_ctype(temp_storage.ctype) > WORD_SIZE {
                    // the caller pushes an address where they want us to copy the returned
                    // (presumably) struct to to return it to them, rather than splitting it
                    // across registers or something.
                    let [address_holder, value_holder] = self.regs();

                    self.b.inline_comment("Grab the return storage address.");
                    self.b.load_dword(address_holder, stack_offset(4));

                    self.b
                        .inline_comment("Put the returned value where requested.");
                    self.b.load_dword(value_holder, temp_storage);
                    self.b
                        .store_dword(value_holder, Address::at(address_holder));

                    self.release_reg([address_holder, value_holder]);
                } else {
                    // we can neatly return the value in one register. yay!
                    self.b.inline_comment("Put the returned value in R0.");
                    self.b.load_dword(Reg::R0, temp_storage);
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

                let condition_debug_str = self.format_expr(condition);

                self.b.header(format!("## if ({condition_debug_str})"));

                let temp_condition_storage = self.allocate_anon(CPrimitive::Bool);
                self.evaluate_expression(condition, temp_condition_storage)?;

                let condition_holder = self.reg();

                self.b.load_dword(condition_holder, temp_condition_storage);
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

                self.b.footer(format!("## endif ({condition_debug_str})"));
                self.b.label(done_label);
            }

            Statement::While { condition, block } => {
                let loop_label = self.b.create_label("While__loop");
                let done_label = self.b.create_label("While__done");

                let condition_debug_str = self.format_expr(condition);

                self.b.header(format!("while ({condition_debug_str})"));
                let temp_condition_storage = self.allocate_anon(CPrimitive::Bool);

                let breakable = Breakable {
                    loop_label,
                    done_label,
                    stack_top_pos: self.stack_top_pos,
                };

                self.breakable_stack.push_back(breakable);

                self.b.label(loop_label);
                self.evaluate_expression(condition, temp_condition_storage)?;

                let condition_holder = self.reg();

                self.b.load_dword(condition_holder, temp_condition_storage);
                self.b.cmp(condition_holder, 0);

                self.release_reg(condition_holder);

                self.b.beq(done_label);

                self.b.header("do");
                self.generate_stmt(block)?;

                self.generate_continue_stmt()?;

                self.b.footer(format!("endwhile ({condition_debug_str})"));
                self.b.label(done_label);
                self.free_local(temp_condition_storage);

                self.breakable_stack.pop_back();
            }

            Statement::Block(block) => self.generate_block(block)?,

            Statement::Asm(content) => self.b.asm(content),
            Statement::Empty => (),
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
    fn evaluate_expression(
        &mut self,
        expr: &Expr,
        destination: impl Into<TypedLocation>,
    ) -> anyhow::Result<()> {
        let destination = destination.into();

        println!(
            "\nGenerating `{expr}`, used registers = {registers}",
            expr = self.format_expr(expr),
            registers = self
                .b
                .register_pool
                .iter()
                .filter_map(|(reg, status)| if status.available {
                    None
                } else {
                    Some(format!("{reg}"))
                })
                .join(", ")
        );

        self.b
            .header(format!("EVALUATE `{expr}`", expr = self.format_expr(expr)));

        match expr {
            Expr::StringLiteral(value) => {
                let string_id = self.generator.file_builder.create_string(value);
                self.b.copy_dword(string_id, destination.location);
            }

            Expr::IntLiteral(value) => {
                // ints are nice and relaxed :)
                self.b.copy_dword(*value, destination.location);
            }

            Expr::BoolLiteral(value) => {
                let CType::AsIs(cconcrete_type) = destination.ctype else {
                    bail!("Can't coerce bool to pointer")
                };

                let cprimitive = match cconcrete_type {
                    CConcreteType::Struct(_) => bail!("Can't coerce bool to struct"),
                    CConcreteType::Func(_) => bail!("Can't coerce bool to function pointer"),
                    CConcreteType::Enum(_cenum_id) => todo!(),
                    CConcreteType::Primitive(cprimitive) => cprimitive,
                    CConcreteType::Void => return Ok(()),
                };

                match cprimitive {
                    CPrimitive::Bool
                    | CPrimitive::Char
                    | CPrimitive::SignedChar
                    | CPrimitive::UnsignedChar => self.b.copy_byte(*value, destination.location),

                    CPrimitive::Short | CPrimitive::UnsignedShort => {
                        self.b.copy_word(*value, destination.location)
                    }

                    CPrimitive::Int
                    | CPrimitive::UnsignedInt
                    | CPrimitive::Long
                    | CPrimitive::UnsignedLong => self.b.copy_dword(*value, destination.location),

                    CPrimitive::LongLong => todo!(),
                    CPrimitive::UnsignedLongLong => todo!(),
                    CPrimitive::Float => todo!(),
                    CPrimitive::Double => todo!(),
                    CPrimitive::LongDouble => todo!(),
                }
            }

            Expr::NullPtr => match destination.ctype {
                CType::AsIs(CConcreteType::Void) => (),

                CType::AsIs(CConcreteType::Primitive(CPrimitive::Bool)) => {
                    self.b.copy_byte(0, destination.location)
                }

                CType::PointerTo(_, None) => self.b.copy_dword(0, destination.location),

                _ => bail!(
                    "Can't assign {} to nullptr",
                    self.format_ctype(destination.ctype)
                ),
            },

            Expr::Reference(name) => {
                if let Some(source) = self.get_localvar(name) {
                    self.b.header(format!(
                        "=== query localvar `{name}` ({}) as a {} ===",
                        self.format_ctype(source.ctype),
                        self.format_ctype(destination.ctype)
                    ));

                    self.copy_lvalue(source, destination)?;

                    self.b.footer(format!("=== done query `{name}` ==="));
                    return Ok(());
                }

                let Some(symbol) = self.generator.program.get_symbol_by_name(name) else {
                    bail!("Reference to undefined variable `{name}`");
                };

                let reg = self.reg();

                match symbol {
                    Symbol::Func(cfunc) => {
                        self.b.asm(format!("MOV {reg}, fn_{name}"));
                        self.copy_lvalue(
                            TypedLocation::new(
                                CConcreteType::Func(cfunc.sig_id).into(),
                                Address::at(reg),
                            ),
                            destination,
                        )?;
                    }

                    Symbol::Var(var_ctype, _) => {
                        self.b.asm(format!("MOV {reg}, #var_{name}"));
                        self.copy_lvalue(
                            TypedLocation::new(*var_ctype, Address::at(reg)),
                            destination,
                        )?;
                    }
                };

                self.release_reg(reg);
            }

            Expr::Call(call) => self.generate_call(&call.target, &call.args, destination)?,

            Expr::BinaryOp(op, left, right) => {
                self.evauluate_binary_op(destination, op.clone(), left, right)?
            }

            Expr::UnaryOp(op, expr) => self.evaluate_unary_op(*op, expr, destination)?,

            Expr::Cast(expr, _) => {
                // Casting is just more persuasive type coercion. Our compiler doesn't really care
                // about type compatibility much, so casting is generally a no-op :P
                return self.evaluate_expression(expr, destination);
            }

            Expr::DotAccess { target, member } => {
                let (member_ctype, member_address) =
                    self.calculate_member_address(target, member)?;
                self.copy_lvalue(member_address.with_ctype(member_ctype), destination)?;
                self.release_reg(member_address.base);
            }
        };

        Ok(())
    }

    fn copy_lvalue(
        &mut self,
        source: impl Into<TypedLocation>,
        destination: impl Into<TypedLocation>,
    ) -> anyhow::Result<()> {
        let source = source.into();
        let destination = destination.into();

        if destination.is_nowhere() {
            return Ok(());
        }

        match (source.ctype, destination.ctype) {
            (CType::AsIs(source_concrete), CType::AsIs(destination_concrete))
                if source_concrete == destination_concrete =>
            {
                // Nice and easy!
                self.b.header("Same type, directly copy the element.");

                self.b.copy_bytes(
                    source.location,
                    destination.location,
                    self.generator.sizeof_ctype(source.ctype),
                );

                return Ok(());
            }

            (
                CType::AsIs(CConcreteType::Primitive(source_primitive)),
                CType::AsIs(CConcreteType::Primitive(destination_primitive)),
            ) => match (
                source_primitive.is_integer_type(),
                destination_primitive.is_integer_type(),
            ) {
                (true, true) => {
                    let source_size = self.generator.sizeof_ctype(source_primitive);

                    let destination_size = self.generator.sizeof_ctype(destination_primitive);

                    match (source_size, destination_size) {
                        (1, 1) | (2, 2) | (4, 4) | (8, 8) => {
                            self.b.header("Casting away signed-ness, enjoy some UB!");
                            self.b
                                .copy_bytes(source.location, destination.location, source_size)
                        }

                        // LE layout means we can just grab whatever portion of the source and
                        // stick it at the destination w/ padding zeros :)
                        (1, 2) => {
                            self.b.copy_word(0, destination.location);
                            self.b.copy_byte(source.location, destination.location);
                        }

                        (1, 4) => {
                            self.b.copy_dword(0, destination.location);
                            self.b.copy_byte(source.location, destination.location);
                        }

                        (2, 4) => {
                            self.b.copy_dword(0, destination.location);
                            self.b.copy_word(source.location, destination.location);
                        }

                        _ => {
                            self.b.header(format!("Casting to a smaller integer size ({source_primitive} to {destination_primitive})"));
                            self.b.copy_bytes(
                                source.location,
                                destination.location,
                                destination_size,
                            )
                        }
                    }
                }

                (true, false) => todo!(),
                (false, true) => todo!(),
                (false, false) => todo!(),
            },

            (CType::PointerTo(_, None), CType::PointerTo(_, None)) => {
                self.b.copy_dword(source.location, destination.location)
            }

            (CType::AsIs(_), CType::AsIs(_)) => {
                // FIXME: Blindly reinterpreting the source type as the destination type, this will
                // cause problems!!
                self.b.copy_bytes(
                    source.location,
                    destination.location,
                    self.generator.sizeof_ctype(destination.ctype),
                );
            }

            (CType::PointerTo(_, _), CType::PointerTo(_, None)) => {
                // Copy a pointer, or decay an array to a pointer to its first element.
                let Location::Address(source_address) = source.location else {
                    bail!(
                        "Cannot get the address of the location {} which is not in memory",
                        self.b.format_location(&source.location)
                    );
                };

                let address_holder = self.reg();

                self.b.load_address(address_holder, source_address, 0);
                self.b.copy_dword(address_holder, destination.location);

                self.release_reg(address_holder);
            }

            _ => bail!(
                "No implicit conversion from type `{}` to `{}`",
                self.format_ctype(source.ctype),
                self.format_ctype(destination.ctype)
            ),
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

        let sig = self.generator.program.get_signature(sig_id);

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
                "{target} has signature {sig}, but was called with arguments ({args}): {call}. {} != {}",
                arg_target_spots.len(),
                call_args.len(),
                target = self.format_expr(call_target),
                sig = self.format_ctype(sig_id),
                args = call_args
                    .iter()
                    .map(|arg| self.type_of_expr(arg))
                    .collect::<anyhow::Result<Vec<CType>>>()?
                    .into_iter()
                    .map(|ctype| self.format_ctype(ctype))
                    .join(", "),
                call = self.generator.program.source_writer.format_call(
                    &self.generator.program,
                    call_target,
                    call_args
                ),
            );
        }

        let stack_pointer_adjustment = initial_frame_top - self.stack_top_pos;

        self.b.header(format!(
            "=== Call {} [{stack_pointer_adjustment} arg bytes] ===",
            self.generator.program.source_writer.format_call(
                &self.generator.program,
                call_target,
                call_args
            )
        ));

        for (arg, out_info) in call_args.iter().zip(&arg_target_spots) {
            self.evaluate_expression(arg, *out_info)?;
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
                self.evaluate_expression(expr, temp_fn_ptr_var)?;

                let reg = self.reg();
                self.b.load_dword(reg, temp_fn_ptr_var);
                self.free_local(temp_fn_ptr_var);

                self.b
                    .inline_comment("Manually set LR (TODO: shouldn't this be +4??)");
                self.b.move_dword(Reg::LinkReg, Reg::ProgCounter);
                self.b.load_dword(Reg::ProgCounter, Address::at(reg));

                self.release_reg(reg);
            }
        };

        self.forget_stack_locals(arg_target_spots.len());

        if destination.is_nowhere() {
            self.b.footer(format!(
                "=== END Call {0} => void ===",
                self.format_expr(call_target)
            ));
            return Ok(());
        };

        if self.generator.sizeof_ctype(sig.returns) <= WORD_SIZE {
            // Expecting callee to place the result in R0.
            self.b.copy_dword(Reg::R0, destination.location);

            self.b.footer(format!(
                "=== END Call {1} => R0 => {} ===",
                self.b.format_location(&destination.location),
                self.format_expr(call_target),
            ));
        } else {
            // no-op, we should've passed this info earlier.
        }

        Ok(())
    }

    fn evauluate_binary_op(
        &mut self,
        destination: TypedLocation,
        op: BinaryOp,
        left: &Expr,
        right: &Expr,
    ) -> anyhow::Result<()> {
        match op {
            BinaryOp::AndThen => {
                self.evaluate_expression(left, NOWHERE)?;
                self.evaluate_expression(right, destination)?;
            }
            BinaryOp::Assign => match left {
                Expr::StringLiteral(_) => bail!("A string is not an assignment target"),
                Expr::IntLiteral(_) => bail!("A number is not an assignment target"),
                Expr::BoolLiteral(_) => bail!("A boolean is not an assignment target"),
                Expr::Call(_) => bail!("A function call is not an assignment target"),
                Expr::NullPtr => bail!("nullptr is not an assignment target"),

                Expr::Reference(name) => {
                    if let Some(local) = self.get_localvar(name) {
                        self.evaluate_expression(right, local)?;

                        if destination.is_somewhere() {
                            todo!("assign-and-return")
                        }

                        return Ok(());
                    }

                    let Some(symbol) = self.generator.program.get_symbol_by_name(name) else {
                        bail!("Bad assignment target: undeclared variable `{name}`")
                    };

                    match symbol {
                        Symbol::Func(cfunc) => {
                            bail!("Function `{name}` is not an assignment target")
                        }

                        Symbol::Var(var_ctype, _) => {
                            let target_address_reg = self.reg();
                            let target_location =
                                TypedLocation::new(*var_ctype, Address::at(target_address_reg));

                            self.b.asm(format!("MOV {target_address_reg}, #var_{name}"));
                            self.evaluate_expression(right, target_location)?;
                            self.copy_lvalue(target_location, destination)?;

                            self.release_reg(target_address_reg);
                        }
                    };
                }

                Expr::DotAccess { target, member } => {
                    let (member_ctype, address) = self.calculate_member_address(target, member)?;
                    let member_location = address.with_ctype(member_ctype);

                    self.evaluate_expression(right, member_location)?;
                    self.copy_lvalue(member_location, destination)?;

                    self.release_reg(address.base);
                }

                Expr::BinaryOp(BinaryOp::ArrayIndex, target_expr, index_expr) => {
                    let CType::PointerTo(element_ctype_id, _) = self.type_of_expr(target_expr)?
                    else {
                        bail!(
                            "Cannot dereference `{}`, which is not a pointer",
                            self.format_expr(target_expr)
                        );
                    };

                    let (element_ctype, ele_address) =
                        self.calculate_element_address(target_expr, index_expr)?;
                    self.evaluate_expression(right, ele_address.with_ctype(element_ctype))?;
                    self.release_reg(ele_address.base);
                }

                Expr::UnaryOp(UnaryOp::Dereference, target) => {
                    let CType::PointerTo(element_ctype_id, _) = self.type_of_expr(target)? else {
                        bail!(
                            "Cannot dereference `{}`, which is not a pointer",
                            self.format_expr(target)
                        );
                    };

                    let pointer_ctype = CType::pointer_to(element_ctype_id);

                    let pointer_holder = self.allocate_anon(pointer_ctype);
                    self.evaluate_expression(target, pointer_holder)?;

                    let pointer_reg = self.reg();

                    self.copy_lvalue(
                        pointer_holder,
                        TypedLocation::new(pointer_ctype, pointer_reg),
                    )?;

                    self.free_local(pointer_holder);

                    self.evaluate_expression(
                        right,
                        TypedLocation::new(
                            self.generator.program.get_ctype(element_ctype_id),
                            Address::at(pointer_reg),
                        ),
                    )?;

                    self.release_reg(pointer_reg);
                }

                Expr::BinaryOp(binary_op, expr, expr1) => {
                    todo!("Assignment target result of BinaryOp")
                }
                Expr::UnaryOp(unary_op, expr) => {
                    todo!("Assignment target result of UnaryOp")
                }

                Expr::Cast(expr, ctype) => todo!("Cast assignment target >:("),
            },

            BinaryOp::OpAndAssign(op) => {
                let address = self.address_of(left)?;

                let target_ctype = self.type_of_expr(left)?;
                let target_location = TypedLocation::new(target_ctype, address);

                self.evauluate_binary_op(target_location, *op, left, right)?;

                if destination.is_somewhere() {
                    self.copy_lvalue(target_location, destination)?;
                }

                self.release_reg(address.base);
            }

            BinaryOp::LogicEqual(mode) => {
                let is_false = self.b.create_label("LogicEqual__isFalse");
                let done = self.b.create_label("LogicEqual__done");

                let common_ctype = self
                    .generator
                    .program
                    .common_ctype(&self.type_of_expr(left)?, &self.type_of_expr(right)?)?;

                let common_ctype_size = self.generator.sizeof_ctype(common_ctype);
                assert!(common_ctype_size >= WORD_SIZE);

                let left_temp = self.allocate_anon(common_ctype);
                let right_temp = self.allocate_anon(common_ctype);

                self.evaluate_expression(left, left_temp)?;
                self.evaluate_expression(right, right_temp)?;

                if common_ctype_size == WORD_SIZE {
                    let [left_reg, right_reg] = self.regs();

                    self.b.load_dword(left_reg, left_temp);
                    self.b.load_dword(right_reg, right_temp);

                    self.free_local(right_temp);
                    self.free_local(left_temp);

                    self.b.cmp(left_reg, right_reg);
                    self.b.release_reg([left_reg, right_reg]);

                    match mode {
                        CompareMode::Equal => self.b.bne(is_false),
                        CompareMode::NotEqual => self.b.beq(is_false),
                    };

                    self.b.copy_dword(1, destination.location);
                    self.b.b(done);

                    self.b.label(is_false);
                    self.b.copy_dword(0, destination.location);

                    self.b.label(done);
                } else {
                    todo!("Compare equality of values which can't fit in a register")
                }
            }

            BinaryOp::Plus | BinaryOp::Minus => {
                if destination.is_nowhere() {
                    // discard the result.
                    return Ok(());
                };

                let debug_binop_header = self.generator.program.source_writer.format_binary_op(
                    &self.generator.program,
                    &op,
                    left,
                    right,
                );

                self.b
                    .header(format!("=== binop({debug_binop_header}) ==="));

                let left_temp = self.allocate_anon(destination.ctype);
                let right_temp = self.allocate_anon(destination.ctype);

                self.evaluate_expression(left, left_temp)?;
                self.evaluate_expression(right, right_temp)?;

                let [left_reg, right_reg] = self.regs();

                self.b.load_dword(left_reg, left_temp);
                self.b.load_dword(right_reg, right_temp);

                self.free_local(left_temp);
                self.free_local(right_temp);

                match op {
                    BinaryOp::Plus => self.b.add(left_reg, left_reg, right_reg),
                    BinaryOp::Minus => self.b.sub(left_reg, left_reg, right_reg),
                    _ => unreachable!(),
                };

                self.release_reg(right_reg);
                self.b.copy_dword(left_reg, destination.location);
                self.release_reg(left_reg);

                self.b
                    .footer(format!("=== END binop({debug_binop_header}) ==="));
            }

            BinaryOp::ArrayIndex => {
                if destination.is_nowhere() {
                    // discard the result.
                    return Ok(());
                };

                let (element_ctype, ele_address) = self.calculate_element_address(left, right)?;
                self.copy_lvalue(ele_address.with_ctype(element_ctype), destination)?;
                self.release_reg(ele_address.base);
            }
            BinaryOp::BitwiseLeftShift => todo!(),
            BinaryOp::BitwiseRightShift => todo!(),

            BinaryOp::LogicOrdering(mode) => {
                let left_ctype = self.type_of_expr(left)?;
                let right_ctype = self.type_of_expr(right)?;

                let left_temp = self.allocate_anon(left_ctype);
                let right_temp = self.allocate_anon(right_ctype);

                self.evaluate_expression(left, left_temp)?;
                self.evaluate_expression(right, right_temp)?;

                if destination.is_nowhere() {
                    // discard the result.
                    return Ok(());
                }

                let [left_reg, right_reg] = self.regs();

                self.b.load_dword(left_reg, left_temp);
                self.b.load_dword(right_reg, right_temp);

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

                self.b.copy_dword(0, destination.location);
                self.b.b(done);

                self.b.label(upon_true);
                self.b.copy_dword(1, destination.location);

                self.b.label(done);
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
                self.evaluate_expression(left, condition_storage)?;

                self.b.header("Test if LHS is truthy");

                let condition_reg = self.reg();

                self.b.load_dword(condition_reg, condition_storage);
                self.b.cmp(condition_reg, 0);

                self.release_reg(condition_reg);

                if is_and {
                    self.b.beq(done_label);
                    self.b.header("LHS is truthy, evaluate RHS");
                } else {
                    self.b.bne(done_label);
                    self.b.header("LHS is falsey, evaluate RHS");
                }

                self.evaluate_expression(right, condition_storage)?;

                self.b.label(done_label);

                if destination.is_somewhere() {
                    if destination.ctype != CPrimitive::Bool.into() {
                        todo!(
                            "Cast bool result to {}",
                            self.format_ctype(destination.ctype)
                        )
                    }

                    self.copy_lvalue(condition_storage, destination)?;
                }

                self.free_local(condition_storage);
            }
        };

        Ok(())
    }

    fn evaluate_unary_op(
        &mut self,
        op: UnaryOp,
        expr: &Expr,
        destination: TypedLocation,
    ) -> anyhow::Result<()> {
        match op {
            UnaryOp::IncrementThenGet => self.evauluate_binary_op(
                destination,
                BinaryOp::OpAndAssign(Box::new(BinaryOp::Plus)),
                expr,
                &Expr::IntLiteral(1),
            )?,

            UnaryOp::DecrementThenGet => self.evauluate_binary_op(
                destination,
                BinaryOp::OpAndAssign(Box::new(BinaryOp::Minus)),
                expr,
                &Expr::IntLiteral(1),
            )?,

            UnaryOp::GetThenIncrement => {
                // Unless we care about the result, `expr++` == `++expr`, which is quicker to
                // perform.
                if destination.is_somewhere() {
                    self.evaluate_expression(expr, destination)?;
                }

                self.evaluate_unary_op(UnaryOp::IncrementThenGet, expr, destination)?;
            }

            UnaryOp::GetThenDecrement => {
                // Unless we care about the result, `expr--` == `--expr`, which is quicker to
                // perform.
                if destination.is_somewhere() {
                    self.evaluate_expression(expr, destination)?;
                }

                self.evaluate_unary_op(UnaryOp::DecrementThenGet, expr, destination)?;
            }

            UnaryOp::SizeOf => {
                let size = self.generator.sizeof_ctype(self.type_of_expr(expr)?);
                self.evaluate_expression(&Expr::IntLiteral(size as i32), destination)?;
            }

            UnaryOp::BooleanNot => {
                self.evaluate_expression(expr, destination)?;

                if destination.is_somewhere() {
                    let upon_truthy_input = self.b.create_label("BooleanNot__uponTruthyInput");
                    let done = self.b.create_label("BooleanNot__done");

                    let reg = self.reg();

                    self.b.copy_dword(destination.location, reg);
                    self.b.cmp(reg, 0);
                    self.b.bne(upon_truthy_input);

                    self.b.move_dword(reg, 1);
                    self.b.b(done);

                    self.b.label(upon_truthy_input);
                    self.b.move_dword(reg, 0);

                    self.b.label(done);
                    self.b.copy_dword(reg, destination.location);

                    self.release_reg(reg);
                }
            }

            UnaryOp::Negative => {
                // let working_type = self
                //     .generator
                //     .common_real_primitive(destination.ctype, self.type_of_expr(expr)?);

                todo!()
            }

            UnaryOp::AddressOf => {
                let pointer_ctype = self.generator.program.pointer_to(self.type_of_expr(expr)?);
                let address = self.address_of(expr)?;

                // "Pack" the address into the register so that value is an lvalue that we can copy
                // to the destination.
                self.b.load_address(address.base, address, 0);

                self.copy_lvalue(
                    Location::Reg(address.base).with_ctype(pointer_ctype),
                    destination,
                )?;

                self.release_reg(address.base);
            }

            UnaryOp::Dereference => {
                let CType::PointerTo(inner_id, _) = self.type_of_expr(expr)? else {
                    bail!("Cannot dereference a non-pointer value");
                };

                let source_ctype = self.generator.program.get_ctype(inner_id);

                let source_address_temp = self.allocate_anon(CType::pointer_to(inner_id));
                self.evaluate_expression(expr, source_address_temp)?;

                let source_address_reg = self.reg();
                self.b.load_dword(source_address_reg, source_address_temp);
                self.free_local(source_address_temp);

                self.copy_lvalue(
                    TypedLocation::new(source_ctype, Address::at(source_address_reg)),
                    destination,
                )?;

                self.release_reg(source_address_reg);
            }
        };

        Ok(())
    }

    fn calculate_element_address(
        &mut self,
        target: &Expr,
        index: &Expr,
    ) -> Result<(CType, LiteralIndexAddress), anyhow::Error> {
        let CType::PointerTo(element_ctypeid, _) = self.type_of_expr(target)? else {
            bail!("{} has a non-indexable type", self.format_expr(target));
        };

        let element_ctype = self.generator.program.get_ctype(element_ctypeid);

        let temp_index_storage = self.allocate_anon(CPrimitive::Int);
        self.evaluate_expression(index, temp_index_storage)?;

        let temp_ptr_storage = self.allocate_anon(self.generator.program.pointer_to(element_ctype));
        self.evaluate_expression(target, temp_ptr_storage)?;

        let ele_pointer = self.reg();
        self.b.load_dword(ele_pointer, temp_ptr_storage);
        self.free_local(temp_ptr_storage);

        let ele_index = self.reg();
        self.b.load_dword(ele_index, temp_index_storage);
        self.free_local(temp_index_storage);

        let element_size = self.generator.sizeof_ctype(element_ctype);

        if element_size > 1 {
            if !element_size.is_power_of_two() {
                todo!("Support indexing non-power of 2 element sizes (requires multiplication)");
            }

            self.b
                .shift_left(ele_index, ele_index, element_size.ilog2());
        }

        self.b.add(ele_pointer, ele_pointer, ele_index);
        self.b.release_reg(ele_index);

        Ok((element_ctype, LiteralIndexAddress::at(ele_pointer)))
    }

    fn calculate_member_address(
        &mut self,
        target: &Expr,
        member: &str,
    ) -> anyhow::Result<(CType, LiteralIndexAddress)> {
        let (ctype, offset) = self.calculate_member_offset(target, member)?;

        let mut address = self.address_of(target)?;
        address.offset += offset;

        Ok((ctype, address))
    }

    fn calculate_member_offset(&self, target: &Expr, member: &str) -> anyhow::Result<(CType, i32)> {
        // TODO: Support unions, which are just structs but with offset 0 every time :D
        match self.type_of_expr(target)? {
            struct_ctype @ CType::AsIs(CConcreteType::Struct(id)) => {
                let cstruct = self.generator.program.get_struct(id);

                let Some(members) = &cstruct.members else {
                    bail!("Cannot access members of an opaque struct type")
                };

                let mut offset = 0;

                for Member {
                    name: member_name,
                    ctype,
                } in members
                {
                    if member_name.as_deref() == Some(member) {
                        return Ok((*ctype, offset as i32));
                    }

                    offset += self.generator.align(self.generator.sizeof_ctype(*ctype));
                }

                bail!(
                    "Struct type `{}` has no member named {member}",
                    self.format_ctype(struct_ctype)
                )
            }

            ctype => bail!("Cannot dot access type {}", self.format_ctype(ctype)),
        }
    }

    fn address_of(&mut self, target: &Expr) -> anyhow::Result<LiteralIndexAddress> {
        match target {
            Expr::Reference(name) => {
                if let Some(var) = self.get_localvar(name) {
                    let reg = self.reg();
                    self.b.load_address(reg, var, 0);

                    return Ok(LiteralIndexAddress {
                        base: reg,
                        offset: 0,
                    });
                }

                let Some(symbol) = self.generator.program.get_symbol_by_name(name) else {
                    bail!("Unknown symbol `{name}`")
                };

                match symbol {
                    Symbol::Func(_) => {
                        let reg = self.reg();
                        self.b.asm(format!("MOV {reg}, #fn_{name}"));

                        Ok(LiteralIndexAddress {
                            base: reg,
                            offset: 0,
                        })
                    }

                    Symbol::Var(_, _) => {
                        let reg = self.reg();
                        self.b.asm(format!("MOV {reg}, #var_{name}"));

                        Ok(LiteralIndexAddress {
                            base: reg,
                            offset: 0,
                        })
                    }
                }
            }

            Expr::BinaryOp(BinaryOp::ArrayIndex, array, index) => {
                Ok(self.calculate_element_address(array, index)?.1)
            }

            Expr::DotAccess { target, member } => {
                Ok(self.calculate_member_address(target, member)?.1)
            }

            Expr::UnaryOp(UnaryOp::Dereference, expr) => {
                let address = LiteralIndexAddress::at(self.reg());
                self.release_reg(address.base);

                self.evaluate_expression(
                    expr,
                    Location::Reg(address.base).with_ctype(self.type_of_expr(expr)?),
                )?;

                assert_eq!(self.reg(), address.base);
                Ok(address)
            }

            Expr::UnaryOp(unary_op, expr) => todo!(
                "Get address of applying {unary_op:?} to {}",
                self.format_expr(expr)
            ),

            expr => bail!(
                "Cannot take the address of non-lvalue `{}`",
                self.format_expr(expr)
            ),
        }
    }

    fn type_of_expr(&self, expr: &Expr) -> anyhow::Result<CType> {
        self.generator.program.type_of_expr(self, expr)
    }

    /// Format an AST statement as a human-readable representation of the C source.
    pub fn format_statement(&self, statement: &Statement) -> String {
        self.generator.program.format_statement(statement)
    }

    /// Format an AST expression as a human-readable representation of the C source.
    pub fn format_expr(&self, expr: &Expr) -> String {
        self.generator.program.format_expr(expr)
    }

    /// Format a type as it would appear in C source code.
    pub fn format_ctype(&self, ctype: impl Into<CType>) -> String {
        self.generator.program.format_ctype(ctype)
    }
}

impl<'a, 'b> ExecutionScope for FuncGenerator<'a, 'b> {
    fn variable_ctype(&self, name: &str) -> Option<CType> {
        Some(self.get_localvar(name)?.ctype)
    }
}

#[derive(Debug, Clone, Copy)]
struct StackLocal {
    offset: i32,
    ctype: CType,
}

impl StackLocal {
    pub const fn location(&self) -> Location {
        Location::Address(stack_offset(self.offset))
    }
}

impl From<StackLocal> for Address {
    fn from(value: StackLocal) -> Self {
        stack_offset(value.offset)
    }
}

const fn stack_offset(offset: i32) -> Address {
    Address::offset(Reg::R11, offset)
}

#[derive(Clone, Copy)]
struct TypedLocation {
    ctype: CType,
    location: Location,
}

impl TypedLocation {
    fn new(ctype: CType, location: impl Into<Location>) -> Self {
        Self {
            ctype,
            location: location.into(),
        }
    }

    const fn is_nowhere(&self) -> bool {
        matches!(self.ctype, CType::AsIs(CConcreteType::Void)) || self.location.is_nowhere()
    }

    const fn is_somewhere(&self) -> bool {
        !self.is_nowhere()
    }
}

impl Address {
    fn with_ctype(self, ctype: CType) -> TypedLocation {
        TypedLocation::new(ctype, self)
    }
}

impl Location {
    fn with_ctype(self, ctype: CType) -> TypedLocation {
        TypedLocation::new(ctype, self)
    }
}

impl LiteralIndexAddress {
    fn with_ctype(self, ctype: CType) -> TypedLocation {
        TypedLocation::new(ctype, self)
    }
}

impl From<StackLocal> for TypedLocation {
    fn from(value: StackLocal) -> Self {
        Self::new(value.ctype, stack_offset(value.offset))
    }
}

const NOWHERE: TypedLocation = TypedLocation {
    ctype: CType::AsIs(CConcreteType::Void),
    location: Location::NOWHERE,
};
