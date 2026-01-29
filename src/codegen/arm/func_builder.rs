use std::{
    cell::Cell,
    collections::{HashMap, HashSet, VecDeque},
    fmt::Display,
    ops::{Add, Not},
};

use bimap::BiMap;
use indexmap::IndexMap;
use itertools::Itertools;

use crate::{
    codegen::{
        arm::{
            address::Address,
            inst::{BranchTarget, CommentPosition, Inst},
            location::Location,
            reg::{OneOrMoreRegisters, Reg},
            reg_or_imm::RegOrImm,
            value::{Value, ValueWidth},
            AsmMode, Imm,
        },
        WORD_SIZE,
    },
    id_type::GetAndIncrement,
    parser::program::ctype::CSig,
};

id_type!(LabelId);

pub(crate) struct FuncBuilder<'a> {
    pub sig: &'a CSig,
    name: &'a str,
    instructions: Vec<Inst>,
    doc_comment: Vec<String>,

    labels: HashMap<LabelId, String>,

    /// A given label ID which is used to refer to the same instruction as a previous label ID is an
    /// alias of that first ID. This is a mapping that matches aliasing ID -> canonical ID, to
    /// coalesce them into a single label, since ARMLite doesn't support multiple labels for the
    /// same instruction.
    label_aliases: HashMap<LabelId, LabelId>,

    next_label_id: Cell<LabelId>,
    pub asm_mode: AsmMode,
    pub(crate) register_pool: IndexMap<Reg, RegStatus>,
}

pub(crate) struct RegStatus {
    pub available: bool,
    clobbered: bool,
}

impl Default for RegStatus {
    fn default() -> Self {
        Self {
            available: true,
            clobbered: false,
        }
    }
}

impl<'a> FuncBuilder<'a> {
    pub fn new(name: &'a str, sig: &'a CSig, asm_mode: AsmMode) -> Self {
        Self {
            doc_comment: Vec::new(),
            instructions: Vec::new(),
            sig,
            name,
            next_label_id: Cell::default(),
            labels: HashMap::default(),
            label_aliases: HashMap::default(),
            asm_mode,
            register_pool: IndexMap::from([
                (Reg::R0, RegStatus::default()),
                (Reg::R1, RegStatus::default()),
                (Reg::R2, RegStatus::default()),
                (Reg::R3, RegStatus::default()),
                // If we allocate any of these, we'll have to deal with clobbering. I really don't
                // want to deal with that because it'll probably require rewriting a bunch of stuff
                // to express referencing arguments rather than as`[R11+#value]`, so that the offset
                // can be calculated after we know how many registers got pushed at the start, or
                // introducing a proper IR, which I'm not yet confident enough that I'd make the
                // right abstractions in doing so.
                //
                // (Reg::R4, RegStatus::default()),
                // (Reg::R5, RegStatus::default()),
                // (Reg::R6, RegStatus::default()),
                // (Reg::R7, RegStatus::default()),
                // (Reg::R8, RegStatus::default()),
                // (Reg::R9, RegStatus::default()),
                // (Reg::R10, RegStatus::default()),
                // (Reg::R12, RegStatus::default()),
                // (Reg::R13, RegStatus::default()),
                // (Reg::R14, RegStatus::default()),
                // (Reg::R15, RegStatus::default()),
            ]),
        }
    }

    pub fn reg(&mut self) -> Reg {
        for (reg, status) in self.register_pool.iter_mut() {
            if status.available {
                status.available = false;
                status.clobbered = true;

                return *reg;
            }
        }

        panic!("No registers left!")
    }

    pub fn regs<const N: usize>(&mut self) -> [Reg; N] {
        let mut array = [Reg::ProgCounter; N];

        for reg in &mut array {
            *reg = self.reg();
        }

        array
    }

    pub fn release_reg<const N: usize>(&mut self, reg: impl Into<[Reg; N]>) {
        for reg in reg.into() {
            assert!(
                self.register_pool[&reg].available.not(),
                "Attempted double-free on {reg}"
            );

            self.register_pool[&reg].available = true;
        }
    }

    /// Copy an arbitrary number of contiguous bytes from the source location to the destination
    /// location.
    pub fn copy_bytes(
        &mut self,
        source: impl Into<Location>,
        destination: impl Into<Location>,
        byte_count: u32,
    ) {
        let source = source.into();
        let destination = destination.into();

        if let Ok(width) = ValueWidth::try_from(byte_count) {
            return self.copy_value(source, destination, width);
        }

        match (source, destination) {
            (Location::Address(source_address), Location::Address(destination_address)) => {
                let [source_addr_reg, destination_addr_reg] = self.regs();

                self.load_address(source_addr_reg, source_address, 0);
                self.load_address(destination_addr_reg, destination_address, 0);

                for _ in 0..(byte_count / 4) {
                    self.copy_dword(
                        Address::at(source_addr_reg),
                        Address::at(destination_addr_reg),
                    );

                    self.add(source_addr_reg, source_addr_reg, 4);
                    self.add(destination_addr_reg, destination_addr_reg, 4);
                }

                match byte_count % 4 {
                    0 => (),

                    1 => self.copy_byte(
                        Address::at(source_addr_reg),
                        Address::at(destination_addr_reg),
                    ),

                    2 => self.copy_word(
                        Address::at(source_addr_reg),
                        Address::at(destination_addr_reg),
                    ),

                    3 => {
                        self.copy_word(
                            Address::at(source_addr_reg),
                            Address::at(destination_addr_reg),
                        );

                        self.copy_byte(source_addr_reg + 2, destination_addr_reg + 2);
                    }

                    4 => self.copy_dword(
                        Address::at(source_addr_reg),
                        Address::at(destination_addr_reg),
                    ),

                    _ => unreachable!(),
                };

                self.release_reg([source_addr_reg, destination_addr_reg]);
            }

            (Location::Address(source_address), Location::Reg(destination_reg)) => {
                assert_eq!(byte_count, 3, "Copying from address to register, byte count must be <= DWORD, got {byte_count}");

                self.load_dword(destination_reg, source_address);
                self.bitwise_and(destination_reg, destination_reg, 0xffffff);
            }

            (Location::Reg(reg), Location::Address(address)) => {
                if byte_count == 3 {
                    self.store_word(reg, address);

                    let addr_reg = self.reg();

                    self.load_address(addr_reg, address, 2);
                    self.copy_byte(reg, addr_reg);

                    self.release_reg(addr_reg);
                } else {
                    todo!(
                        "Pad copy of {byte_count} bytes from {reg} to {}",
                        self.format_address(&address)
                    )
                }
            }

            (Location::Reg(source_reg), Location::Reg(destination_reg)) if byte_count == 3 => {
                self.bitwise_and(destination_reg, destination_reg, 0xff000000_u32);
                self.bitwise_and(source_reg, source_reg, 0x00ffffff);
                self.or(destination_reg, destination_reg, source_reg);
            }

            (Location::Reg(source_reg), Location::Reg(destination_reg)) => {
                panic!("Can't copy {byte_count} bytes from {source_reg} to {destination_reg}")
            }
        }
    }

    /// Copy a value with a given size to the given location.
    pub fn copy_value(
        &mut self,
        value: impl Into<Value>,
        destination: impl Into<Location>,
        width: ValueWidth,
    ) {
        let value = value.into();
        let destination = destination.into();

        match (value, destination) {
            (Value::Address(source_address), Location::Address(destination_address)) => match width
            {
                ValueWidth::Byte => {
                    let value_holder = self.reg();

                    self.load_byte(value_holder, source_address);
                    self.store_byte(value_holder, destination_address);

                    self.release_reg(value_holder);
                }

                ValueWidth::Word => {
                    let reg = self.reg();

                    // We can ignore the upper 16 bits, loading a dword is faster.
                    self.load_dword(reg, source_address);
                    self.store_word(reg, destination_address);

                    self.release_reg(reg);
                }

                ValueWidth::DWord => {
                    let value_holder = self.reg();

                    self.load_dword(value_holder, source_address);
                    self.store_dword(value_holder, destination_address);

                    self.release_reg(value_holder);
                }

                ValueWidth::QWord => {
                    let [value_holder, addr_holder] = self.regs();

                    self.load_dword(value_holder, source_address);
                    self.store_dword(value_holder, destination_address);

                    self.load_address(addr_holder, source_address, 4);
                    self.load_dword(value_holder, Address::at(addr_holder));

                    self.load_address(addr_holder, destination_address, 4);
                    self.store_dword(value_holder, Address::at(addr_holder));

                    self.release_reg([value_holder, addr_holder]);
                }
            },

            (Value::Address(address), Location::Reg(destination_reg)) => match width {
                ValueWidth::Byte => self.load_byte(destination_reg, address),
                ValueWidth::Word => self.load_word(destination_reg, address),
                ValueWidth::DWord => self.load_dword(destination_reg, address),
                ValueWidth::QWord => panic!("A register can't hold a QWord"),
            },

            (Value::Reg(source_reg), Location::Address(destination_address)) => match width {
                ValueWidth::Byte => self.store_dword(source_reg, destination_address),
                ValueWidth::Word => self.store_word(source_reg, destination_address),
                ValueWidth::DWord => self.store_dword(source_reg, destination_address),
                ValueWidth::QWord => {
                    self.store_dword(source_reg, destination_address);

                    let addr_holder = self.reg();

                    self.load_address(addr_holder, destination_address, 4);
                    self.store_dword(0, Address::at(addr_holder));

                    self.release_reg(addr_holder);
                }
            },

            (Value::Reg(source_reg), Location::Reg(destination_reg)) => match width {
                ValueWidth::Byte => self.move_byte(destination_reg, source_reg),
                ValueWidth::Word => self.move_word(destination_reg, source_reg),
                ValueWidth::DWord => self.move_dword(destination_reg, source_reg),
                ValueWidth::QWord => panic!("A register can't hold a QWord"),
            },

            (Value::Imm(imm), Location::Reg(destination_reg)) => match width {
                ValueWidth::Byte => self.move_byte(destination_reg, imm),
                ValueWidth::Word => self.move_word(destination_reg, imm),
                ValueWidth::DWord => self.move_dword(destination_reg, imm),
                ValueWidth::QWord => panic!("A register can't hold a QWord"),
            },

            (Value::Imm(imm), Location::Address(destination_address)) => match width {
                ValueWidth::Byte => self.store_byte(imm, destination_address),
                ValueWidth::Word => self.store_word(imm, destination_address),
                ValueWidth::DWord => self.store_dword(imm, destination_address),
                ValueWidth::QWord => {
                    self.store_dword(imm, destination_address);

                    let addr_holder = self.reg();

                    self.load_address(addr_holder, destination_address, 4);
                    self.store_dword(0, Address::at(addr_holder));

                    self.release_reg(addr_holder);
                }
            },
        }
    }

    /// Copy a qword-sized value to the given location.
    pub fn copy_qword(&mut self, value: impl Into<Value>, destination: impl Into<Location>) {
        self.copy_value(value, destination, ValueWidth::QWord);
    }

    /// Copy a dword-sized value to the given location.
    pub fn copy_dword(&mut self, value: impl Into<Value>, destination: impl Into<Location>) {
        self.copy_value(value, destination, ValueWidth::DWord);
    }

    /// Copy a word-sized value to the given location.
    pub fn copy_word(&mut self, value: impl Into<Value>, destination: impl Into<Location>) {
        self.copy_value(value, destination, ValueWidth::Word);
    }

    /// Copy a byte-sized value to the given location.
    pub fn copy_byte(&mut self, value: impl Into<Value>, destination: impl Into<Location>) {
        self.copy_value(value, destination, ValueWidth::Byte);
    }

    pub fn store_dword(&mut self, src: impl Into<RegOrImm>, addr: impl Into<Address>) {
        match src.into() {
            RegOrImm::Reg(reg) => self.append(Inst::Store(reg, addr.into())),

            src => {
                let reg = self.reg();

                self.move_dword(reg, src);
                self.append(Inst::Store(reg, addr.into()));

                self.release_reg(reg);
            }
        }
    }

    /// Store a word-sized value to the given address.
    pub fn store_word(&mut self, source: impl Into<RegOrImm>, destination: impl Into<Address>) {
        let source = source.into();
        let destination = destination.into();

        let [value_reg, addr_reg] = self.regs();

        self.move_dword(value_reg, source);
        self.store_byte(value_reg, destination);

        self.load_address(addr_reg, destination, 1);
        self.shift_right(value_reg, value_reg, 8);
        self.store_byte(value_reg, Address::at(addr_reg));

        self.release_reg([value_reg, addr_reg]);
    }

    /// Store a byte-sized value to the given address.
    pub fn store_byte(&mut self, src: impl Into<RegOrImm>, addr: impl Into<Address>) {
        match src.into() {
            RegOrImm::Reg(reg) => self.append(Inst::StoreB(reg, addr.into())),

            src => {
                let reg = self.reg();

                self.move_dword(reg, src);
                self.append(Inst::StoreB(reg, addr.into()));

                self.release_reg(reg);
            }
        }
    }

    pub fn load_dword(&mut self, dest: Reg, addr: impl Into<Address>) {
        self.append(Inst::Load(dest, addr.into()))
    }

    pub fn load_word(&mut self, dest: Reg, addr: impl Into<Address>) {
        self.load_dword(dest, addr);
        self.truncate_register(dest, ValueWidth::Word);
    }

    pub fn load_byte(&mut self, dest: Reg, addr: impl Into<Address>) {
        self.append(Inst::LoadB(dest, addr.into()))
    }

    /// Truncate the value stored in the given register to the given width.
    pub fn truncate_register(&mut self, reg: Reg, width: ValueWidth) {
        match width {
            ValueWidth::Byte => self.bitwise_and(reg, reg, 0xff),
            ValueWidth::Word => self.bitwise_and(reg, reg, 0xffff),
            ValueWidth::DWord | ValueWidth::QWord => (/* No-op */),
        }
    }

    pub fn append_doc_line(&mut self, line: impl Into<String>) {
        self.doc_comment.push(line.into());
    }

    pub fn comment(&mut self, comment: impl Into<String>) {
        self.append(Inst::Comment(comment.into(), CommentPosition::Line));
    }

    pub fn header(&mut self, comment: impl Into<String>) {
        self.append(Inst::Comment(comment.into(), CommentPosition::Header));
    }

    pub fn footer(&mut self, comment: impl Into<String>) {
        self.append(Inst::Comment(comment.into(), CommentPosition::Footer));
    }

    pub fn inline_comment(&mut self, comment: impl Into<String>) {
        self.append(Inst::Comment(comment.into(), CommentPosition::Inline));
    }

    pub fn create_label(&mut self, name: impl Into<String>) -> LabelId {
        let id = self.next_label_id.get_and_increment();
        self.labels.insert(id, name.into());

        id
    }

    pub fn label(&mut self, id: LabelId) {
        let Some(Inst::Label(canonical_id)) = self.instructions.last() else {
            self.append(Inst::Label(id));
            return;
        };

        self.label_aliases.insert(id, *canonical_id);

        let alias_name = self.labels.remove(&id).unwrap();
        *self.labels.get_mut(canonical_id).unwrap() += &format!("_aka_{alias_name}");
    }

    pub fn asm(&mut self, asm: impl Into<String>) {
        self.append(Inst::InlineAsm(asm.into()));
    }

    pub fn push(&mut self, regs: impl Into<OneOrMoreRegisters>) {
        self.append(Inst::Push(regs.into()))
    }

    pub fn pop(&mut self, regs: impl Into<OneOrMoreRegisters>) {
        self.append(Inst::Pop(regs.into()))
    }

    pub fn move_dword(&mut self, dest: Reg, src: impl Into<RegOrImm>) {
        self.append(Inst::Mov(dest, src.into()))
    }

    pub fn move_word(&mut self, dest: Reg, src: impl Into<RegOrImm>) {
        self.move_dword(dest, src);
        self.truncate_register(dest, ValueWidth::Word);
    }

    pub fn move_byte(&mut self, dest: Reg, src: impl Into<RegOrImm>) {
        self.move_dword(dest, src);
        self.truncate_register(dest, ValueWidth::Byte);
    }

    pub fn cmp(&mut self, left: Reg, right: impl Into<RegOrImm>) {
        self.append(Inst::Cmp(left, right.into()))
    }

    pub fn add(&mut self, dest: Reg, left: Reg, right: impl Into<RegOrImm>) {
        self.append(Inst::Add(dest, left, right.into()))
    }

    pub fn sub(&mut self, dest: Reg, left: Reg, right: impl Into<RegOrImm>) {
        self.append(Inst::Sub(dest, left, right.into()))
    }

    pub fn or(&mut self, dest: Reg, left: Reg, right: impl Into<RegOrImm>) {
        self.append(Inst::BitOr(dest, left, right.into()))
    }

    pub fn bitwise_and(&mut self, dest: Reg, left: Reg, right: impl Into<RegOrImm>) {
        self.append(Inst::BitAnd(dest, left, right.into()))
    }

    pub fn xor(&mut self, dest: Reg, left: Reg, right: impl Into<RegOrImm>) {
        self.append(Inst::BitXor(dest, left, right.into()))
    }

    pub fn shift_left(&mut self, dest: Reg, left: Reg, right: impl Into<RegOrImm>) {
        self.append(Inst::BitShl(dest, left, right.into()))
    }

    pub fn shift_right(&mut self, dest: Reg, left: Reg, right: impl Into<RegOrImm>) {
        self.append(Inst::BitShr(dest, left, right.into()))
    }

    pub fn call(&mut self, name: impl Into<String>) {
        self.append(Inst::Call(name.into()))
    }

    pub fn b(&mut self, name: impl Into<BranchTarget>) {
        self.append(Inst::B(name.into()))
    }

    pub fn beq(&mut self, name: impl Into<BranchTarget>) {
        self.append(Inst::BEq(name.into()))
    }

    pub fn bne(&mut self, name: impl Into<BranchTarget>) {
        self.append(Inst::BNe(name.into()))
    }

    pub fn blt(&mut self, name: impl Into<BranchTarget>) {
        self.append(Inst::BLt(name.into()))
    }

    pub fn bgt(&mut self, name: impl Into<BranchTarget>) {
        self.append(Inst::BGt(name.into()))
    }

    pub fn ret(&mut self) {
        self.append(Inst::Ret)
    }

    /// Load the address `addr` into the destination register `dest`. Works like x86's `LEA`, but
    /// also takes an extra offset (or 0).
    ///
    /// When the address is a `LiteralIndexAddress`, the offset can be added to the existing offset
    /// for free, rather than also performing an `add`.
    pub fn load_address(&mut self, dest: Reg, addr: impl Into<Address>, extra_offset: i32) {
        let addr = addr.into();

        match addr {
            Address::LiteralIndex { base, offset: 0 } => match extra_offset {
                0 => self.move_dword(dest, base),
                _ => self.load_address(dest, base + extra_offset, 0),
            },

            Address::RelativeIndex(addr) => {
                if addr.negate_offset {
                    self.sub(dest, addr.base, addr.offset)
                } else {
                    self.add(dest, addr.base, addr.offset)
                }

                if extra_offset != 0 {
                    self.add(dest, dest, extra_offset);
                }
            }

            Address::LiteralIndex { base, offset } => {
                if offset < 0 {
                    self.sub(dest, base, -offset - extra_offset)
                } else {
                    self.add(dest, base, offset + extra_offset)
                }
            }
        };
    }

    pub fn build(self) -> String {
        format!("{self}")
    }

    fn append(&mut self, inst: Inst) {
        self.instructions.push(inst);
    }

    fn format_label(&self, id: LabelId) -> String {
        let canonical_id = self.label_aliases.get(&id).unwrap_or(&id);

        format!(
            "L{}_{}__{}",
            self.labels[canonical_id], canonical_id.0, self.name
        )
    }

    fn format_branch_target(&self, target: &BranchTarget) -> String {
        match target {
            BranchTarget::Label(id) => self.format_label(*id),
            BranchTarget::Relative(offset) => {
                format!(".{}{}", if *offset >= 0 { '+' } else { '-' }, offset.abs())
            }
            BranchTarget::VerbatimLabel(name) => format!("#{name}"),
        }
    }

    fn format_fn(&self, name: &str) -> String {
        format!("fn_{name}")
    }

    fn format_operand(&self, operand: RegOrImm) -> String {
        match operand {
            RegOrImm::Reg(reg) => format!("{reg}"),
            RegOrImm::Imm(Imm::I32(value)) => format!("#{value}"),
            RegOrImm::Imm(Imm::U32(value)) => {
                format!("#{value}", value = u32::cast_signed(value))
            }
            RegOrImm::Imm(Imm::F32(value)) => {
                // Convert the number into the IEEE 754 32-bit float
                // representation, but display it as a signed integer literal, since ARMLite doesn't
                // support floats.
                //
                // For manipulating these floats, we need to do that in software, unfortunately.

                let bitcasted_float = f32::to_bits(value) as i32;
                format!("#{bitcasted_float}")
            }
            RegOrImm::Imm(Imm::StringId(string_id)) => format!("#str_{}", string_id.value()),
        }
    }

    pub fn format_address(&self, address: &Address) -> String {
        match address {
            Address::LiteralIndex { base, offset: 0 } => format!("[{base}]"),

            Address::RelativeIndex(addr) => match self.asm_mode {
                AsmMode::ArmLite => format!(
                    "[{base}{symbol}{offset}]",
                    base = addr.base,
                    symbol = if addr.negate_offset { '-' } else { '+' },
                    offset = addr.offset
                ),
                AsmMode::ArmV7 => {
                    format!("[{base}, {offset}]", base = addr.base, offset = addr.offset)
                }
            },

            &Address::LiteralIndex { base, offset } => match self.asm_mode {
                AsmMode::ArmLite => format!(
                    "[{base}{symbol}#{offset}]",
                    symbol = if offset < 0 { "-" } else { "+" },
                    offset = offset.abs()
                ),
                AsmMode::ArmV7 => format!("[{base}, #{offset}]"),
            },
        }
    }

    pub fn format_location(&self, location: &Location) -> String {
        match location {
            Location::Address(address) => self.format_address(address),
            Location::Reg(reg) => format!("{reg}"),
        }
    }
}

impl<'a> Display for FuncBuilder<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (comment_leader, comment_separator) = match self.asm_mode {
            AsmMode::ArmLite => (";", "\n\t; "),
            AsmMode::ArmV7 => ("@", "\n\t@ "),
        };

        writeln!(
            f,
            "{comment_leader} =============================================================================="
        )?;

        for line in &self.doc_comment {
            writeln!(f, "{comment_leader} {line}")?;
        }

        writeln!(
            f,
            "{comment_leader} =============================================================================="
        )?;

        writeln!(f, "{}:", self.format_fn(self.name))?;

        let mut inline_comments = VecDeque::<&str>::new();

        for inst in &self.instructions {
            match inst {
                Inst::InlineAsm(asm) => {
                    if asm.ends_with(':') && !asm.contains(comment_leader) {
                        write!(f, "{asm}")?;
                    } else {
                        write!(f, "\t{asm}")?
                    }
                }

                Inst::Comment(text, position) => match position {
                    CommentPosition::Header => writeln!(
                        f,
                        "\n\t{comment_leader} {}",
                        text.trim().lines().join(comment_separator)
                    )?,

                    CommentPosition::Footer => writeln!(
                        f,
                        "\t{comment_leader} {}\n",
                        text.trim().lines().join(comment_separator)
                    )?,

                    CommentPosition::Line => writeln!(
                        f,
                        "\t{comment_leader} {}",
                        text.trim().lines().join(comment_separator)
                    )?,

                    CommentPosition::Inline => {
                        inline_comments.push_back(text);
                    }
                },

                Inst::Label(id) => write!(f, "\n{}:", self.format_label(*id))?,

                Inst::Mov(dest, src) => {
                    write!(f, "\tMOV {dest}, {src}", src = self.format_operand(*src))?
                }

                Inst::Add(dest, left, right) => write!(
                    f,
                    "\tADD {dest}, {left}, {right}",
                    right = self.format_operand(*right)
                )?,

                Inst::Sub(dest, left, right) => write!(
                    f,
                    "\tSUB {dest}, {left}, {right}",
                    right = self.format_operand(*right)
                )?,

                Inst::Store(src, address) => write!(
                    f,
                    "\tSTR {src}, {address}",
                    address = self.format_address(address)
                )?,
                Inst::Load(dest, address) => write!(
                    f,
                    "\tLDR {dest}, {address}",
                    address = self.format_address(address)
                )?,
                Inst::StoreB(src, address) => write!(
                    f,
                    "\tSTRB {src}, {address}",
                    address = self.format_address(address)
                )?,
                Inst::LoadB(dest, address) => write!(
                    f,
                    "\tLDRB {dest}, {address}",
                    address = self.format_address(address)
                )?,
                Inst::Push(regs) => write!(f, "\tPUSH {{{}}}", regs.into_iter().join(", "))?,
                Inst::Pop(regs) => write!(f, "\tPOP {{{}}}", regs.into_iter().join(", "))?,

                Inst::BitOr(dest, left, right) => write!(
                    f,
                    "\tORR {dest}, {left}, {right}",
                    right = self.format_operand(*right)
                )?,

                Inst::BitAnd(dest, left, right) => write!(
                    f,
                    "\tAND {dest}, {left}, {right}",
                    right = self.format_operand(*right)
                )?,

                Inst::BitXor(dest, left, right) => write!(
                    f,
                    "\tEOR {dest}, {left}, {right}",
                    right = self.format_operand(*right)
                )?,

                Inst::Call(name) => write!(f, "\tBL {}", self.format_fn(name))?,
                Inst::B(target) => write!(f, "\tB {}", self.format_branch_target(target))?,
                Inst::BNe(target) => write!(f, "\tBNE {}", self.format_branch_target(target))?,
                Inst::BEq(target) => write!(f, "\tBEQ {}", self.format_branch_target(target))?,
                Inst::BLt(target) => write!(f, "\tBLT {}", self.format_branch_target(target))?,
                Inst::BGt(target) => write!(f, "\tBGT {}", self.format_branch_target(target))?,

                Inst::Ret => match self.asm_mode {
                    AsmMode::ArmLite => write!(f, "\tRET")?,
                    AsmMode::ArmV7 => write!(f, "\tMOV PC, LR")?,
                },

                Inst::Cmp(left, right) => write!(
                    f,
                    "\tCMP {left}, {right}",
                    right = self.format_operand(*right)
                )?,

                Inst::BitShl(dest, left, right) => write!(
                    f,
                    "\tLSL {dest}, {left}, {right}",
                    right = self.format_operand(*right)
                )?,

                Inst::BitShr(dest, left, right) => write!(
                    f,
                    "\tLSR {dest}, {left}, {right}",
                    right = self.format_operand(*right)
                )?,
            };

            if matches!(inst, Inst::Comment(_, _)).not() {
                if let Some(text) = inline_comments.pop_front() {
                    write!(f, "\t\t{comment_leader} {text}")?;
                }

                writeln!(f)?;
            }
        }

        Ok(())
    }
}
