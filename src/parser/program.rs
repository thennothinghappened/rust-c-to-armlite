use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    error::Error,
    fmt::Display,
};

use anyhow::{anyhow, bail, Context};
use bimap::BiMap;
use indexmap::IndexMap;
use itertools::Itertools;
use thiserror::Error;

use crate::{
    id_type::GetAndIncrement,
    parser::program::{
        ctype::{
            CConcreteType, CEnum, CEnumId, CFunc, CFuncBody, CPrimitive, CSig, CSigId, CStruct,
            CStructId, CType, CTypeId, Member,
        },
        expr::{BinaryOp, CompareMode, Expr, OrderMode, UnaryOp},
        source_writer::SourceWriter,
        statement::{Block, Statement, Variable},
        target_architecture::TargetArchitecture,
    },
};

pub mod ctype;
pub mod expr;
pub mod source_writer;
pub mod statement;
pub(crate) mod target_architecture;

#[derive(Debug, Clone)]
pub enum Symbol {
    Func(CFunc),
    Var(CType, Option<Expr>),
}

#[derive(Default)]
pub struct Program {
    global_symbols: IndexMap<String, Symbol>,

    struct_names: BiMap<String, CStructId>,
    structs: HashMap<CStructId, CStruct>,
    next_struct_id: CStructId,

    enums_by_name: HashMap<String, CEnumId>,
    enums: HashMap<CEnumId, CEnum>,
    next_enum_id: CEnumId,

    func_types: HashMap<CSigId, CSig>,
    next_func_type_id: CSigId,

    type_aliases: HashMap<String, CTypeId>,
    ctypes: RefCell<HashMap<CTypeId, CType>>,
    next_ctype_id: Cell<CTypeId>,

    pub source_writer: SourceWriter,
    pub arch: TargetArchitecture,
}

impl Program {
    pub fn new(target: TargetArchitecture) -> Self {
        let mut this = Self {
            arch: target,
            ..Default::default()
        };

        for (&name, &primitive) in target.type_aliases() {
            this.create_type_alias(name.to_owned(), this.get_ctype_id(&primitive.into()));
        }

        this
    }

    /// Declare that the given type name refers to a specific type, whether declared inline, or an
    /// known existing type.
    pub fn create_type_alias(
        &mut self,
        name: String,
        ctype_id: CTypeId,
    ) -> Result<(), anyhow::Error> {
        self.type_aliases.insert(name, ctype_id);
        Ok(())
    }

    /// Resolve a user-defined type from its name, if it exists.
    pub fn get_type_alias_by_name(&mut self, name: &str) -> Option<CType> {
        Some(
            *self
                .ctypes
                .borrow()
                .get(self.type_aliases.get(name)?)
                .expect("typedef names->ids shouldn't de-sync with ids->types!!"),
        )
    }

    pub fn pointer_to(&self, ctype: impl Into<CType>) -> CType {
        CType::pointer_to(self.get_ctype_id(&ctype.into()))
    }

    pub fn create_struct(
        &mut self,
        name: String,
        cstruct: CStruct,
    ) -> Result<CStructId, anyhow::Error> {
        let Some((existing_id, existing_struct)) = self
            .struct_names
            .get_by_left(&name)
            .map(|&id| (id, self.get_struct(id)))
        else {
            let id = self.create_anonymous_struct(cstruct)?;
            self.struct_names.insert(name, id);

            return Ok(id);
        };

        if *existing_struct == cstruct {
            return Ok(existing_id);
        }

        if existing_struct.members.is_none() {
            // We're giving a concrete definition to an existing opaque struct.
            self.structs.insert(existing_id, cstruct);
            return Ok(existing_id);
        }

        if cstruct.members.is_none() {
            // Subsequent redeclaration of an existing struct.
            return Ok(existing_id);
        }

        Err(anyhow!("Tried to redefine struct `{name}` as `{cstruct:?}`, when it already has concrete definition `{existing_struct:?}`"))
    }

    pub fn create_anonymous_struct(&mut self, cstruct: CStruct) -> anyhow::Result<CStructId> {
        let id = self.next_struct_id.get_and_increment();
        self.structs.insert(id, cstruct);

        Ok(id)
    }

    pub fn create_function(&mut self, name: String, func: CFunc) -> Result<(), anyhow::Error> {
        let Some(Symbol::Func(existing_cfunc)) = self.global_symbols.get(&name) else {
            self.global_symbols.insert(name, Symbol::Func(func));
            return Ok(());
        };

        if existing_cfunc.sig_id != func.sig_id {
            bail!("Attempted to re-declare function `{name}` with a different signature");
        }

        match (&existing_cfunc.body, func.body) {
            (CFuncBody::Defined(_), CFuncBody::Defined(_)) => {
                bail!("Function `{name}` already has a body, cannot re-define it")
            }

            (CFuncBody::Defined(_), CFuncBody::Extern) => {
                bail!("Function `{name}` cannot be defined as extern when it has a body")
            }

            (CFuncBody::Extern, CFuncBody::Defined(_)) => {
                bail!("Function `{name}` cannot be defined when it was declared extern")
            }

            (CFuncBody::None, CFuncBody::Defined(block)) => self.set_function_body(&name, block),

            _ => Ok(()),
        }
    }

    pub fn set_function_body(&mut self, name: &str, block: Block) -> anyhow::Result<()> {
        let Some(Symbol::Func(cfunc)) = self.global_symbols.get_mut(name) else {
            bail!("Can't set the body of non-existing function `{name}`");
        };

        cfunc.body = CFuncBody::Defined(block);
        Ok(())
    }

    pub fn create_global_variable(&mut self, variable: Variable) -> Result<(), anyhow::Error> {
        if let Some(symbol) = self.global_symbols.get(&variable.name) {
            let Symbol::Var(old_type, old_value) = symbol else {
                panic!("{variable:?} name clash with function {symbol:?}");
            };

            if *old_type != variable.ctype {
                return Err(anyhow!("Tried to redefine global variable `{}` with new type `{:?}`, when it previously had type `{:?}`", variable.name, variable.ctype, old_type));
            }

            if variable.value.is_none() {
                return Ok(());
            }

            if let (Some(old_value), Some(new_value)) = (old_value, variable.value.clone()) {
                return Err(anyhow!("Tried to redefine global variable `{}` with a different initializing expression `{new_value:?}`, to the original expression `{old_value:?}`", variable.name));
            }
        };

        self.global_symbols
            .insert(variable.name, Symbol::Var(variable.ctype, variable.value));

        Ok(())
    }

    pub fn get_ctype(&self, id: CTypeId) -> CType {
        *self
            .ctypes
            .borrow()
            .get(&id)
            .expect("Getting a CType by its ID should NEVER fail or we're out of sync")
    }

    pub fn get_struct(&self, id: CStructId) -> &CStruct {
        self.structs
            .get(&id)
            .expect("Getting a CStruct by its ID should NEVER fail or we're out of sync")
    }

    pub fn get_signature(&self, id: CSigId) -> &CSig {
        self.func_types
            .get(&id)
            .expect("Getting a CFuncSig by its ID should NEVER fail or we're out of sync")
    }

    /// Define a new function type, or return the existing ID if it isn't unique.
    pub fn get_signature_id(&mut self, cfunc_type: CSig) -> CSigId {
        for (id, sig) in &self.func_types {
            if cfunc_type == *sig {
                return *id;
            }
        }

        let id = self.next_func_type_id.get_and_increment();
        self.func_types.insert(id, cfunc_type);

        id
    }

    /// Define a new type upon encountering it. However, if this type has already been seen, the old
    /// type ID is returned instead.
    pub fn get_ctype_id(&self, ctype: &CType) -> CTypeId {
        // Slow? probably horrendously! but hey, make it work first :P
        if let Some(existing_id) =
            self.ctypes.borrow().iter().find_map(
                |pair| {
                    if pair.1 == ctype {
                        Some(pair.0)
                    } else {
                        None
                    }
                },
            )
        {
            return *existing_id;
        }

        let id = self.next_ctype_id.get_and_increment();
        self.ctypes.borrow_mut().insert(id, *ctype);

        id
    }

    /// Return whether the given C type is a generic pointer type (`char*` or `void*`), which can be
    /// cast to/from any other pointer type.
    pub fn is_generic_pointer(&self, ctype: impl Into<CType>) -> bool {
        let CType::PointerTo(inner_id, _) = ctype.into() else {
            return false;
        };

        matches!(
            self.get_ctype(inner_id),
            CType::AsIs(CConcreteType::Primitive(CPrimitive::Char) | CConcreteType::Void)
        )
    }

    pub fn get_functions(&self) -> impl Iterator<Item = (&str, &CFunc)> {
        self.global_symbols
            .iter()
            .filter_map(|(name, symbol)| match symbol {
                Symbol::Func(cfunc) => Some((name.as_str(), cfunc)),
                _ => None,
            })
    }

    pub fn get_global_variables(&self) -> impl Iterator<Item = (&str, (&CType, &Option<Expr>))> {
        self.global_symbols
            .iter()
            .filter_map(|(name, symbol)| match symbol {
                Symbol::Var(ctype, expr) => Some((name.as_str(), (ctype, expr))),
                _ => None,
            })
    }

    pub fn get_symbol_by_name(&self, name: &str) -> Option<&Symbol> {
        self.global_symbols.get(name)
    }

    /// Format an AST statement as a human-readable representation of the C source.
    pub fn format_statement(&self, statement: &Statement) -> String {
        self.source_writer.format_statement(self, statement)
    }

    /// Format an AST expression as a human-readable representation of the C source.
    pub fn format_expr(&self, expr: &Expr) -> String {
        self.source_writer.format_expr(self, expr)
    }

    /// Format a type as it would appear in C source code.
    pub fn format_ctype(&self, ctype: impl Into<CType>) -> String {
        self.source_writer.format_ctype(self, ctype)
    }

    /// Determine the type of the given expression, assuming it is valid. References to named
    /// symbols are resolved by first querying the provided `scope`'s variables, then falling back
    /// to our global symbol list.
    pub fn type_of_expr(&self, scope: &impl ExecutionScope, expr: &Expr) -> anyhow::Result<CType> {
        match expr {
            Expr::StringLiteral(_) => Ok(self.pointer_to(CPrimitive::Char)),
            Expr::IntLiteral(_) => Ok(CPrimitive::Int.into()),
            Expr::BoolLiteral(_) => Ok(CPrimitive::Bool.into()),
            Expr::NullPtr => Ok(self.pointer_to(CConcreteType::Void)),

            Expr::Reference(name) => {
                if let Some(ctype) = scope.variable_ctype(name) {
                    return Ok(ctype);
                }

                if let Some(symbol) = self.get_symbol_by_name(name) {
                    return match symbol {
                        Symbol::Func(cfunc) => Ok(CType::AsIs(cfunc.sig_id.into())),
                        Symbol::Var(ctype, _) => Ok(*ctype),
                    };
                }

                bail!("Can't get the ctype of the undefined variable `{name}`")
            }

            Expr::Call(call) => match self.type_of_expr(scope, &call.target)? {
                CType::AsIs(CConcreteType::Func(sig_id)) => Ok(sig_id.into()),
                CType::AsIs(inner) => {
                    bail!("{} is not a valid call target", self.format_ctype(inner))
                }
                CType::PointerTo(_, _) => {
                    bail!("Non-function pointer is not a valid call target")
                }
            },

            Expr::BinaryOp(op, left, right) => match op {
                BinaryOp::AndThen => self.type_of_expr(scope, right),

                BinaryOp::Assign | BinaryOp::OpAndAssign(_) => self.type_of_expr(scope, left),

                BinaryOp::LogicEqual(_)
                | BinaryOp::LogicOrdering(_)
                | BinaryOp::LogicAnd
                | BinaryOp::LogicOr => Ok(CPrimitive::Bool.into()),

                BinaryOp::Plus | BinaryOp::Minus => {
                    let left_ctype = self.type_of_expr(scope, left);
                    let right_ctype = self.type_of_expr(scope, right);

                    todo!("numeric type implicit conversion rules")
                }

                BinaryOp::ArrayIndex => match self.type_of_expr(scope, left)? {
                    CType::AsIs(_) => bail!("Can't index a concrete type as an array!"),
                    CType::PointerTo(inner, _) => Ok(self.get_ctype(inner)),
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
                | UnaryOp::GetThenDecrement => self.type_of_expr(scope, expr),

                UnaryOp::SizeOf => Ok(CPrimitive::Int.into()),
                UnaryOp::BooleanNot => Ok(CPrimitive::Bool.into()),
                UnaryOp::Negative => self.type_of_expr(scope, expr),

                UnaryOp::AddressOf => Ok(self.pointer_to(self.type_of_expr(scope, expr)?)),

                UnaryOp::Dereference => match self.type_of_expr(scope, expr)? {
                    CType::AsIs(_) => bail!("Can't dereference a concrete type!"),
                    CType::PointerTo(inner, _) => Ok(self.get_ctype(inner)),
                },
            },

            // fixme: blindly accepting whatever the programmer is saying here right now! we really
            // should define a stricter ast that simply doesn't allow invalid constructs, like
            // casting a struct to a number, for instance.
            Expr::Cast(expr, ctype) => Ok(*ctype),

            Expr::DotAccess { target, member } => match self.type_of_expr(scope, target)? {
                struct_ctype @ CType::AsIs(CConcreteType::Struct(id)) => {
                    let cstruct = self.get_struct(id);

                    let Some(members) = &cstruct.members else {
                        bail!("Cannot access members of an opaque struct type")
                    };

                    for Member {
                        name: member_name,
                        ctype,
                    } in members
                    {
                        if member_name.as_deref() == Some(member) {
                            return Ok(*ctype);
                        }
                    }

                    bail!(
                        "Struct type `{}` has no member named {member}",
                        self.format_ctype(struct_ctype)
                    )
                }

                ctype => bail!("Cannot dot access type {}", self.format_ctype(ctype)),
            },
        }
    }

    /// Obtain the most specific common type between `a` and `b`, if one exists.
    pub fn common_ctype(&self, a: &CType, b: &CType) -> anyhow::Result<CType> {
        if a == b {
            return Ok(*a);
        }

        match (a, b) {
            (CType::PointerTo(a_inner, None), CType::PointerTo(b_inner, None)) => {
                match (self.is_generic_pointer(*a), self.is_generic_pointer(*b)) {
                    // Either will do.
                    (true, true) => Ok(*a),

                    // Take the non-generic (a).
                    (false, true) => Ok(*a),

                    // Take the non-generic (b).
                    (true, false) => Ok(*b),

                    // No overlap between these types.
                    (false, false) => bail!(
                        "No overlap between pointer types `{}` and `{}`",
                        self.format_ctype(*a),
                        self.format_ctype(*b)
                    ),
                }
            }

            (
                CType::AsIs(CConcreteType::Primitive(a_primitive)),
                CType::AsIs(CConcreteType::Primitive(b_primitive)),
            ) => Ok(self
                .common_real_primitive(*a_primitive, *b_primitive)
                .into()),

            _ => bail!(
                "No overlap between types {} and {}",
                self.format_ctype(*a),
                self.format_ctype(*b)
            ),
        }
    }

    /// Obtain a common primitive type from the given primitives `a` and `b` which can most
    /// adequately represent the range of values the input types can hold.
    pub fn common_real_primitive(&self, a: CPrimitive, b: CPrimitive) -> CPrimitive {
        let (promoted_a, promoted_b) = match (a.is_floating_type(), b.is_floating_type()) {
            // Pass through to picking the larger float type.
            (true, true) => (a, b),

            // Convert the integer to a float.
            (true, false) => (a, a),
            (false, true) => (b, b),

            // Both are integers, promote!
            (false, false) => (self.promote_integer(a), self.promote_integer(b)),
        };

        if self.arch.rank_of_primitive(promoted_a) > self.arch.rank_of_primitive(promoted_b) {
            promoted_a
        } else {
            promoted_b
        }
    }

    /// Promote an integer type up to `int` or `unsigned int`, if its rank is lower, or preserve the
    /// given type should its rank exceed either of those.
    pub fn promote_integer(&self, integer: CPrimitive) -> CPrimitive {
        assert!(
            integer.is_integer_type(),
            "Shouldn't pass a non-integer primitive type to promotion"
        );

        if self.arch.rank_of_primitive(integer) < self.arch.rank_of_primitive(CPrimitive::Int) {
            match self.is_signed_primitive(integer) {
                true => CPrimitive::Int,
                false => CPrimitive::UnsignedInt,
            }
        } else {
            integer
        }
    }

    /// Return whether the given primitive is a signed type on this platform.
    pub fn is_signed_primitive(&self, primitive: CPrimitive) -> bool {
        match primitive {
            CPrimitive::Char
            | CPrimitive::SignedChar
            | CPrimitive::Short
            | CPrimitive::Int
            | CPrimitive::Long
            | CPrimitive::LongLong
            | CPrimitive::Float
            | CPrimitive::Double
            | CPrimitive::LongDouble => true,

            CPrimitive::Bool
            | CPrimitive::UnsignedChar
            | CPrimitive::UnsignedShort
            | CPrimitive::UnsignedInt
            | CPrimitive::UnsignedLong
            | CPrimitive::UnsignedLongLong => false,
        }
    }
}

pub(crate) trait ExecutionScope {
    /// Get the type that the variable with name `name` has, or `None` if that variable does not
    /// exist.
    fn variable_ctype(&self, name: &str) -> Option<CType>;
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "functions: {:?}", self.get_functions().collect_vec())?;
        writeln!(
            f,
            "global vars: {:?}",
            self.get_global_variables().collect_vec()
        )?;
        writeln!(f, "enums: {:?}", self.enums_by_name)?;
        writeln!(f, "structs: {:?}", self.struct_names)?;
        writeln!(f, "type id mapping: {:?}", self.ctypes)?;
        writeln!(f, "typedefs: {:?}", self.type_aliases)?;
        Ok(())
    }
}

pub struct StructBuilder {
    members: Vec<Member>,
}

impl StructBuilder {
    pub fn new() -> Self {
        Self {
            members: Vec::default(),
        }
    }

    pub fn build(self) -> CStruct {
        CStruct {
            members: Some(self.members),
        }
    }

    pub fn member(&mut self, name: Option<String>, ctype: CType) -> Result<(), StructBuilderError> {
        if let Some(definite_name) = &name {
            if self.members.iter().any(|member| member.name == name) {
                return Err(StructBuilderError::DuplicateName(definite_name.clone()));
            }
        }

        self.members.push(Member { name, ctype });
        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum StructBuilderError {
    #[error("A member with the name {0} is already defined")]
    DuplicateName(String),
}
