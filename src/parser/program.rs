use std::{collections::HashMap, error::Error, fmt::Display};

use anyhow::anyhow;
use itertools::Itertools;
use thiserror::Error;

use crate::parser::{
    program::{
        expr::Expr,
        statement::{Block, Variable},
        types::{
            CConcreteType, CEnum, CEnumId, CFunc, CFuncType, CFuncTypeId, CStruct, CStructId,
            CType, CTypeId, Member, TypeDef,
        },
    },
    TodoType,
};

pub mod expr;
pub mod statement;
pub mod types;

#[derive(Debug, Clone)]
pub enum Symbol {
    Func(CFunc),
    Var(CType, Option<Expr>),
}

#[derive(Default, Clone)]
pub struct Program {
    global_symbols: HashMap<String, Symbol>,

    structs_by_name: HashMap<String, CStructId>,
    structs: HashMap<CStructId, CStruct>,
    next_struct_id: CStructId,

    enums_by_name: HashMap<String, CEnumId>,
    enums: HashMap<CEnumId, CEnum>,
    next_enum_id: CEnumId,

    func_types: HashMap<CFuncTypeId, CFuncType>,
    next_func_type_id: CFuncTypeId,

    ctypes_by_name: HashMap<String, CTypeId>,
    ctypes: HashMap<CTypeId, CType>,
    next_ctype_id: CTypeId,
}

impl Program {
    /// Declare that the given type name refers to a specific type, whether declared inline, or an
    /// known existing type.
    pub fn typedef(&mut self, typedef: TypeDef) -> Result<(), anyhow::Error> {
        let id = self.ctype_id(typedef.ctype);
        self.ctypes_by_name.insert(typedef.name, id);

        Ok(())
    }

    /// Resolve a user-defined type from its name, if it exists.
    pub fn resolve_typedef(&mut self, name: &str) -> Option<CType> {
        Some(
            *self
                .ctypes
                .get(self.ctypes_by_name.get(name)?)
                .expect("typedef names->ids shouldn't desync with ids->types!!"),
        )
    }

    pub fn declare_named_struct(
        &mut self,
        name: String,
        cstruct: CStruct,
    ) -> Result<CStructId, anyhow::Error> {
        let Some((existing_id, existing_struct)) = self
            .structs_by_name
            .get(&name)
            .and_then(|&id| Some((id, self.get_struct_by_id(id))))
        else {
            let id = self.next_struct_id.get_and_increment();

            self.structs.insert(id, cstruct);
            self.structs_by_name.insert(name, id);

            return Ok(id);
        };

        if *existing_struct != cstruct {
            if existing_struct.members.is_none() {
                // We're giving a concrete definition to an existing opaque struct.
                self.structs.insert(existing_id, cstruct.into());

                return Ok(existing_id);
            }

            return Err(anyhow!("Tried to redefine struct `{name}` as `{cstruct:?}`, when it already has concrete definition `{existing_struct:?}`"));
        }

        Ok(existing_id)
    }

    pub fn declare_function(&mut self, name: String, func: CFunc) -> Result<(), anyhow::Error> {
        // todo: ensure we don't redefine a func wrongly (follow struct example.)
        self.global_symbols.insert(name, Symbol::Func(func));
        Ok(())
    }

    pub fn declare_global_var(&mut self, variable: Variable) -> Result<(), anyhow::Error> {
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

    pub fn get_ctype_by_id(&self, id: CTypeId) -> &CType {
        self.ctypes
            .get(&id)
            .expect("Getting a CType by its ID should NEVER fail or we're out of sync")
    }

    pub fn get_struct_by_id(&self, id: CStructId) -> &CStruct {
        self.structs
            .get(&id)
            .expect("Getting a CStruct by its ID should NEVER fail or we're out of sync")
    }

    pub fn get_func_type_by_id(&self, id: CFuncTypeId) -> &CFuncType {
        self.func_types
            .get(&id)
            .expect("Getting a CFuncSig by its ID should NEVER fail or we're out of sync")
    }

    /// Define a new function type, or return the existing ID if it isn't unique.
    pub fn func_type(&mut self, cfunc_type: CFuncType) -> CFuncTypeId {
        for (id, sig) in &self.func_types {
            if cfunc_type == *sig {
                return *id;
            }
        }

        let id = self.next_func_type_id.get_and_increment();
        self.func_types.insert(id, cfunc_type);

        return id;
    }

    /// Define a new type upon encountering it. However, if this type has already been seen, the old
    /// type ID is returned instead.
    pub fn ctype_id(&mut self, ctype: impl Into<CType>) -> CTypeId {
        let actual_ctype = ctype.into();

        // Slow? probably horrendously! but hey, make it work first :P
        if let Some(existing_id) = self.ctypes.iter().find_map(|pair| {
            if *pair.1 == actual_ctype {
                Some(pair.0)
            } else {
                None
            }
        }) {
            return *existing_id;
        }

        let id = self.next_ctype_id;
        self.next_ctype_id = self.next_ctype_id.next();

        self.ctypes.insert(id, actual_ctype);
        id
    }

    pub fn get_defined_functions(&self) -> impl Iterator<Item = (&str, &CFunc)> {
        self.global_symbols
            .iter()
            .filter_map(|(name, symbol)| match symbol {
                Symbol::Func(cfunc) => Some((name.as_str(), cfunc)),
                _ => None,
            })
    }

    pub fn get_global_vars(&self) -> impl Iterator<Item = (&str, (&CType, &Option<Expr>))> {
        self.global_symbols
            .iter()
            .filter_map(|(name, symbol)| match symbol {
                Symbol::Var(ctype, expr) => Some((name.as_str(), (ctype, expr))),
                _ => None,
            })
    }

    pub fn get_symbol(&self, name: &str) -> Option<&Symbol> {
        self.global_symbols.get(name)
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "functions: {:?}",
            self.get_defined_functions().collect_vec()
        )?;
        writeln!(f, "global vars: {:?}", self.get_global_vars().collect_vec())?;
        writeln!(f, "enums: {:?}", self.enums_by_name)?;
        writeln!(f, "structs: {:?}", self.structs_by_name)?;
        writeln!(f, "type id mapping: {:?}", self.ctypes)?;
        writeln!(f, "typedefs: {:?}", self.ctypes_by_name)?;
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
