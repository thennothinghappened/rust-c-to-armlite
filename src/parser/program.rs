use std::{collections::HashMap, fmt::Display};

use thiserror::Error;

use crate::parser::{
    program::{
        expr::Expr,
        statement::Variable,
        types::{Function, Member, Struct, Type, TypeDef, TypeInfo},
    },
    TodoType,
};

pub mod expr;
pub mod statement;
pub mod types;

#[derive(Default, Clone)]
pub struct Program {
    functions: HashMap<String, Function>,
    global_variables: HashMap<String, (Type, Option<Expr>)>,
    structs: HashMap<String, TypeId>,
    enums: HashMap<String, TypeId>,
    typedefs: HashMap<String, TypeId>,
    id_to_concrete_type: HashMap<TypeId, TypeInfo>,
    next_type_id: TypeId,
}

impl Program {
    /// Declare that the given type name refers to a specific type, whether declared inline, or an
    /// known existing type.
    pub fn typedef(&mut self, typedef: TypeDef) -> Result<(), DefineTypeError> {
        let id = match typedef.target_type {
            Type::Inline(type_info) => self.define_type(type_info),
            Type::WithId(id) => id,
        };

        self.typedefs.insert(typedef.name, id);
        Ok(())
    }

    /// Resolve a user-defined type from its name, if it exists.
    pub fn typedef_get_id(&mut self, name: &str) -> Option<&TypeId> {
        self.typedefs.get(name)
    }

    pub fn declare_named_struct(
        &mut self,
        name: String,
        struct_type: Struct,
    ) -> Result<TypeId, DefineTypeError> {
        let Some((&existing_id, existing_struct)) = self
            .structs
            .get(&name)
            .and_then(|id| Some((id, self.get_struct_by_id(id)?)))
        else {
            let id = self.define_type(struct_type.into());
            self.structs.insert(name, id);

            return Ok(id);
        };

        if *existing_struct != struct_type {
            if existing_struct.members.is_none() {
                // We're giving a concrete definition to an existing opaque struct.
                self.id_to_concrete_type
                    .insert(existing_id, struct_type.into());

                return Ok(existing_id);
            }

            return Err(DefineTypeError::Redefinition(
                name,
                Box::new(existing_struct.clone().into()),
                Box::new(struct_type.into()),
            ));
        }

        Ok(existing_id)
    }

    pub fn declare_function(&mut self, name: String, function: Function) -> Result<(), TodoType> {
        // todo: error checking!
        self.functions.insert(name, function);
        Ok(())
    }

    pub fn declare_global_var(&mut self, variable: Variable) -> Result<(), DeclareGlobalVarError> {
        if let Some((old_type, old_value)) = self.global_variables.get(&variable.name) {
            if *old_type != variable.var_type {
                return Err(DeclareGlobalVarError::RedefinitionDifferingType(
                    variable.name,
                    Box::new(self.resolve_concrete_type(old_type).unwrap()),
                    Box::new(self.resolve_concrete_type(&variable.var_type).unwrap()),
                ));
            }

            if variable.value.is_none() {
                return Ok(());
            }

            if let (Some(old_value), Some(new_value)) = (old_value, variable.value.clone()) {
                return Err(DeclareGlobalVarError::RedefinitionReassign(
                    variable.name,
                    Box::new(old_value.clone()),
                    Box::new(new_value),
                ));
            }
        };

        self.global_variables
            .insert(variable.name, (variable.var_type, variable.value));

        Ok(())
    }

    pub fn resolve_concrete_type(&self, ref_type: &Type) -> Option<TypeInfo> {
        match ref_type {
            Type::Inline(type_info) => Some(type_info.clone()),
            Type::WithId(id) => Some(self.get_type_by_id(&id)?.clone()),
        }
    }

    pub fn get_type_by_id(&self, id: &TypeId) -> Option<&TypeInfo> {
        self.id_to_concrete_type.get(id)
    }

    pub fn get_struct_by_id(&self, id: &TypeId) -> Option<&Struct> {
        if let Some(TypeInfo::Struct(struct_info)) = self.get_type_by_id(id) {
            return Some(struct_info);
        }

        None
    }

    pub fn define_type(&mut self, type_info: TypeInfo) -> TypeId {
        let type_id = self.next_type_id;
        self.next_type_id = self.next_type_id.next();

        self.id_to_concrete_type.insert(type_id, type_info);
        type_id
    }

    pub fn get_functions(&self) -> &HashMap<String, Function> {
        &self.functions
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "functions: {:?}", self.functions)?;
        writeln!(f, "global vars: {:?}", self.global_variables)?;
        writeln!(f, "enums: {:?}", self.enums)?;
        writeln!(f, "structs: {:?}", self.structs)?;
        writeln!(f, "type id mapping: {:?}", self.id_to_concrete_type)?;
        writeln!(f, "typedefs: {:?}", self.typedefs)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct TypeId {
    value: usize,
}

impl TypeId {
    pub(super) fn new(value: usize) -> Self {
        Self { value }
    }

    pub(super) fn next(&self) -> Self {
        Self::new(self.value + 1)
    }
}

#[derive(Debug, Error)]
pub enum DeclareGlobalVarError {
    #[error("Variable `{0}` was previously defined with the type `{1:?}`, but was redefined with type `{2:?}`")]
    RedefinitionDifferingType(String, Box<TypeInfo>, Box<TypeInfo>),

    #[error("Variable `{0}` was previously initialised as `{1:?}`, but was re-defined with a new initialised value of `{2:?}`")]
    RedefinitionReassign(String, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Error)]
pub enum DefineTypeError {
    #[error(
        "The type `{0}` was previously defined as `{1:?}`, but was redefined differently as `{2:?}`"
    )]
    Redefinition(String, Box<TypeInfo>, Box<TypeInfo>),

    #[error(transparent)]
    ForStruct(#[from] StructBuilderError),
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

    pub fn build(self) -> Struct {
        Struct {
            members: Some(self.members),
        }
    }

    pub fn member(
        &mut self,
        name: Option<String>,
        type_info: Type,
    ) -> Result<(), StructBuilderError> {
        if let Some(definite_name) = &name {
            if self.members.iter().any(|member| member.name == name) {
                return Err(StructBuilderError::DuplicateName(definite_name.clone()));
            }
        }

        self.members.push(Member { name, type_info });
        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum StructBuilderError {
    #[error("A member with the name {0} is already defined")]
    DuplicateName(String),
}
