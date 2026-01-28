use std::{
    fmt::Display,
    ops::{Add, Sub},
};

use itertools::Itertools;

use crate::{
    codegen::{
        arm::{address::Address, file_builder::StringId, imm::Imm, reg::Reg},
        func_builder::LabelId,
        Generator,
    },
    parser::program::ctype::{CFunc, CSig},
};

pub(super) mod address;
pub(super) mod file_builder;
pub(super) mod func_builder;
pub(super) mod imm;
pub(super) mod inst;
pub(super) mod location;
pub(super) mod reg;
pub(super) mod reg_or_imm;
pub(super) mod value;

#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub(crate) enum AsmMode {
    #[default]
    ArmLite,
    ArmV7,
}
