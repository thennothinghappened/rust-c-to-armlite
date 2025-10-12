use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    fs,
    rc::Rc,
};

use crate::lexer::tokenkind::IdentId;

/// Context for a given compilation. This is shared information used by lexers, and made available
/// to parsers for resolving identifiers.
#[derive(Default)]
pub(crate) struct Context {
    sources: RefCell<HashMap<String, Rc<String>>>,
    idents: RefCell<Vec<Rc<String>>>,
}

impl Context {
    pub(crate) fn load_file(&self, path: &str) -> Rc<String> {
        self.sources
            .borrow_mut()
            .entry(path.to_owned())
            .or_insert_with(|| {
                fs::read_to_string(path)
                    .expect("failed to read file that was included")
                    .into()
            })
            .clone()
    }

    pub(crate) fn allocate_ident(&self, data: impl Into<String>) -> IdentId {
        let id = self.idents.borrow().len();
        self.idents.borrow_mut().push(data.into().into());

        id
    }

    pub(crate) fn get_ident(&self, id: IdentId) -> String {
        (*self.idents.borrow()[id]).clone()
    }
}
