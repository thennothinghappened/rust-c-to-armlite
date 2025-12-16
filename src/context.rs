use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    fs, io,
    marker::PhantomData,
    rc::Rc,
    str::Chars,
};

use crate::{id_type::GetAndIncrement, lexer::tokenkind::IdentId};

id_type!(
    /// Unique identifier for a source file, either as the compilation root, or via an `#include`
    /// directive.
    SourceId
);

/// Context for a given compilation. This is shared information used by lexers, and made available
/// to parsers for resolving identifiers.
#[derive(Default)]
pub(crate) struct Context<'a> {
    sources_by_path: RefCell<HashMap<String, SourceId>>,
    sources: RefCell<HashMap<SourceId, Box<str>>>,
    next_source_id: Cell<SourceId>,

    idents: RefCell<Vec<Rc<String>>>,
    macros: RefCell<HashMap<String, Rc<String>>>,
    phantom_data: PhantomData<Chars<'a>>,
}

impl<'a> Context<'a> {
    pub(crate) fn add_source_text(&self, text: String) -> SourceId {
        let id = self.next_source_id.get_and_increment();
        self.sources.borrow_mut().insert(id, text.into_boxed_str());

        id
    }

    pub(crate) fn add_source_file_path(&self, path: &str) -> Result<SourceId, io::Error> {
        if let Some(&existing_id) = self.sources_by_path.borrow().get(path) {
            return Ok(existing_id);
        }

        let text = fs::read_to_string(path)?;
        let id = self.add_source_text(text);

        self.sources_by_path
            .borrow_mut()
            .insert(path.to_owned(), id);

        Ok(id)
    }

    pub(crate) fn get_source(&self, id: SourceId) -> &'a str {
        let sources = self.sources.borrow();
        let string_ptr = sources[&id].as_ref();

        // Safety: We NEVER EVER remove sources from the list, thus we guarantee that the buffer for
        // a given SourceId will have a lifetime from the insertion of that source, until the lexer
        // context is dropped.
        //
        // The sources HashMap may be reallocated when sources are appended, but we are acquiring
        // a reference to the underlying source string for this ID, rather than a reference to its
        // address in the HashMap, so the reference remains valid.
        unsafe { std::mem::transmute::<&str, &'a str>(string_ptr) }
    }

    pub(crate) fn allocate_ident(&self, data: impl Into<String>) -> IdentId {
        let id = self.idents.borrow().len();
        self.idents.borrow_mut().push(data.into().into());

        id
    }

    pub(crate) fn get_ident(&self, id: IdentId) -> String {
        (*self.idents.borrow()[id]).clone()
    }

    pub(crate) fn preproc_define(&self, name: impl Into<String>, value: impl Into<String>) {
        self.macros
            .borrow_mut()
            .insert(name.into(), Rc::new(value.into()));
    }

    pub(crate) fn preproc_get(&self, name: &str) -> Option<Rc<String>> {
        self.macros.borrow().get(name).cloned()
    }
}
