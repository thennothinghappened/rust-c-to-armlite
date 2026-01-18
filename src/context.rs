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
    sources: RefCell<HashMap<SourceId, SourceData>>,
    sources_ref_phantom: PhantomData<&'a str>,
    next_source_id: Cell<SourceId>,
    next_sourcemap_start_index: Cell<usize>,

    idents: RefCell<Vec<Rc<String>>>,
    macros: RefCell<HashMap<String, Rc<String>>>,
}

struct SourceData {
    text: Box<str>,
    sourcemap_start_index: usize,
    pragma_once_enabled: bool,
}

impl<'a> Context<'a> {
    pub(crate) fn add_source_text(&self, text: String) -> SourceId {
        let id = self.next_source_id.get_and_increment();

        let sourcemap_start_index = self.next_sourcemap_start_index.get();
        self.next_sourcemap_start_index
            .set(sourcemap_start_index + text.len());

        self.sources.borrow_mut().insert(
            id,
            SourceData {
                text: text.into_boxed_str(),
                sourcemap_start_index,
                pragma_once_enabled: false,
            },
        );

        id
    }

    pub(crate) fn add_source_file_path(&self, path: String) -> Result<SourceId, io::Error> {
        if let Some(&existing_id) = self.sources_by_path.borrow().get(&path) {
            return Ok(existing_id);
        }

        let text = fs::read_to_string(&path)?;
        let id = self.add_source_text(text);

        self.sources_by_path.borrow_mut().insert(path, id);

        Ok(id)
    }

    pub(crate) fn get_source(&self, id: SourceId) -> &'a str {
        let sources = self.sources.borrow();
        let string_ptr = sources[&id].text.as_ref();

        // Safety: We NEVER EVER remove sources from the list, thus we guarantee that the buffer for
        // a given SourceId will have a lifetime from the insertion of that source, until the lexer
        // context is dropped.
        //
        // The sources HashMap may be reallocated when sources are appended, but we are acquiring
        // a reference to the underlying source string for this ID, rather than a reference to its
        // address in the HashMap, so the reference remains valid.
        unsafe { std::mem::transmute::<&str, &'a str>(string_ptr) }
    }

    pub(crate) fn get_source_name(&self, id: SourceId) -> String {
        self.sources_by_path
            .borrow()
            .iter()
            .find_map(|entry| {
                if *entry.1 == id {
                    Some(entry.0.to_owned())
                } else {
                    None
                }
            })
            .unwrap_or_else(|| format!("anon {}", id.0))
    }

    pub(crate) fn get_source_start_index(&self, id: SourceId) -> usize {
        self.sources.borrow()[&id].sourcemap_start_index
    }

    pub(crate) fn allow_reading(&self, id: SourceId) -> bool {
        !self.sources.borrow()[&id].pragma_once_enabled
    }

    pub(crate) fn source_enable_pragma_once(&self, id: SourceId) {
        let mut sources = self.sources.borrow_mut();
        sources.get_mut(&id).unwrap().pragma_once_enabled = true;
    }

    pub(crate) fn get_source_id_from_index(&self, index: usize) -> SourceId {
        for id in (0..self.next_source_id.get().0).map(SourceId) {
            let start_index = self.get_source_start_index(id);

            if start_index > index {
                return SourceId(id.0 - 1);
            }
        }

        SourceId(self.next_source_id.get().0 - 1)
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
