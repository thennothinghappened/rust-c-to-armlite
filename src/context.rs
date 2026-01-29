use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    fs, io,
    marker::PhantomData,
    path::{Path, PathBuf},
    rc::Rc,
    str::Chars,
};

use crate::{
    codegen::arm::AsmMode,
    id_type::GetAndIncrement,
    lexer::{
        tokenkind::{IdentId, TokenKind},
        Token,
    },
    span::Span,
};

id_type!(
    /// Unique identifier for a source file, either as the compilation root, or via an `#include`
    /// directive.
    SourceId
);

/// Context for a given compilation. This is shared information used by lexers, and made available
/// to parsers for resolving identifiers.
#[derive(Default)]
pub(crate) struct Context<'a> {
    sources_by_path: RefCell<HashMap<(String, IncludeType), SourceId>>,
    sources: RefCell<HashMap<SourceId, SourceData>>,
    sources_ref_phantom: PhantomData<&'a str>,
    next_source_id: Cell<SourceId>,
    next_sourcemap_start_index: Cell<usize>,

    idents: RefCell<Vec<Rc<String>>>,
    macros: RefCell<HashMap<String, Rc<Vec<Token>>>>,
    asm_mode: AsmMode,
}

struct SourceData {
    text: Box<str>,
    sourcemap_start_index: usize,
    pragma_once_enabled: bool,
}

#[derive(Eq, PartialEq, Hash, Copy, Clone)]
pub enum IncludeType {
    System,
    Local,
}

impl<'a> Context<'a> {
    pub fn new(asm_mode: AsmMode) -> Self {
        let this = Self {
            asm_mode,
            ..Default::default()
        };

        // Identify this compiler.
        this.define_macro(
            "__armlitec__",
            vec![Token {
                kind: TokenKind::IntLiteral(1),
                span: Span { start: 0, end: 0 },
            }],
        );

        match asm_mode {
            AsmMode::ArmLite => this.define_macro(
                "__armlite__",
                vec![Token {
                    kind: TokenKind::IntLiteral(1),
                    span: Span { start: 0, end: 0 },
                }],
            ),

            AsmMode::ArmV7 => this.define_macro(
                "__arm__",
                vec![Token {
                    kind: TokenKind::IntLiteral(1),
                    span: Span { start: 0, end: 0 },
                }],
            ),
        };

        this
    }

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

    pub(crate) fn add_source_file_path(
        &self,
        path: String,
        include_type: IncludeType,
    ) -> Result<SourceId, io::Error> {
        let key = (path, include_type);

        if let Some(&existing_id) = self.sources_by_path.borrow().get(&key) {
            return Ok(existing_id);
        }

        let text = match include_type {
            IncludeType::System => ["./stdlib/include", "./include"]
                .iter()
                .find_map(|system_path| {
                    fs::read_to_string(PathBuf::from(system_path).join(&key.0)).ok()
                })
                .ok_or_else(io::Error::last_os_error)?,

            IncludeType::Local => fs::read_to_string(&key.0)?,
        };

        let id = self.add_source_text(text);
        self.sources_by_path.borrow_mut().insert(key, id);

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
            .find_map(|((entry_name, _), &entry_id)| {
                if entry_id == id {
                    Some(entry_name.to_owned())
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

    pub(crate) fn define_macro(&self, name: impl Into<String>, value: Vec<Token>) {
        self.macros.borrow_mut().insert(name.into(), Rc::new(value));
    }

    pub(crate) fn get_macro(&self, name: &str) -> Option<Rc<Vec<Token>>> {
        self.macros.borrow().get(name).cloned()
    }

    pub(crate) fn remove_macro(&self, name: &str) {
        self.macros.borrow_mut().remove(name);
    }
}
