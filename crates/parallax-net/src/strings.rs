use string_interner::{DefaultSymbol, StringInterner};

/// An interned string, represented by a unique ID
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IString(pub(crate) DefaultSymbol);

/// Global string interner for efficient string storage
#[derive(Debug, Default)]
pub struct StringPool {
    interner: StringInterner,
}

impl StringPool {
    /// Creates a new empty string pool
    pub fn new() -> Self {
        Self::default()
    }

    /// Interns a string and returns its unique ID
    pub fn intern(&mut self, s: &str) -> IString {
        IString(self.interner.get_or_intern(s))
    }

    /// Gets the string for a given ID
    pub fn resolve(&self, id: IString) -> Option<&str> {
        self.interner.resolve(id.0)
    }
}