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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_pool_creation() {
        let pool = StringPool::new();
        let mut pool2 = StringPool::new();
        let id = pool2.intern("test");
        assert!(pool.resolve(id).is_none());
    }

    #[test]
    fn test_string_interning() {
        let mut pool = StringPool::new();
        let test_str = "test_string";
        let id = pool.intern(test_str);
        assert_eq!(pool.resolve(id), Some(test_str));
    }

    #[test]
    fn test_string_deduplication() {
        let mut pool = StringPool::new();
        let test_str = "test_string";
        let id1 = pool.intern(test_str);
        let id2 = pool.intern(test_str);
        assert_eq!(id1, id2);
    }

    #[test]
    fn test_multiple_strings() {
        let mut pool = StringPool::new();
        let str1 = "string1";
        let str2 = "string2";
        let id1 = pool.intern(str1);
        let id2 = pool.intern(str2);
        assert_ne!(id1, id2);
        assert_eq!(pool.resolve(id1), Some(str1));
        assert_eq!(pool.resolve(id2), Some(str2));
    }

    #[test]
    fn test_istring_clone() {
        let mut pool = StringPool::new();
        let id = pool.intern("test");
        let cloned = id.clone();
        assert_eq!(id, cloned);
    }

    #[test]
    fn test_istring_copy() {
        let mut pool = StringPool::new();
        let id = pool.intern("test");
        let copied = id;  // This should copy, not move
        assert_eq!(id, copied);  // We can still use id after copying
    }

    #[test]
    fn test_istring_hash() {
        use std::collections::HashSet;
        let mut pool = StringPool::new();
        let id1 = pool.intern("test1");
        let id2 = pool.intern("test2");
        let id3 = pool.intern("test1");

        let mut set = HashSet::new();
        set.insert(id1);
        set.insert(id2);
        set.insert(id3);
        assert_eq!(set.len(), 2);  // id1 and id3 should be the same
    }

    #[test]
    fn test_string_pool_debug() {
        let pool = StringPool::new();
        assert!(format!("{:?}", pool).contains("StringPool"));
    }
} 