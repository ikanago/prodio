/// Struct to have location of code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Loc(pub usize, pub usize);

impl Loc {
    /// Function to merge two Loc.
    pub fn merge(&self, other: &Loc) -> Loc {
        use std::cmp::{max, min};
        Loc(min(self.0, other.0), max(self.1, other.1))
    }
}

/// Struct to hold value and location.
/// `value` will be Token or AST node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Annotation<T> {
    pub value: T,
    pub loc: Loc,
}

impl<T> Annotation<T> {
    /// Construct new `Annotation`
    pub fn new(value: T, loc: Loc) -> Self {
        Self { value, loc }
    }
}
