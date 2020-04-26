#[macro_use]
pub mod macros;
pub mod code;
pub mod dump_info;
pub mod ir;
pub mod parse;
pub mod token;

use std::fmt;
use std::fs::File;
use std::io::Read;

const REGISTER_COUNT: usize = 7;
const ARG_REGISTER_COUNT: usize = 6;

pub fn read_file_content<P: AsRef<std::path::Path>>(
    source_file_path: P,
) -> Result<String, std::io::Error> {
    let mut source_file = File::open(source_file_path)?;
    let mut source_code = String::new();
    source_file.read_to_string(&mut source_code)?;
    Ok(source_code)
}

/// Struct to have location of code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Loc(pub usize, pub usize);

impl Loc {
    /// Function to merge two `Loc`.
    pub fn merge(&self, other: &Loc) -> Loc {
        use std::cmp::{max, min};
        Loc(min(self.0, other.0), max(self.1, other.1))
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.0, self.1)
    }
}

/// Struct to hold value and location.
/// `value` will be TokenKind, AstKind, or LexErrorKind.
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
