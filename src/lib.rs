//! Marked YAML
//! ===========
//!
//! TODO: Add nice frontmatter

#![deny(missing_docs)]

pub mod loader;
pub mod types;

pub use loader::{parse_yaml, LoadError};
pub use types::{Marker, Node, Span};
