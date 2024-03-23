//! Marked YAML
//! ===========
//!
//! Currently this library only supports loading YAML from strings,
//! but this is sufficient for most users' purposes.  We would not
//! recommend an un-streamed processing engine for massive data anyway.
//!
//! To load some YAML you simply need to:
//!
//! ```
//! let node = marked_yaml::parse_yaml(0, r#"
//! toplevel: must be a mapping
//! but:
//!  - it
//!  - may
//!  - contain lists
//! and:
//!  mappings: are permitted
//!  as: sub-mappings
//! "#);
//! assert!(node.is_ok());
//! ```
//!
//! Parsing a valid YAML file may fail because `marked_yaml` adds some
//! additional constraints:
//!
//! * The top level of the YAML **MUST** be a mapping.
//! * Mapping keys **MUST** be scalars (strings).
//! * Aliases and anchors **MAY NOT** be used (though this limit may be lifted in the future).
//!
//! In addition, you can convert between `marked_yaml::Node` and `yaml_rust::Yaml`
//! though doing so will not give you any useful markers.

#![deny(missing_docs)]

pub mod loader;
pub mod types;

#[doc(inline)]
pub use loader::{parse_yaml, parse_yaml_with_options, LoadError, LoaderOptions};
#[doc(inline)]
pub use types::{Marker, Node, Span};

#[cfg(feature = "serde")]
#[doc(hidden)]
pub mod spanned_serde;

#[cfg(feature = "serde")]
#[doc(inline)]
pub use spanned_serde::{from_node, Spanned};
