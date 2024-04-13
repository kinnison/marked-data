//! Marked YAML
//! ===========
//!
//! Currently this library only supports parsing YAML from strings,
//! but this is sufficient for most users' purposes.  We would not
//! recommend an un-streamed processing engine for massive data anyway.
//!
//! To parse some YAML you simply need to:
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
//! Parsing a valid YAML string may fail because `marked_yaml` adds some
//! additional constraints:
//!
//! * The top level of the YAML **MUST** be a mapping.
//! * Mapping keys **MUST** be scalars (strings).
//! * Aliases and anchors **MAY NOT** be used (though this limit may be lifted in the future).
//!
//! In addition, you can convert between `marked_yaml::Node` and `yaml_rust::Yaml`
//! though doing so will not give you any useful markers.
#![cfg_attr(
    feature = "serde",
    doc = r#"

Should you so choose, you may use serde to deserialise YAML
strings directly into structures, any amount of which could be annotated
with the [`Spanned`] type to capture information about where the value came
from in the input.

```
# use marked_yaml::{from_yaml, Marker, Spanned};
# use std::collections::HashMap;
let YAML = "Daniel: Author\nUser: Not Author\n";
let roles: HashMap<Spanned<String>, Spanned<String>> = from_yaml(0, YAML).unwrap();

assert_eq!(roles["Daniel"], "Author");
assert_eq!(roles["User"].span().start().copied(), Some(Marker::new(0, 2, 7)));
```

You do not have to have all values [`Spanned`], and you can deserialize from an already
parsed set of nodes with [`from_node`] instead.
"#
)]
#![deny(missing_docs)]
#![cfg_attr(docsrs, feature(doc_cfg))]

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
#[cfg_attr(docsrs, doc(cfg(feature = "serde")))]
#[doc(inline)]
pub use spanned_serde::{
    from_node, from_yaml, from_yaml_with_options, Error, FromNodeError, FromYamlError, Spanned,
};
