# Marked YAML

This library builds atop [`yaml-rust`][yaml-rust] to provide a YAML AST which
includes the marks for where the YAML data comes from. It explicitly operates
at a low level, providing only the _base_ **safe** YAML types (i.e. the vanilla
tags `tag:yaml.org,2002:seq`, `tag:yaml.org,2002:map`, and `tag:yaml.org,2002:str`)

[yaml-rust]: https://crates.io/crates/yaml-rust

The subset of YAML which is supported is quite deliberately limited in order
that users of this crate will implicitly discourage complex use of YAML which
is harder to manage user expectations with. As an example, the mapping type
in this crate explicitly only permits scalars as keys, and since all scalars
are treated as strings, mappings always have string keys.

The primary value of this kind of representation of YAML data is to allow
applications which want to be very explicit about where input came from an
opportunity to do this in a way which normal YAML parsers do not allow.

# Using Marked YAML

Currently this library only supports loading YAML from strings,
but this is sufficient for most users' purposes. We would not
recommend an un-streamed processing engine for massive data anyway.

To load some YAML you simply need to:

```rust
let node = marked_yaml::parse_yaml(0, r#"
toplevel: must be a mapping
but:
 - it
 - may
 - contain lists
and:
 mappings: are permitted
 as: sub-mappings
"#);
assert!(node.is_ok());
```

Parsing a valid YAML file may fail because `marked_yaml` adds some
additional constraints:

- The top level of the YAML **MUST** be a mapping.
- Mapping keys **MUST** be scalars (strings).
- Aliases and anchors **MAY NOT** be used (though this limit may be lifted in the future).

In addition, you can convert between `marked_yaml::Node` and `yaml_rust::Yaml`
though doing so will not give you any useful markers.
