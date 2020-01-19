# Marked YAML

This library builds atop [`yaml-rust`][yaml-rust] to provide a YAML AST which
includes the marks for where the YAML data comes from. It explicitly operates
at a low level, providing only the _base_ **safe** YAML types (i.e. the vanilla
tags `tag:yaml.org,2002:seq`, `tag:yaml.org,2002:map`, and `tag:yaml.org,2002:str`)

[yaml-rust]: https://crates.io/crates/yaml-rust

The primary value of this kind of representation of YAML data is to allow
applications which want to be very explicit about where input came from an
opportunity to do this in a way which normal YAML parsers do not allow.
