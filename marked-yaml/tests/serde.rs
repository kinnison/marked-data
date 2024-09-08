#![cfg(feature = "serde")]
//! All these tests require serde
//!

use std::collections::HashMap;

use marked_yaml::{
    from_node, from_yaml, from_yaml_with_options, parse_yaml, FromYamlError, LoaderOptions, Span,
    Spanned,
};
use serde::Deserialize;

const TEST_DOC: &str = r#"# Line one is a comment
top:
  - level
  - is always
  - two
  - strings
u8s: [ 0, 1, 2, "255" ]
i8s: [ -128, 0, 127 ]
u32s: [ 65537 ]
thingy: blue
outcome: 
    bad: stuff
looksee: { ugly: [ first, second ] }
known: { unknown: { name: Jeff, age: 14 } }
kvs:
    first: one
    second: two
    third: banana
falsy: false
truthy: true
yes: true
"#;

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct FullTest {
    top: Vec<Spanned<String>>,
    u8s: Spanned<Vec<u8>>,
    i8s: Vec<i8>,
    u32s: Vec<u32>,
    missing: Option<String>,
    thingy: Colour,
    outcome: EnumCheck,
    looksee: EnumCheck,
    known: EnumCheck,
    kvs: HashMap<Spanned<String>, Spanned<String>>,
    falsy: Spanned<bool>,
    truthy: Spanned<bool>,
    yes: Spanned<bool>,
}

#[derive(Deserialize, Debug)]
#[allow(dead_code)]
enum EnumCheck {
    #[serde(alias = "good")]
    Good(String),
    #[serde(alias = "bad")]
    Bad(Spanned<String>),
    #[serde(alias = "ugly")]
    Ugly(String, String),
    #[serde(alias = "unknown")]
    Unknown { name: Spanned<String>, age: i64 },
}

#[derive(Deserialize, Debug)]
enum Colour {
    #[serde(alias = "blue")]
    Blue,
    #[serde(alias = "red")]
    Red,
}

#[test]
fn read_everything() {
    let nodes = parse_yaml(0, TEST_DOC).unwrap();
    let doc: FullTest = from_node(&nodes).unwrap();
    println!("{doc:?}");
    assert_eq!(doc.top[0].as_str(), "level");
    assert!(doc.falsy == false);
    assert!(doc.truthy == true);
    assert_ne!(doc.truthy, doc.falsy);
    assert_eq!(doc.truthy, doc.yes);
    assert_eq!(doc.top[2], "two");
    assert_ne!(doc.top[3], "two");
    let s = String::from("s");
    assert!(doc.top[0] != s);
}

#[test]
fn ergonomics() {
    let doc: FullTest = from_yaml(0, TEST_DOC).unwrap();
    assert_eq!(doc.kvs.get("first").map(|s| s.as_str()), Some("one"));
    let k1 = Spanned::new(Span::new_blank(), "k1");
    let mut map = HashMap::new();
    map.insert(k1, "v1");
    let k2 = Spanned::new(Span::new_blank(), "k2");
    assert!(!map.contains_key(&k2));
    assert!(map.contains_key("k1"));
}

#[test]
fn parse_fails() {
    let err = from_yaml::<FullTest>(0, "hello world").err().unwrap();
    assert!(matches!(err, FromYamlError::ParseYaml(_)));
    let err = from_yaml::<FullTest>(0, "hello: world").err().unwrap();
    assert!(matches!(err, FromYamlError::FromNode(_)));
    let s = format!("{err}");
    assert!(s.starts_with("missing field"));
    #[derive(Deserialize)]
    #[allow(dead_code)]
    struct MiniDoc {
        colour: Colour,
    }
    let err = from_yaml::<MiniDoc>(0, "colour: {Red: optional}")
        .err()
        .unwrap();
    let s = format! {"{err}"};
    #[cfg(feature = "serde-path")]
    assert_eq!(s, "colour.Red: invalid type: map, expected String");
    #[cfg(not(feature = "serde-path"))]
    assert_eq!(s, "invalid type: map, expected String");
}

#[test]
fn parse_fails_coerce() {
    let options = LoaderOptions::default().prevent_coercion(true);
    let err = from_yaml_with_options::<FullTest>(0, TEST_DOC, options)
        .err()
        .unwrap();
    let s = format!("{err}");
    #[cfg(feature = "serde-path")]
    assert!(s.starts_with("u8s[3]"));
    #[cfg(feature = "serde-path")]
    let s = s.strip_prefix("u8s[3]: ").unwrap();
    assert!(s.starts_with("invalid type: string"));
    assert!(s.contains("expected u8"));
}

#[test]
fn empty_scalar_map() {
    #[allow(dead_code)]
    #[derive(Debug, Deserialize)]
    struct Foo {
        foo: HashMap<String, usize>,
    }
    let _: Foo = from_yaml(0, "foo:").unwrap();
}

#[test]
fn empty_scalar_seq() {
    #[allow(dead_code)]
    #[derive(Debug, Deserialize)]
    struct Foo {
        foo: Vec<String>,
    }
    let _: Foo = from_yaml(0, "foo:").unwrap();
}

#[test]
fn empty_scalar_unit() {
    #[allow(dead_code)]
    #[derive(Debug, Deserialize)]
    struct Foo {
        foo: (),
    }
    let _: Foo = from_yaml(0, "foo:").unwrap();
}

#[test]
fn empty_scalar_struct_with_default() {
    #[allow(dead_code)]
    #[derive(Default, Debug, Deserialize)]
    struct Bar {
        buz: Option<String>,
    }

    #[allow(dead_code)]
    #[derive(Debug, Deserialize)]
    struct Foo {
        #[serde(default)]
        bar: Bar,
    }
    let _: Foo = from_yaml(0, "bar:").unwrap();
}
