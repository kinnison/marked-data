#![cfg(feature = "serde")]
//! All these tests require serde
//!

use std::collections::HashMap;

use marked_yaml::{from_node, from_yaml, parse_yaml, FromYamlError, Span, Spanned};
use serde::Deserialize;

const TEST_DOC: &str = r#"# Line one is a comment
top:
  - level
  - is always
  - two
  - strings
u8s: [ 0, 1, 2, 255 ]
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
}
