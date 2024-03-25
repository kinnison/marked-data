#![cfg(feature = "serde")]
//! All these tests require serde
//!

use marked_yaml::{from_node, parse_yaml, Spanned};
use serde::Deserialize;

const TEST_DOC: &str = r#"# Line one is a comment
top:
  - level
  - is always
  - strings
u8s: [ 0, 1, 2, 255 ]
i8s: [ -128, 0, 127 ]
u32s: [ 65537 ]
thingy: blue
"#;

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct FullTest {
    top: Vec<Spanned<String>>,
    u8s: Spanned<Vec<u8>>,
    i8s: Vec<i8>,
    u32s: Vec<u32>,
    missing: Option<String>,
}

#[test]
fn read_everything() {
    let nodes = parse_yaml(0, TEST_DOC).unwrap();
    let doc: FullTest = from_node(&nodes).unwrap();
    println!("{doc:?}");
}
