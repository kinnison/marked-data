//! All the tests in here relate to the ability to get character offsets for things.

#[test]
fn fix_24_character_offset_works() {
    let input = r#"# some comment

key:       value   #   another  comment
"#
    .to_string();

    let document = marked_yaml::parse_yaml(0, &input).unwrap();

    // With the above shape, we know the document is a mapping, so let's retrieve
    // the "key" value from it

    let node = document.as_mapping().unwrap().get_scalar("key").unwrap();
    let span = node.span();
    eprintln!("{span:?}");
    eprintln!("{node:?}");
    let (start, end) = (
        span.start().unwrap().character(),
        // Because span.end() isn't reliable for scalar nodes right now,
        // if there's no end marker, try and synthesise it from the node's value.
        // This is unreliable because the node might have been folded, or had escaped
        // characters in it which we won't notice.
        span.end()
            .map(marked_yaml::Marker::character)
            .unwrap_or_else(|| span.start().unwrap().character() + node.as_str().chars().count()),
    );

    let output = input
        .chars()
        .take(start)
        .chain("new_value".chars())
        .chain(input.chars().skip(end))
        .collect::<String>();

    let expected_output = r#"# some comment

key:       new_value   #   another  comment
"#;

    assert_eq!(output, expected_output);
}
