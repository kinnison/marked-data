//! Loading YAML
//!

use crate::types::*;

use hashlink::linked_hash_map::Entry;
use yaml_rust::parser::{Event, MarkedEventReceiver, Parser};
use yaml_rust::scanner::ScanError;
use yaml_rust::scanner::{Marker as YamlMarker, TScalarStyle};

use std::error::Error;
use std::fmt::{self, Display};

/// An error indicating that a duplicate key was detected in a mapping
#[derive(Debug, PartialEq, Eq)]

pub struct DuplicateKeyInner {
    /// The first key
    pub prev_key: MarkedScalarNode,
    /// The second key
    pub key: MarkedScalarNode,
}

/// Errors which can occur during loading of YAML
#[derive(Debug, PartialEq, Eq)]
#[non_exhaustive]
pub enum LoadError {
    /// Something other than a mapping detected at the top level
    TopLevelMustBeMapping(Marker),
    /// Something other than a sequence detected at the top level
    TopLevelMustBeSequence(Marker),
    /// Unexpected definition of anchor
    UnexpectedAnchor(Marker),
    /// Mapping keys must be scalars
    MappingKeyMustBeScalar(Marker),
    /// An explicit tag was detected
    UnexpectedTag(Marker),
    /// A YAML scanner error occured
    ScanError(Marker, ScanError),
    /// A duplicate key was detected in a mapping
    DuplicateKey(Box<DuplicateKeyInner>),
}

/// Options for loading YAML
///
/// Default options ([`LoaderOptions::default()`]) are:
///
/// - Permit duplicate keys
///
#[derive(Debug)]
pub struct LoaderOptions {
    error_on_duplicate_keys: bool,
    prevent_coercion: bool,
    toplevel_is_mapping: bool,
    lowercase_keys: bool,
}

impl Default for LoaderOptions {
    fn default() -> Self {
        Self {
            error_on_duplicate_keys: false,
            prevent_coercion: false,
            toplevel_is_mapping: true,
            lowercase_keys: false,
        }
    }
}

impl LoaderOptions {
    /// Enable errors on duplicate keys
    ///
    /// If enabled, duplicate keys in mappings will cause an error.
    /// If disabled, the last key/value pair will be used.
    pub fn error_on_duplicate_keys(self, enable: bool) -> Self {
        Self {
            error_on_duplicate_keys: enable,
            ..self
        }
    }

    /// Prevent coercion of scalar nodes
    ///
    /// If you want to disable things like [`.as_bool()`](crate::types::MarkedScalarNode::as_bool())
    /// then you can call this and set coercion to be prevented.
    pub fn prevent_coercion(self, prevent: bool) -> Self {
        Self {
            prevent_coercion: prevent,
            ..self
        }
    }

    /// Require that the top level is a mapping node
    ///
    /// This is the default, but you can call this to be explicit.
    pub fn toplevel_mapping(self) -> Self {
        Self {
            toplevel_is_mapping: true,
            ..self
        }
    }

    /// Require that the top level is a sequence node
    ///
    /// Without calling this, the top level of the YAML is must be a mapping node
    pub fn toplevel_sequence(self) -> Self {
        Self {
            toplevel_is_mapping: false,
            ..self
        }
    }

    /// Whether or not to force-lowercase mapping keys when loading
    ///
    /// By default, the loader will leave key names alone, but in some
    /// cases it can be preferable to normalise them to lowercase
    pub fn lowercase_keys(self, force_lowercase: bool) -> Self {
        Self {
            lowercase_keys: force_lowercase,
            ..self
        }
    }
}

impl Display for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use LoadError::*;
        #[allow(deprecated)]
        match self {
            TopLevelMustBeMapping(m) => write!(f, "{}: Top level must be a mapping", m),
            TopLevelMustBeSequence(m) => write!(f, "{}: Top level must be a sequence", m),
            UnexpectedAnchor(m) => write!(f, "{}: Unexpected definition of anchor", m),
            MappingKeyMustBeScalar(m) => write!(f, "{}: Keys in mappings must be scalar", m),
            UnexpectedTag(m) => write!(f, "{}: Unexpected use of YAML tag", m),
            DuplicateKey(inner) => {
                let DuplicateKeyInner { prev_key, key } = inner.as_ref();
                write!(
                    f,
                    "Duplicate key \"{}\" in mapping at {} and {}",
                    prev_key.as_str(),
                    prev_key
                        .span()
                        .start()
                        .map(ToString::to_string)
                        .unwrap_or_else(|| "?".to_string()),
                    key.span()
                        .start()
                        .map(ToString::to_string)
                        .unwrap_or_else(|| "?".to_string()),
                )
            }
            ScanError(m, e) => {
                // e.description() is deprecated but it's the only way to get
                // the exact info we want out of yaml-rust
                write!(f, "{}: {}", m, e.description())
            }
        }
    }
}

impl Error for LoadError {}

#[derive(Debug, PartialEq, Eq)]
enum LoaderState {
    Initial,
    StartStream,
    StartDocument,
    MappingWaitingOnKey(Marker, MappingHash),
    MappingWaitingOnValue(Marker, MappingHash, MarkedScalarNode),
    SequenceWaitingOnValue(Marker, Vec<Node>),
    Finished(Node),
    Error(LoadError),
}
use LoaderState::*;

impl LoaderState {
    fn is_error(&self) -> bool {
        matches!(self, Error(_))
    }
}

struct MarkedLoader {
    source: usize,
    state_stack: Vec<LoaderState>,
    options: LoaderOptions,
}

impl MarkedEventReceiver for MarkedLoader {
    fn on_event(&mut self, ev: Event, mark: YamlMarker) {
        // Short-circuit if the state stack is in error
        if self.state_stack[self.state_stack.len() - 1].is_error() {
            return;
        }
        let mark = self.marker(mark);
        let curstate = self
            .state_stack
            .pop()
            .expect("State stack became unbalanced");
        let newstate = match ev {
            Event::Alias(_) => unreachable!(),
            Event::StreamStart => {
                assert_eq!(curstate, Initial);
                StartStream
            }
            Event::DocumentStart => {
                assert_eq!(curstate, StartStream);
                StartDocument
            }
            Event::MappingStart(aid, tag) => {
                if tag.is_some() {
                    Error(LoadError::UnexpectedTag(mark))
                } else if aid == 0 {
                    match curstate {
                        StartDocument => {
                            if self.options.toplevel_is_mapping {
                                MappingWaitingOnKey(mark, MappingHash::new())
                            } else {
                                Error(LoadError::TopLevelMustBeSequence(mark))
                            }
                        }
                        MappingWaitingOnKey(_, _) => Error(LoadError::MappingKeyMustBeScalar(mark)),
                        MappingWaitingOnValue(_, _, _) => {
                            self.state_stack.push(curstate);
                            MappingWaitingOnKey(mark, MappingHash::new())
                        }
                        SequenceWaitingOnValue(_, _) => {
                            self.state_stack.push(curstate);
                            MappingWaitingOnKey(mark, MappingHash::new())
                        }
                        _ => unreachable!(),
                    }
                } else {
                    Error(LoadError::UnexpectedAnchor(mark))
                }
            }
            Event::MappingEnd => match curstate {
                MappingWaitingOnKey(startmark, map) => {
                    let span = Span::new_with_marks(startmark, mark);
                    let node = Node::from(MarkedMappingNode::new(span, map));
                    if let Some(topstate) = self.state_stack.pop() {
                        match topstate {
                            MappingWaitingOnValue(mark, mut map, key) => {
                                match map.entry(key.clone()) {
                                    Entry::Occupied(entry)
                                        if self.options.error_on_duplicate_keys =>
                                    {
                                        Error(LoadError::DuplicateKey(Box::new(
                                            DuplicateKeyInner {
                                                prev_key: entry.key().clone(),
                                                key,
                                            },
                                        )))
                                    }
                                    _ => {
                                        map.insert(key, node);
                                        MappingWaitingOnKey(mark, map)
                                    }
                                }
                            }
                            SequenceWaitingOnValue(mark, mut list) => {
                                list.push(node);
                                SequenceWaitingOnValue(mark, list)
                            }
                            _ => unreachable!(),
                        }
                    } else {
                        Finished(node)
                    }
                }
                _ => unreachable!(),
            },
            Event::SequenceStart(aid, tag) => {
                if tag.is_some() {
                    Error(LoadError::UnexpectedTag(mark))
                } else if aid == 0 {
                    match curstate {
                        StartDocument => {
                            if self.options.toplevel_is_mapping {
                                Error(LoadError::TopLevelMustBeMapping(mark))
                            } else {
                                SequenceWaitingOnValue(mark, Vec::new())
                            }
                        }
                        MappingWaitingOnKey(_, _) => Error(LoadError::MappingKeyMustBeScalar(mark)),
                        mv @ MappingWaitingOnValue(_, _, _) => {
                            self.state_stack.push(mv);
                            SequenceWaitingOnValue(mark, Vec::new())
                        }
                        sv @ SequenceWaitingOnValue(_, _) => {
                            self.state_stack.push(sv);
                            SequenceWaitingOnValue(mark, Vec::new())
                        }
                        _ => unreachable!(),
                    }
                } else {
                    Error(LoadError::UnexpectedAnchor(mark))
                }
            }
            Event::SequenceEnd => match curstate {
                SequenceWaitingOnValue(startmark, list) => {
                    let span = Span::new_with_marks(startmark, mark);
                    let node = Node::from(MarkedSequenceNode::new(span, list));
                    if let Some(topstate) = self.state_stack.pop() {
                        match topstate {
                            MappingWaitingOnValue(mark, mut map, key) => {
                                match map.entry(key.clone()) {
                                    Entry::Occupied(entry)
                                        if self.options.error_on_duplicate_keys =>
                                    {
                                        Error(LoadError::DuplicateKey(Box::new(
                                            DuplicateKeyInner {
                                                prev_key: entry.key().clone(),
                                                key,
                                            },
                                        )))
                                    }
                                    _ => {
                                        map.insert(key, node);
                                        MappingWaitingOnKey(mark, map)
                                    }
                                }
                            }
                            SequenceWaitingOnValue(mark, mut list) => {
                                list.push(node);
                                SequenceWaitingOnValue(mark, list)
                            }
                            _ => unreachable!(),
                        }
                    } else {
                        Finished(node)
                    }
                }
                _ => unreachable!(),
            },
            Event::DocumentEnd => match curstate {
                Finished(_) => curstate,
                _ => unreachable!(),
            },
            Event::StreamEnd => match curstate {
                StartStream => Finished(Node::from(MarkedMappingNode::new_empty(
                    Span::new_with_marks(mark, mark),
                ))),
                Finished(_) => curstate,
                _ => unreachable!(),
            },
            Event::Scalar(val, kind, aid, tag) => {
                if aid == 0 {
                    if tag.is_some() {
                        Error(LoadError::UnexpectedTag(mark))
                    } else {
                        let span = Span::new_start(mark);
                        let val = if matches!(curstate, MappingWaitingOnKey(_, _))
                            && self.options.lowercase_keys
                        {
                            val.to_lowercase()
                        } else {
                            val
                        };
                        let mut node = MarkedScalarNode::new(span, val);
                        if self.options.prevent_coercion {
                            node.set_coerce(matches!(kind, TScalarStyle::Plain));
                        }
                        match curstate {
                            MappingWaitingOnKey(mark, map) => {
                                MappingWaitingOnValue(mark, map, node)
                            }
                            MappingWaitingOnValue(mark, mut map, key) => {
                                match map.entry(key.clone()) {
                                    Entry::Occupied(entry)
                                        if self.options.error_on_duplicate_keys =>
                                    {
                                        Error(LoadError::DuplicateKey(Box::new(
                                            DuplicateKeyInner {
                                                prev_key: entry.key().clone(),
                                                key,
                                            },
                                        )))
                                    }
                                    _ => {
                                        map.insert(key, Node::from(node));
                                        MappingWaitingOnKey(mark, map)
                                    }
                                }
                            }
                            SequenceWaitingOnValue(mark, mut list) => {
                                list.push(Node::from(node));
                                SequenceWaitingOnValue(mark, list)
                            }
                            StartDocument => Error(LoadError::TopLevelMustBeMapping(mark)),
                            _ => unreachable!(),
                        }
                    }
                } else {
                    Error(LoadError::UnexpectedAnchor(mark))
                }
            }
            Event::Nothing => unreachable!(),
        };
        self.state_stack.push(newstate);
    }
}

impl MarkedLoader {
    fn new(source: usize, options: LoaderOptions) -> Self {
        Self {
            source,
            state_stack: vec![Initial],
            options,
        }
    }

    fn marker(&self, mark: YamlMarker) -> Marker {
        Marker::new(self.source, mark.index(), mark.line(), mark.col() + 1)
    }

    fn finish(mut self) -> Result<Node, LoadError> {
        let top = self.state_stack.pop();
        match top.expect("YAML parser state stack unexpectedly empty") {
            Finished(n) => Ok(n),
            Error(e) => Err(e),
            _ => unreachable!(),
        }
    }
}

/// Parse YAML from a string and return a Node representing
/// the content.
///
/// When parsing YAML, the source is stored into all markers which are
/// in the node spans.  This means that later if you only have a node,
/// you can determine which source it came from without needing complex
/// lifetimes to bind strings or other non-copy data to nodes.
///
/// This function requires that the top level be a mapping, but the returned
/// type here is the generic Node enumeration to make it potentially easier
/// for callers to use.  Regardless, it's always possible to treat the
/// returned node as a mapping node without risk of panic.
///
/// If you wish to load a sequence instead of a mapping, then you will
/// need to use [`parse_yaml_with_options`] to request that.
///
/// ```
/// # use marked_yaml::*;
/// let node = parse_yaml(0, include_str!("../examples/everything.yaml"))
///     .unwrap()
///     .as_mapping()
///     .unwrap();
/// ```
pub fn parse_yaml<S>(source: usize, yaml: S) -> Result<Node, LoadError>
where
    S: AsRef<str>,
{
    let options = LoaderOptions::default();

    parse_yaml_with_options(source, yaml, options)
}

/// Parse YAML from a string and return a Node representing
/// the content.
///
/// Takes an additional LoaderOptions struct to control the behavior of the loader.
///
/// This is the way to parse a file with a top-level sequence instead of a mapping
/// node.
///
/// See `parse_yaml` for more information.
pub fn parse_yaml_with_options<S>(
    source: usize,
    yaml: S,
    options: LoaderOptions,
) -> Result<Node, LoadError>
where
    S: AsRef<str>,
{
    let mut loader = MarkedLoader::new(source, options);
    let mut parser = Parser::new(yaml.as_ref().chars());
    parser.load(&mut loader, false).map_err(|se| {
        let mark = loader.marker(*se.marker());
        LoadError::ScanError(mark, se)
    })?;
    loader.finish()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn smoke_basics() {
        let node = parse_yaml(0, "{}").unwrap();
        assert!(node.as_mapping().is_some());
    }

    #[test]
    fn load_everything() {
        let node = parse_yaml(0, include_str!("../examples/everything.yaml")).unwrap();
        let map = node.as_mapping().unwrap();
        assert_eq!(map.get_scalar("simple").unwrap().as_str(), "scalar");
        assert_eq!(map.get_scalar("boolean1").unwrap().as_bool(), Some(true));
        assert_eq!(map.get_scalar("boolean2").unwrap().as_bool(), Some(false));
    }

    #[test]
    fn prevent_coercion() {
        let node = parse_yaml_with_options(
            0,
            include_str!("../examples/everything.yaml"),
            LoaderOptions::default().prevent_coercion(true),
        )
        .unwrap();
        let map = node.as_mapping().unwrap();
        assert_eq!(map.get_scalar("simple").unwrap().as_str(), "scalar");
        assert_eq!(map.get_scalar("boolean1").unwrap().as_str(), "true");
        assert_eq!(map.get_scalar("boolean1").unwrap().as_bool(), None);
        assert_eq!(map.get_scalar("boolean2").unwrap().as_str(), "false");
        assert_eq!(map.get_scalar("boolean2").unwrap().as_bool(), Some(false));
        assert_eq!(map.get_scalar("integer").unwrap().as_str(), "1234");
        assert_eq!(map.get_scalar("integer").unwrap().as_i32(), None);
        assert_eq!(map.get_scalar("float").unwrap().as_str(), "12.34");
        assert_eq!(map.get_scalar("float").unwrap().as_f32(), Some(12.34));
    }

    #[test]
    fn toplevel_is_empty() {
        let node = parse_yaml(0, "").unwrap();
        let map = node.as_mapping().unwrap();
        assert!(map.is_empty());
    }

    #[test]
    fn toplevel_is_empty_inline() {
        let node = parse_yaml(0, "{}").unwrap();
        let map = node.as_mapping().unwrap();
        assert!(map.is_empty());
    }

    #[test]
    fn toplevel_is_scalar() {
        let err = parse_yaml(0, "foo");
        assert_eq!(
            err,
            Err(LoadError::TopLevelMustBeMapping(Marker::new(0, 0, 1, 1)))
        );
        assert!(format!("{}", err.err().unwrap()).contains("1:1: "));
    }

    #[test]
    fn toplevel_is_sequence() {
        assert_eq!(
            parse_yaml(0, "[]"),
            Err(LoadError::TopLevelMustBeMapping(Marker::new(0, 0, 1, 1)))
        );
    }

    #[test]
    fn duplicate_key() {
        let err = parse_yaml_with_options(
            0,
            "{foo: bar, foo: baz}",
            LoaderOptions::default().error_on_duplicate_keys(true),
        );

        assert_eq!(
            err,
            Err(LoadError::DuplicateKey(Box::new(DuplicateKeyInner {
                prev_key: MarkedScalarNode::new(Span::new_start(Marker::new(0, 0, 1, 1)), "foo"),
                key: MarkedScalarNode::new(Span::new_start(Marker::new(0, 10, 1, 11)), "foo")
            })))
        );

        assert_eq!(
            format!("{}", err.err().unwrap()),
            "Duplicate key \"foo\" in mapping at 1:2 and 1:12"
        );

        // Without error_on_duplicate_keys, the last key wins
        let node = parse_yaml(0, "{foo: bar, foo: baz}").unwrap();
        let map = node.as_mapping().unwrap();
        assert_eq!(map.get_scalar("foo").unwrap().as_str(), "baz");
    }

    #[test]
    fn unexpected_anchor() {
        let err = parse_yaml(0, "&foo {}");
        assert_eq!(
            err,
            Err(LoadError::UnexpectedAnchor(Marker::new(0, 5, 1, 6)))
        );
        assert!(format!("{}", err.err().unwrap()).starts_with("1:6: "));
    }

    #[test]
    fn unexpected_anchor2() {
        assert_eq!(
            parse_yaml(0, "{bar: &foo []}"),
            Err(LoadError::UnexpectedAnchor(Marker::new(0, 11, 1, 12)))
        );
    }

    #[test]
    fn unexpected_anchor3() {
        assert_eq!(
            parse_yaml(0, "{bar: &foo susan}"),
            Err(LoadError::UnexpectedAnchor(Marker::new(0, 11, 1, 12)))
        );
    }

    #[test]
    fn mapping_key_mapping() {
        let err = parse_yaml(0, "{? {} : {}}");
        assert_eq!(
            err,
            Err(LoadError::MappingKeyMustBeScalar(Marker::new(0, 3, 1, 4)))
        );
        assert!(format!("{}", err.err().unwrap()).starts_with("1:4: "));
    }

    #[test]
    fn mapping_key_sequence() {
        assert_eq!(
            parse_yaml(0, "{? [] : {}}"),
            Err(LoadError::MappingKeyMustBeScalar(Marker::new(0, 3, 1, 4)))
        );
    }

    #[test]
    fn unexpected_tag() {
        let err = parse_yaml(0, "{foo: !!str bar}");
        assert_eq!(
            err,
            Err(LoadError::UnexpectedTag(Marker::new(0, 12, 1, 13)))
        );
        assert!(format!("{}", err.err().unwrap()).starts_with("1:13: "));
    }

    #[test]
    fn nested_mapping_key_mapping() {
        assert_eq!(
            parse_yaml(0, "{foo: {? [] : {}}}"),
            Err(LoadError::MappingKeyMustBeScalar(Marker::new(0, 9, 1, 10)))
        );
    }

    #[test]
    fn malformed_yaml_for_scanerror() {
        let err = parse_yaml(0, "{");
        assert!(err.is_err());
        assert!(format!("{}", err.err().unwrap()).starts_with("2:1: "));
    }

    #[test]
    fn toplevel_sequence_wanted() {
        let node =
            parse_yaml_with_options(0, "[yaml]", LoaderOptions::default().toplevel_sequence())
                .unwrap();
        assert!(node.as_sequence().is_some());
    }

    #[test]
    fn toplevel_sequence_wanted_got_mapping() {
        assert_eq!(
            parse_yaml_with_options(0, "{}", LoaderOptions::default().toplevel_sequence()),
            Err(LoadError::TopLevelMustBeSequence(Marker::new(0, 0, 1, 1)))
        );
    }

    #[test]
    fn lowercase_keys() {
        let node = parse_yaml_with_options(
            0,
            "KEY: VALUE",
            LoaderOptions::default().lowercase_keys(false),
        )
        .unwrap();
        assert!(node.as_mapping().unwrap().contains_key("KEY"));
        assert!(!node.as_mapping().unwrap().contains_key("key"));

        let node = parse_yaml_with_options(
            0,
            "KEY: VALUE",
            LoaderOptions::default().lowercase_keys(true),
        )
        .unwrap();
        assert!(!node.as_mapping().unwrap().contains_key("KEY"));
        assert!(node.as_mapping().unwrap().contains_key("key"));
    }
}
