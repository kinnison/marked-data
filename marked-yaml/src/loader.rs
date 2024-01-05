//! Loading YAML
//!

use crate::types::*;

use yaml_rust::parser::{Event, MarkedEventReceiver, Parser};
use yaml_rust::scanner::Marker as YamlMarker;
use yaml_rust::scanner::ScanError;

use std::error::Error;
use std::fmt::{self, Display};

/// Errors which can occur during loading of YAML
#[derive(Debug, PartialEq, Eq)]
pub enum LoadError {
    /// Something other than a mapping detected at the top level
    TopLevelMustBeMapping(Marker),
    /// Unexpected definition of anchor
    UnexpectedAnchor(Marker),
    /// Mapping keys must be scalars
    MappingKeyMustBeScalar(Marker),
    /// An explicit tag was detected
    UnexpectedTag(Marker),
    /// A YAML scanner error occured
    ScanError(Marker, ScanError),
}

impl Display for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use LoadError::*;
        #[allow(deprecated)]
        match self {
            TopLevelMustBeMapping(m) => write!(f, "{}: Top level must be a mapping", m),
            UnexpectedAnchor(m) => write!(f, "{}: Unexpected definition of anchor", m),
            MappingKeyMustBeScalar(m) => write!(f, "{}: Keys in mappings must be scalar", m),
            UnexpectedTag(m) => write!(f, "{}: Unexpected use of YAML tag", m),
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
            Event::MappingStart(aid) => {
                if aid == 0 {
                    match curstate {
                        StartDocument => MappingWaitingOnKey(mark, MappingHash::new()),
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
                                map.insert(key, node);
                                MappingWaitingOnKey(mark, map)
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
            Event::SequenceStart(aid) => {
                if aid == 0 {
                    match curstate {
                        StartDocument => Error(LoadError::TopLevelMustBeMapping(mark)),
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
                                map.insert(key, node);
                                MappingWaitingOnKey(mark, map)
                            }
                            SequenceWaitingOnValue(mark, mut list) => {
                                list.push(node);
                                SequenceWaitingOnValue(mark, list)
                            }
                            _ => unreachable!(),
                        }
                    } else {
                        unreachable!()
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
            Event::Scalar(val, _kind, aid, tag) => {
                if aid == 0 {
                    if tag.is_some() {
                        Error(LoadError::UnexpectedTag(mark))
                    } else {
                        let span = Span::new_start(mark);
                        let node = MarkedScalarNode::new(span, val);
                        match curstate {
                            MappingWaitingOnKey(mark, map) => {
                                MappingWaitingOnValue(mark, map, node)
                            }
                            MappingWaitingOnValue(mark, mut map, key) => {
                                map.insert(key, Node::from(node));
                                MappingWaitingOnKey(mark, map)
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
    fn new(source: usize) -> Self {
        Self {
            source,
            state_stack: vec![Initial],
        }
    }

    fn marker(&self, mark: YamlMarker) -> Marker {
        Marker::new(self.source, mark.line(), mark.col() + 1)
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
/// This library requires that the top level be a mapping, but the returned
/// type here is the generic Node enumeration to make it potentially easier
/// for callers to use.  Regardless, it's always possible to treat the
/// returned node as a mapping node without risk of panic.
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
    let mut loader = MarkedLoader::new(source);
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
            Err(LoadError::TopLevelMustBeMapping(Marker::new(0, 1, 1)))
        );
        assert!(format!("{}", err.err().unwrap()).contains("1:1: "));
    }

    #[test]
    fn toplevel_is_sequence() {
        assert_eq!(
            parse_yaml(0, "[]"),
            Err(LoadError::TopLevelMustBeMapping(Marker::new(0, 1, 1)))
        );
    }

    #[test]
    fn unexpected_anchor() {
        let err = parse_yaml(0, "&foo {}");
        assert_eq!(err, Err(LoadError::UnexpectedAnchor(Marker::new(0, 1, 6))));
        assert!(format!("{}", err.err().unwrap()).starts_with("1:6: "));
    }

    #[test]
    fn unexpected_anchor2() {
        assert_eq!(
            parse_yaml(0, "{bar: &foo []}"),
            Err(LoadError::UnexpectedAnchor(Marker::new(0, 1, 12)))
        );
    }

    #[test]
    fn unexpected_anchor3() {
        assert_eq!(
            parse_yaml(0, "{bar: &foo susan}"),
            Err(LoadError::UnexpectedAnchor(Marker::new(0, 1, 12)))
        );
    }

    #[test]
    fn mapping_key_mapping() {
        let err = parse_yaml(0, "{? {} : {}}");
        assert_eq!(
            err,
            Err(LoadError::MappingKeyMustBeScalar(Marker::new(0, 1, 4)))
        );
        assert!(format!("{}", err.err().unwrap()).starts_with("1:4: "));
    }

    #[test]
    fn mapping_key_sequence() {
        assert_eq!(
            parse_yaml(0, "{? [] : {}}"),
            Err(LoadError::MappingKeyMustBeScalar(Marker::new(0, 1, 4)))
        );
    }

    #[test]
    fn unexpected_tag() {
        let err = parse_yaml(0, "{foo: !!str bar}");
        assert_eq!(err, Err(LoadError::UnexpectedTag(Marker::new(0, 1, 13))));
        assert!(format!("{}", err.err().unwrap()).starts_with("1:13: "));
    }

    #[test]
    fn nested_mapping_key_mapping() {
        assert_eq!(
            parse_yaml(0, "{foo: {? [] : {}}}"),
            Err(LoadError::MappingKeyMustBeScalar(Marker::new(0, 1, 10)))
        );
    }

    #[test]
    fn malformed_yaml_for_scanerror() {
        let err = parse_yaml(0, "{");
        assert!(err.is_err());
        assert!(format!("{}", err.err().unwrap()).starts_with("2:1: "));
    }
}
