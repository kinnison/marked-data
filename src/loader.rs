//! Loading YAML from strings
//!
//! TODO: better frontmatter

use crate::types::*;

use yaml_rust::parser::{Event, MarkedEventReceiver, Parser};
use yaml_rust::scanner::Marker as YamlMarker;
use yaml_rust::scanner::ScanError;

use std::fmt::{self, Display};

/// Errors which can occur during loading of YAML
#[derive(Debug, PartialEq, Eq)]
pub enum LoadError {
    /// An unknown error occurred
    Unknown,
    /// Something other than a mapping detected at the top level
    TopLevelMustBeMapping(Marker),
    /// Unexpected definition of anchor
    UnexpectedAnchor(Marker),
    /// Unexpected use of an alias
    UnexpectedAlias(Marker),
    /// Mapping keys must be scalars
    MappingKeyMustBeScalar(Marker),
    /// An explicit tag was detected
    UnexpectedTag(Marker),
    /// A YAML scanner error occured
    ScanError(ScanError),
}

impl Display for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use LoadError::*;
        match self {
            Unknown => write!(f, "Unknown error"),
            ScanError(e) => write!(f, "Low level error: {}", e),
            TopLevelMustBeMapping(m) => write!(f, "{}: Top level must be a mapping", m),
            UnexpectedAnchor(m) => write!(f, "{}: Unexpected definition of anchor", m),
            UnexpectedAlias(m) => write!(f, "{}: Unexpected use of alias", m),
            MappingKeyMustBeScalar(m) => write!(f, "{}: Keys in mappings must be scalar", m),
            UnexpectedTag(m) => write!(f, "{}: Unexpected use of YAML tag", m),
        }
    }
}

impl std::error::Error for LoadError {}

#[derive(Debug)]
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
        match self {
            Error(_) => true,
            _ => false,
        }
    }
}

struct MarkedLoader {
    source: usize,
    state_stack: Vec<LoaderState>,
}

impl MarkedEventReceiver for MarkedLoader {
    fn on_event(&mut self, ev: Event, mark: YamlMarker) {
        // Short-circuit if the state stack is in error
        if self.state_stack.len() == 1 && self.state_stack[0].is_error() {
            return;
        }
        let mark = self.marker(mark);
        let curstate = self
            .state_stack
            .pop()
            .expect("State stack became unbalanced");
        let newstate = match ev {
            Event::Alias(_) => Error(LoadError::UnexpectedAlias(mark)),
            Event::StreamStart => {
                if let Initial = curstate {
                    StartStream
                } else {
                    Error(LoadError::Unknown)
                }
            }
            Event::DocumentStart => {
                if let StartStream = curstate {
                    StartDocument
                } else {
                    Error(LoadError::Unknown)
                }
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
                        _ => Error(LoadError::Unknown),
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
                            _ => Error(LoadError::Unknown),
                        }
                    } else {
                        Finished(node)
                    }
                }
                _ => Error(LoadError::Unknown),
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
                        _ => Error(LoadError::Unknown),
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
                            _ => Error(LoadError::Unknown),
                        }
                    } else {
                        Error(LoadError::Unknown)
                    }
                }
                _ => Error(LoadError::Unknown),
            },
            Event::DocumentEnd => match curstate {
                Finished(_) => curstate,
                _ => Error(LoadError::Unknown),
            },
            Event::StreamEnd => match curstate {
                Finished(_) => curstate,
                _ => Error(LoadError::Unknown),
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
                            _ => Error(LoadError::Unknown),
                        }
                    }
                } else {
                    Error(LoadError::UnexpectedAnchor(mark))
                }
            }
            Event::Nothing => Error(LoadError::Unknown),
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
        match self.state_stack.len() {
            0 => Err(LoadError::Unknown),
            1 => match self.state_stack.pop().unwrap() {
                Finished(n) => Ok(n),
                Error(e) => Err(e),
                _ => Err(LoadError::Unknown),
            },
            _ => Err(LoadError::Unknown),
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
pub fn parse_yaml(source: usize, yaml: &str) -> Result<Node, LoadError> {
    let mut loader = MarkedLoader::new(source);
    let mut parser = Parser::new(yaml.chars());
    parser
        .load(&mut loader, false)
        .map_err(LoadError::ScanError)?;
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
    fn toplevel_is_scalar() {
        assert_eq!(
            parse_yaml(0, "foo"),
            Err(LoadError::TopLevelMustBeMapping(Marker::new(0, 1, 1)))
        );
    }

    #[test]
    fn toplevel_is_sequence() {
        assert_eq!(
            parse_yaml(0, "foo"),
            Err(LoadError::TopLevelMustBeMapping(Marker::new(0, 1, 1)))
        );
    }

    #[test]
    fn unexpected_anchor() {
        assert_eq!(
            parse_yaml(0, "&foo {}"),
            Err(LoadError::UnexpectedAnchor(Marker::new(0, 1, 6)))
        );
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
    #[ignore = "not sure yet if this error is possible"]
    fn unexpected_alias() {
        assert_eq!(
            parse_yaml(0, "{<<: *foo}"),
            Err(LoadError::UnexpectedAlias(Marker::new(0, 1, 1)))
        );
    }

    #[test]
    fn mapping_key_mapping() {
        assert_eq!(
            parse_yaml(0, "{? {} : {}}"),
            Err(LoadError::MappingKeyMustBeScalar(Marker::new(0, 1, 4)))
        );
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
        assert_eq!(
            parse_yaml(0, "{foo: !!str bar}"),
            Err(LoadError::UnexpectedTag(Marker::new(0, 1, 13)))
        );
    }

    #[test]
    fn nested_mapping_key_mapping() {
        assert_eq!(
            parse_yaml(0, "{foo: {? [] : {}}}"),
            Err(LoadError::MappingKeyMustBeScalar(Marker::new(0, 1, 10)))
        );
    }
}
