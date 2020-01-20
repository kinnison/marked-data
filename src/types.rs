//! Various basic types for YAML handling
//!

use linked_hash_map::LinkedHashMap;
use std::borrow::Cow;
use std::convert::TryFrom;
use std::hash::{Hash, Hasher};
use std::iter::FromIterator;
use std::ops::Deref;
use yaml_rust::Yaml as YamlNode;

/// A marker for a YAML node
///
/// This indicates where a node started or ended.
/// TODO: example
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Marker {
    source: usize,
    line: usize,
    column: usize,
}

impl Marker {
    /// Create a new Marker
    ///
    /// This will typically not be used because markers will come from
    /// parsing YAML, however it is provided for completeness and in case
    /// you need it for your own tests.
    ///
    /// ```
    /// # use marked_yaml::Marker;
    /// let marker = Marker::new(0, 1, 2);
    /// # assert_eq!(marker.source(), 0);
    /// # assert_eq!(marker.line(), 1);
    /// # assert_eq!(marker.column(), 2);
    /// ```
    pub fn new(source: usize, line: usize, column: usize) -> Self {
        Self {
            source,
            line,
            column,
        }
    }

    /// The source index given for this marker.
    ///
    /// When parsing YAML, we record where nodes start (and often finish).
    /// This is the source index provided when parsing the YAML.  Likely this
    /// refers to a vector of PathBuf recording where input came from, but that
    /// is entirely up to the user of this library crate.
    ///
    /// This is likely most useful to computers, not humans.
    ///
    /// ```
    /// # use marked_yaml::Marker;
    /// # let marker = Marker::new(0, 1, 2);
    /// assert_eq!(marker.source(), 0);
    /// ```
    pub fn source(&self) -> usize {
        self.source
    }

    /// The line number on which this marker resides, 1-indexed
    ///
    /// When parsing YAML, we record where nodes start (and often finish).
    /// This is the line number of where this marker resides.  Line numbers
    /// start with 1 to make them more useful to humans.
    ///
    /// ```
    /// # use marked_yaml::Marker;
    /// # let marker = Marker::new(0, 1, 2);
    /// assert_eq!(marker.line(), 1);
    /// ```
    pub fn line(&self) -> usize {
        self.line
    }

    /// The column number at which this marker resides, 1-indexed
    ///
    /// When parsing YAML, we record where nodes start (and often finish).
    /// This is the column number of where this marker resides.  Column numbers
    /// start with 1 to make them more useful to humans.
    ///
    /// ```
    /// # use marked_yaml::Marker;
    /// # let marker = Marker::new(0, 1, 2);
    /// assert_eq!(marker.column(), 2);
    /// ```
    pub fn column(&self) -> usize {
        self.column
    }
}

/// The span for a YAML marked node
///
/// TODO: example
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Span {
    start: Option<Marker>,
    end: Option<Marker>,
}

impl Span {
    /// Create a span with no marker information
    ///
    /// Sometimes we simply do not know where information came from (for example
    /// if it was created by software) and in that case we can create a blank
    /// span.
    ///
    /// ```
    /// # use marked_yaml::Span;
    /// let blank = Span::new_blank();
    /// # assert_eq!(blank.start(), None);
    /// # assert_eq!(blank.end(), None);
    /// ```
    pub fn new_blank() -> Self {
        Self {
            start: None,
            end: None,
        }
    }

    /// Create a span with only start information
    ///
    /// Sometimes when creating a span we know where it started but not where
    /// it ends.  This might be during parsing, or for some other reason.
    ///
    /// ```
    /// # use marked_yaml::{Marker, Span};
    /// let span = Span::new_start(Marker::new(0, 1, 2));
    /// # assert_eq!(span.start().unwrap(), &Marker::new(0, 1, 2));
    /// # assert_eq!(span.end(), None);
    /// ```
    pub fn new_start(start: Marker) -> Self {
        Self {
            start: Some(start),
            end: None,
        }
    }

    /// Create a span with both start and end markers
    ///
    /// When we know both the start and end of a node, we can create a span
    /// which has all that knowledge.
    ///
    /// ```
    /// # use marked_yaml::{Marker,Span};
    /// let span = Span::new_with_marks(Marker::new(0, 1, 1), Marker::new(10, 2, 1));
    /// # assert_eq!(span.start().unwrap(), &Marker::new(0, 1, 1));
    /// # assert_eq!(span.end().unwrap(), &Marker::new(10, 2, 1));
    /// ```
    pub fn new_with_marks(start: Marker, end: Marker) -> Self {
        Self {
            start: Some(start),
            end: Some(end),
        }
    }

    /// The start of the span
    ///
    /// ```
    /// # use marked_yaml::{Marker, Span};
    /// # let span = Span::new_with_marks(Marker::new(0, 1, 1), Marker::new(10, 2, 1));
    /// assert_eq!(span.start(), Some(&Marker::new(0, 1, 1)));
    /// ```
    pub fn start(&self) -> Option<&Marker> {
        self.start.as_ref()
    }

    /// The end of the span
    ///
    /// ```
    /// # use marked_yaml::{Marker, Span};
    /// # let span = Span::new_with_marks(Marker::new(0, 1, 1), Marker::new(10, 2, 1));
    /// assert_eq!(span.end(), Some(&Marker::new(10, 2, 1)));
    /// ```
    pub fn end(&self) -> Option<&Marker> {
        self.end.as_ref()
    }
}

/// A marked YAML node
///
/// **NOTE**: Nodes are considered equal even if they don't come from the
/// same place.  *i.e. their spans are ignored for equality and hashing*
///
/// TODO: explain simplified YAML
/// TODO: example
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Node {
    /// A YAML scalar
    ///
    /// You can test if a node is a scalar, and retrieve it as one if you
    /// so wish.
    Scalar(MarkedScalarNode),
    /// A YAML mapping
    ///
    /// You can test if a node is a mapping, and retrieve it as one if you
    /// so wish.
    Mapping(MarkedMappingNode),
    /// A YAML sequence
    ///
    /// You can test if a node is a sequence, and retrieve it as one if you
    /// so wish.
    Sequence(MarkedSequenceNode),
}

/// A marked scalar YAML node
///
/// Scalar nodes are treated by this crate as strings, though a few special
/// values are processed into the types which YAML would ascribe.  In particular
/// strings of the value `null`, `true`, `false`, etc. are able to present as
/// their special values to make it a bit easier for users of the crate.
///
/// **NOTE**: Nodes are considered equal even if they don't come from the
/// same place.  *i.e. their spans are ignored for equality and hashing*
///
/// TODO: example
#[derive(Clone, Debug)]
pub struct MarkedScalarNode {
    span: Span,
    value: String,
}

/// A marked YAML mapping node
///
/// Mapping nodes in YAML are defined as a key/value mapping where the keys are
/// unique and always scalars, whereas values may be YAML nodes of any kind.
///
/// Because *some* users of this crate may need to care about insertion order
/// we use `linked_hash_map` for this.
///
/// **NOTE**: Nodes are considered equal even if they don't come from the
/// same place.  *i.e. their spans are ignored for equality and hashing*
///
/// TODO: example
#[derive(Clone, Debug)]
pub struct MarkedMappingNode {
    span: Span,
    value: LinkedHashMap<MarkedScalarNode, Node>,
}

/// A marked YAML sequence node
///
/// Sequence nodes in YAML are simply ordered lists of YAML nodes.
///
/// **NOTE**: Nodes are considered equal even if they don't come from the
/// same place.  *i.e. their spans are ignored for equality and hashing*
///
/// TODO: example
#[derive(Clone, Debug)]
pub struct MarkedSequenceNode {
    span: Span,
    value: Vec<Node>,
}

macro_rules! basic_traits {
    ($t:path) => {
        impl PartialEq for $t {
            fn eq(&self, other: &Self) -> bool {
                self.value == other.value
            }

            fn ne(&self, other: &Self) -> bool {
                self.value != other.value
            }
        }

        impl Eq for $t {}

        impl Hash for $t {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.value.hash(state);
            }
        }
    };
}

basic_traits!(MarkedScalarNode);
basic_traits!(MarkedSequenceNode);
basic_traits!(MarkedMappingNode);

impl<T> From<T> for Node
where
    T: Into<MarkedScalarNode>,
{
    fn from(value: T) -> Node {
        Node::Scalar(value.into())
    }
}

impl From<MarkedSequenceNode> for Node {
    fn from(value: MarkedSequenceNode) -> Node {
        Node::Sequence(value)
    }
}

impl<T> From<Vec<T>> for Node
where
    T: Into<Node>,
{
    fn from(value: Vec<T>) -> Node {
        Node::Sequence(value.into_iter().collect())
    }
}

impl From<MarkedMappingNode> for Node {
    fn from(value: MarkedMappingNode) -> Node {
        Node::Mapping(value)
    }
}

macro_rules! node_span {
    ($t:path) => {
        impl $t {
            /// Retrieve the Span from this node
            ///
            pub fn span(&self) -> &Span {
                &self.span
            }
        }
    };
}

node_span!(MarkedScalarNode);
node_span!(MarkedMappingNode);
node_span!(MarkedSequenceNode);

impl Node {
    /// Retrieve the Span from the contained Node
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let node: Node = "foobar".into();
    /// let span = node.span();
    /// assert_eq!(span.start(), None);
    /// ```
    pub fn span(&self) -> &Span {
        match self {
            Node::Scalar(msn) => msn.span(),
            Node::Sequence(msn) => msn.span(),
            Node::Mapping(mmn) => mmn.span(),
        }
    }
}

impl MarkedScalarNode {
    /// Create a new scalar node
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let node = MarkedScalarNode::new(Span::new_blank(), "foobar");
    /// ```
    pub fn new<'a, S: Into<Cow<'a, str>>>(span: Span, content: S) -> Self {
        Self {
            span,
            value: content.into().into_owned(),
        }
    }
}

impl<'a> From<&'a str> for MarkedScalarNode {
    /// Convert from any borrowed string into a node
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let node: MarkedScalarNode = "foobar".into();
    /// ```
    fn from(value: &'a str) -> Self {
        Self::new(Span::new_blank(), value)
    }
}

impl From<String> for MarkedScalarNode {
    /// Convert from any owned string into a node
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let foobar = "foobar".to_string();
    /// let node: MarkedScalarNode = foobar.into();
    /// ```
    fn from(value: String) -> Self {
        Self::new(Span::new_blank(), value)
    }
}

impl From<bool> for MarkedScalarNode {
    /// Convert from a boolean into a node
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let node = MarkedScalarNode::from(true);
    /// ```
    fn from(value: bool) -> Self {
        if value {
            "true".into()
        } else {
            "false".into()
        }
    }
}

impl Deref for MarkedScalarNode {
    type Target = str;

    /// Borrow the string value inside this scalar node
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// # use std::str::FromStr;
    /// let truth: MarkedScalarNode = "true".into();
    /// assert!(bool::from_str(&truth).unwrap())
    /// ```
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl MarkedSequenceNode {
    /// Create a new empty sequence node
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let node = MarkedSequenceNode::new_empty(Span::new_blank());
    /// ```
    pub fn new_empty(span: Span) -> Self {
        Self {
            span,
            value: Vec::new(),
        }
    }

    /// Get the number of entries in the node
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let node: MarkedSequenceNode = (0..5).map(|_| "Hello").collect();
    /// assert_eq!(node.len(), 5);
    /// ```
    pub fn len(&self) -> usize {
        self.value.len()
    }
}

impl<T> FromIterator<T> for MarkedSequenceNode
where
    T: Into<Node>,
{
    /// Allow collecting things into a sequence node
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let node: MarkedSequenceNode = vec!["hello", "world"].into_iter().collect();
    /// ```
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let value: Vec<Node> = iter.into_iter().map(Into::into).collect();
        let span = match value.len() {
            0 => Span::new_blank(),
            1 => value[0].span().clone(),
            _ => Span {
                start: value[0].span().start,
                end: value[value.len() - 1].span().end,
            },
        };
        Self { span, value }
    }
}

impl<T> From<Vec<T>> for MarkedSequenceNode
where
    T: Into<Node>,
{
    /// Allow converting from vectors of things to sequence nodes
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let node: MarkedSequenceNode = vec!["hello", "world"].into();
    /// ```
    fn from(value: Vec<T>) -> Self {
        let value: Vec<Node> = value.into_iter().map(Into::into).collect();
        let span = match value.len() {
            0 => Span::new_blank(),
            1 => value[0].span().clone(),
            _ => {
                let start = value[0].span().start;
                let end = value[value.len() - 1].span().end;
                Span { start, end }
            }
        };
        Self { span, value }
    }
}

impl MarkedMappingNode {
    /// Create a new empty mapping node
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let node = MarkedMappingNode::new_empty(Span::new_blank());
    /// ```
    pub fn new_empty(span: Span) -> Self {
        Self {
            span,
            value: LinkedHashMap::new(),
        }
    }
}

impl<T, U> FromIterator<(T, U)> for MarkedMappingNode
where
    T: Into<MarkedScalarNode>,
    U: Into<Node>,
{
    /// Allow collecting into a mapping node
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// # use std::collections::HashMap;
    /// # let mut hashmap = HashMap::new();
    /// hashmap.insert("hello", vec!["world".to_string()]);
    /// hashmap.insert("key", vec!["value".to_string()]);
    /// let node: MarkedMappingNode = hashmap.into_iter().collect();
    /// ```
    fn from_iter<I: IntoIterator<Item = (T, U)>>(iter: I) -> Self {
        let value: LinkedHashMap<MarkedScalarNode, Node> = iter
            .into_iter()
            .map(|(k, v)| (k.into(), v.into()))
            .collect();
        let span = match value.len() {
            0 => Span::new_blank(),
            // Unwrap is safe because there's at least one span here
            1 => {
                let (k, v) = value.iter().next().unwrap();
                Span {
                    start: k.span().start,
                    end: v.span().end,
                }
            }
            _ => {
                let mut iter = value.iter();
                // Unwraps save because there's at least two spans here
                let start = iter.next().unwrap().0.span().start;
                let end = iter.last().unwrap().1.span().end;
                Span { start, end }
            }
        };
        Self { span, value }
    }
}

/// Errors which could be encountered while converting from a `yaml_rust::Yaml`
#[derive(Debug)]
pub enum YamlConversionError {
    /// An alias was encountered while converting
    Alias,
    /// A BadValue was encountered while converting
    BadValue,
    /// A non-scalar value was encountered when a scalar was expected
    NonScalar,
}

impl TryFrom<YamlNode> for MarkedScalarNode {
    type Error = YamlConversionError;

    fn try_from(value: YamlNode) -> Result<Self, Self::Error> {
        match value {
            YamlNode::Alias(_) => Err(YamlConversionError::Alias),
            YamlNode::Array(_) => Err(YamlConversionError::NonScalar),
            YamlNode::BadValue => Err(YamlConversionError::BadValue),
            YamlNode::Boolean(b) => Ok(if b { "true".into() } else { "false".into() }),
            YamlNode::Hash(_) => Err(YamlConversionError::NonScalar),
            YamlNode::Integer(i) => Ok(format!("{}", i).into()),
            YamlNode::Null => Ok("null".into()),
            YamlNode::Real(s) => Ok(s.into()),
            YamlNode::String(s) => Ok(s.into()),
        }
    }
}

impl TryFrom<YamlNode> for Node {
    type Error = YamlConversionError;

    /// Convert from any `yaml_rust::Yaml` to a Node
    ///
    /// ```
    /// # use yaml_rust::YamlLoader;
    /// # use marked_yaml::types::*;
    /// # use std::convert::TryFrom;
    /// let docs = YamlLoader::load_from_str("[1, 2]").unwrap();
    /// let yaml = docs.into_iter().next().unwrap();
    /// let node = Node::try_from(yaml).unwrap();
    /// ```
    fn try_from(value: YamlNode) -> Result<Self, Self::Error> {
        match value {
            YamlNode::Array(arr) => Ok(Node::Sequence(
                arr.into_iter()
                    .map(Node::try_from)
                    .collect::<Result<MarkedSequenceNode, Self::Error>>()?,
            )),
            YamlNode::Hash(h) => Ok(Node::Mapping(
                h.into_iter()
                    .map(|(k, v)| Ok((MarkedScalarNode::try_from(k)?, Node::try_from(v)?)))
                    .collect::<Result<MarkedMappingNode, Self::Error>>()?,
            )),
            scalar => Ok(Node::Scalar(MarkedScalarNode::try_from(scalar)?)),
        }
    }
}
