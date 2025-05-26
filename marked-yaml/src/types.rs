//! Various basic types for YAML handling
//!

use doc_comment::doc_comment;
use hashlink::LinkedHashMap;
use std::borrow::{Borrow, Cow};
use std::fmt::{self, Display};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
use yaml_rust::Yaml as YamlNode;

/// A displayable marker for a YAML node
///
/// While `Marker` can be `Display`'d it doesn't understand what its source
/// means.  This struct is the result of asking a Marker to render itself.
///
/// ```
/// # use marked_yaml::*;
/// let filenames = vec!["examples/everything.yaml"];
/// let nodes: Vec<_> = filenames
///     .iter()
///     .enumerate()
///     .map(|(i, name)| parse_yaml(i, std::fs::read_to_string(name).unwrap()).unwrap())
///     .collect();
/// for (n, node) in nodes.iter().enumerate() {
///     let marker = node.span().start().unwrap();
///     let rendered = format!("{}", marker.render(|i| filenames[i]));
///     assert!(rendered.starts_with(filenames[n]));
/// }
/// ```
pub struct RenderedMarker<D> {
    source: D,
    line: usize,
    column: usize,
}

impl<D> Display for RenderedMarker<D>
where
    D: Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.source.fmt(f)?;
        write!(f, ":{}:{}", self.line, self.column)
    }
}

/// A marker for a YAML node
///
/// This indicates where a node started or ended.
///
/// ```
/// use marked_yaml::{parse_yaml, Marker};
/// let node = parse_yaml(100, "{foo: bar}").unwrap();
/// let map = node.as_mapping().unwrap();
/// let bar = map.get("foo").unwrap();
/// // the "bar" string started on line 1, column 7 of source ID 100.
/// assert_eq!(bar.span().start(), Some(&Marker::new(100, 6, 1, 7)));
/// ```
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Marker {
    source: usize,
    character: usize,
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
    /// let marker = Marker::new(0, 3, 1, 2);
    /// # assert_eq!(marker.source(), 0);
    /// # assert_eq!(marker.character(), 3);
    /// # assert_eq!(marker.line(), 1);
    /// # assert_eq!(marker.column(), 2);
    /// ```
    pub fn new(source: usize, character: usize, line: usize, column: usize) -> Self {
        Self {
            source,
            character,
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
    /// # let marker = Marker::new(0, 3, 1, 2);
    /// assert_eq!(marker.source(), 0);
    /// ```
    pub fn source(&self) -> usize {
        self.source
    }

    /// The character index at which this marker resides
    ///
    /// When parsing YAML, we record where nodes start (and often finish).
    /// This is the character index into the source text of where this
    /// marker resides.  Character indices start with zero since they're
    /// meant for software rather than humans.
    ///
    /// ```
    /// # use marked_yaml::Marker;
    /// # let marker = Marker::new(0, 3, 1, 2);
    /// assert_eq!(marker.character(), 3);
    /// ```
    pub fn character(&self) -> usize {
        self.character
    }

    /// The line number on which this marker resides, 1-indexed
    ///
    /// When parsing YAML, we record where nodes start (and often finish).
    /// This is the line number of where this marker resides.  Line numbers
    /// start with 1 to make them more useful to humans.
    ///
    /// ```
    /// # use marked_yaml::Marker;
    /// # let marker = Marker::new(0, 3, 1, 2);
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
    /// # let marker = Marker::new(0, 3, 1, 2);
    /// assert_eq!(marker.column(), 2);
    /// ```
    pub fn column(&self) -> usize {
        self.column
    }

    /// Render this marker
    ///
    /// Markers have a source identifier, typically as passed to `parse_yaml()`
    /// but have no way in and of themselves to turn that into a useful name.
    /// This function allows you to create a rendered marker which knows how
    /// to show the source name.
    ///
    /// ```
    /// # use marked_yaml::Marker;
    /// # let marker = Marker::new(0, 3, 1, 2);
    /// let rendered = marker.render(|_| "name");
    /// assert_eq!(format!("{}", rendered), "name:1:2")
    /// ```
    pub fn render<D, F>(self, renderfn: F) -> RenderedMarker<D>
    where
        D: Display,
        F: FnOnce(usize) -> D,
    {
        RenderedMarker {
            source: renderfn(self.source),
            line: self.line,
            column: self.column,
        }
    }

    /// Set the source index for this marker
    ///
    /// ```
    /// # use marked_yaml::Marker;
    /// # let mut marker = Marker::new(0, 0, 0, 0);
    /// assert_ne!(marker.source(), 1);
    /// marker.set_source(1);
    /// assert_eq!(marker.source(), 1);
    /// ```
    pub fn set_source(&mut self, source: usize) {
        self.source = source;
    }

    /// Set the character index for this marker
    ///
    ///
    /// ```
    /// # use marked_yaml::Marker;
    /// # let mut marker = Marker::new(0, 0, 0, 0);
    /// assert_ne!(marker.character(), 1);
    /// marker.set_character(1);
    /// assert_eq!(marker.character(), 1);
    /// ```
    pub fn set_character(&mut self, character: usize) {
        self.character = character;
    }

    /// Set the line number for this marker
    ///
    /// ```
    /// # use marked_yaml::Marker;
    /// # let mut marker = Marker::new(0, 0, 0, 0);
    /// assert_ne!(marker.line(), 1);
    /// marker.set_line(1);
    /// assert_eq!(marker.line(), 1);
    /// ```
    pub fn set_line(&mut self, line: usize) {
        self.line = line;
    }

    /// Set the column number for this marker
    ///
    /// ```
    /// # use marked_yaml::Marker;
    /// # let mut marker = Marker::new(0, 0, 0, 0);
    /// assert_ne!(marker.column(), 1);
    /// marker.set_column(1);
    /// assert_eq!(marker.column(), 1);
    /// ```
    pub fn set_column(&mut self, column: usize) {
        self.column = column;
    }
}

impl Display for Marker {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// The span for a YAML marked node
///
/// ```
/// use marked_yaml::{parse_yaml, Marker, Span};
/// let node = parse_yaml(100, "{foo: bar}").unwrap();
/// let map = node.as_mapping().unwrap();
/// assert_eq!(map.span(), &Span::new_with_marks(Marker::new(100, 0, 1, 1), Marker::new(100, 9, 1, 10)));
/// ```
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
    /// let span = Span::new_start(Marker::new(0, 1, 1, 2));
    /// # assert_eq!(span.start().unwrap(), &Marker::new(0, 1, 1, 2));
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
    /// let span = Span::new_with_marks(Marker::new(0, 0, 1, 1), Marker::new(10, 1, 2, 1));
    /// # assert_eq!(span.start().unwrap(), &Marker::new(0, 0, 1, 1));
    /// # assert_eq!(span.end().unwrap(), &Marker::new(10, 1, 2, 1));
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
    /// # let span = Span::new_with_marks(Marker::new(0, 0, 1, 1), Marker::new(10, 1, 2, 1));
    /// assert_eq!(span.start(), Some(&Marker::new(0, 0, 1, 1)));
    /// ```
    pub fn start(&self) -> Option<&Marker> {
        self.start.as_ref()
    }

    /// The end of the span
    ///
    /// ```
    /// # use marked_yaml::{Marker, Span};
    /// # let span = Span::new_with_marks(Marker::new(0, 0, 1, 1), Marker::new(10, 1, 2, 1));
    /// assert_eq!(span.end(), Some(&Marker::new(10, 1, 2, 1)));
    /// ```
    pub fn end(&self) -> Option<&Marker> {
        self.end.as_ref()
    }

    /// The start of the span, mutably
    ///
    /// ```
    /// # use marked_yaml::{Marker, Span};
    /// # let mut span = Span::new_with_marks(Marker::new(0, 0, 1, 1), Marker::new(10, 1, 2, 1));
    /// span.start_mut().unwrap().set_line(5);
    /// assert_eq!(span.start(), Some(&Marker::new(0, 0, 5, 1)));
    /// ```
    pub fn start_mut(&mut self) -> Option<&mut Marker> {
        self.start.as_mut()
    }

    /// The end of the span, mutably
    ///
    /// ```
    /// # use marked_yaml::{Marker, Span};
    /// # let mut span = Span::new_with_marks(Marker::new(0, 0, 1, 1), Marker::new(10, 1, 2, 1));
    /// span.end_mut().unwrap().set_line(5);
    /// assert_eq!(span.end(), Some(&Marker::new(10, 1, 5, 1)));
    /// ```
    pub fn end_mut(&mut self) -> Option<&mut Marker> {
        self.end.as_mut()
    }

    /// Replace the start of the span
    ///
    /// ```
    /// # use marked_yaml::{Marker, Span};
    /// # let mut span = Span::new_blank();
    /// assert_eq!(span.start(), None);
    /// span.set_start(Some(Marker::new(0, 1, 1, 2)));
    /// assert_eq!(span.start(), Some(&Marker::new(0, 1, 1, 2)));
    /// ```
    pub fn set_start(&mut self, start: Option<Marker>) {
        self.start = start;
    }

    /// Replace the end of the span
    ///
    /// ```
    /// # use marked_yaml::{Marker, Span};
    /// # let mut span = Span::new_blank();
    /// assert_eq!(span.end(), None);
    /// span.set_end(Some(Marker::new(0, 1, 1, 2)));
    /// assert_eq!(span.end(), Some(&Marker::new(0, 1, 1, 2)));
    /// ```
    pub fn set_end(&mut self, end: Option<Marker>) {
        self.end = end;
    }
}

/// A marked YAML node
///
/// **NOTE**: Nodes are considered equal even if they don't come from the
/// same place.  *i.e. their spans are ignored for equality and hashing*
///
/// ```
/// use marked_yaml::parse_yaml;
/// let node = parse_yaml(100, "{foo: bar}").unwrap();
/// assert!(node.as_mapping().is_some());
/// ```
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
/// ```
/// use marked_yaml::{parse_yaml, Marker};
/// let node = parse_yaml(100, "{foo: bar}").unwrap();
/// let map = node.as_mapping().unwrap();
/// let bar = map.get("foo").unwrap();
/// // the "bar" string started on line 1, column 7 of source ID 100.
/// assert_eq!(bar.span().start(), Some(&Marker::new(100, 6, 1, 7)));
/// ```
#[derive(Clone, Debug)]
pub struct MarkedScalarNode {
    span: Span,
    value: String,
    may_coerce: bool,
}

pub(crate) type MappingHash = LinkedHashMap<MarkedScalarNode, Node>;

/// A marked YAML mapping node
///
/// Mapping nodes in YAML are defined as a key/value mapping where the keys are
/// unique and always scalars, whereas values may be YAML nodes of any kind.
///
/// Because *some* users of this crate may need to care about insertion order
/// we use [`hashlink::LinkedHashMap`] for this.
///
/// **NOTE**: Nodes are considered equal even if they don't come from the
/// same place.  *i.e. their spans are ignored for equality and hashing*
///
/// ```
/// use marked_yaml::{parse_yaml, Marker, Span};
/// let node = parse_yaml(100, "{foo: bar}").unwrap();
/// let map = node.as_mapping().unwrap();
/// assert_eq!(map.span(), &Span::new_with_marks(Marker::new(100, 0, 1, 1), Marker::new(100, 9, 1, 10)));
/// ```
#[derive(Clone, Debug)]
pub struct MarkedMappingNode {
    span: Span,
    value: MappingHash,
}

/// A marked YAML sequence node
///
/// Sequence nodes in YAML are simply ordered lists of YAML nodes.
///
/// **NOTE**: Nodes are considered equal even if they don't come from the
/// same place.  *i.e. their spans are ignored for equality and hashing*
///
/// ```
/// use marked_yaml::{parse_yaml, Marker, Span};
/// let node = parse_yaml(100, "{foo: [bar]}").unwrap();
/// let map = node.as_mapping().unwrap();
/// let seq = map.get("foo").unwrap();
/// let seq = seq.as_sequence().unwrap();
/// assert_eq!(seq.span(), &Span::new_with_marks(Marker::new(100, 6, 1, 7), Marker::new(100, 10, 1, 11)));
/// ```
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

            #[allow(clippy::partialeq_ne_impl)]
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

impl From<MappingHash> for Node {
    fn from(value: MappingHash) -> Node {
        Node::Mapping(value.into())
    }
}

macro_rules! node_span {
    ($t:path) => {
        impl $t {
            doc_comment!(
                concat!(
                    r#"Retrieve the Span from this node.

```
# use marked_yaml::types::*;
let node = "#,
                    stringify!($t),
                    r#"::new_empty(Span::new_blank());
assert_eq!(node.span(), &Span::new_blank());
```"#
                ),
                pub fn span(&self) -> &Span {
                    &self.span
                }
            );
            doc_comment!(
                concat!(
                    r#"Retrieve the Span from this node mutably.

```
# use marked_yaml::types::*;
let mut node = "#,
                    stringify!($t),
                    r#"::new_empty(Span::new_blank());
node.span_mut().set_start(Some(Marker::new(0, 0, 1, 0)));
assert_eq!(node.span().start(), Some(&Marker::new(0, 0, 1, 0)));
```"#
                ),
                pub fn span_mut(&mut self) -> &mut Span {
                    &mut self.span
                }
            );
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

    /// Retrieve the Span from the contained Node, mutably
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let mut node: Node = "foobar".into();
    /// let mut span = node.span_mut();
    /// assert_eq!(span.start(), None);
    /// span.set_start(Some(Marker::new(0, 0, 1, 0)));
    /// assert_eq!(span.start(), Some(&Marker::new(0, 0, 1, 0)));
    /// ```
    pub fn span_mut(&mut self) -> &mut Span {
        match self {
            Node::Scalar(msn) => msn.span_mut(),
            Node::Sequence(msn) => msn.span_mut(),
            Node::Mapping(mmn) => mmn.span_mut(),
        }
    }

    /// Retrieve the scalar from this node if there is one
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let node: Node = "foobar".into();
    /// let scalar = node.as_scalar();
    /// assert!(scalar.is_some());
    /// ```
    pub fn as_scalar(&self) -> Option<&MarkedScalarNode> {
        match self {
            Node::Scalar(msn) => Some(msn),
            _ => None,
        }
    }

    /// Retrieve the sequence from this node if there is one
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let node: Node = vec!["foobar"].into();
    /// let sequence = node.as_sequence();
    /// assert!(sequence.is_some());
    /// ```
    pub fn as_sequence(&self) -> Option<&MarkedSequenceNode> {
        match self {
            Node::Sequence(msn) => Some(msn),
            _ => None,
        }
    }

    /// Retrieve the mapping from this node if there is one
    ///
    /// ```
    /// # use marked_yaml::*;
    /// let node: Node = parse_yaml(0, "{foobar: baz}").unwrap();
    /// let mapping = node.as_mapping();
    /// assert!(mapping.is_some());
    /// ```
    pub fn as_mapping(&self) -> Option<&MarkedMappingNode> {
        match self {
            Node::Mapping(mmn) => Some(mmn),
            _ => None,
        }
    }

    /// Retrieve the scalar from this node if there is one, mutably
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let mut node: Node = "foobar".into();
    /// let mut scalar = node.as_scalar_mut();
    /// assert!(scalar.is_some());
    /// ```
    pub fn as_scalar_mut(&mut self) -> Option<&mut MarkedScalarNode> {
        match self {
            Node::Scalar(msn) => Some(msn),
            _ => None,
        }
    }

    /// Retrieve the sequence from this node if there is one, mutably
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let mut node: Node = vec!["foobar"].into();
    /// let mut sequence = node.as_sequence_mut();
    /// assert!(sequence.is_some());
    /// ```
    pub fn as_sequence_mut(&mut self) -> Option<&mut MarkedSequenceNode> {
        match self {
            Node::Sequence(msn) => Some(msn),
            _ => None,
        }
    }

    /// Retrieve the mapping from this node if there is one, mutably
    ///
    /// ```
    /// # use marked_yaml::*;
    /// let mut node: Node = parse_yaml(0, "{foobar: baz}").unwrap();
    /// let mut mapping = node.as_mapping_mut();
    /// assert!(mapping.is_some());
    /// ```
    pub fn as_mapping_mut(&mut self) -> Option<&mut MarkedMappingNode> {
        match self {
            Node::Mapping(mmn) => Some(mmn),
            _ => None,
        }
    }
}

impl MarkedScalarNode {
    /// Create a new scalar node with no value
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let node = MarkedScalarNode::new_empty(Span::new_blank());
    /// ```
    pub fn new_empty(span: Span) -> Self {
        Self {
            span,
            value: String::new(),
            may_coerce: true,
        }
    }

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
            may_coerce: true,
        }
    }

    /// Treat the scalar node as a string
    ///
    /// Since scalars are always stringish, this is always safe.
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let node: MarkedScalarNode = "foobar".into();
    /// assert_eq!(node.as_str(), "foobar");
    /// ```
    pub fn as_str(&self) -> &str {
        self.value.as_str()
    }

    /// Enable or disable permission to coerce values
    ///
    /// The various [`as_bool()`][Self::as_bool()] and other methods
    /// which can coerce a string can be forced to always deny
    /// coercion.
    ///
    #[cfg_attr(
        feature = "serde",
        doc = r#"
Note: this also applies for deserializing nodes via serde.

"#
    )]
    /// ```
    /// # use marked_yaml::types::*;
    /// let mut node: MarkedScalarNode = "true".into();
    /// assert_eq!(node.as_bool(), Some(true));
    /// node.set_coerce(false);
    /// assert_eq!(node.as_bool(), None);
    /// ```
    pub fn set_coerce(&mut self, may_coerce: bool) {
        self.may_coerce = may_coerce;
    }

    /// Retrieve whether or not this node is set to be coerceable
    ///
    /// The various [`as_bool()`][Self::as_bool()] and other methods
    /// which can coerce a string can be forced to deny coercion.
    ///
    #[cfg_attr(
        feature = "serde",
        doc = r#"
Note: this also applies for deserializing nodes via serde.

"#
    )]
    /// ```
    /// # use marked_yaml::types::*;
    /// let mut node: MarkedScalarNode = "true".into();
    /// assert_eq!(node.as_bool(), Some(true));
    /// assert_eq!(node.may_coerce(), true);
    /// node.set_coerce(false);
    /// assert_eq!(node.as_bool(), None);
    /// assert_eq!(node.may_coerce(), false);
    /// ```
    pub fn may_coerce(&self) -> bool {
        self.may_coerce
    }

    /// Treat the scalar node as a boolean
    ///
    /// If the scalar contains any of the following then it is true:
    ///
    /// * `true`
    /// * `True`
    /// * `TRUE`
    ///
    /// The following are considered false:
    ///
    /// * `false`
    /// * `False`
    /// * `FALSE`
    ///
    /// Everything else is not a boolean and so will return None
    ///
    /// Note: If you have done [`.set_coerce(false)`](MarkedScalarNode::set_coerce())
    /// then no matter the string's value, this will return `None`.
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let node: MarkedScalarNode = "true".into();
    /// assert_eq!(node.as_bool(), Some(true));
    /// let node: MarkedScalarNode = "FALSE".into();
    /// assert_eq!(node.as_bool(), Some(false));
    /// let node: MarkedScalarNode = "NO".into(); // YAML boolean, but not for us
    /// assert_eq!(node.as_bool(), None);
    /// let mut node: MarkedScalarNode = "true".into();
    /// node.set_coerce(false);
    /// assert_eq!(node.as_bool(), None);
    /// ```
    pub fn as_bool(&self) -> Option<bool> {
        if self.may_coerce {
            match self.value.as_str() {
                "true" | "True" | "TRUE" => Some(true),
                "false" | "False" | "FALSE" => Some(false),
                _ => None,
            }
        } else {
            None
        }
    }

    #[cfg(feature = "serde")]
    pub(crate) fn is_empty_scalar(&self) -> bool {
        self.value.is_empty() && self.may_coerce
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

macro_rules! scalar_from_to_number {
    ($t:ident, $as:ident) => {
        scalar_from_to_number!($t, $as, 0);
    };

    ($t:ident, $as:ident, $zero:expr) => {
        impl From<$t> for MarkedScalarNode {
            doc_comment!(
                concat!(
                    "Convert from ",
                    stringify!($t),
                    r#" into a node

```
# use marked_yaml::types::*;
let value: "#,
                    stringify!($t),
                    " = ",
                    stringify!($zero),
                    r#";
let node: MarkedScalarNode = value.into();
assert_eq!(&*node, "0");
```"#
                ),
                fn from(value: $t) -> Self {
                    format!("{}", value).into()
                }
            );
        }

        impl MarkedScalarNode {
            doc_comment!(
                concat!(
                    "Treat the scalar node as ",
                    stringify!($t),
                    r#".

If this scalar node's value can be represented properly as
a number of the right kind then return it.  This is essentially
a shortcut for using the `FromStr` trait on the return value of
`.as_str()`.

Note, this honours the setting of [`MarkedScalarNode::set_coerce()`]

```
# use marked_yaml::types::*;
let mut node: MarkedScalarNode = "0".into();
assert_eq!(node.as_"#,
                    stringify!($t),
                    r#"(), Some(0"#,
                    stringify!($t),
                    r#"));
node.set_coerce(false);
assert_eq!(node.as_"#,
                    stringify!($t),
                    r#"(), None);
```"#
                ),
                pub fn $as(&self) -> Option<$t> {
                    if self.may_coerce {
                        use std::str::FromStr;
                        $t::from_str(&self.value).ok()
                    } else {
                        None
                    }
                }
            );
        }
    };
}

scalar_from_to_number!(i8, as_i8);
scalar_from_to_number!(i16, as_i16);
scalar_from_to_number!(i32, as_i32);
scalar_from_to_number!(i64, as_i64);
scalar_from_to_number!(i128, as_i128);
scalar_from_to_number!(isize, as_isize);
scalar_from_to_number!(u8, as_u8);
scalar_from_to_number!(u16, as_u16);
scalar_from_to_number!(u32, as_u32);
scalar_from_to_number!(u64, as_u64);
scalar_from_to_number!(u128, as_u128);
scalar_from_to_number!(usize, as_usize);
scalar_from_to_number!(f32, as_f32, 0.0);
scalar_from_to_number!(f64, as_f64, 0.0);

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

impl Borrow<str> for MarkedScalarNode {
    fn borrow(&self) -> &str {
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

    /// Create a new sequence node from a vector of nodes
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let node = MarkedSequenceNode::new(Span::new_blank(), Vec::new());
    /// ```
    pub fn new(span: Span, value: Vec<Node>) -> Self {
        Self { span, value }
    }

    /// Get the node at the given index
    ///
    /// If the index is invalid then None will be returned
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let seq: MarkedSequenceNode = vec!["foobar"].into_iter().collect();
    /// assert_eq!(seq.get_node(0)
    ///     .and_then(Node::as_scalar)
    ///     .map(MarkedScalarNode::as_str)
    ///     .unwrap(),
    ///     "foobar");
    pub fn get_node(&self, index: usize) -> Option<&Node> {
        self.value.get(index)
    }

    /// Get the scalar at the given index
    ///
    /// If the index is invalid, or the node at that index is not a scalar
    /// node, then None will be returned.
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let seq: MarkedSequenceNode = vec!["foobar"].into_iter().collect();
    /// assert_eq!(seq.get_scalar(0)
    ///     .map(MarkedScalarNode::as_str)
    ///     .unwrap(),
    ///     "foobar");
    /// ```
    pub fn get_scalar(&self, index: usize) -> Option<&MarkedScalarNode> {
        self.get_node(index).and_then(Node::as_scalar)
    }

    /// Get the sequence at the given index
    ///
    /// If the index is invalid, or the node at that index is not a sequence
    /// node, then None will be returned.
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// let seq: MarkedSequenceNode = vec![vec!["foobar"]].into_iter().collect();
    /// assert_eq!(seq.get_sequence(0)
    ///     .and_then(|s| s.get_scalar(0))
    ///     .map(MarkedScalarNode::as_str)
    ///     .unwrap(),
    ///     "foobar");
    /// ```
    pub fn get_sequence(&self, index: usize) -> Option<&MarkedSequenceNode> {
        self.get_node(index).and_then(Node::as_sequence)
    }

    /// Get the mapping at the given index
    ///
    /// If the index is invalid, or the node at that index is not a mapping
    /// node, then None will be returned.
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// # use hashlink::LinkedHashMap;
    /// # let map: LinkedHashMap<MarkedScalarNode, Node> = LinkedHashMap::new();
    /// let seq: MarkedSequenceNode = vec![map].into_iter().collect();
    /// assert!(seq.get_mapping(0).is_some());
    /// ```
    pub fn get_mapping(&self, index: usize) -> Option<&MarkedMappingNode> {
        self.get_node(index).and_then(Node::as_mapping)
    }
}

impl Deref for MarkedSequenceNode {
    type Target = Vec<Node>;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl DerefMut for MarkedSequenceNode {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
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
            1 => *value[0].span(),
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
            1 => *value[0].span(),
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

    /// Create a new mapping node from the given hash table
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// # use hashlink::LinkedHashMap;
    /// let node = MarkedMappingNode::new(Span::new_blank(), LinkedHashMap::new());
    /// ```
    pub fn new(span: Span, value: MappingHash) -> Self {
        Self { span, value }
    }

    /// Get the node for the given string key
    ///
    /// If the index is not found then None is returned.
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// # use marked_yaml::parse_yaml;
    /// let node = parse_yaml(0, "{key: value}").unwrap();
    /// let map = node.as_mapping().unwrap();
    /// assert_eq!(map.get_node("key")
    ///     .and_then(Node::as_scalar)
    ///     .map(MarkedScalarNode::as_str)
    ///     .unwrap(),
    ///     "value");
    /// ```
    pub fn get_node(&self, index: &str) -> Option<&Node> {
        self.value.get(index)
    }

    /// Get the scalar for the given string key
    ///
    /// If the key is not found, or the node for that key is not a scalar
    /// node, then None will be returned.
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// # use marked_yaml::parse_yaml;
    /// let node = parse_yaml(0, "{key: value}").unwrap();
    /// let map = node.as_mapping().unwrap();
    /// assert_eq!(map.get_scalar("key")
    ///     .map(MarkedScalarNode::as_str)
    ///     .unwrap(),
    ///     "value");
    /// ```
    pub fn get_scalar(&self, index: &str) -> Option<&MarkedScalarNode> {
        self.get_node(index).and_then(Node::as_scalar)
    }

    /// Get the sequence at the given index
    ///
    /// If the key is not found, or the node for that key is not a sequence
    /// node, then None will be returned.
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// # use marked_yaml::parse_yaml;
    /// let node = parse_yaml(0, "{key: [value]}").unwrap();
    /// let map = node.as_mapping().unwrap();
    /// assert_eq!(map.get_sequence("key")
    ///     .and_then(|s| s.get_scalar(0))
    ///     .map(MarkedScalarNode::as_str)
    ///     .unwrap(),
    ///     "value");
    /// ```
    pub fn get_sequence(&self, index: &str) -> Option<&MarkedSequenceNode> {
        self.get_node(index).and_then(Node::as_sequence)
    }

    /// Get the mapping at the given index
    ///
    /// If the key is not found, or the node for that key is not a mapping
    /// node, then None will be returned.
    ///
    /// ```
    /// # use marked_yaml::types::*;
    /// # use marked_yaml::parse_yaml;
    /// let node = parse_yaml(0, "{key: {inner: value}}").unwrap();
    /// let map = node.as_mapping().unwrap();
    /// assert_eq!(map.get_mapping("key")
    ///     .and_then(|m| m.get_scalar("inner"))
    ///     .map(MarkedScalarNode::as_str)
    ///     .unwrap(),
    ///     "value");
    /// ```
    pub fn get_mapping(&self, index: &str) -> Option<&MarkedMappingNode> {
        self.get_node(index).and_then(Node::as_mapping)
    }
}

impl Deref for MarkedMappingNode {
    type Target = MappingHash;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl DerefMut for MarkedMappingNode {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl From<MappingHash> for MarkedMappingNode {
    fn from(value: MappingHash) -> Self {
        Self::new(Span::new_blank(), value)
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
        let value: MappingHash = iter
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
#[derive(Debug, PartialEq, Eq)]
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
            YamlNode::Boolean(b) => Ok(b.into()),
            YamlNode::Hash(_) => Err(YamlConversionError::NonScalar),
            YamlNode::Integer(i) => Ok(i.into()),
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

impl From<MarkedScalarNode> for YamlNode {
    fn from(value: MarkedScalarNode) -> Self {
        YamlNode::String(value.value)
    }
}

impl From<MarkedSequenceNode> for YamlNode {
    fn from(value: MarkedSequenceNode) -> Self {
        YamlNode::Array(value.value.into_iter().map(Into::into).collect())
    }
}

impl From<MarkedMappingNode> for YamlNode {
    fn from(value: MarkedMappingNode) -> Self {
        YamlNode::Hash(
            value
                .value
                .into_iter()
                .map(|(k, v)| (k.into(), v.into()))
                .collect(),
        )
    }
}

impl From<Node> for YamlNode {
    fn from(value: Node) -> Self {
        match value {
            Node::Scalar(msn) => msn.into(),
            Node::Sequence(msn) => msn.into(),
            Node::Mapping(mmn) => mmn.into(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::*;
    use super::*;

    #[test]
    fn basic_marker_checks() {
        let marker = Marker::new(0, 3, 1, 2);
        assert_eq!(marker.source(), 0);
        assert_eq!(marker.character(), 3);
        assert_eq!(marker.line(), 1);
        assert_eq!(marker.column(), 2);
        assert_eq!(format!("{}", marker), "1:2");
        let rendered = marker.render(|n| {
            assert_eq!(n, 0);
            "name"
        });
        assert_eq!(format!("{}", rendered), "name:1:2");
    }

    #[test]
    fn basic_span_checks() {
        let span = Span::new_blank();
        assert_eq!(span.start(), None);
        assert_eq!(span.end(), None);
        let mark = Marker::new(0, 1, 1, 2);
        let mark2 = Marker::new(3, 9, 4, 5);
        let span = Span::new_start(mark);
        assert_eq!(span.start(), Some(&mark));
        assert_eq!(span.end(), None);
        let span = Span::new_with_marks(mark, mark2);
        assert_eq!(span.start(), Some(&mark));
        assert_eq!(span.end(), Some(&mark2));
    }

    #[test]
    fn basic_explore_load_test() {
        let node = parse_yaml(0, include_str!("../examples/everything.yaml")).unwrap();
        let map = node.as_mapping().unwrap();
        assert_eq!(node.as_scalar(), None);
        assert_eq!(node.as_sequence(), None);
        assert_eq!(map.get_node("XXX NOT PRESENT XXX"), None);
        assert_eq!(map.get_scalar("mapping"), None);
        assert_eq!(map.get_sequence("mapping"), None);
        assert_eq!(map.get_mapping("simple"), None);
        // This actually uses .eq()
        assert_ne!(map.get_scalar("boolean1"), map.get_scalar("boolean2"));
        // Whereas this uses .ne()
        assert!(map.get_scalar("boolean1") != map.get_scalar("boolean2"));
        // Now check the spans
        assert_eq!(node.span(), map.span());
        let seq = map.get_sequence("heterogenous").unwrap();
        assert_eq!(seq.span().start(), Some(&Marker::new(0, 431, 24, 3)));
        assert_eq!(seq.span(), map.get_node("heterogenous").unwrap().span());
        // Helpers for the sequence node
        assert_eq!(seq.get_node(0), seq.first());
        assert_ne!(seq.get_node(0), None);
        assert_ne!(seq.get_scalar(0), None);
        assert_ne!(seq.get_mapping(1), None);
        assert_ne!(seq.get_sequence(2), None);
    }

    #[test]
    fn basic_scalar_features() {
        let scalar1 = MarkedScalarNode::new(Span::new_blank(), "");
        let scalar2 = MarkedScalarNode::new_empty(Span::new_blank());
        assert_eq!(scalar1, scalar2);
        assert_eq!(scalar1.as_str(), "");
        assert_eq!(scalar1.as_bool(), None);
        assert_eq!(scalar1.as_usize(), None);
        let truth: MarkedScalarNode = "true".into();
        assert_eq!(truth.as_bool(), Some(true));
        let falsehood: MarkedScalarNode = "false".to_string().into();
        assert_eq!(falsehood.as_bool(), Some(false));
        assert_eq!(truth, true.into());
        assert_eq!(falsehood, false.into());
        let zero: MarkedScalarNode = "0".into();
        assert_eq!(zero.as_usize(), Some(0));
        assert_eq!(zero, 0usize.into());
        assert_eq!(&*zero, "0");
    }

    #[test]
    fn basic_sequence_features() {
        // For features not covered by other tests
        let mut seq = MarkedSequenceNode::new_empty(Span::new_blank());
        let seq2: MarkedSequenceNode = vec!["foo"].into_iter().collect();
        let scalar: MarkedScalarNode = "foo".into();
        seq.push(Node::from(scalar));
        assert_eq!(seq, seq2);
        assert_eq!(seq, vec!["foo"].into());
        let seq3: MarkedSequenceNode = vec!["foo", "bar"].into_iter().collect();
        seq.push(Node::from("bar"));
        assert_eq!(seq, seq3);
        assert_eq!(seq, vec!["foo", "bar"].into());
    }

    #[test]
    fn basic_mapping_features() {
        // For features not covered by other tests
        let mut map = MarkedMappingNode::new_empty(Span::new_blank());
        let mut hash = MappingHash::new();
        hash.insert("foo".into(), "bar".into());
        let map2 = MarkedMappingNode::from(hash);
        map.insert("foo".into(), "bar".into());
        assert_eq!(map, map2);
        assert_eq!(map.get("foo").unwrap().as_scalar().unwrap().as_str(), "bar");
        let map3: MarkedMappingNode = vec![("foo", "bar")].into_iter().collect();
        assert_eq!(map, map3);
        map.insert("baz".into(), "meta".into());
        let map4: MarkedMappingNode = vec![("foo", "bar"), ("baz", "meta")].into_iter().collect();
        assert_eq!(map, map4);
    }

    #[test]
    fn extra_node_impls() {
        let node = Node::from(vec!["foo"]);
        assert_ne!(node.as_sequence(), None);
        let node = Node::from(MappingHash::new());
        assert!(node.as_mapping().unwrap().is_empty());
    }

    #[test]
    fn yaml_conversions() {
        use yaml_rust::YamlLoader;
        let mut everything =
            YamlLoader::load_from_str(include_str!("../examples/everything.yaml")).unwrap();
        let everything = everything.pop().unwrap();
        let node = Node::try_from(everything.clone()).unwrap();
        assert!(node.as_mapping().is_some());
        let badscalar = MarkedScalarNode::try_from(everything);
        assert_eq!(badscalar, Err(YamlConversionError::NonScalar));
        let badscalar = MarkedScalarNode::try_from(YamlNode::BadValue);
        assert_eq!(badscalar, Err(YamlConversionError::BadValue));
        let badscalar = MarkedScalarNode::try_from(YamlNode::Array(vec![]));
        assert_eq!(badscalar, Err(YamlConversionError::NonScalar));
    }

    fn flatten(node: YamlNode) -> YamlNode {
        match node {
            YamlNode::Array(arr) => YamlNode::Array(arr.into_iter().map(flatten).collect()),
            YamlNode::Boolean(b) => {
                YamlNode::String((if b { "true" } else { "false" }).to_string())
            }
            YamlNode::Hash(h) => YamlNode::Hash(
                h.into_iter()
                    .map(|(k, v)| (flatten(k), flatten(v)))
                    .collect(),
            ),
            YamlNode::Integer(i) => YamlNode::String(format!("{}", i)),
            YamlNode::Null => YamlNode::String("null".to_string()),
            YamlNode::Real(r) => YamlNode::String(r),
            other => other,
        }
    }

    #[test]
    fn back_yaml_conversion() {
        use yaml_rust::YamlLoader;
        let mut everything =
            YamlLoader::load_from_str(include_str!("../examples/everything.yaml")).unwrap();
        let everything = everything.pop().unwrap();
        let node = Node::try_from(everything.clone()).unwrap();
        let other: YamlNode = node.into();
        let flat = flatten(everything);
        assert_eq!(flat, other);
    }
}
