//! Serde support for marked data deserialisation

use std::{fmt, hash::Hash, iter::Peekable, marker::PhantomData, ops::Deref};

use serde::{
    de::{value::BorrowedStrDeserializer, IntoDeserializer, MapAccess, SeqAccess, Visitor},
    forward_to_deserialize_any, Deserialize, Deserializer, Serialize,
};

use crate::{
    types::{MarkedMappingNode, MarkedScalarNode, MarkedSequenceNode},
    Marker, Node, Span,
};

/// Wrapper which can be used when deserialising data from [`Node`]
///
/// You must use a compatible deserializer if you want to deserialize these values,
/// however when serializing you will lose the span information so do not expect
/// to round-trip these values.
#[derive(Debug)]
pub struct Spanned<T> {
    span: Span,
    inner: T,
}

impl<T> Spanned<T> {
    /// Wrap an instance of something with the given span
    pub fn new(span: Span, inner: T) -> Self {
        Self { span, inner }
    }

    /// The span associated with this value
    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> PartialEq for Spanned<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl<T> Eq for Spanned<T> where T: Eq {}

impl<T> Hash for Spanned<T>
where
    T: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.hash(state);
    }
}

// -------------------------------------------------------------------------------

// Convention for these markers comes from the toml crates

const SPANNED_TYPE: &str = "$___::marked_data::serde::Spanned<T>";
const SPANNED_SPAN_START_SOURCE: &str = "$___::marked_data::serde::Spanned<T>::span_start_source";
const SPANNED_SPAN_START_LINE: &str = "$___::marked_data::serde::Spanned<T>::span_start_line";
const SPANNED_SPAN_START_COLUMN: &str = "$___::marked_data::serde::Spanned<T>::span_start_column";
const SPANNED_SPAN_END_SOURCE: &str = "$___::marked_data::serde::Spanned<T>::span_end_source";
const SPANNED_SPAN_END_LINE: &str = "$___::marked_data::serde::Spanned<T>::span_end_line";
const SPANNED_SPAN_END_COLUMN: &str = "$___::marked_data::serde::Spanned<T>::span_end_column";
const SPANNED_INNER: &str = "$___::marked_data::serde::Spanned<T>::inner";

const SPANNED_FIELDS: [&str; 7] = [
    SPANNED_SPAN_START_SOURCE,
    SPANNED_SPAN_START_LINE,
    SPANNED_SPAN_START_COLUMN,
    SPANNED_SPAN_END_SOURCE,
    SPANNED_SPAN_END_LINE,
    SPANNED_SPAN_END_COLUMN,
    SPANNED_INNER,
];

impl<'de, T> Deserialize<'de> for Spanned<T>
where
    T: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct MarkedNodeVisitor<T>(PhantomData<T>);

        impl<'de, T> Visitor<'de> for MarkedNodeVisitor<T>
        where
            T: Deserialize<'de>,
        {
            type Value = Spanned<T>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a MarkedNode of some kind")
            }

            fn visit_map<V>(self, mut visitor: V) -> Result<Self::Value, V::Error>
            where
                V: MapAccess<'de>,
            {
                let mut key: Option<&str> = visitor.next_key()?;

                let span_start = if key == Some(SPANNED_SPAN_START_SOURCE) {
                    let source: usize = visitor.next_value()?;
                    if visitor.next_key()? != Some(SPANNED_SPAN_START_LINE) {
                        return Err(serde::de::Error::custom(
                            "marked node span start line missing",
                        ));
                    }
                    let line: usize = visitor.next_value()?;
                    if visitor.next_key()? != Some(SPANNED_SPAN_START_COLUMN) {
                        return Err(serde::de::Error::custom(
                            "marked node span start column missing",
                        ));
                    }
                    let column: usize = visitor.next_value()?;
                    key = visitor.next_key()?;
                    Some(Marker::new(source, line, column))
                } else {
                    None
                };

                let span_end = if key == Some(SPANNED_SPAN_END_SOURCE) {
                    let source: usize = visitor.next_value()?;
                    if visitor.next_key()? != Some(SPANNED_SPAN_END_LINE) {
                        return Err(serde::de::Error::custom(
                            "marked node span end line missing",
                        ));
                    }
                    let line: usize = visitor.next_value()?;
                    if visitor.next_key()? != Some(SPANNED_SPAN_END_COLUMN) {
                        return Err(serde::de::Error::custom(
                            "marked node span end column missing",
                        ));
                    }
                    let column: usize = visitor.next_value()?;
                    key = visitor.next_key()?;
                    Some(Marker::new(source, line, column))
                } else {
                    None
                };

                if key != Some(SPANNED_INNER) {
                    return Err(serde::de::Error::custom(
                        "marked node inner value not found",
                    ));
                }
                let inner: T = visitor.next_value()?;

                let mut span = Span::new_blank();
                span.set_start(span_start);
                span.set_end(span_end);

                Ok(Spanned::new(span, inner))
            }
        }
        let visitor = MarkedNodeVisitor(PhantomData);

        deserializer.deserialize_struct(SPANNED_TYPE, &SPANNED_FIELDS, visitor)
    }
}

impl<T> Serialize for Spanned<T>
where
    T: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.inner.serialize(serializer)
    }
}

// -------------------------------------------------------------------------------

/// Errors which can come from deserialisation
#[non_exhaustive]
#[derive(Debug)]
pub enum Error {
    /// The value was not a valid boolean
    NotBoolean(Span),
    /// Some other error occurred
    Other(Box<dyn std::error::Error>),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::NotBoolean(_s) => f.write_str("Value was not a boolean"),
            Error::Other(e) => e.fmt(f),
        }
    }
}

impl std::error::Error for Error {}

impl serde::de::Error for Error {
    fn custom<T>(msg: T) -> Self
    where
        T: fmt::Display,
    {
        Error::Other(msg.to_string().into())
    }
}

// -------------------------------------------------------------------------------

impl<'de> IntoDeserializer<'de, Error> for &'de Node {
    type Deserializer = NodeDeserializer<'de>;

    fn into_deserializer(self) -> Self::Deserializer {
        NodeDeserializer { node: self }
    }
}

/// Deserializer for nodes
pub struct NodeDeserializer<'node> {
    node: &'node Node,
}

impl<'node> NodeDeserializer<'node> {
    /// Create a new deserializer over a borrowed node
    pub fn new(node: &'node Node) -> Self {
        Self { node }
    }
}

/// Deserialize some [`Node`] into the requisite type
///
/// This permits deserialisation of [`Node`]s into any structure
/// which [`serde`] can deserialize.  In addition, if any part of
/// the type tree is [`Spanned`] then the spans are provided
/// from the requisite marked node.
///
/// ```
/// # use serde::Deserialize;
/// # use marked_yaml::Spanned;
/// const YAML: &str = "hello: world\n";
/// let node = marked_yaml::parse_yaml(0, YAML).unwrap();
/// #[derive(Deserialize)]
/// struct Greeting {
///     hello: Spanned<String>,
/// }
/// let greets: Greeting = marked_yaml::from_node(&node).unwrap();
/// let start = greets.hello.span().start().unwrap();
/// assert_eq!(start.line(), 1);
/// assert_eq!(start.column(), 8);
/// ```
pub fn from_node<'de, T>(node: &'de Node) -> Result<T, Error>
where
    T: Deserialize<'de>,
{
    T::deserialize(NodeDeserializer::new(node))
}

impl<'de> Deserializer<'de> for NodeDeserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.node {
            Node::Scalar(s) => s.into_deserializer().deserialize_any(visitor),
            Node::Mapping(m) => m.into_deserializer().deserialize_any(visitor),
            Node::Sequence(s) => s.into_deserializer().deserialize_any(visitor),
        }
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.node {
            Node::Scalar(s) => s
                .into_deserializer()
                .deserialize_struct(name, fields, visitor),
            Node::Mapping(m) => m
                .into_deserializer()
                .deserialize_struct(name, fields, visitor),
            Node::Sequence(s) => s
                .into_deserializer()
                .deserialize_struct(name, fields, visitor),
        }
    }

    forward_to_deserialize_any! [
        bool i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 char str string bytes byte_buf
        option unit unit_struct newtype_struct seq tuple tuple_struct map
        enum identifier ignored_any
    ];
}

// -------------------------------------------------------------------------------

trait MarkedValue {
    fn mark_span(&self) -> &Span;
}

impl MarkedValue for MarkedScalarNode {
    fn mark_span(&self) -> &Span {
        self.span()
    }
}

impl MarkedValue for MarkedMappingNode {
    fn mark_span(&self) -> &Span {
        self.span()
    }
}

impl MarkedValue for MarkedSequenceNode {
    fn mark_span(&self) -> &Span {
        self.span()
    }
}

impl MarkedValue for Node {
    fn mark_span(&self) -> &Span {
        self.span()
    }
}

// -------------------------------------------------------------------------------

struct SpannedDeserializer<'de, T> {
    node: &'de T,
    state: SpannedDeserializerState,
}

enum SpannedDeserializerState {
    SendStartSource,
    SendStartLine,
    SendStartColumn,
    SendEndSource,
    SendEndLine,
    SendEndColumn,
    SendValue,
    Done,
}

impl<'de, T> SpannedDeserializer<'de, T>
where
    T: MarkedValue,
{
    fn new(node: &'de T) -> Self {
        let state = if node.mark_span().start().is_some() {
            SpannedDeserializerState::SendStartSource
        } else if node.mark_span().end().is_some() {
            SpannedDeserializerState::SendEndSource
        } else {
            SpannedDeserializerState::SendValue
        };
        Self { node, state }
    }
}

impl<'de, T> MapAccess<'de> for SpannedDeserializer<'de, T>
where
    T: MarkedValue,
    &'de T: IntoDeserializer<'de, Error>,
{
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: serde::de::DeserializeSeed<'de>,
    {
        let key = match self.state {
            SpannedDeserializerState::SendStartSource => SPANNED_SPAN_START_SOURCE,
            SpannedDeserializerState::SendStartLine => SPANNED_SPAN_START_LINE,
            SpannedDeserializerState::SendStartColumn => SPANNED_SPAN_START_COLUMN,
            SpannedDeserializerState::SendEndSource => SPANNED_SPAN_END_SOURCE,
            SpannedDeserializerState::SendEndLine => SPANNED_SPAN_END_LINE,
            SpannedDeserializerState::SendEndColumn => SPANNED_SPAN_END_COLUMN,
            SpannedDeserializerState::SendValue => SPANNED_INNER,
            SpannedDeserializerState::Done => return Ok(None),
        };
        seed.deserialize(BorrowedStrDeserializer::new(key))
            .map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::DeserializeSeed<'de>,
    {
        match self.state {
            SpannedDeserializerState::SendStartSource => {
                let v = self
                    .node
                    .mark_span()
                    .start()
                    .expect("Span missing start")
                    .source();
                self.state = SpannedDeserializerState::SendStartLine;
                seed.deserialize(v.into_deserializer())
            }
            SpannedDeserializerState::SendStartLine => {
                let v = self
                    .node
                    .mark_span()
                    .start()
                    .expect("Span missing start")
                    .line();
                self.state = SpannedDeserializerState::SendStartColumn;
                seed.deserialize(v.into_deserializer())
            }
            SpannedDeserializerState::SendStartColumn => {
                let v = self
                    .node
                    .mark_span()
                    .start()
                    .expect("Span missing start")
                    .column();
                self.state = if self.node.mark_span().end().is_some() {
                    SpannedDeserializerState::SendEndSource
                } else {
                    SpannedDeserializerState::SendValue
                };
                seed.deserialize(v.into_deserializer())
            }
            SpannedDeserializerState::SendEndSource => {
                let v = self
                    .node
                    .mark_span()
                    .end()
                    .expect("Span missing end")
                    .source();
                self.state = SpannedDeserializerState::SendEndLine;
                seed.deserialize(v.into_deserializer())
            }
            SpannedDeserializerState::SendEndLine => {
                let v = self
                    .node
                    .mark_span()
                    .end()
                    .expect("Span missing end")
                    .line();
                self.state = SpannedDeserializerState::SendEndColumn;
                seed.deserialize(v.into_deserializer())
            }
            SpannedDeserializerState::SendEndColumn => {
                let v = self
                    .node
                    .mark_span()
                    .end()
                    .expect("Span missing end")
                    .column();
                self.state = SpannedDeserializerState::SendValue;
                seed.deserialize(v.into_deserializer())
            }
            SpannedDeserializerState::SendValue => {
                self.state = SpannedDeserializerState::Done;
                seed.deserialize(self.node.into_deserializer())
            }
            SpannedDeserializerState::Done => panic!("next_value_seed called before next_key_seed"),
        }
    }
}

// -------------------------------------------------------------------------------

impl<'de> IntoDeserializer<'de, Error> for &'de MarkedScalarNode {
    type Deserializer = MarkedScalarNodeDeserializer<'de>;
    fn into_deserializer(self) -> MarkedScalarNodeDeserializer<'de> {
        MarkedScalarNodeDeserializer { node: self }
    }
}

/// Deserializer for scalar nodes
pub struct MarkedScalarNodeDeserializer<'node> {
    node: &'node MarkedScalarNode,
}

impl<'de> Deserializer<'de> for MarkedScalarNodeDeserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.node
            .deref()
            .into_deserializer()
            .deserialize_any(visitor)
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_bool(
            self.node
                .as_bool()
                .ok_or(Error::NotBoolean(*self.node.span()))?,
        )
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if name == SPANNED_TYPE && fields == SPANNED_FIELDS {
            return visitor.visit_map(SpannedDeserializer::new(self.node));
        }

        self.deserialize_any(visitor)
    }

    forward_to_deserialize_any! [
        i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 char str string bytes byte_buf
        option unit unit_struct newtype_struct seq tuple tuple_struct map
        enum identifier ignored_any
    ];
}

// -------------------------------------------------------------------------------

type MappingValueSeq<'de> = linked_hash_map::Iter<'de, MarkedScalarNode, Node>;
struct MappingAccess<'de> {
    items: Peekable<MappingValueSeq<'de>>,
}

impl<'de> MappingAccess<'de> {
    fn new(items: MappingValueSeq<'de>) -> Self {
        Self {
            items: items.peekable(),
        }
    }
}

impl<'de> MapAccess<'de> for MappingAccess<'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: serde::de::DeserializeSeed<'de>,
    {
        if let Some(next_key) = self.items.peek().map(|(k, _v)| k) {
            seed.deserialize(next_key.into_deserializer()).map(Some)
        } else {
            Ok(None)
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::DeserializeSeed<'de>,
    {
        seed.deserialize(
            self.items
                .next()
                .expect("next_value_seed called before next_key_seed")
                .1
                .into_deserializer(),
        )
    }
}

// -------------------------------------------------------------------------------

impl<'de> IntoDeserializer<'de, Error> for &'de MarkedMappingNode {
    type Deserializer = MarkedMappingNodeDeserializer<'de>;

    fn into_deserializer(self) -> Self::Deserializer {
        MarkedMappingNodeDeserializer { node: self }
    }
}

/// Deserializer for mapping nodes
pub struct MarkedMappingNodeDeserializer<'de> {
    node: &'de MarkedMappingNode,
}

impl<'de> Deserializer<'de> for MarkedMappingNodeDeserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_map(MappingAccess::new(self.node.iter()))
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if name == SPANNED_TYPE && fields == SPANNED_FIELDS {
            return visitor.visit_map(SpannedDeserializer::new(self.node));
        }

        self.deserialize_any(visitor)
    }

    forward_to_deserialize_any! [
        bool i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 char str string bytes byte_buf
        option unit unit_struct newtype_struct seq tuple tuple_struct
        map enum identifier ignored_any
    ];
}

// -------------------------------------------------------------------------------

struct SequenceAccess<'de> {
    items: &'de [Node],
    pos: usize,
}

impl<'de> SequenceAccess<'de> {
    fn new(items: &'de [Node]) -> Self {
        Self { items, pos: 0 }
    }
}

impl<'de> SeqAccess<'de> for SequenceAccess<'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        if self.pos == self.items.len() {
            return Ok(None);
        }
        let pos = self.pos;
        self.pos += 1;

        seed.deserialize(self.items[pos].into_deserializer())
            .map(Some)
    }
}

// -------------------------------------------------------------------------------

impl<'de> IntoDeserializer<'de, Error> for &'de MarkedSequenceNode {
    type Deserializer = MarkedSequenceNodeDeserializer<'de>;

    fn into_deserializer(self) -> Self::Deserializer {
        MarkedSequenceNodeDeserializer { node: self }
    }
}

/// Deserializer for sequence nodes
pub struct MarkedSequenceNodeDeserializer<'de> {
    node: &'de MarkedSequenceNode,
}

impl<'de> Deserializer<'de> for MarkedSequenceNodeDeserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_seq(SequenceAccess::new(self.node.as_slice()))
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if name == SPANNED_TYPE && fields == SPANNED_FIELDS {
            return visitor.visit_map(SpannedDeserializer::new(self.node));
        }

        self.deserialize_any(visitor)
    }

    forward_to_deserialize_any! [
        bool i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 char str string bytes byte_buf
        option unit unit_struct newtype_struct seq tuple tuple_struct
        map enum identifier ignored_any
    ];
}

// -------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use super::*;

    const TEST_DOC: &str = r#"hello: world
some: [ value, or, other ]
says: { grow: nothing, or: die }
"#;

    #[test]
    #[allow(dead_code)]
    fn basic_deserialize() {
        #[derive(Deserialize, Debug)]
        struct TestDoc {
            hello: String,
            some: Vec<String>,
            says: HashMap<String, String>,
        }
        let node = crate::parse_yaml(0, TEST_DOC).unwrap();
        let doc: TestDoc = from_node(&node).unwrap();
        println!("{doc:#?}");
    }

    #[test]
    #[allow(dead_code)]
    fn basic_deserialize_spanned_scalars() {
        #[derive(Deserialize, Debug)]
        struct TestDoc {
            hello: Spanned<String>,
            some: Vec<Spanned<String>>,
            says: HashMap<Spanned<String>, Spanned<String>>,
        }
        let node = crate::parse_yaml(0, TEST_DOC).unwrap();
        let doc: TestDoc = from_node(&node).unwrap();
        println!("{doc:#?}");
    }

    #[test]
    #[allow(dead_code)]
    fn basic_deserialize_spanned_everything() {
        #[derive(Deserialize, Debug)]
        struct TestDoc {
            hello: Spanned<String>,
            some: Spanned<Vec<Spanned<String>>>,
            says: Spanned<HashMap<Spanned<String>, Spanned<String>>>,
        }
        let node = crate::parse_yaml(0, TEST_DOC).unwrap();
        let doc: Spanned<TestDoc> = from_node(&node).unwrap();
        println!("{doc:#?}");
    }
}