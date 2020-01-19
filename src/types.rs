use yaml_rust::scanner::Marker as YamlMarker;

/// A marker for a YAML node
///
/// This indicates where a node started or ended.
/// TODO: example
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Marker {
    index: usize,
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
    /// # assert_eq!(marker.index(), 0);
    /// # assert_eq!(marker.line(), 1);
    /// # assert_eq!(marker.column(), 2);
    /// ```
    pub fn new(index: usize, line: usize, column: usize) -> Self {
        Self {
            index,
            line,
            column,
        }
    }

    /// The index into the input string at which this point exists, 0-indexed
    ///
    /// When parsing YAML, we record where nodes start (and often finish).
    /// This is the byte-index into the input string where this marker resides.
    ///
    /// This is likely most useful to computers, not humans.
    ///
    /// ```
    /// # use marked_yaml::Marker;
    /// # let marker = Marker::new(0, 1, 2);
    /// assert_eq!(marker.index(), 0);
    /// ```
    pub fn index(&self) -> usize {
        self.index
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

/// Permit conversion from the `yaml_rust::scanner::Marker` type.
///
/// To ensure that documentation is consistent in this crate, we trivially
/// convert the `yaml_rust` crate's `Marker` type to our own
impl std::convert::From<YamlMarker> for Marker {
    fn from(value: YamlMarker) -> Self {
        Self {
            index: value.index(),
            line: value.line(),
            column: value.col(),
        }
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
