//! Basic marked data types
//! =======================
//!
//! The types in this module are the basics of marked data.  They consist of the
//! data sources and data marks.
//!
//! Typically you will import these via the prelude, but if you are implementing
//! specific data types, you may want to be more precise

use std::borrow::Cow;
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use either::*;
use fehler::throws;
use lazy_static::lazy_static;

/// A kind of source of data
///
/// When loading data, you need to load it from somewhere.  The source kind
/// represents where that data comes from and must be used to instantiate a
/// data source.
///
/// ```rust
/// # use marked_data::basics::*;
/// let datasource = DataSource::new(DataSourceKind::Env, DataSourceOrigin::OsStr("RUST_LOG".into()));
/// ```
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DataSourceKind {
    /// The data was built into the application
    Intrinsic,
    /// The data was passed to the program on the CLI
    CommandLine,
    /// The data was sourced from the environment, from the given variable name
    Env,
    /// The data was sourced from the filesystem, from the given file
    File,
}

/// Data source origin
///
/// When loading data, various of the kinds of data sources have an origin which
/// takes the form of a string of some kind.  Where the data's origin is known
/// we can store it and thus be able to retrieve it later
///
/// ```rust
/// # use marked_data::basics::*;
/// let datasource = DataSource::new(DataSourceKind::File, DataSourceOrigin::Str("myprog.conf".into()));
/// ```
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum DataSourceOrigin {
    /// The data source origin is unknown
    Unknown,
    /// The data origin can be represented as a string
    Str(String),
    /// The data origin can be represented as a filepath
    File(PathBuf),
}

/// A source of data which will come pre-marked for convenience
///
/// Data sources, once created, will remain in memory, in their
/// entirety, until all references to them are lost.  Any data
/// marks refer to the data source they came from, so if you think
/// your program is leaking memory, it may simply still have marks
/// to data from large sources.
///
/// If you create a data source which contains a string then the
/// data source will derive the line number indices immediately
/// upon creation so that any data marks can be expressed in terms
/// of lines and characters if so desired.
#[derive(Debug, Clone)]
pub struct DataSource(Arc<DataSourceInner>);

lazy_static! {
    /// Anonymous data source.  This is used when marked data is created ex-nihilo
    /// from unmarked data.
    pub static ref ANON_DATASOURCE: DataSource = DataSource(Arc::new(DataSourceInner {
        kind: DataSourceKind::Intrinsic,
        origin: DataSourceOrigin::Unknown,
        data: Left(Cow::Borrowed(&[])),
        lines: Vec::new(),
    }));
}

impl PartialEq for DataSource {
    fn eq(&self, other: &DataSource) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for DataSource {}

impl DataSource {
    /// Create a new intrinsic data source, intrinsic data comes from
    /// the binary (or any other static data source) and can be named.
    pub fn new_intrinsic_data<D>(origin: DataSourceOrigin, data: D) -> Self
    where
        D: Into<Cow<'static, [u8]>>,
    {
        Self(Arc::new(DataSourceInner::create_data(
            DataSourceKind::Intrinsic,
            origin,
            data.into(),
        )))
    }

    /// Create a new intrinsic string source, intrinsic strings come from
    /// the binary (or any other static data source) and can be named.
    pub fn new_intrinsic_string<D>(origin: DataSourceOrigin, data: D) -> Self
    where
        D: Into<Cow<'static, str>>,
    {
        Self(Arc::new(DataSourceInner::create_string(
            DataSourceKind::Intrinsic,
            origin,
            data.into(),
        )))
    }

    /// Create a new commandline data source.
    ///
    /// Data sourced from the command line has no origin
    pub fn new_commandline_data<D>(data: D) -> Self
    where
        D: Into<Cow<'static, [u8]>>,
    {
        Self(Arc::new(DataSourceInner::create_data(
            DataSourceKind::CommandLine,
            DataSourceOrigin::Unknown,
            data.into(),
        )))
    }

    /// Create a new commandline string source.
    ///
    /// Strings sourced from the command line have no origin
    pub fn new_commandline_string<D>(data: D) -> Self
    where
        D: Into<Cow<'static, str>>,
    {
        Self(Arc::new(DataSourceInner::create_string(
            DataSourceKind::CommandLine,
            DataSourceOrigin::Unknown,
            data.into(),
        )))
    }

    /// Create a new environment variable sourced string
    ///
    /// This will retrieve the environment variable when called.
    ///
    /// If the environment variable's content is not unicode then
    /// it will fail to create.
    #[throws(std::env::VarError)]
    pub fn new_from_env(var: String) -> Self {
        let data = std::env::var(&var)?;
        Self(Arc::new(DataSourceInner::create_string(
            DataSourceKind::Env,
            DataSourceOrigin::Str(var),
            data.into(),
        )))
    }

    /// Create a file based datasource with data in
    ///
    /// This will load the entirety of the file into memory when called.
    ///
    /// Any IO errors are returned to you
    #[throws(std::io::Error)]
    pub fn new_file_data<P>(path: P) -> Self
    where
        P: AsRef<Path>,
    {
        let path = path.as_ref().to_path_buf();
        let data = std::fs::read(&path)?;
        Self(Arc::new(DataSourceInner::create_data(
            DataSourceKind::File,
            DataSourceOrigin::File(path),
            data.into(),
        )))
    }

    /// Create a file based datasource with a string in
    ///
    /// This will load the entirety of the file into memory when called.
    ///
    /// Any IO errors are returned to you
    #[throws(std::io::Error)]
    pub fn new_file_string<P>(path: P) -> Self
    where
        P: AsRef<Path>,
    {
        let path = path.as_ref().to_path_buf();
        let data = std::fs::read_to_string(&path)?;
        Self(Arc::new(DataSourceInner::create_string(
            DataSourceKind::File,
            DataSourceOrigin::File(path),
            data.into(),
        )))
    }

    /// Retrieve the kind of the data source
    pub fn kind(&self) -> DataSourceKind {
        self.0.kind
    }

    /// Retrieve a clone of the data source origin
    pub fn origin(&self) -> &DataSourceOrigin {
        &self.0.origin
    }

    /// Retrieve the data of the data source
    ///
    /// This will always work since strings can be represented as byte slices.
    pub fn data(&self) -> &[u8] {
        self.0.data()
    }

    /// Retrieve the string of the data source
    ///
    /// If this was constructed with data then we attempt to represent it
    /// as a string, this may fail
    #[throws(std::str::Utf8Error)]
    pub fn string(&self) -> &str {
        self.0.string()?
    }

    /// Retrieve a mark representing the whole of the data source
    pub fn mark(&self) -> DataMark {
        let data = self.data();
        DataMark::new_from_offsets(self, 0, data.len() - 1)
    }
}

#[derive(Debug)]
struct DataSourceInner {
    kind: DataSourceKind,
    origin: DataSourceOrigin,
    data: Either<Cow<'static, [u8]>, Cow<'static, str>>,
    lines: Vec<(usize, usize)>,
}

impl DataSourceInner {
    fn create_data(
        kind: DataSourceKind,
        origin: DataSourceOrigin,
        data: Cow<'static, [u8]>,
    ) -> Self {
        Self {
            kind,
            origin,
            data: Left(data),
            lines: Vec::new(),
        }
    }

    fn create_string(
        kind: DataSourceKind,
        origin: DataSourceOrigin,
        data: Cow<'static, str>,
    ) -> Self {
        let mut lines = Vec::new();
        let mut last_was_newline = true;
        for (charcount, (pos, ch)) in data.char_indices().enumerate() {
            if last_was_newline {
                lines.push((pos, charcount));
                last_was_newline = false;
            }
            if ch == '\n' {
                last_was_newline = true;
            }
        }
        if lines.is_empty() {
            lines.push((0, 0));
        }
        Self {
            kind,
            origin,
            data: Right(data),
            lines,
        }
    }

    fn data(&self) -> &[u8] {
        match &self.data {
            Left(d) => &d,
            Right(s) => s.as_bytes(),
        }
    }

    #[throws(std::str::Utf8Error)]
    fn string(&self) -> &str {
        match &self.data {
            Left(d) => std::str::from_utf8(d)?,
            Right(s) => s,
        }
    }

    fn offset_to_lineidx(&self, offset: usize) -> usize {
        use std::cmp::Ordering;
        let mut earliest = 0;
        let mut latest = self.lines.len();
        loop {
            if earliest == latest {
                break earliest;
            }
            let mid = (earliest + latest) >> 1;
            let midpos = self.lines[mid].0;
            match midpos.cmp(&offset) {
                Ordering::Equal => break midpos,
                Ordering::Less => earliest = midpos + 1,
                Ordering::Greater => latest = midpos,
            }
        }
    }

    fn offset_as_line_col(&self, offset: usize) -> LineColumn {
        match &self.data {
            Left(_) => LineColumn {
                line: NonZeroUsize::new(offset + 1).unwrap(),
                column: NonZeroUsize::new(1).unwrap(),
            },
            Right(d) => {
                let lineidx = self.offset_to_lineidx(offset);
                let slen = offset - self.lines[lineidx].0;
                let s = &d[self.lines[lineidx].0..];
                let s = &s[..slen];
                LineColumn {
                    line: NonZeroUsize::new(self.lines[lineidx].1 + 1).unwrap(),
                    column: NonZeroUsize::new(s.chars().count() + 1).unwrap(),
                }
            }
        }
    }

    fn line_col_as_offset(&self, line: usize, col: usize) -> usize {
        debug_assert!(line > 0);
        debug_assert!(col > 0);
        match &self.data {
            Left(_) => line + col - 2,
            Right(d) => {
                if line > self.lines.len() {
                    // Off the end, so just return the end of the data
                    d.len() - 1
                } else {
                    let s = &d[self.lines[line - 1].0..];
                    let ci = s.char_indices().nth(col - 1);
                    match ci {
                        None => {
                            // Ran out of characters, so just return the end
                            // of the string
                            d.len() - 1
                        }
                        Some(ci) => self.lines[line - 1].0 + ci.0,
                    }
                }
            }
        }
    }
}

/// A line column pair
pub struct LineColumn {
    /// The line number (lines start at 1)
    pub line: NonZeroUsize,
    /// The column number (columns start at 1)
    pub column: NonZeroUsize,
}

impl LineColumn {
    /// Create a new LineColumn from the given line and column numbers
    ///
    /// If either of line or column are zero, this returns None
    pub fn new(line: usize, column: usize) -> Option<LineColumn> {
        let line = NonZeroUsize::new(line);
        let column = NonZeroUsize::new(column);
        if let (Some(line), Some(column)) = (line, column) {
            Some(LineColumn { line, column })
        } else {
            None
        }
    }
}

/// A data mark
///
/// Data marks store a location in a data source.  They can be manipulated to
/// create new marks and are pretty cheap to clone.  If the data source they
/// are derived from happens to be based on a string then they can be used
/// in terms of lines and columns, otherwise only the offsets are valid.
///
/// Any attempt to manipulate lines and columns of data-based sources will simply
/// treat the data source as having single byte lines, so offset == line - 1,
/// and column will always be clamped to 1.
///
/// Marks have both a start and end position.  If the mark is for a point in
/// a data source then the start and end positions will be identical.  Positions
/// in marks are exclusive.  i.e. a mark from 0 to 0 covers nothing
/// and one from 0 to 10 covers 10 bytes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataMark {
    source: DataSource,
    start_offset: usize,
    end_offset: usize,
}

impl DataMark {
    /// Create a data mark from a data source and two offsets
    pub fn new_from_offsets(source: &DataSource, start: usize, end: usize) -> Self {
        Self {
            source: source.clone(),
            start_offset: start,
            end_offset: end,
        }
    }

    /// Create a data mark from a data source, an offset, and a length
    /// in bytes.
    pub fn new_from_offset_len(source: &DataSource, start: usize, len: usize) -> Self {
        Self {
            source: source.clone(),
            start_offset: start,
            end_offset: start + len - 1,
        }
    }

    /// Create a data mark from a data source, and a start/end linecolumn pair
    pub fn new_from_linecolumn(source: &DataSource, start: LineColumn, end: LineColumn) -> Self {
        Self {
            source: source.clone(),
            start_offset: source
                .0
                .line_col_as_offset(start.line.get(), start.column.get()),
            end_offset: source
                .0
                .line_col_as_offset(end.line.get(), end.column.get()),
        }
    }

    /// Create a data mark from a data source, a start linecolumn, and a length
    pub fn new_from_linecolumn_len(source: &DataSource, start: LineColumn, len: usize) -> Self {
        let start_offset = source
            .0
            .line_col_as_offset(start.line.get(), start.column.get());
        Self {
            source: source.clone(),
            start_offset,
            end_offset: start_offset + len - 1,
        }
    }

    /// Get the data source of this mark
    pub fn source(&self) -> &DataSource {
        &self.source
    }

    /// Get the start offset of the mark
    pub fn start_offset(&self) -> usize {
        self.start_offset
    }

    /// Get the end offset of the mark
    pub fn end_offset(&self) -> usize {
        self.end_offset
    }

    /// Get the start line,column of the mark
    pub fn start_linecolumn(&self) -> LineColumn {
        self.source.0.offset_as_line_col(self.start_offset)
    }

    /// Get the end line,column of the mark
    pub fn end_linecolumn(&self) -> LineColumn {
        self.source.0.offset_as_line_col(self.end_offset)
    }
}
