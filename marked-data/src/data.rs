//! Marked data types, these are the fundamental marked types.
//!
//! It is expected that almost any marked data structure can be built
//! from these data types, at least any which wants to remain fundamentally
//! pure data.

use std::ops::Deref;

use crate::basics::*;

/// Marked String
///
/// The fundamental data type representing a sequence of characters.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MarkedString {
    mark: DataMark,
    data: String,
}

impl MarkedString {
    /// Create a new MarkedString from the given mark and data
    pub fn new_from_string<S>(mark: DataMark, data: S) -> Self
    where
        S: Into<String>,
    {
        Self {
            mark,
            data: data.into(),
        }
    }

    /// Access the mark for this string
    pub fn mark(&self) -> &DataMark {
        &self.mark
    }
}

impl From<String> for MarkedString {
    fn from(value: String) -> Self {
        Self {
            mark: DataMark::new_from_offset_len(&ANON_DATASOURCE, 0, value.len()),
            data: value,
        }
    }
}

impl From<&str> for MarkedString {
    fn from(value: &str) -> Self {
        Self {
            mark: DataMark::new_from_offset_len(&ANON_DATASOURCE, 0, value.len()),
            data: value.into(),
        }
    }
}

impl Deref for MarkedString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}
