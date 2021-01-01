//! Marked Data
//! ===========
//!
//! Data marks are similar to `Span`s from other systems, but they also carry
//! information about the file from which they were loaded.  Files carry some
//! information about whether or not they were loaded in such a way as support
//! line and column numbering, or just offset information.
//!
//! The primary intent of data marks are to provide for a way for provenance to
//! be carried around with the data which it marks.  As such, this crate provides
//! not only the means of marking data, but also a variety of types for storing
//! structured and marked data in ways useful to a majority of users.
//!
//! To get started, simply create a `DataSource` and then proceed from there.

#![deny(missing_docs)]

pub mod basics;
pub mod data;
pub mod errors;
