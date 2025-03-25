//! MIR place definitions
//!
//! This module contains data structures for places in the MIR.
//! Places represent memory locations that can be read from or written to.

use super::function::LocalId;
use std::fmt;

/// A place in the MIR
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Place {
    /// The local variable at the base of this place
    pub local: LocalId,
    /// A series of projections from the base local
    pub projection: Vec<ProjectionElem>,
}

/// A projection element
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ProjectionElem {
    /// Field access for a struct
    Field {
        /// Name of the field
        name: String,
        /// Index of the field
        index: usize,
    },
    /// Tuple element access
    TupleElement(usize),
    /// Array index
    Index(LocalId),
    /// Array slice
    Slice,
    /// Dereference of a pointer
    Deref,
}

impl Place {
    /// Create a new place for a local variable
    pub fn from_local(local: LocalId) -> Self {
        Place {
            local,
            projection: Vec::new(),
        }
    }

    /// Create a field projection
    pub fn field(mut self, name: String, index: usize) -> Self {
        self.projection.push(ProjectionElem::Field { name, index });
        self
    }

    /// Create a tuple element projection
    pub fn tuple_element(mut self, index: usize) -> Self {
        self.projection.push(ProjectionElem::TupleElement(index));
        self
    }

    /// Create an array index projection
    pub fn index(mut self, index: LocalId) -> Self {
        self.projection.push(ProjectionElem::Index(index));
        self
    }

    /// Create a slice projection
    pub fn slice(mut self) -> Self {
        self.projection.push(ProjectionElem::Slice);
        self
    }

    /// Create a dereference projection
    pub fn deref(mut self) -> Self {
        self.projection.push(ProjectionElem::Deref);
        self
    }
}

impl fmt::Display for Place {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "_{}", self.local)?;
        for proj in &self.projection {
            match proj {
                ProjectionElem::Field { name, .. } => write!(f, ".{}", name)?,
                ProjectionElem::TupleElement(idx) => write!(f, ".{}", idx)?,
                ProjectionElem::Index(idx) => write!(f, "[_{}]", idx)?,
                ProjectionElem::Slice => write!(f, "[..]")?,
                ProjectionElem::Deref => write!(f, "*")?,
            }
        }
        Ok(())
    }
}

impl fmt::Display for ProjectionElem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ProjectionElem::Field { name, .. } => write!(f, ".{}", name),
            ProjectionElem::TupleElement(idx) => write!(f, ".{}", idx),
            ProjectionElem::Index(idx) => write!(f, "[_{}]", idx),
            ProjectionElem::Slice => write!(f, "[..]"),
            ProjectionElem::Deref => write!(f, "*"),
        }
    }
} 