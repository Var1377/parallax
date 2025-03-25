use rustc_hash::FxHashMap;
use crate::{
    db::TypeCheckingDatabase,
    error::TypeError,
    traits::PredicateObligation,
};
use parallax_lang::ast;

/// A type variable identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVid(pub u32);

/// A type in the type system
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    /// A type variable used during inference
    Var(TyVid),
    /// A concrete type like i32, String, etc.
    Concrete(ConcreteTy),
    /// A function type
    Function {
        params: Vec<Ty>,
        ret: Box<Ty>,
    },
    /// A tuple type
    Tuple(Vec<Ty>),
}

/// A concrete (fully known) type
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConcreteTy {
    Int,
    Float,
    Bool,
    String,
    Char,
    Named {
        name: String,
        args: Vec<Ty>,
    },
}

/// A type constraint generated during inference
#[derive(Debug)]
pub enum Constraint {
    Eq(Ty, Ty),
    SubType(Ty, Ty),
}

/// The main type checking context
pub struct TypeContext<'tcx> {
    /// The database for querying resolved information
    pub db: &'tcx dyn TypeCheckingDatabase,
    
    /// Map from AST nodes to their types
    pub type_map: FxHashMap<ast::Span, Ty>,
    
    /// The current set of constraints
    pub constraints: Vec<Constraint>,
    
    /// Trait obligations that need to be proved
    pub obligations: Vec<PredicateObligation>,
    
    /// Type checking errors
    pub errors: Vec<TypeError>,
    
    /// The next type variable ID
    next_ty_vid: u32,
}

impl<'tcx> TypeContext<'tcx> {
    pub fn new(db: &'tcx dyn TypeCheckingDatabase) -> Self {
        Self {
            db,
            type_map: FxHashMap::default(),
            constraints: Vec::new(),
            obligations: Vec::new(),
            errors: Vec::new(),
            next_ty_vid: 0,
        }
    }
    
    /// Create a fresh type variable
    pub fn new_ty_var(&mut self) -> Ty {
        let vid = TyVid(self.next_ty_vid);
        self.next_ty_vid += 1;
        Ty::Var(vid)
    }
    
    /// Add a new constraint
    pub fn add_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }
    
    /// Add a new trait obligation that needs to be proved
    pub fn add_obligation(&mut self, obligation: PredicateObligation) {
        self.obligations.push(obligation);
    }
    
    /// Record an error
    pub fn error(&mut self, error: TypeError) {
        self.errors.push(error);
    }
    
    /// Record a type for an AST node
    pub fn record_type(&mut self, span: ast::Span, ty: Ty) {
        self.type_map.insert(span, ty);
    }
}