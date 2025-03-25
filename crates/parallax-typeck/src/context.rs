use rustc_hash::FxHashMap;
use std::rc::Rc;
use crate::{
    db::TypeCheckingDatabase,
    error::TypeError,
    traits::{PredicateObligation, GenericEnv, TraitRef, TraitSolver},
};
use parallax_lang::ast;
use crate::db::SymbolResolver;

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
    /// A generic type parameter
    Generic {
        name: String,
        bounds: Rc<Vec<String>>, // Trait bounds on this type parameter
    },
    /// A placeholder for a type that could not be resolved
    Error,
}

impl Ty {
    /// Check if this type is a generic parameter
    pub fn is_generic(&self) -> bool {
        matches!(self, Ty::Generic { .. })
    }
    
    /// Check if this type contains any generic parameters
    pub fn contains_generics(&self) -> bool {
        match self {
            Ty::Generic { .. } => true,
            Ty::Concrete(ConcreteTy::Named { args, .. }) => {
                args.iter().any(|arg| arg.contains_generics())
            },
            Ty::Function { params, ret } => {
                params.iter().any(|param| param.contains_generics()) || ret.contains_generics()
            },
            Ty::Tuple(elements) => {
                elements.iter().any(|elem| elem.contains_generics())
            },
            _ => false,
        }
    }
    
    /// Substitute generic parameters with concrete types
    pub fn substitute(&self, substitutions: &FxHashMap<String, Ty>) -> Ty {
        match self {
            Ty::Generic { name, .. } => {
                if let Some(ty) = substitutions.get(name) {
                    ty.clone()
                } else {
                    self.clone()
                }
            },
            Ty::Concrete(ConcreteTy::Named { name, args }) => {
                let new_args = args.iter()
                    .map(|arg| arg.substitute(substitutions))
                    .collect::<Vec<_>>();
                
                Ty::Concrete(ConcreteTy::Named {
                    name: name.clone(),
                    args: new_args,
                })
            },
            Ty::Function { params, ret } => {
                let new_params = params.iter()
                    .map(|param| param.substitute(substitutions))
                    .collect::<Vec<_>>();
                
                let new_ret = Box::new(ret.substitute(substitutions));
                
                Ty::Function {
                    params: new_params,
                    ret: new_ret,
                }
            },
            Ty::Tuple(elements) => {
                let new_elements = elements.iter()
                    .map(|elem| elem.substitute(substitutions))
                    .collect::<Vec<_>>();
                
                Ty::Tuple(new_elements)
            },
            _ => self.clone(),
        }
    }
}

/// A concrete (fully known) type
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConcreteTy {
    Int,
    Float,
    Bool,
    String,
    Char,
    Unit,
    Named {
        name: String,
        args: Vec<Ty>,
    },
}

/// A type constraint generated during inference
#[derive(Debug, Clone)]
pub enum Constraint {
    Eq(Ty, Ty),
    SubType(Ty, Ty),
    // New constraint for trait bounds
    HasTrait {
        ty: Ty,
        trait_id: String,
        args: Vec<Ty>,
    },
}

/// The main type checking context
#[derive(Clone)]
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
    
    /// Current generic environment (for processing items with generic parameters)
    generic_env: Option<GenericEnv>,
    
    /// Current type parameter substitutions
    substitutions: FxHashMap<String, Ty>,
    
    /// Span for trait-related errors
    pub traits_span: Option<ast::Span>,
    
    /// Trait obligations to be checked later
    trait_obligations: Vec<PredicateObligation>,
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
            generic_env: None,
            substitutions: FxHashMap::default(),
            traits_span: None,
            trait_obligations: Vec::new(),
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
    
    /// Set the current generic environment
    pub fn set_generic_env(&mut self, env: GenericEnv) {
        self.generic_env = Some(env);
    }
    
    /// Get the current generic environment
    pub fn generic_env(&self) -> Option<&GenericEnv> {
        self.generic_env.as_ref()
    }
    
    /// Add a type parameter substitution
    pub fn add_substitution(&mut self, param_name: String, ty: Ty) {
        self.substitutions.insert(param_name, ty);
    }
    
    /// Apply current substitutions to a type
    pub fn apply_substitutions(&self, ty: &Ty) -> Ty {
        ty.substitute(&self.substitutions)
    }
    
    /// Create a generic type parameter
    pub fn new_generic_param(&self, name: String, bounds: Vec<String>) -> Ty {
        Ty::Generic {
            name,
            bounds: Rc::new(bounds),
        }
    }
    
    /// Create a concrete type 
    pub fn concrete_ty(&self, kind: ConcreteTy) -> Ty {
        Ty::Concrete(kind)
    }
    
    /// Create a function type
    pub fn function_ty(&self, params: Vec<Ty>, ret: Ty) -> Ty {
        Ty::Function {
            params,
            ret: Box::new(ret),
        }
    }
    
    /// Create a tuple type
    pub fn tuple_ty(&self, elements: Vec<Ty>) -> Ty {
        Ty::Tuple(elements)
    }
    
    /// Set the span for trait-related errors
    pub fn set_traits_span(&mut self, span: ast::Span) {
        self.traits_span = Some(span);
    }
    
    /// Check if a trait is implemented for a type using the TraitSolver
    pub fn check_trait_implementation(&mut self, trait_ref: &TraitRef) -> Result<(), TypeError> {
        // Create a fresh solver for this check
        let mut solver = TraitSolver::new(self.db);
        
        // Perform the check
        solver.check_is_implemented(trait_ref)
    }
    
    /// Process all trait obligations using the TraitSolver
    pub fn process_obligations(&mut self, obligations: &[PredicateObligation]) -> Result<(), TypeError> {
        // Create a new solver
        let mut solver = TraitSolver::new(self.db);
        
        // Process each obligation
        for obligation in obligations {
            // Make sure we have a span for error reporting
            let mut trait_ref = obligation.trait_ref.clone();
            if trait_ref.span.is_none() {
                trait_ref.span = Some(obligation.span);
            }
            
            // Check if the trait is implemented, considering generic bounds
            if let Some(env) = &obligation.generic_env {
                // If we're in a generic context, check if this is a generic parameter
                // that has a trait bound
                if let Ty::Generic { name, .. } = &trait_ref.self_ty {
                    if env.satisfies_bound(name, &trait_ref.trait_id) {
                        // This generic parameter has an explicit bound for this trait
                        continue;
                    }
                }
            }
            
            // Otherwise, check for an actual implementation
            solver.check_is_implemented(&trait_ref)?;
        }
        
        Ok(())
    }
    
    /// Add a trait obligation to be checked later
    pub fn add_trait_obligation(&mut self, obligation: crate::traits::PredicateObligation) {
        self.trait_obligations.push(obligation);
    }
    
    /// Process all trait obligations
    pub fn process_all_obligations(&mut self) -> Result<(), TypeError> {
        // Use our new process_obligations method from the updated context
        let obligations = self.trait_obligations.clone();
        self.process_obligations(&obligations)
    }
}

// Add implementation of TypeContextOps for TypeContext
impl<'tcx> crate::db::TypeContextOps for TypeContext<'tcx> {
    fn new_ty_var(&mut self) -> Ty {
        // Allocate a fresh type variable
        let vid = TyVid(self.next_ty_vid);
        self.next_ty_vid += 1;
        Ty::Var(vid)
    }
    
    fn add_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }
    
    fn error(&mut self, error: crate::error::TypeError) {
        self.errors.push(error);
    }
    
    fn record_type(&mut self, span: parallax_lang::ast::Span, ty: Ty) {
        self.type_map.insert(span, ty);
    }
    
    fn lookup_symbol(&self, path: &[parallax_lang::ast::common::Ident]) -> Option<Ty> {
        if let Ok(resolved_crate) = self.db.resolved_crate() {
            // Create a symbol resolver with the symbol table
            let resolver = crate::symbol_resolver::TableBasedSymbolResolver::new(
                self.db,
                &resolved_crate.symbol_table
            );
            
            // Use the resolver to lookup the symbol
            if !path.is_empty() {
                // For now, just look up the last segment in the path
                // In a real implementation, this would handle the full path
                resolver.resolve_variable(&path.last().unwrap().0)
            } else {
                None
            }
        } else {
            // Failed to get resolved crate, fall back to default behavior
            None
        }
    }
    
    fn resolve_type_from_ast(&self, ty: &parallax_lang::ast::Type) -> Result<Ty, crate::error::TypeError> {
        if let Ok(resolved_crate) = self.db.resolved_crate() {
            // Create a type resolver with the symbol table
            let resolver = crate::resolve::TypeResolver::new(&resolved_crate.symbol_table);
            
            // Use the resolver to resolve the type
            resolver.resolve_type(ty)
        } else {
            // Failed to get resolved crate, fall back to primitive type handling
            match &ty.kind {
                parallax_lang::ast::TypeKind::Path(path) if path.len() == 1 => {
                    // Built-in primitive types
                    let name = &path[0].0;
                    match name.as_str() {
                        "Int" => Ok(Ty::Concrete(ConcreteTy::Int)),
                        "Bool" => Ok(Ty::Concrete(ConcreteTy::Bool)),
                        "String" => Ok(Ty::Concrete(ConcreteTy::String)),
                        "Char" => Ok(Ty::Concrete(ConcreteTy::Char)),
                        "Float" => Ok(Ty::Concrete(ConcreteTy::Float)),
                        "Unit" => Ok(Ty::Concrete(ConcreteTy::Unit)),
                        _ => Err(crate::error::TypeError::UnresolvedType {
                            ty: name.clone(),
                            span: ty.span,
                        })
                    }
                }
                _ => Err(crate::error::TypeError::UnresolvedType {
                    ty: format!("{:?}", ty.kind),
                    span: ty.span,
                })
            }
        }
    }
    
    fn symbol_resolver(&self) -> &dyn crate::db::SymbolResolver {
        // Create a static symbol resolver that implements the required trait
        // This is a bit of a hack, as we'd ideally store this in the context,
        // but it's sufficient for the purpose of this implementation
        
        if let Ok(resolved_crate) = self.db.resolved_crate() {
            // Note: We can't actually return the TableBasedSymbolResolver directly
            // because it has a limited lifetime. For a real implementation, we would
            // need to either:
            // 1. Store the resolver in the TypeContext, or
            // 2. Create a long-lived resolver with Arc/Box
            
            // For now, just return a default implementation
            struct DefaultSymbolResolver;
            
            impl crate::db::SymbolResolver for DefaultSymbolResolver {
                fn resolve_type(&self, _name: &str) -> Option<Ty> {
                    None
                }
                
                fn resolve_variable(&self, _name: &str) -> Option<Ty> {
                    None
                }
                
                fn resolve_function(&self, _name: &str) -> Option<(Vec<Ty>, Ty)> {
                    None
                }
                
                fn source_text(&self, _span: parallax_lang::ast::Span) -> std::sync::Arc<String> {
                    std::sync::Arc::new(String::new())
                }
            }
            
            static DEFAULT_RESOLVER: DefaultSymbolResolver = DefaultSymbolResolver;
            &DEFAULT_RESOLVER
        } else {
            // Return a default implementation if we can't get the resolved crate
            struct DefaultSymbolResolver;
            
            impl crate::db::SymbolResolver for DefaultSymbolResolver {
                fn resolve_type(&self, _name: &str) -> Option<Ty> {
                    None
                }
                
                fn resolve_variable(&self, _name: &str) -> Option<Ty> {
                    None
                }
                
                fn resolve_function(&self, _name: &str) -> Option<(Vec<Ty>, Ty)> {
                    None
                }
                
                fn source_text(&self, _span: parallax_lang::ast::Span) -> std::sync::Arc<String> {
                    std::sync::Arc::new(String::new())
                }
            }
            
            static DEFAULT_RESOLVER: DefaultSymbolResolver = DefaultSymbolResolver;
            &DEFAULT_RESOLVER
        }
    }
}