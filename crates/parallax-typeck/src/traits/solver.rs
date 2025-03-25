//!
//! This module contains the primary trait solver for Parallax.
//!
//! It checks if types implement specific traits, handling built-in implementations
//! and looking up user-defined implementations (potentially using caching).

use crate::{context::{Ty, ConcreteTy}, db::TypeCheckingDatabase, error::TypeError, traits::{TraitRef, GenericEnv}};
use parallax_lang::ast::{self, items::{ImplDef, ImplItem}};
use std::collections::{HashMap, HashSet};

// Define a key for the cache that implements Hash and Eq by using string representations
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TraitRefKey {
    trait_id: String,
    // Use a string representation of the type for hashing
    self_ty_repr: String,
}

impl From<&TraitRef> for TraitRefKey {
    fn from(trait_ref: &TraitRef) -> Self {
        Self {
            trait_id: trait_ref.trait_id.clone(),
            self_ty_repr: format!("{:?}", trait_ref.self_ty),
        }
    }
}

/// Represents a trait implementation with generic parameters
#[derive(Debug, Clone)]
struct ImplCandidate {
    /// The trait being implemented
    trait_path: Vec<String>,
    /// The type implementing the trait
    self_ty: Ty,
    /// Generic parameters in the impl
    generic_params: Vec<String>,
    /// Where clause predicates
    where_predicates: Vec<(Ty, String)>,
    /// Source impl definition
    impl_def: ImplDef,
}

/// The primary trait solver.
pub struct TraitSolver<'db> {
    db: &'db dyn TypeCheckingDatabase,
    // Cache for solving trait obligations to avoid redundant work
    cache: HashMap<TraitRefKey, bool>,
    // Cache of impl candidates found in the crate
    impl_candidates: Option<Vec<ImplCandidate>>,
}

impl<'db> TraitSolver<'db> {
    pub fn new(db: &'db dyn TypeCheckingDatabase) -> Self {
        Self {
            db,
            cache: HashMap::new(),
            impl_candidates: None,
        }
    }

    /// Check if a trait is implemented for a type.
    /// Returns Ok(()) if implemented, Err(TypeError::TraitNotImplemented) otherwise.
    pub fn check_is_implemented(&mut self, trait_ref: &TraitRef) -> Result<(), TypeError> {
        // Create key for the cache
        let key = TraitRefKey::from(trait_ref);

        // Check the cache first
        if let Some(&result) = self.cache.get(&key) {
            return if result {
                Ok(()) 
            } else {
                Err(self.not_implemented_error(trait_ref))
            };
        }

        // Check built-in implementations
        if let Some(result) = self.check_builtin_impl(trait_ref) {
            self.cache.insert(key, result);
            return if result {
                Ok(()) 
            } else {
                Err(self.not_implemented_error(trait_ref))
            };
        }

        // Check if the trait is implemented in the crate (enhanced implementation)
        if let Ok(crate_) = self.db.resolved_crate() {
            if self.check_crate_impls(trait_ref) {
                self.cache.insert(key, true);
                return Ok(());
            }
        }

        // If we get here, we couldn't find an implementation
        self.cache.insert(key, false);
        Err(self.not_implemented_error(trait_ref))
    }

    /// Helper to create the TraitNotImplemented error.
    fn not_implemented_error(&self, trait_ref: &TraitRef) -> TypeError {
        TypeError::TraitNotImplemented {
            trait_: trait_ref.trait_id.clone(),
            ty: trait_ref.self_ty.clone(),
            span: ast::Span { start: 0, end: 0 },
        }
    }
    
    /// Enhanced check for implementations within the resolved crate.
    /// This properly scans for all impl blocks and handles generic implementations.
    fn check_crate_impls(&mut self, trait_ref: &TraitRef) -> bool {
        // Initialize impl candidates if needed
        if self.impl_candidates.is_none() {
            self.initialize_impl_candidates();
        }
        
        // First, find all potentially matching implementations and prepare
        // the verification data
        let matching_predicates = self.find_matching_implementations(trait_ref);
        
        // Then verify the where clauses of each candidate separately to avoid borrowing issues
        for (predicates, substitutions) in matching_predicates {
            let all_predicates_satisfied = predicates.iter().all(|(ty, trait_name)| {
                // Substitute generic parameters in the type
                let concrete_ty = self.substitute_type(ty, &substitutions);
                
                // Create a trait reference for this predicate
                let predicate_trait_ref = TraitRef {
                    trait_id: trait_name.clone(),
                    self_ty: concrete_ty,
                    args: Vec::new(),
                    span: None,
                };
                
                // Simple check without recursion
                self.check_builtin_impl(&predicate_trait_ref)
                    .unwrap_or(false) // Only use built-ins for predicates to avoid cycles
            });
            
            if all_predicates_satisfied {
                return true;
            }
        }
        
        false
    }
    
    /// Find all implementations that potentially match the given trait reference
    /// Returns a list of (predicates, substitutions) for each matching candidate
    fn find_matching_implementations(&self, trait_ref: &TraitRef) -> Vec<(Vec<(Ty, String)>, HashMap<String, Ty>)> {
        let mut result = Vec::new();
        
        // Get the trait and target type
        let trait_name = &trait_ref.trait_id;
        let target_type = &trait_ref.self_ty;
        
        // Iterate through all impl candidates
        if let Some(candidates) = &self.impl_candidates {
            for candidate in candidates {
                // Check if this impl is for the right trait
                if !self.is_same_trait(&candidate.trait_path, trait_name) {
                    continue;
                }
                
                // Try to match the self type with the target type, considering generics
                if let Some(substitutions) = self.match_types(&candidate.self_ty, target_type) {
                    // Add to the list of candidates with their predicates and substitutions
                    let predicates = candidate.where_predicates.clone();
                    result.push((predicates, substitutions));
                }
            }
        }
        
        result
    }
    
    /// Initialize impl candidates by scanning all impl blocks in the crate
    fn initialize_impl_candidates(&mut self) {
        let mut candidates = Vec::new();
        
        if let Ok(crate_) = self.db.resolved_crate() {
            // Scan all items looking for impl blocks
            self.scan_items_for_impls(&crate_.resolved_ast, &mut candidates);
        }
        
        self.impl_candidates = Some(candidates);
    }
    
    /// Recursively scan all items in the AST for impl blocks
    fn scan_items_for_impls(&self, item: &ast::Item, candidates: &mut Vec<ImplCandidate>) {
        match &item.kind {
            ast::items::ItemKind::Impl(impl_def) => {
                // Only collect trait impls, not inherent impls
                if let Some(trait_type) = &impl_def.trait_type {
                    // Extract the trait path and convert to internal representation
                    let trait_path = self.extract_path_from_type(trait_type);
                    
                    // Extract generic parameters
                    let generic_params = if let Some(params) = &impl_def.generic_params {
                        params.iter().map(|p| p.name.0.clone()).collect()
                    } else {
                        Vec::new()
                    };
                    
                    // Extract where predicates
                    let where_predicates = if let Some(where_clause) = &impl_def.where_clause {
                        where_clause.predicates.iter()
                            .flat_map(|pred| {
                                pred.bounds.iter().map(move |bound| {
                                    let trait_name = self.extract_path_from_type(bound).join("::");
                                    (self.convert_ast_type_to_ty(&pred.ty), trait_name)
                                })
                            })
                            .collect()
                    } else {
                        Vec::new()
                    };
                    
                    // Convert the self type to our internal type representation
                    let self_ty = self.convert_ast_type_to_ty(&impl_def.self_type);
                    
                    // Create an ImplCandidate and add it to the list
                    candidates.push(ImplCandidate {
                        trait_path,
                        self_ty,
                        generic_params,
                        where_predicates,
                        impl_def: impl_def.clone(),
                    });
                }
            },
            ast::items::ItemKind::Module(module) => {
                // Recursively scan module items
                for item in &module.items {
                    self.scan_items_for_impls(item, candidates);
                }
            },
            _ => {}
        }
    }
    
    /// Extract a path from a type (for trait names)
    fn extract_path_from_type(&self, ty: &ast::Type) -> Vec<String> {
        match &ty.kind {
            ast::types::TypeKind::Path(path) => {
                path.iter().map(|segment| segment.0.clone()).collect()
            },
            _ => Vec::new(),
        }
    }
    
    /// Convert an AST type to our internal type representation
    fn convert_ast_type_to_ty(&self, ty: &ast::Type) -> Ty {
        match &ty.kind {
            ast::types::TypeKind::Path(path) if path.len() == 1 => {
                let name = &path[0].0;
                match name.as_str() {
                    "Int" => Ty::Concrete(ConcreteTy::Int),
                    "Float" => Ty::Concrete(ConcreteTy::Float),
                    "Bool" => Ty::Concrete(ConcreteTy::Bool),
                    "String" => Ty::Concrete(ConcreteTy::String),
                    "Char" => Ty::Concrete(ConcreteTy::Char),
                    "Unit" => Ty::Concrete(ConcreteTy::Unit),
                    _ => {
                        // Check if this is a generic parameter
                        Ty::Generic {
                            name: name.clone(),
                            bounds: std::rc::Rc::new(Vec::new()),
                        }
                    }
                }
            },
            ast::types::TypeKind::Path(path) => {
                // For paths longer than 1 segment, create a named type
                let name = path.iter()
                    .map(|segment| segment.0.clone())
                    .collect::<Vec<_>>()
                    .join("::");
                
                // Handle generic arguments if present
                let args = Vec::new(); // No args in simple path
                
                Ty::Concrete(ConcreteTy::Named {
                    name,
                    args,
                })
            },
            ast::types::TypeKind::Array(elem_ty, _) => {
                // Create a tuple type for arrays (simplified)
                Ty::Tuple(vec![self.convert_ast_type_to_ty(elem_ty)])
            },
            ast::types::TypeKind::Tuple(types) => {
                // Convert tuple types
                let elements = types.iter()
                    .map(|t| self.convert_ast_type_to_ty(t))
                    .collect();
                
                Ty::Tuple(elements)
            },
            ast::types::TypeKind::Function(params, ret) => {
                // Convert function types
                let param_ty = self.convert_ast_type_to_ty(params);
                let ret_ty = self.convert_ast_type_to_ty(ret);
                
                Ty::Function {
                    params: vec![param_ty],
                    ret: Box::new(ret_ty),
                }
            },
            _ => Ty::Error, // For unknown/unconvertible types
        }
    }
    
    /// Check if two trait paths refer to the same trait
    fn is_same_trait(&self, trait_path: &[String], trait_name: &str) -> bool {
        if trait_path.is_empty() {
            return false;
        }
        
        // Try different ways to match the trait name
        let full_path = trait_path.join("::");
        if full_path == trait_name {
            return true;
        }
        
        // Try just the last part of the trait path
        if let Some(last) = trait_path.last() {
            if last == trait_name {
                return true;
            }
        }
        
        false
    }
    
    /// Match two types, returning a set of substitutions for generic parameters
    fn match_types(&self, impl_ty: &Ty, target_ty: &Ty) -> Option<HashMap<String, Ty>> {
        let mut substitutions = HashMap::new();
        
        if self.match_types_recursive(impl_ty, target_ty, &mut substitutions) {
            Some(substitutions)
        } else {
            None
        }
    }
    
    /// Recursive helper for matching types
    fn match_types_recursive(&self, impl_ty: &Ty, target_ty: &Ty, substitutions: &mut HashMap<String, Ty>) -> bool {
        match (impl_ty, target_ty) {
            // Generic parameter in impl type - add to substitutions
            (Ty::Generic { name, .. }, _) => {
                if let Some(existing) = substitutions.get(name) {
                    // Generic parameter already has a substitution, check if consistent
                    self.types_compatible(existing, target_ty)
                } else {
                    // Add new substitution
                    substitutions.insert(name.clone(), target_ty.clone());
                    true
                }
            },
            
            // Match concrete types
            (Ty::Concrete(impl_concrete), Ty::Concrete(target_concrete)) => {
                match (impl_concrete, target_concrete) {
                    // Basic primitive types match if they're the same
                    (ConcreteTy::Int, ConcreteTy::Int) => true,
                    (ConcreteTy::Float, ConcreteTy::Float) => true,
                    (ConcreteTy::Bool, ConcreteTy::Bool) => true,
                    (ConcreteTy::String, ConcreteTy::String) => true,
                    (ConcreteTy::Char, ConcreteTy::Char) => true,
                    (ConcreteTy::Unit, ConcreteTy::Unit) => true,
                    
                    // Named types
                    (ConcreteTy::Named { name: impl_name, args: impl_args },
                     ConcreteTy::Named { name: target_name, args: target_args }) => {
                        // Names must match
                        if impl_name != target_name {
                            return false;
                        }
                        
                        // Argument counts must match
                        if impl_args.len() != target_args.len() {
                            return false;
                        }
                        
                        // Each argument must match
                        for (impl_arg, target_arg) in impl_args.iter().zip(target_args.iter()) {
                            if !self.match_types_recursive(impl_arg, target_arg, substitutions) {
                                return false;
                            }
                        }
                        
                        true
                    },
                    
                    // Mismatched concrete types
                    _ => false,
                }
            },
            
            // Match function types
            (Ty::Function { params: impl_params, ret: impl_ret },
             Ty::Function { params: target_params, ret: target_ret }) => {
                // Parameter counts must match
                if impl_params.len() != target_params.len() {
                    return false;
                }
                
                // Each parameter must match
                for (impl_param, target_param) in impl_params.iter().zip(target_params.iter()) {
                    if !self.match_types_recursive(impl_param, target_param, substitutions) {
                        return false;
                    }
                }
                
                // Return types must match
                self.match_types_recursive(impl_ret, target_ret, substitutions)
            },
            
            // Match tuple types
            (Ty::Tuple(impl_elements), Ty::Tuple(target_elements)) => {
                // Element counts must match
                if impl_elements.len() != target_elements.len() {
                    return false;
                }
                
                // Each element must match
                for (impl_elem, target_elem) in impl_elements.iter().zip(target_elements.iter()) {
                    if !self.match_types_recursive(impl_elem, target_elem, substitutions) {
                        return false;
                    }
                }
                
                true
            },
            
            // Mismatched type kinds
            _ => false,
        }
    }
    
    /// Check if two types are compatible (simple compatibility check)
    fn types_compatible(&self, ty1: &Ty, ty2: &Ty) -> bool {
        match (ty1, ty2) {
            // Exact match
            _ if ty1 == ty2 => true,
            
            // Various cases of compatibility would go here
            // This is simplified for now
            
            _ => false,
        }
    }
    
    /// Substitute generic parameters in a type
    fn substitute_type(&self, ty: &Ty, substitutions: &HashMap<String, Ty>) -> Ty {
        match ty {
            Ty::Generic { name, .. } => {
                if let Some(concrete) = substitutions.get(name) {
                    concrete.clone()
                } else {
                    ty.clone()
                }
            },
            Ty::Concrete(ConcreteTy::Named { name, args }) => {
                let new_args = args.iter()
                    .map(|arg| self.substitute_type(arg, substitutions))
                    .collect();
                
                Ty::Concrete(ConcreteTy::Named {
                    name: name.clone(),
                    args: new_args,
                })
            },
            Ty::Function { params, ret } => {
                let new_params = params.iter()
                    .map(|param| self.substitute_type(param, substitutions))
                    .collect();
                
                let new_ret = Box::new(self.substitute_type(ret, substitutions));
                
                Ty::Function {
                    params: new_params,
                    ret: new_ret,
                }
            },
            Ty::Tuple(elements) => {
                let new_elements = elements.iter()
                    .map(|elem| self.substitute_type(elem, substitutions))
                    .collect();
                
                Ty::Tuple(new_elements)
            },
            _ => ty.clone(),
        }
    }
    
    /// A simplified implementation check for breaking recursion cycles
    fn check_simple_impl(&self, trait_ref: &TraitRef) -> bool {
        // Just check if names match, without recursing into where clauses
        if let Ok(crate_) = self.db.resolved_crate() {
            let trait_name = &trait_ref.trait_id;
            let type_name = match &trait_ref.self_ty {
                Ty::Concrete(ConcreteTy::Named { name, .. }) => Some(name.as_str()),
                Ty::Concrete(ConcreteTy::Int) => Some("Int"),
                Ty::Concrete(ConcreteTy::Float) => Some("Float"),
                Ty::Concrete(ConcreteTy::Bool) => Some("Bool"),
                Ty::Concrete(ConcreteTy::String) => Some("String"),
                Ty::Concrete(ConcreteTy::Char) => Some("Char"),
                Ty::Concrete(ConcreteTy::Unit) => Some("Unit"),
                _ => None,
            };

            if let Some(type_name) = type_name {
                let trait_exists = !crate_.symbol_table.lookup(trait_name).is_empty();
                let type_exists = !crate_.symbol_table.lookup(type_name).is_empty();
                return trait_exists && type_exists;
            }
        }
        
        false
    }

    /// Check for built-in implementations (primitive types, core traits).
    fn check_builtin_impl(&self, trait_ref: &TraitRef) -> Option<bool> {
        let trait_id = &trait_ref.trait_id;
        let ty = &trait_ref.self_ty;

        match (trait_id.as_str(), ty) {
            // Numeric traits
            ("Add" | "Sub" | "Mul" | "Div", Ty::Concrete(ConcreteTy::Int | ConcreteTy::Float)) => Some(true),
            
            // Comparison traits
            ("Eq" | "PartialEq", Ty::Concrete(c)) => {
                Some(matches!(c, 
                    ConcreteTy::Int | ConcreteTy::Float | ConcreteTy::Bool | 
                    ConcreteTy::Char | ConcreteTy::String
                ))
            },
            ("Ord" | "PartialOrd", Ty::Concrete(c)) => {
                Some(matches!(c, 
                    ConcreteTy::Int | ConcreteTy::Float | ConcreteTy::Char
                ))
            },
            
            // Debug/Display (assume implemented for all for now)
            ("Debug" | "Display", _) => Some(true),
            
            // Default
            ("Default", Ty::Concrete(c)) => {
                Some(matches!(c, 
                    ConcreteTy::Int | ConcreteTy::Float | ConcreteTy::Bool | 
                    ConcreteTy::String | ConcreteTy::Unit
                ))
            },
            
            // Clone/Copy
            ("Clone", _) => Some(true), // Assume Clone for all
            ("Copy", Ty::Concrete(c)) => {
                Some(matches!(c, 
                    ConcreteTy::Int | ConcreteTy::Float | ConcreteTy::Bool | 
                    ConcreteTy::Char | ConcreteTy::Unit
                ))
            },
            
            // Not a built-in implementation
            _ => None,
        }
    }
} 