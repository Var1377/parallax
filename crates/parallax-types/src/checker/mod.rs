// Placeholder for checker module 

// Added imports, adjusted paths
use std::collections::HashMap;
use std::sync::Arc;
use miette::SourceSpan;
use operators::TraitRef;
use parallax_resolve::types::{Symbol, ResolvedModuleStructure, ResolvedType, ResolvedStruct, ResolvedEnum, ResolvedTrait, ResolvedFunction, ResolvedImpl};
use parallax_resolve::{definitions::DefinitionKind, ResolvedDefinitions, types::ResolvedEnumVariant};

use crate::error::{TypeError, TypeResult};
use crate::context::inference::{InferenceContext, Substitution, TypeEnvironment};
use crate::context::trait_repo::{TraitRepository, ImplId};
use crate::types::*;
use crate::{FunctionSignature, TypeDatabase, TypedModule}; // Keep existing imports
use crate::checker::resolve::{resolve_single_generic_param, resolve_type_to_ty};
use crate::error::display_type; // For logging types
use std::cell::RefCell; // Added RefCell

// Define submodules for the checker
pub mod expr;
pub mod pattern;
pub mod aggregates;
pub mod control_flow;
pub mod invocation;
pub mod operators;
pub mod resolve;
pub mod generics;
pub mod substitute;

pub struct TypeChecker<'checker> { // Renamed from TypeCheckerCtx
    #[allow(dead_code)]
    db: &'checker dyn TypeDatabase,
    #[allow(dead_code)]
    definitions: &'checker ResolvedDefinitions,
    #[allow(dead_code)]
    pub type_ctx: TypeContext, // <-- Made public
    #[allow(dead_code)]
    pub trait_repo: TraitRepository, // <-- Made public
    #[allow(dead_code)]
    pub inference_ctx: InferenceContext,
    pub _type_env: Arc<TypeEnvironment>, // Renamed to avoid conflict
    // pub(crate) current_module_symbol: Option<Symbol>,
    #[allow(dead_code)]
    pub generic_scopes: Vec<HashMap<String, TypeId>>,
    #[allow(dead_code)]
    pub errors: Vec<TypeError>, // <-- Already public
    /// Collects concrete function signatures (impl methods, instantiated generics) needed during body checking.
    pub needed_functions: RefCell<HashMap<Symbol, FunctionSignature>>,
}


impl<'checker> TypeChecker<'checker> {
    pub fn new(db: &'checker dyn TypeDatabase, definitions: &'checker ResolvedDefinitions, type_ctx: TypeContext, trait_repo: TraitRepository) -> Self {
        Self {
            db,
            definitions,
            type_ctx,
            trait_repo,
            inference_ctx: InferenceContext::new(),
            _type_env: Arc::new(TypeEnvironment::new()),
            // current_module_symbol: None,
            generic_scopes: Vec::new(),
            errors: Vec::new(),
            needed_functions: RefCell::new(HashMap::new()), // Initialize the new field
        }
    }
    
    // Helper to get a dummy span (replace with actual span propagation later)
    pub(crate) fn dummy_span() -> SourceSpan {
        SourceSpan::from((0, 0))
    }

    // Placeholder for get_name_for_symbol
    pub(crate) fn get_name_for_symbol(&self, symbol: Symbol) -> TypeResult<String> {
        self.type_ctx.get_name_for_symbol(&symbol).cloned().ok_or_else(|| {
            TypeError::InternalError {
                message: format!("Name not found for symbol {:?} in type context", symbol),
                span: None, // TODO: Add span?
            }
        })
    }
    
    /// Retrieves the resolved DefinitionKind, name, and span for a given symbol.
    pub(crate) fn get_definition_info(&self, symbol: Symbol) -> Option<(DefinitionKind, String, SourceSpan)> {
        // Search through the stored ResolvedDefinitions
        if let Some(func) = self.definitions.functions.iter().find(|f| f.symbol == symbol) {
            return Some((DefinitionKind::Function, func.name.clone(), func.span));
        }
        if let Some(struct_def) = self.definitions.structs.iter().find(|s| s.symbol == symbol) {
            return Some((DefinitionKind::Struct, struct_def.name.clone(), struct_def.span));
        }
        if let Some(enum_def) = self.definitions.enums.iter().find(|e| e.symbol == symbol) {
            // Check if the symbol is for the enum itself or a variant
            if let Some(variant) = enum_def.variants.iter().find(|v| {
                match v {
                    ResolvedEnumVariant::Unit { symbol: variant_symbol, .. } => *variant_symbol == symbol,
                    ResolvedEnumVariant::Tuple { symbol: variant_symbol, .. } => *variant_symbol == symbol,
                    ResolvedEnumVariant::Struct { symbol: variant_symbol, .. } => *variant_symbol == symbol,
                }
            }) {
                // Extract common fields by matching on the variant kind
                let (variant_name, variant_span) = match variant {
                    ResolvedEnumVariant::Unit { name, span, .. } => (name.clone(), *span),
                    ResolvedEnumVariant::Tuple { name, span, .. } => (name.clone(), *span),
                    ResolvedEnumVariant::Struct { name, span, .. } => (name.clone(), *span),
                };
                return Some((DefinitionKind::EnumVariant, variant_name, variant_span));
            }
            return Some((DefinitionKind::Enum, enum_def.name.clone(), enum_def.span));
        }
        if let Some(r#trait) = self.definitions.traits.iter().find(|t| t.symbol == symbol) {
            return Some((DefinitionKind::Trait, r#trait.name.clone(), r#trait.span));
        }
        if let Some(r#impl) = self.definitions.impls.iter().find(|i| i.impl_symbol == symbol) {
            // Generate a descriptive name for the impl block
            let name = if let Some(trait_symbol) = r#impl.trait_symbol {
                let trait_name = self.get_name_for_symbol(trait_symbol).unwrap_or_else(|_| "?".to_string());
                // TODO: Resolve implementing type name
                format!("<impl {} for ...>", trait_name)
            } else {
                // TODO: Resolve implementing type name
                "<impl ...>".to_string()
            };
            return Some((DefinitionKind::Impl, name, r#impl.span));
        }
        // TODO: Handle Module kind if needed (requires module info in ResolvedDefinitions)
        None 
    }
    
    // Placeholder for fresh_infer_var
    pub(crate) fn fresh_infer_var(&mut self, span: SourceSpan) -> Ty {
        self.inference_ctx.fresh_var_with_span(span)
    }
    
    // Placeholder for resolve_type
    pub(crate) fn resolve_type(&self, ty: &Ty) -> Ty {
        // First, resolve inference variables using the context
        let resolved_vars_ty = self.inference_ctx.resolve_type(ty);

        // Then, substitute SelfType using the environment
        if let Some(concrete_self_ty) = self._type_env.get("Self") {
            // Use the dedicated recursive substitution helper
            // Need to handle potential recursion limits/errors if substitute_self changes
            substitute::substitute_self(&resolved_vars_ty, concrete_self_ty, 20) // Using recursion limit from substitute.rs
        } else {
            // If 'Self' is not in the environment, just return the var-resolved type.
            // Check if the var-resolved type *still* contains SelfType - this would be an error.
            if substitute::contains_self(&resolved_vars_ty) {
                // It's an error to have SelfType here if 'Self' is not defined in the env.
                 println!(
                     "[ResolveType] ERROR: Found SelfType in {:?} but 'Self' is not in the environment: {:?}", 
                     resolved_vars_ty, 
                     self._type_env // Log env for debugging
                 );
                 // Return an error type or the original? Let's return Error.
                 Ty::new(TyKind::Error) 
            } else {
                resolved_vars_ty // No SelfType present, return as is.
            }
        }
    }
    
    // Placeholder for unify
    pub(crate) fn unify(&mut self, ty1: &Ty, ty2: &Ty) -> TypeResult<Substitution> {
        // Resolve both types *before* passing them to the inference context unification.
        // This ensures SelfType is substituted correctly based on the checker's environment.
        let resolved_ty1 = self.resolve_type(ty1);
        let resolved_ty2 = self.resolve_type(ty2);
        
        // Special handling for integer/float literals
        match (&resolved_ty1.kind, &resolved_ty2.kind) {
            // Allow IntegerLiteral to unify with any concrete integer type
            (TyKind::Primitive(PrimitiveType::IntegerLiteral), TyKind::Primitive(p2)) if p2.is_integer() && *p2 != PrimitiveType::IntegerLiteral => {
                // Prefer the concrete type, no substitution needed here as inference context will handle vars
                return Ok(Substitution::new());
            }
            (TyKind::Primitive(p1), TyKind::Primitive(PrimitiveType::IntegerLiteral)) if p1.is_integer() && *p1 != PrimitiveType::IntegerLiteral => {
                // Prefer the concrete type
                return Ok(Substitution::new());
            }
            // Allow FloatLiteral to unify with f32 or f64
            (TyKind::Primitive(PrimitiveType::FloatLiteral), TyKind::Primitive(p2 @ (PrimitiveType::F32 | PrimitiveType::F64))) => {
                // Prefer the concrete type
                return Ok(Substitution::new());
            }
            (TyKind::Primitive(p1 @ (PrimitiveType::F32 | PrimitiveType::F64)), TyKind::Primitive(PrimitiveType::FloatLiteral)) => {
                // Prefer the concrete type
                return Ok(Substitution::new());
            }
            // Let the inference context handle other cases, including Var unification
            _ => {}
        }

        // Now unify the resolved types using the inference context.
        self.inference_ctx.unify(&resolved_ty1, &resolved_ty2)
    }

    // Placeholder for lookup_trait_id_by_symbol
    pub(crate) fn lookup_trait_id_by_symbol(&self, symbol: Symbol) -> TypeResult<crate::context::trait_repo::TraitId> {
        self.trait_repo.get_trait_id_by_symbol(symbol).ok_or_else(|| {
            // Find span if possible
            let span = TypeChecker::dummy_span();
            TypeError::UnknownTrait { name: format!("symbol_{}", symbol.id()), span }
        })
    }

    // Placeholder for resolve_type_to_ty
    pub(crate) fn resolve_type_to_ty(&mut self, resolved_ty: &ResolvedType) -> Result<Ty, TypeError> {
        resolve::resolve_type_to_ty(self, resolved_ty) // Delegate to resolve.rs
    }
    
    // Placeholder for manual_substitute_signature
     pub(crate) fn manual_substitute_signature(&self, sig: &FunctionSignature, subst: &Substitution) -> TypeResult<FunctionSignature> {
         // Substitute generic parameter definitions (if needed, maybe not?)
         // let new_generic_params = sig.generic_params.iter().map(|p| { ... }).collect();

         // Substitute parameter types
         let new_params = sig.params.iter().map(|p| {
             ParamType {
                 name: p.name.clone(),
                 ty: p.ty.apply_subst(subst),
                 span: p.span,
             }
         }).collect();

         // Substitute return type
         let new_return_type = sig.return_type.apply_subst(subst);

         Ok(FunctionSignature {
             name: sig.name.clone(),
             self_param: sig.self_param.clone(), // Self kind doesn't change
             generic_params: sig.generic_params.clone(), // Bounds might need substitution?
             params: new_params,
             return_type: new_return_type,
             span: sig.span,
         })
     }

     // --- Placeholder Definition Checking Methods --- 

     /// Checks the definition of a struct, adding it to the TypeContext.
     pub(crate) fn check_struct_definition(&mut self, resolved_struct: &ResolvedStruct) -> TypeResult<()> {
         println!("[CheckStructDef] Checking struct: {} ({:?})", resolved_struct.name, resolved_struct.symbol);
         let mut generic_param_scope = HashMap::new();
         let definition_context = Some((DefinitionKind::Struct, resolved_struct.symbol));
         let mut checker_generic_params = Vec::new();
         println!("[CheckStructDef]   Resolving {} generic parameters...", resolved_struct.generic_params.len());

         // Resolve generic parameters first, adding them to the scope
         for resolved_gen_param in &resolved_struct.generic_params {
             println!("[CheckStructDef]     Resolving generic param: {}", resolved_gen_param.name);
             match resolve_single_generic_param(self, resolved_gen_param, definition_context) {
                 Ok(checker_param) => {
                     println!("[CheckStructDef]       Resolved to ID: {:?}", checker_param.id);
                     generic_param_scope.insert(checker_param.name.clone(), checker_param.id);
                     checker_generic_params.push(checker_param);
                 }
                 Err(e) => { self.errors.push(e); }
             }
         }
         self.generic_scopes.push(generic_param_scope);
         println!("[CheckStructDef]   Pushed generic scope: {:?}", self.generic_scopes.last());

         let mut checker_fields = Vec::new();
         println!("[CheckStructDef]   Resolving {} fields...", resolved_struct.fields.len());
         for resolved_field in &resolved_struct.fields {
             println!("[CheckStructDef]     Resolving field: {} ({:?})", resolved_field.name, resolved_field.symbol);
             match self.resolve_type_to_ty(&resolved_field.field_type) {
                 Ok(ty) => {
                     println!("[CheckStructDef]       Resolved type: {}", display_type(&ty));
                     checker_fields.push(Field {
                         name: resolved_field.name.clone(),
                         symbol: resolved_field.symbol,
                         ty,
                         span: resolved_field.span,
                     });
                 }
                 Err(e) => { self.errors.push(e); }
             }
         }
         println!("[CheckStructDef]   Popping generic scope.");
         self.generic_scopes.pop();

         let struct_def = StructDef {
             name: resolved_struct.name.clone(),
             symbol: resolved_struct.symbol,
             generic_params: checker_generic_params,
             fields: checker_fields,
             span: resolved_struct.span,
         };
         self.type_ctx.add_type(resolved_struct.symbol, resolved_struct.name.clone(), TypeDef::Struct(struct_def));
          Ok(())
     }

     /// Checks the definition of an enum, adding it to the TypeContext.
     pub(crate) fn check_enum_definition(&mut self, resolved_enum: &ResolvedEnum) -> TypeResult<()> {
         println!("[CheckEnumDef] Checking enum: {} ({:?})", resolved_enum.name, resolved_enum.symbol);
         let mut generic_param_scope = HashMap::new();
         let definition_context = Some((DefinitionKind::Enum, resolved_enum.symbol));
         let mut checker_generic_params = Vec::new();
         println!("[CheckEnumDef]   Resolving {} generic parameters...", resolved_enum.generic_params.len());

         // Resolve generic parameters first
         for resolved_gen_param in &resolved_enum.generic_params {
             println!("[CheckEnumDef]     Resolving generic param: {}", resolved_gen_param.name);
             match resolve_single_generic_param(self, resolved_gen_param, definition_context) {
                 Ok(checker_param) => {
                     println!("[CheckEnumDef]       Resolved to ID: {:?}", checker_param.id);
                     generic_param_scope.insert(checker_param.name.clone(), checker_param.id);
                     checker_generic_params.push(checker_param);
                 }
                 Err(e) => { self.errors.push(e); }
             }
         }
         self.generic_scopes.push(generic_param_scope);
         println!("[CheckEnumDef]   Pushed generic scope: {:?}", self.generic_scopes.last());

         let mut checker_variants = Vec::new();
         println!("[CheckEnumDef]   Checking {} variants...", resolved_enum.variants.len());
         for resolved_variant in &resolved_enum.variants {
             // Match on the ResolvedEnumVariant kind
             let checker_variant = match resolved_variant {
                  parallax_resolve::types::ResolvedEnumVariant::Unit { name, symbol, span } => {
                      println!("[CheckEnumDef]     Checking Unit variant: {} ({:?})", name, symbol);
                      // Add symbol->name mapping
                      self.type_ctx.add_symbol_name(*symbol, name.clone());
                      println!("[CheckEnumDef]       Added variant symbol {:?} -> name '{}' mapping.", symbol, name);
                      // Create the checker's EnumVariant
                      EnumVariant {
                           name: name.clone(),
                           symbol: *symbol,
                           fields: vec![], // Unit variant has no fields
                           span: *span,
                       }
                   }
                  parallax_resolve::types::ResolvedEnumVariant::Tuple { name, symbol, fields: resolved_field_types, span } => {
                      println!("[CheckEnumDef]     Checking Tuple variant: {} ({:?})", name, symbol);
                      self.type_ctx.add_symbol_name(*symbol, name.clone());
                      let mut checker_fields = Vec::new();
                       println!("[CheckEnumDef]       Variant has {} tuple fields. Resolving types...", resolved_field_types.len());
                       for (i, resolved_field_ty) in resolved_field_types.iter().enumerate() {
                           println!("[CheckEnumDef]         Resolving tuple field {}", i);
                           if let ResolvedType::UserDefined { symbol: field_symbol, type_args } = resolved_field_ty {
                               if *field_symbol == resolved_enum.symbol {
                                   println!("[CheckEnumDef]         Recursive field type detected: {}", resolved_enum.name);
                                   // Manually construct the recursive type using already resolved generic args
                                   let mut resolved_generic_args = Vec::new();
                                   if let Some(args) = type_args {
                                       for arg in args {
                                           match self.resolve_type_to_ty(arg) {
                                               Ok(ty) => resolved_generic_args.push(ty),
                                               Err(e) => {
                                                   self.errors.push(e);
                                                   resolved_generic_args.push(Ty::new(TyKind::Error));
                                               }
                                           }
                                       }
                                   }
                                   let field_ty = Ty::with_span(
                                       TyKind::Named {
                                           name: resolved_enum.name.clone(),
                                           symbol: Some(resolved_enum.symbol),
                                           args: resolved_generic_args,
                                       },
                                       *span // Use variant span
                                   );
                                   println!("[CheckEnumDef]           Constructed recursive type: {}", display_type(&field_ty));
                                   checker_fields.push(Field {
                                       name: i.to_string(),
                                       symbol: Symbol::new(0),
                                       ty: field_ty,
                                       span: *span,
                                   });
                               } else {
                                   // Not recursive, resolve normally
                                   match self.resolve_type_to_ty(resolved_field_ty) {
                                       Ok(field_ty) => {
                                           println!("[CheckEnumDef]           Resolved type: {}", display_type(&field_ty));
                                           checker_fields.push(Field {
                                               name: i.to_string(),
                                               symbol: Symbol::new(0),
                                               ty: field_ty,
                                               span: *span,
                                           });
                                       }
                                       Err(e) => {
                                           println!("[CheckEnumDef]           ERROR resolving tuple field type: {:?}", e);
                                           self.errors.push(e);
                                       }
                                   }
                               }
                           } else {
                               // Not recursive, resolve normally
                               match self.resolve_type_to_ty(resolved_field_ty) {
                                   Ok(field_ty) => {
                                       println!("[CheckEnumDef]           Resolved type: {}", display_type(&field_ty));
                                       checker_fields.push(Field {
                                           name: i.to_string(),
                                           symbol: Symbol::new(0),
                                           ty: field_ty,
                                           span: *span,
                                       });
                                   }
                                   Err(e) => {
                                       println!("[CheckEnumDef]           ERROR resolving tuple field type: {:?}", e);
                                       self.errors.push(e);
                                   }
                               }
                           }
                       }
                       println!("[CheckEnumDef]       Added variant symbol {:?} -> name '{}' mapping.", symbol, name);
                       EnumVariant {
                           name: name.clone(),
                           symbol: *symbol,
                           fields: checker_fields,
                           span: *span,
                       }
                  }
                   parallax_resolve::types::ResolvedEnumVariant::Struct { name, symbol, fields: resolved_fields, span } => {
                      println!("[CheckEnumDef]     Checking Struct variant: {} ({:?})", name, symbol);
                      self.type_ctx.add_symbol_name(*symbol, name.clone());
                      let mut checker_fields = Vec::new();
                       println!("[CheckEnumDef]       Variant has {} struct fields. Resolving types...", resolved_fields.len());
                       for resolved_field in resolved_fields {
                           println!("[CheckEnumDef]         Resolving field: {} ({:?})", resolved_field.name, resolved_field.symbol);
                           match self.resolve_type_to_ty(&resolved_field.field_type) {
                               Ok(field_ty) => {
                                   println!("[CheckEnumDef]           Resolved type: {}", display_type(&field_ty));
                                   checker_fields.push(Field {
                                       name: resolved_field.name.clone(),
                                       symbol: resolved_field.symbol,
                                       ty: field_ty,
                                       span: resolved_field.span,
                                   });
                               }
                               Err(e) => {
                                   println!("[CheckEnumDef]           ERROR resolving struct field type: {:?}", e);
                                   self.errors.push(e);
                               }
                           }
                       }
                       println!("[CheckEnumDef]       Added variant symbol {:?} -> name '{}' mapping.", symbol, name);
                       EnumVariant {
                           name: name.clone(),
                           symbol: *symbol,
                           fields: checker_fields,
                           span: *span,
                       }
                   }
              };
             checker_variants.push(checker_variant);
         }
         println!("[CheckEnumDef]   Popping generic scope.");
         self.generic_scopes.pop();

         let enum_def = EnumDef {
             name: resolved_enum.name.clone(),
             symbol: resolved_enum.symbol,
             generic_params: checker_generic_params,
             variants: checker_variants,
             span: resolved_enum.span,
         };
         self.type_ctx.add_type(resolved_enum.symbol, resolved_enum.name.clone(), TypeDef::Enum(enum_def));
          Ok(())
     }

     /// Checks the definition of a trait, adding it to the TypeContext and TraitRepository.
     pub(crate) fn check_trait_definition(&mut self, resolved_trait: &ResolvedTrait) -> TypeResult<()> {
         println!("[CheckTraitDef] Checking trait: {} ({:?})", resolved_trait.name, resolved_trait.symbol);
         let mut generic_param_scope = HashMap::new();
         let definition_context = Some((DefinitionKind::Trait, resolved_trait.symbol));
         let mut checker_generic_params = Vec::new();
         println!("[CheckTraitDef]   Resolving {} generic parameters...", resolved_trait.generic_params.len());

         // Resolve generic parameters first
         for resolved_gen_param in &resolved_trait.generic_params {
             println!("[CheckTraitDef]     Resolving generic param: {}", resolved_gen_param.name);
             match resolve_single_generic_param(self, resolved_gen_param, definition_context) {
                 Ok(checker_param) => {
                     println!("[CheckTraitDef]       Resolved to ID: {:?}", checker_param.id);
                     generic_param_scope.insert(checker_param.name.clone(), checker_param.id);
                     checker_generic_params.push(checker_param);
                 }
                 Err(e) => { self.errors.push(e); }
             }
         }
         self.generic_scopes.push(generic_param_scope);
         println!("[CheckTraitDef]   Pushed generic scope: {:?}", self.generic_scopes.last());

         // Resolve method signatures within the trait's scope
         let mut checker_methods = HashMap::new();
         let parent_kind = Some(DefinitionKind::Trait);
         println!("[CheckTraitDef]   Checking {} method signatures...", resolved_trait.methods.len());
         for resolved_method in &resolved_trait.methods {
             // Find the corresponding ResolvedFunction first
             if let Some(method_resolved_func) = self.definitions.functions.iter().find(|f| f.symbol == resolved_method.func_symbol) {
                 println!("[CheckTraitDef]     Checking method signature: {} ({:?})", method_resolved_func.name, resolved_method.func_symbol);
                 match self.check_function_signature(method_resolved_func, parent_kind) {
                     Ok(signature) => {
                         println!("[CheckTraitDef]       Signature OK.");
                         checker_methods.insert(
                             resolved_method.func_symbol, // Use func_symbol as key
                             TraitMethod {
                                 name: method_resolved_func.name.clone(), // Use name from ResolvedFunction
                                 method_symbol: resolved_method.func_symbol,
                                 signature,
                             }
                         );
                     }
                     Err(e) => { self.errors.push(e); }
                 }
             } else {
                 // Handle case where ResolvedFunction is not found (should be an error)
                 println!("[CheckTraitDef]     ERROR: ResolvedFunction not found for trait method symbol {:?}", resolved_method.func_symbol);
                 self.errors.push(TypeError::InternalError {
                     message: format!("ResolvedFunction not found for trait method symbol {:?}", resolved_method.func_symbol),
                     span: Some(resolved_trait.span), // Approximate span
                 });
             }
         }

         // Resolve associated types defined in the trait
         let mut checker_associated_types = HashMap::new();
         println!("[CheckTraitDef]   Checking {} associated types...", resolved_trait.associated_types.len());
         for resolved_assoc_ty in &resolved_trait.associated_types {
             println!("[CheckTraitDef]     Checking associated type: {} ({:?}) with {} bounds", 
                      resolved_assoc_ty.name, resolved_assoc_ty.symbol, resolved_assoc_ty.bounds.len());
             // Resolve associated type bounds from Vec<Symbol>
             let bounds: Vec<TraitRef> = resolved_assoc_ty.bounds.iter()
                 .filter_map(|bound_symbol| {
                     println!("[CheckTraitDef]       Checking bound symbol: {:?}", bound_symbol);
                     // Look up the symbol to see if it's a known trait ID first.
                     match self.lookup_trait_id_by_symbol(*bound_symbol) {
                         Ok(trait_id) => {
                             println!("[CheckTraitDef]         Bound resolved to trait ID: {:?}", trait_id);
                             Some(TraitRef {
                                 trait_id,
                                 type_arguments: vec![], // Assume no generic args on bound for now
                                 span: resolved_assoc_ty.span, // Use assoc type span as approx
                             })
                         }
                         Err(_) => {
                             // Symbol doesn't exist or isn't a trait.
                             // Error was already reported by lookup_trait_id_by_symbol or get_definition_info.
                             println!("[CheckTraitDef]         Bound symbol {:?} is not a known trait ID.", bound_symbol);
                             None
                         }
                     }
                 })
                 .collect();

             // Create the AssociatedTypeDef
             let assoc_type_def = AssociatedTypeDef {
                 name: resolved_assoc_ty.name.clone(),
                 symbol: resolved_assoc_ty.symbol,
                 bounds, // Store resolved bounds
                 default: None, // TODO: Handle default associated type later
                 span: resolved_assoc_ty.span,
             };
             // Add the definition to our map
             // Add symbol->name mapping as well
             self.type_ctx.add_symbol_name(resolved_assoc_ty.symbol, resolved_assoc_ty.name.clone());
             checker_associated_types.insert(resolved_assoc_ty.symbol, assoc_type_def);
         }
         println!("[CheckTraitDef]   Popping generic scope.");
         self.generic_scopes.pop();

         // Create and add the TraitDef
         let trait_id = self.trait_repo.next_trait_id(); // Get ID before moving
         let trait_def = TraitDef {
             id: trait_id,
             trait_symbol: resolved_trait.symbol,
             name: resolved_trait.name.clone(),
             generic_params: checker_generic_params,
             methods: checker_methods,
             associated_types: checker_associated_types,
             span: resolved_trait.span,
         };

         // Add to TypeContext and TraitRepository
         self.type_ctx.add_trait_symbol(resolved_trait.symbol, resolved_trait.name.clone(), trait_id);
         self.trait_repo.add_trait(trait_def);

         Ok(())
     }

     /// Checks the signature of a function (standalone or associated).
     /// Adds the signature to TypeContext if valid.
     pub(crate) fn check_function_signature(
         &mut self,
         resolved_func: &ResolvedFunction,
         parent_kind: Option<DefinitionKind>, // Context: Is this inside a Trait or Impl?
     ) -> TypeResult<FunctionSignature> {
         println!("[CheckFuncSig] Checking signature for: {} ({:?}), Parent: {:?}", resolved_func.name, resolved_func.symbol, parent_kind);
         let mut generic_param_scope = HashMap::new();
         let definition_context = Some((parent_kind.unwrap_or(DefinitionKind::Function), resolved_func.symbol));
         let mut checker_generic_params = Vec::new();
         println!("[CheckFuncSig]   Resolving {} generic parameters...", resolved_func.generic_params.len());

         // Resolve generic parameters first
         for resolved_gen_param in &resolved_func.generic_params {
             println!("[CheckFuncSig]     Resolving generic param: {}", resolved_gen_param.name);
             match resolve_single_generic_param(self, resolved_gen_param, definition_context) {
                 Ok(checker_param) => {
                     println!("[CheckFuncSig]       Resolved to ID: {:?}", checker_param.id);
                     generic_param_scope.insert(checker_param.name.clone(), checker_param.id);
                     checker_generic_params.push(checker_param);
                 }
                 Err(e) => { self.errors.push(e); }
             }
         }
         self.generic_scopes.push(generic_param_scope);
         println!("[CheckFuncSig]   Pushed generic scope: {:?}", self.generic_scopes.last());

         // Resolve parameter types
         let mut checker_params = Vec::new();
         println!("[CheckFuncSig]   Resolving {} parameters...", resolved_func.parameters.len());
         let mut self_param_kind = SelfParamKind::None;
         for resolved_param in &resolved_func.parameters {
             println!("[CheckFuncSig]     Resolving param: {} ({:?})", resolved_param.name, resolved_param.symbol);
             if resolved_param.name == "self" {
                 if parent_kind.is_none() {
                     println!("[CheckFuncSig]       ERROR: 'self' parameter found in standalone function.");
                     self.errors.push(TypeError::NotAMethod { 
                         method: resolved_func.name.clone(), 
                         ty: "<standalone function>".to_string(), // Indicate it was called on nothing
                         span: resolved_func.span // Use the function span
                     });
                 } else {
                     // Determine SelfParamKind based on resolved_param.param_type?
                     // For now, assume Value if `self` is present in a trait/impl method.
                     println!("[CheckFuncSig]       'self' parameter found, setting SelfParamKind::Value.");
                     self_param_kind = SelfParamKind::Value;
                 }
                 // Don't add `self` to the regular parameter list
                 continue;
             }
             match self.resolve_type_to_ty(&resolved_param.param_type) {
                 Ok(ty) => {
                     println!("[CheckFuncSig]       Resolved type: {}", display_type(&ty));
                     checker_params.push(ParamType {
                         name: resolved_param.name.clone(),
                         ty,
                         span: resolved_param.span,
                     });
                 }
                 Err(e) => { 
                     // Don't add to checker_params if resolution fails
                     println!("[CheckFuncSig]       ERROR resolving param type: {:?}", e);
                     self.errors.push(e);
                 }
             }
         }

         // Resolve return type
         println!("[CheckFuncSig]   Resolving return type: {:?}", resolved_func.return_type);
         let checker_return_type = match self.resolve_type_to_ty(&resolved_func.return_type) {
             Ok(ty) => ty,
             Err(e) => {
                 println!("[CheckFuncSig]     ERROR resolving return type: {:?}", e);
                 self.errors.push(e);
                 Ty::new(TyKind::Error) // Use error type if resolution fails
             }
         };
         println!("[CheckFuncSig]     Resolved return type: {}", display_type(&checker_return_type));

         // Pop generic scope
         println!("[CheckFuncSig]   Popping generic scope.");
         self.generic_scopes.pop();

         let func_sig = FunctionSignature {
             name: resolved_func.name.clone(),
             self_param: if self_param_kind == SelfParamKind::None { None } else { Some(self_param_kind.clone()) }, // Explicitly clone
             generic_params: checker_generic_params,
             params: checker_params,
             return_type: checker_return_type,
             span: resolved_func.span,
         };
         println!("[CheckFuncSig]   Signature construction complete.");

         // Add the function signature to the TypeContext
         // Only add if it's not an error originating from self param issue
         // Corrected check: Use a temporary boolean flag
         let self_error_occurred = self_param_kind == SelfParamKind::None && resolved_func.parameters.iter().any(|p| p.name == "self") && parent_kind.is_none();
         if !self_error_occurred {
             println!("[CheckFuncSig]   Adding signature for symbol {:?} to TypeContext.", resolved_func.symbol);
             self.type_ctx.add_type(
                 resolved_func.symbol,
                 resolved_func.name.clone(),
                 TypeDef::Function(func_sig.clone()),
             );
         } else {
             println!("[CheckFuncSig]   Skipping adding signature due to 'self' error.");
         }

         // Return the created signature
         Ok(func_sig)
     }

     // Function `get_resolved_function` removed as it's unused.
     /// Check the body of an implementation block (after signatures/stubs are done).
     /// This primarily involves checking method bodies against expected signatures.
     pub(crate) fn check_impl_body(&mut self, resolved_impl: &ResolvedImpl) -> TypeResult<()> {
         let impl_desc = format!("impl for symbol {:?}", resolved_impl.impl_symbol);
         println!("[CheckImplBody] Checking body for: {}", impl_desc);
         // Retrieve the ImplDef stub created in Pass 1
         let impl_def = match self.trait_repo.get_impl_def_by_symbol(resolved_impl.impl_symbol) {
             Some(def) => def.clone(), // Clone to avoid borrowing issues below
             None => return Err(TypeError::InternalError {
                 // This might happen if check_impl_definition_stub failed
                 message: format!("ImplDef stub not found for symbol {:?}", resolved_impl.impl_symbol),
                 span: Some(resolved_impl.span),
             })
         };
         println!("[CheckImplBody]   Found ImplDef stub. Implementing type: {}", display_type(&impl_def.implementing_type));

         // Retrieve the TraitDef if this is a trait impl
         let trait_def = if let Some(ref checker_trait_ref) = impl_def.trait_ref {
             // Use the trait_id from the resolved checker_trait_ref
             self.trait_repo.traits.get(&checker_trait_ref.trait_id).cloned()
         } else {
             None // Inherent impl
         };

         // Define a local enum to hold raw signature error data temporarily
         #[derive(Debug)]
         enum LocalSignatureError {
             ParamCountMismatch { name: String, expected: usize, found: usize, span: SourceSpan },
             MethodParamMismatch { trait_name: String, method_name: String, param_index: usize, expected_ty: Ty, found_ty: Ty, span: SourceSpan, error: Box<TypeError> },
             MethodReturnTypeMismatch { trait_name: String, method_name: String, expected_ty: Ty, found_ty: Ty, span: SourceSpan, error: Box<TypeError> },
             GenericParamCountMismatch { kind: String, name: String, expected: usize, found: usize, span: SourceSpan },
             SubstitutionError { method_name: String, error: TypeError, span: Option<SourceSpan> },
             TraitMethodNotFound { trait_name: String, trait_method_symbol: Symbol, span: Option<SourceSpan> },
             InternalError { message: String, span: Option<SourceSpan> },
         }

         // Iterate through methods defined in the impl block
         println!("[CheckImplBody]   Checking {} methods defined in impl...", resolved_impl.methods.len());
         for resolved_assoc_func in &resolved_impl.methods {
             // Look up the function name using the symbol
             let method_name = self.definitions.functions.iter()
                 .find(|f| f.symbol == resolved_assoc_func.func_symbol)
                 .map_or_else(|| "<unknown>".to_string(), |f| f.name.clone());
             println!("[CheckImplBody]     Checking method: {} ({:?}), Trait method symbol: {:?}", 
                      method_name, resolved_assoc_func.func_symbol, resolved_assoc_func.trait_method_symbol);
             // --- Fetch ResolvedFunction for the method ---
             let _method_resolved_func_opt = self.definitions.functions.iter().find(|f| f.symbol == resolved_assoc_func.func_symbol);

             // --- Get Impl Method Signature --- 
             // This was already checked and stored in TypeContext during check_impl_definition_stub
             let impl_method_sig = match self.type_ctx.get_type_by_symbol(&resolved_assoc_func.func_symbol) {
                 Some(TypeDef::Function(sig)) => sig.clone(),
                 _ => {
                     self.errors.push(TypeError::InternalError {
                         // Should not happen if stub check succeeded
                         message: format!("Signature not found for impl method symbol {:?}", resolved_assoc_func.func_symbol),
                         span: Some(resolved_impl.span), // Approximate span
                     });
                     continue; // Skip this method
                 }
             };
             println!("[CheckImplBody]       Retrieved impl method signature from TypeContext.");

             // --- Check Signature Compatibility (if trait impl) and collect raw error data ---
             // Note: Actual signature comparison and error reporting happened in check_impl_definition_stub.
             // This check here is redundant if we trust the stub check passed.
             // We might remove this block later if confidence increases.
             let mut local_signature_errors: Vec<LocalSignatureError> = Vec::new();
             if let (Some(ref current_trait_def), Some(trait_method_symbol)) = (&trait_def, resolved_assoc_func.trait_method_symbol) {
                 // This block now only calculates local_signature_errors
                 println!("[CheckImplBody]       Comparing signature against trait '{}' method symbol {:?}", current_trait_def.name, trait_method_symbol);
                 // Use .get() because current_trait_def.methods is a HashMap
                 if let Some(trait_method) = current_trait_def.methods.get(&trait_method_symbol) {
                     println!("[CheckImplBody]         Found trait method: {}", trait_method.name);
                     let trait_method_sig = &trait_method.signature;

                     // Create a substitution that maps Self to the implementing type
                     let mut combined_subst = Substitution::new();
                     combined_subst.insert_self(&impl_def.implementing_type);
                     
                     // Add generic parameter substitutions
                     for impl_gen_param in &impl_def.generic_params {
                         combined_subst.insert(impl_gen_param.id, Ty::new(TyKind::Var(impl_gen_param.id)));
                     }
                     
                     
                     // Use try block or match to handle potential error from manual_substitute_signature
                     match self.manual_substitute_signature(trait_method_sig, &combined_subst) {
                         Ok(partially_subst_trait_sig) => {
                             // Create fully concrete versions with all Self types replaced
                             let concrete_trait_sig = substitute::substitute_signature_self(
                                 &partially_subst_trait_sig, 
                                 &impl_def.implementing_type, 
                                 20 // Increase recursion limit
                             );
                             
                             let implementing_type = impl_def.implementing_type.clone();

                             // ALSO substitute Self in the impl_method_sig using the implementing_type
                             // Use the substitute module helper with a recursion limit
                             let substituted_impl_method_sig = match substitute::substitute_signature_self(&impl_method_sig, &implementing_type, 10) {
                                 sig => sig,
                                 // Handle potential error during substitution if needed, though unlikely for simple Self
                                 // For now, fallback to original if error (which might still lead to mismatch)
                             };

                             // --- DEBUG PRINT ---
                             // Print types before unification for params
                             
                             // Compare parameters
                             if substituted_impl_method_sig.params.len() != concrete_trait_sig.params.len() {
                                 // --- DEBUG PRINT ---
                                 local_signature_errors.push(LocalSignatureError::ParamCountMismatch {
                                     name: trait_method.name.clone(),
                                     expected: concrete_trait_sig.params.len(), // Trait defines expected number
                                     found: substituted_impl_method_sig.params.len(), // Found in impl
                                     span: impl_method_sig.span,
                                 });
                             } else {
                                 // --- DEBUG PRINT ---
                                 println!("[CheckImplBody] Checking method: {}::{}", current_trait_def.name, trait_method.name);
                                 println!("[CheckImplBody] Comparing Params:");
                                 println!("  Trait Sig Params (Substituted): {:?}", concrete_trait_sig.params.iter().map(|p| display_type(&p.ty)).collect::<Vec<_>>());
                                 println!("  Impl Sig Params (Substituted): {:?}", substituted_impl_method_sig.params.iter().map(|p| display_type(&p.ty)).collect::<Vec<_>>());
                                 println!("  Impl Sig Params (Raw): {:?}", impl_method_sig.params.iter().map(|p| display_type(&p.ty)).collect::<Vec<_>>());
                                 println!("  Implementing Type: {}", display_type(&implementing_type));
                                 for (i, (trait_param, impl_param)) in
                                     concrete_trait_sig.params.iter().zip(substituted_impl_method_sig.params.iter()).enumerate()
                                 {
                                     // --- DEBUG PRINT ---
                                     if let Err(unification_err) = self.unify(&trait_param.ty, &impl_param.ty) {
                                         // --- DEBUG PRINT ---
                                         local_signature_errors.push(LocalSignatureError::MethodParamMismatch {
                                             trait_name: current_trait_def.name.clone(),
                                             method_name: trait_method.name.clone(),
                                             param_index: i,
                                             expected_ty: trait_param.ty.clone(),
                                             found_ty: impl_param.ty.clone(),
                                             span: impl_param.span,
                                             error: Box::new(unification_err),
                                         });
                                     } else {
                                         // --- DEBUG PRINT ---
                                     }
                                 }
                             }

                             // Compare return types
                             // --- DEBUG PRINT ---
                             println!("[CheckImplBody] Comparing Return Types:");
                             println!("  Trait Sig Return (Substituted): {}", display_type(&concrete_trait_sig.return_type));
                             println!("  Impl Sig Return (Substituted): {}", display_type(&substituted_impl_method_sig.return_type));
                             println!("  Impl Sig Return (Raw): {}", display_type(&impl_method_sig.return_type));
                             println!("  Implementing Type: {}", display_type(&implementing_type));
                             if let Err(unification_err) = self.unify(&substituted_impl_method_sig.return_type, &concrete_trait_sig.return_type) { // Unify impl return with trait return
                                 // --- DEBUG PRINT ---
                                 println!("[CheckImplBody]   Return type unification FAILED for {}::{}: {}", current_trait_def.name, trait_method.name, unification_err);
                                 local_signature_errors.push(LocalSignatureError::MethodReturnTypeMismatch {
                                     trait_name: current_trait_def.name.clone(),
                                     method_name: trait_method.name.clone(),
                                     expected_ty: concrete_trait_sig.return_type.clone(),
                                     found_ty: substituted_impl_method_sig.return_type.clone(), // Use substituted type
                                     span: impl_method_sig.span,
                                     error: Box::new(unification_err),
                                 });
                             } else {
                                 // --- DEBUG PRINT ---
                             }

                             // Compare generic parameter counts
                             if substituted_impl_method_sig.generic_params.len() != concrete_trait_sig.generic_params.len() { // Use substituted count
                                 // --- DEBUG PRINT ---
                                 local_signature_errors.push(LocalSignatureError::GenericParamCountMismatch {
                                     kind: "Method".to_string(),
                                     name: trait_method.name.clone(),
                                     expected: concrete_trait_sig.generic_params.len(),
                                     found: substituted_impl_method_sig.generic_params.len(), // Use substituted count
                                     span: impl_method_sig.span,
                                 });
                             } else {
                                 // --- DEBUG PRINT ---
                             }
                         },
                         Err(e) => {
                             // --- DEBUG PRINT ---
                             // Error during substitution, wrap it
                             local_signature_errors.push(LocalSignatureError::SubstitutionError {
                                 method_name: trait_method.name.clone(),
                                 error: e,
                                 span: Some(trait_method_sig.span)
                             });
                         }
                     }
                 } else {
                     println!("[CheckImplBody]       ERROR: Trait method symbol {:?} NOT FOUND in TraitDef '{}'", trait_method_symbol, current_trait_def.name);
                     // Trait method symbol provided, but not found in TraitDef - internal error
                     local_signature_errors.push(LocalSignatureError::TraitMethodNotFound {
                         trait_name: current_trait_def.name.clone(),
                         trait_method_symbol,
                         span: Some(impl_method_sig.span), // Use impl method span
                     });
                 }
                 
                 // Convert local errors to actual TypeErrors and add them
                 for local_err in local_signature_errors {
                     let final_error = match local_err {
                         LocalSignatureError::ParamCountMismatch { name, expected, found, span } => {
                             TypeError::ParamCountMismatch { name, expected, found, span }
                         }
                         LocalSignatureError::MethodParamMismatch { trait_name, method_name, param_index, expected_ty, found_ty, span, error } => {
                             TypeError::MethodParamMismatch {
                                 trait_name,
                                 method_name,
                                 param_index,
                                 expected: display_type(&expected_ty),
                                 found: display_type(&found_ty),
                                 span,
                                 error,
                             }
                         }
                         LocalSignatureError::MethodReturnTypeMismatch { trait_name, method_name, expected_ty, found_ty, span, error } => {
                             TypeError::MethodReturnTypeMismatch {
                                 trait_name,
                                 method_name,
                                 expected: display_type(&expected_ty),
                                 found: display_type(&found_ty),
                                 span,
                                 error,
                             }
                         }
                         LocalSignatureError::GenericParamCountMismatch { kind, name, expected, found, span } => {
                             TypeError::GenericParamCountMismatch { kind, name, expected, found, span }
                         }
                         LocalSignatureError::SubstitutionError { method_name, error, span } => {
                             TypeError::InternalError {
                                 message: format!("Error substituting trait signature for method '{}': {}", method_name, error),
                                 span
                             }
                         }
                         LocalSignatureError::TraitMethodNotFound { trait_name, trait_method_symbol, span } => {
                             TypeError::InternalError {
                                 message: format!("Trait method symbol {:?} not found in trait '{}' during impl check", trait_method_symbol, trait_name),
                                 span
                             }
                         }
                         LocalSignatureError::InternalError { message, span } => {
                             TypeError::InternalError { message, span }
                         }
                     };
                     self.errors.push(final_error);
                 }
             } // End of signature compatibility block

             // Type check the actual method body is handled elsewhere
         }

         // Handle generic scope for impl block
         let mut impl_generic_scope = HashMap::new();
         let setup_failed = false;
         for gen_param in &impl_def.generic_params {
             impl_generic_scope.insert(gen_param.name.clone(), gen_param.id);
         }

         if !setup_failed {
             self.generic_scopes.push(impl_generic_scope); // PUSH SCOPE FOR <T>
         } else {
             // Handle setup failure if necessary
             return Err(TypeError::InternalError { 
                 message: "Failed to setup generic scope for impl".to_string(), 
                 span: Some(resolved_impl.span) 
             });
         }

         // Pop the scope BEFORE returning
         if !setup_failed {
             self.generic_scopes.pop(); // POP SCOPE FOR <T>
         }

         Ok(())
     }

     /// Checks the stub of an `impl` block (trait or inherent).
     /// This involves checking associated function signatures and adding the
     /// impl definition (without checked bodies) to the TraitRepository.
     pub(crate) fn check_impl_definition_stub(&mut self, resolved_impl: &ResolvedImpl) -> TypeResult<()> {
         let impl_desc = format!("impl for symbol {:?}", resolved_impl.impl_symbol);
         println!("[CheckImplStub] Checking stub for: {}", impl_desc);
         let mut checker_generic_params = Vec::new();
         let mut generic_param_scope = HashMap::new();
        let definition_context = Some((DefinitionKind::Impl, resolved_impl.impl_symbol));

        for resolved_gen_param in &resolved_impl.generic_params {
            let checker_param = resolve_single_generic_param(self, resolved_gen_param, definition_context)?;
            generic_param_scope.insert(checker_param.name.clone(), checker_param.id);
            checker_generic_params.push(checker_param);
        }
        // --- SCOPE DEBUG (Impl Stub) ---
        self.generic_scopes.push(generic_param_scope);

        // Resolve the implementing type (Self type)
        let implementing_ty = resolve_type_to_ty(self, &resolved_impl.implementing_type)?;
 
        // Resolve the trait reference, if this is a trait impl
        let checker_trait_ref = if let Some(trait_symbol) = resolved_impl.trait_symbol {
            let trait_id = self.lookup_trait_id_by_symbol(trait_symbol).map_err(|e| {
                // Attach the span of the impl block for better error location
                if let TypeError::UnknownTrait { name, .. } = e {
                    TypeError::UnknownTrait { name, span: resolved_impl.span }
                } else {
                    e // Return original error if it wasn't UnknownTrait
                }
            })?;
 
            // Lookup the TraitDef to check generic parameter count
            let trait_def_opt = self.trait_repo.traits.get(&trait_id);
 
            // Get expected count *before* resolving types
            let expected_arg_count = trait_def_opt.map_or(0, |def| def.generic_params.len());
            let trait_name_for_error = trait_def_opt.map_or_else(|| format!("trait_id_{}", trait_id.0), |d| d.name.clone());
 
            // Resolve trait type arguments provided in the impl block
            let mut type_arguments = Vec::new();
            let resolved_arg_types: Vec<Result<Ty, TypeError>> = 
                if let Some(resolved_args) = &resolved_impl.trait_type_arguments {
                    resolved_args.iter()
                        .map(|arg| resolve_type_to_ty(self, arg))
                        .collect()
                } else {
                    Vec::new()
                };
 
            // Now process the results
            for arg_result in resolved_arg_types {
                match arg_result {
                    Ok(ty) => type_arguments.push(ty),
                    Err(e) => {
                        self.errors.push(e); // Collect error
                        type_arguments.push(Ty::new(TyKind::Error)); // Push error type
                    }
                }
            }
 
            // Check arg count after resolving
            if type_arguments.len() != expected_arg_count {
                self.errors.push(TypeError::GenericArgCountMismatch {
                    kind: "Trait implementation".to_string(),
                    name: trait_name_for_error,
                    expected: expected_arg_count,
                    found: type_arguments.len(),
                    span: resolved_impl.span,
                });
            }
 
            Some(TraitRef {
                trait_id,
                type_arguments, // Use the resolved type arguments
                span: resolved_impl.span,
            })
        } else {
            None // Inherent impl
        };
 
        // Resolve associated type bindings provided in the impl block
        let mut associated_type_bindings = HashMap::new();
        for binding in &resolved_impl.associated_type_bindings {
            match resolve_type_to_ty(self, &binding.bound_type) {
                Ok(bound_ty) => {
                    associated_type_bindings.insert(binding.assoc_type_symbol, bound_ty);
                }
                Err(e) => {
                    self.errors.push(e);
                    // Insert an error type for this binding
                    associated_type_bindings.insert(binding.assoc_type_symbol, Ty::new(TyKind::Error));
                }
            }
        }
 
        // --- Check Signatures for Methods within the Impl Context ---
        let mut checker_methods = HashMap::new();
        for assoc_func in &resolved_impl.methods {
            // Find the ResolvedFunction definition for this method
            if let Some(method_resolved_func) = self.definitions.functions.iter().find(|f| f.symbol == assoc_func.func_symbol) {
                // Check the signature *WITHIN* the impl's generic scope
                match self.check_function_signature(method_resolved_func, Some(DefinitionKind::Impl)) {
                    Ok(_signature) => {
                        // Signature is OK, store the mapping if it's a trait method
                        if let Some(trait_meth_sym) = assoc_func.trait_method_symbol {
                            checker_methods.insert(trait_meth_sym, assoc_func.func_symbol);
                        }
                        // For inherent impls, the signature is stored in TypeContext later
                    }
                    Err(e) => {
                        self.errors.push(e);
                    }
                }
            } else {
                self.errors.push(TypeError::InternalError { 
                    message: format!("ResolvedFunction not found for method symbol {:?}", assoc_func.func_symbol), 
                    span: Some(resolved_impl.span) 
                });
            }
        }
 
        // Pop the generic scope
        if !self.generic_scopes.is_empty() {
            self.generic_scopes.pop();
        }
 
        // Create and store the ImplDef stub (methods checked in Pass 2)
        let impl_def = ImplDef {
            id: ImplId(u32::MAX), // Real ID assigned by trait_repo.add_impl
            impl_symbol: resolved_impl.impl_symbol,
            trait_ref: checker_trait_ref,
            implementing_type: implementing_ty.clone(),
            generic_params: checker_generic_params,
            methods: checker_methods,
            associated_type_bindings,
            span: resolved_impl.span,
        };
        self.trait_repo.add_impl(impl_def);
 
        Ok(())
    }
}

// TODO: Move definitions from the old typecheck/mod.rs Scope, ScopeKind, FnCtx, LoopCtx, LambdaCtx, ScopeCtx trait if they are still needed here.
// TODO: Move the actual checking functions (check_item, check_expr, check_stmt, etc.) here. 

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::trait_repo::TraitId;
    use crate::types::{Field, GenericParamDef, StructDef, TypeDef, PrimitiveType, TypeId, TyKind, Ty, FunctionSignature, ParamType, TraitDef, AssociatedTypeDef};
    use parallax_resolve::ResolveDatabase;
    // Import necessary items from parallax_resolve using suggested paths
    use parallax_resolve::{
        ResolvedDefinitions,
        definitions::*,
        types::{Symbol, ResolvedType, ResolvedGenericParamDef, PrimitiveType as ResolverPrimitiveType, ResolvedAssociatedType, ResolvedTrait, ResolvedImpl}
    };
    // Import DB traits
    use parallax_source::SourceDatabase;
    use parallax_syntax::SyntaxDatabase;
    use salsa::Database;
    use std::{collections::HashMap, sync::{Arc, Mutex}};

    // --- Test Setup (Reusing existing setup) ---
    #[salsa::db]
    #[derive(Default, Clone)]
    pub struct DummyDb {
        storage: salsa::Storage<Self>,
    }

    impl salsa::Database for DummyDb {
        fn salsa_event(&self, event: &dyn Fn() -> salsa::Event) {}
    }


    #[salsa::db]
    impl SyntaxDatabase for DummyDb {}
    #[salsa::db]
    impl SourceDatabase for DummyDb {}
    #[salsa::db]
    impl ResolveDatabase for DummyDb {}
    #[salsa::db]
    impl TypeDatabase for DummyDb {}

    fn dummy_span() -> SourceSpan {
        SourceSpan::from((0, 0))
    }

    // Helper to create checker with provided definitions
    fn setup_checker(definitions: ResolvedDefinitions) -> TypeChecker<'static> {
        let db_mock = DummyDb::default();
        let db_leaked: &'static DummyDb = Box::leak(Box::new(db_mock));
        // Leak the provided definitions
        let defs_leaked: &'static ResolvedDefinitions = Box::leak(Box::new(definitions));
        let type_ctx = TypeContext::new();
        let trait_repo = TraitRepository::new();
        TypeChecker::new(db_leaked, defs_leaked, type_ctx, trait_repo)
    }

    // Basic checker with empty definitions
    fn setup_basic_checker() -> TypeChecker<'static> {
        setup_checker(ResolvedDefinitions::default())
    }

    // Helper to create checker with a pre-defined struct and trait
    fn setup_checker_with_defs() -> TypeChecker<'static> {
        let struct_sym = Symbol::new(1);
        let trait_sym = Symbol::new(2);
        let func_sym = Symbol::new(3);
        let mut definitions = ResolvedDefinitions::default();
        definitions.structs.push(ResolvedStruct {
            name: "MyStruct".to_string(),
            symbol: struct_sym,
            fields: vec![],
            generic_params: vec![],
            span: dummy_span(),
            module_symbol: Symbol::new(0), // Added
            is_public: true, // Added
        });
         definitions.traits.push(ResolvedTrait {
            name: "MyTrait".to_string(),
            symbol: trait_sym,
            generic_params: vec![],
            methods: vec![],
            associated_types: vec![],
            span: dummy_span(),
            module_symbol: Symbol::new(0), // Added
            is_public: true, // Added
            supertraits: vec![], // Added
         });
          definitions.functions.push(ResolvedFunction {
            name: "my_func".to_string(),
            symbol: func_sym,
            parameters: vec![],
            generic_params: vec![],
            // Use aliased ResolverPrimitiveType::Unit if applicable, else map manually
            return_type: ResolvedType::Primitive(ResolverPrimitiveType::Unit),
            body: None,
            span: dummy_span(),
            is_effectful: false,
            module_symbol: Symbol::new(0), // Added
            is_public: true, // Added
          });

        // Create checker using the setup function
        let mut checker = setup_checker(definitions);

        // Add corresponding type/trait info to contexts (already done by setup_checker using the defs? No, TypeContext/TraitRepo are empty initially)
        // We still need to populate the checker's contexts manually after creation
        checker.type_ctx.add_type(struct_sym, "MyStruct".to_string(), TypeDef::Struct(StructDef { name: "MyStruct".to_string(), symbol: struct_sym, generic_params: vec![], fields: vec![], span: dummy_span() }));
        let trait_id = checker.trait_repo.next_trait_id();
        checker.trait_repo.add_trait(TraitDef { id: trait_id, trait_symbol: trait_sym, name: "MyTrait".to_string(), generic_params: vec![], methods: HashMap::new(), associated_types: HashMap::new(), span: dummy_span() });
        checker.type_ctx.add_trait_symbol(trait_sym, "MyTrait".to_string(), trait_id);
        // Use crate::types::PrimitiveType for the checker's internal types
        checker.type_ctx.add_type(func_sym, "my_func".to_string(), TypeDef::Function(FunctionSignature { name: "my_func".to_string(), self_param: None, generic_params: vec![], params: vec![], return_type: Ty::new(TyKind::Primitive(PrimitiveType::Unit)), span: dummy_span() }));

        checker
    }

     // Use crate::types::PrimitiveType
     fn ty_prim(prim: PrimitiveType) -> Ty {
        Ty::new(TyKind::Primitive(prim))
    }

    fn ty_var(id: u32) -> Ty {
        Ty::new(TyKind::Var(TypeId(id)))
    }

    // --- Tests for Core Checker Helpers ---
    #[test]
    fn test_fresh_infer_var() {
        let mut checker = setup_basic_checker();
        let var1 = checker.fresh_infer_var(dummy_span());
        let var2 = checker.fresh_infer_var(dummy_span());
        assert!(matches!(var1.kind, TyKind::Var(_)));
        assert!(matches!(var2.kind, TyKind::Var(_)));
        assert_ne!(var1, var2);
        if let (TyKind::Var(id1), TyKind::Var(id2)) = (var1.kind, var2.kind) {
            assert_ne!(id1, id2);
        }
    }

    #[test]
    fn test_resolve_type_simple() {
        let checker = setup_basic_checker();
        let i32_ty = ty_prim(PrimitiveType::I32);
        let resolved = checker.resolve_type(&i32_ty);
        assert_eq!(resolved, i32_ty);
    }

    #[test]
    fn test_resolve_type_with_var() {
        let mut checker = setup_basic_checker();
        let var = checker.fresh_infer_var(dummy_span());
        let i32_ty = ty_prim(PrimitiveType::I32);
        checker.inference_ctx.unify(&var, &i32_ty).unwrap();

        let resolved = checker.resolve_type(&var);
        assert_eq!(resolved.kind, TyKind::Primitive(PrimitiveType::I32));
    }

    #[test]
    fn test_unify_simple_match() {
        let mut checker = setup_basic_checker();
        let i32_ty1 = ty_prim(PrimitiveType::I32);
        let i32_ty2 = ty_prim(PrimitiveType::I32);
        let result = checker.unify(&i32_ty1, &i32_ty2);
        assert!(result.is_ok());
        assert!(result.unwrap().is_empty());
    }

    #[test]
    fn test_unify_simple_mismatch() {
        let mut checker = setup_basic_checker();
        let i32_ty = ty_prim(PrimitiveType::I32);
        let bool_ty = ty_prim(PrimitiveType::Bool);
        let result = checker.unify(&i32_ty, &bool_ty);
        assert!(result.is_err());
        assert!(matches!(result.err().unwrap(), TypeError::TypeMismatch { .. }));
    }

    #[test]
    fn test_unify_var_concrete() {
        let mut checker = setup_basic_checker();
        let var = checker.fresh_infer_var(dummy_span());
        let i32_ty = ty_prim(PrimitiveType::I32);
        let result = checker.unify(&var, &i32_ty);
        assert!(result.is_ok());
        let subst = result.unwrap();
        let resolved_var = var.apply_subst(&subst);
        assert_eq!(resolved_var.kind, i32_ty.kind);
    }

    #[test]
    fn test_unify_concrete_var() {
        let mut checker = setup_basic_checker();
        let var = checker.fresh_infer_var(dummy_span());
        let i32_ty = ty_prim(PrimitiveType::I32);
        let result = checker.unify(&i32_ty, &var);
        assert!(result.is_ok());
        let subst = result.unwrap();
        let resolved_var = var.apply_subst(&subst);
        assert_eq!(resolved_var.kind, i32_ty.kind);
    }

    #[test]
    fn test_unify_vars() {
        let mut checker = setup_basic_checker();
        let var1 = checker.fresh_infer_var(dummy_span());
        let var2 = checker.fresh_infer_var(dummy_span());
        let result = checker.unify(&var1, &var2);
        assert!(result.is_ok());
        let subst = result.unwrap();
        let resolved1 = var1.apply_subst(&subst);
        let resolved2 = var2.apply_subst(&subst);
        assert_eq!(resolved1, resolved2);
        assert!(matches!(resolved1.kind, TyKind::Var(_)));
    }

    #[test]
    fn test_unify_occurs_check() {
         let mut checker = setup_basic_checker();
         let var = checker.fresh_infer_var(dummy_span());
         let vec_of_var = Ty::new(TyKind::Named {
             name: "Vec".to_string(),
             symbol: None,
             args: vec![var.clone()],
         });
         let result = checker.unify(&var, &vec_of_var);
         assert!(result.is_err());
         assert!(matches!(result.err().unwrap(), TypeError::InferenceRecursionLimit { .. } | TypeError::TypeMismatch { .. }));
    }

    #[test]
    fn test_get_name_for_symbol() {
        let checker = setup_checker_with_defs();
        let struct_sym = Symbol::new(1);
        let func_sym = Symbol::new(3);
        let unknown_sym = Symbol::new(99);

        assert_eq!(checker.get_name_for_symbol(struct_sym).unwrap(), "MyStruct");
        assert_eq!(checker.get_name_for_symbol(func_sym).unwrap(), "my_func");
        assert!(checker.get_name_for_symbol(unknown_sym).is_err());
    }

    #[test]
    fn test_get_definition_info() {
         let checker = setup_checker_with_defs();
         let struct_sym = Symbol::new(1);
         let trait_sym = Symbol::new(2);
         let func_sym = Symbol::new(3);
         let unknown_sym = Symbol::new(99);

         let (kind1, name1, _) = checker.get_definition_info(struct_sym).unwrap();
         assert_eq!(kind1, DefinitionKind::Struct);
         assert_eq!(name1, "MyStruct");

         let (kind2, name2, _) = checker.get_definition_info(trait_sym).unwrap();
         assert_eq!(kind2, DefinitionKind::Trait);
         assert_eq!(name2, "MyTrait");

         let (kind3, name3, _) = checker.get_definition_info(func_sym).unwrap();
         assert_eq!(kind3, DefinitionKind::Function);
         assert_eq!(name3, "my_func");

         assert!(checker.get_definition_info(unknown_sym).is_none());
    }

    #[test]
    fn test_lookup_trait_id_by_symbol() {
        let checker = setup_checker_with_defs();
        let trait_sym = Symbol::new(2);
        let unknown_sym = Symbol::new(99);

        let trait_id = checker.lookup_trait_id_by_symbol(trait_sym).unwrap();
        assert_eq!(trait_id.0, 0);

        assert!(checker.lookup_trait_id_by_symbol(unknown_sym).is_err());
        let err = checker.lookup_trait_id_by_symbol(unknown_sym).err().unwrap();
        assert!(matches!(err, TypeError::UnknownTrait { .. }));
    }

    // --- Existing Trait Tests (Keep them, but update setup) ---
    const DEBUG_TRAIT_SYMBOL: Symbol = Symbol(500);
    const DEBUG_TRAIT_ID: TraitId = TraitId(50);
    const CLONE_TRAIT_SYMBOL: Symbol = Symbol(501);
    const CLONE_TRAIT_ID: TraitId = TraitId(51);

    fn add_dummy_trait(checker: &mut TypeChecker, name: &str, symbol: Symbol, id: TraitId) {
        let trait_def = TraitDef {
            id,
            trait_symbol: symbol,
            name: name.to_string(),
            generic_params: vec![],
            methods: HashMap::new(),
            associated_types: HashMap::new(),
            span: dummy_span(),
        };
        checker.trait_repo.add_trait(trait_def);
        checker.type_ctx.add_trait_symbol(symbol, name.to_string(), id);
    }

    #[test]
    fn test_check_trait_def_simple_assoc_type() {
        let trait_symbol = Symbol::new(100);
        let assoc_type_symbol = Symbol::new(101);
        let resolved_trait = ResolvedTrait {
            symbol: trait_symbol,
            name: "MyIterator".to_string(),
            module_symbol: Symbol::new(0),
            is_public: true,
            generic_params: vec![],
            supertraits: vec![],
            methods: vec![],
            associated_types: vec![
                ResolvedAssociatedType {
                    symbol: assoc_type_symbol,
                    name: "Item".to_string(),
                    bounds: vec![],
                    span: dummy_span(),
                }
            ],
            span: dummy_span(),
        };
        let mut definitions = ResolvedDefinitions::default();
        definitions.traits.push(resolved_trait);
        let mut checker = setup_checker(definitions);

        // Call the function under test
        let result = checker.check_trait_definition(&checker.definitions.traits[0]); // Get trait from checker defs
        assert!(result.is_ok());
        assert!(checker.errors.is_empty());

        let trait_def_opt = checker.trait_repo.get_trait_def_by_symbol(trait_symbol);
        assert!(trait_def_opt.is_some());
        let trait_def = trait_def_opt.unwrap();
        assert_eq!(trait_def.name, "MyIterator");
        assert_eq!(trait_def.associated_types.len(), 1);
        assert!(trait_def.associated_types.contains_key(&assoc_type_symbol));
        let assoc_ty_def = trait_def.associated_types.get(&assoc_type_symbol).unwrap();
        assert_eq!(assoc_ty_def.name, "Item");
        assert_eq!(assoc_ty_def.symbol, assoc_type_symbol);
        assert!(assoc_ty_def.bounds.is_empty());
        assert!(assoc_ty_def.default.is_none());
    }

    #[test]
    fn test_check_trait_def_assoc_type_bounds() {
        let trait_symbol = Symbol::new(200);
        let assoc_type_symbol = Symbol::new(201);
        let resolved_trait = ResolvedTrait {
            symbol: trait_symbol,
            name: "MyIterable".to_string(),
            module_symbol: Symbol::new(0),
            is_public: true,
            generic_params: vec![],
            supertraits: vec![],
            methods: vec![],
            associated_types: vec![
                ResolvedAssociatedType {
                    symbol: assoc_type_symbol,
                    name: "Item".to_string(),
                    bounds: vec![DEBUG_TRAIT_SYMBOL],
                    span: dummy_span(),
                }
            ],
            span: dummy_span(),
        };
        let mut definitions = ResolvedDefinitions::default();
        definitions.traits.push(resolved_trait);
        let mut checker = setup_checker(definitions);
        add_dummy_trait(&mut checker, "Debug", DEBUG_TRAIT_SYMBOL, DEBUG_TRAIT_ID);

        let result = checker.check_trait_definition(&checker.definitions.traits[0]);
        assert!(result.is_ok(), "check_trait_definition failed: {:?}", result.err());
        assert!(checker.errors.is_empty(), "Checker errors found: {:?}", checker.errors);

        let trait_def_opt = checker.trait_repo.get_trait_def_by_symbol(trait_symbol);
        assert!(trait_def_opt.is_some());
        let trait_def = trait_def_opt.unwrap();
        assert_eq!(trait_def.name, "MyIterable");
        assert_eq!(trait_def.associated_types.len(), 1);
        let assoc_ty_def = trait_def.associated_types.get(&assoc_type_symbol).unwrap();
        assert_eq!(assoc_ty_def.name, "Item");
        assert_eq!(assoc_ty_def.bounds.len(), 1);
        let bound_ref = &assoc_ty_def.bounds[0];
        assert_eq!(bound_ref.trait_id, DEBUG_TRAIT_ID);
        assert!(bound_ref.type_arguments.is_empty());
    }

    #[test]
    fn test_check_trait_def_assoc_type_bound_not_trait() {
        let struct_symbol = Symbol::new(600);
        let trait_symbol = Symbol::new(210);
        let assoc_type_symbol = Symbol::new(211);

        let mut definitions = ResolvedDefinitions::default();
        definitions.structs.push(ResolvedStruct {
            symbol: struct_symbol,
            name: "NotATrait".to_string(),
            module_symbol: Symbol::new(0),
            is_public: true,
            generic_params: vec![],
            fields: vec![],
            span: dummy_span(),
        });
         definitions.traits.push(ResolvedTrait {
            symbol: trait_symbol,
            name: "MyProblematicIterable".to_string(),
            module_symbol: Symbol::new(0),
            is_public: true,
            generic_params: vec![],
            supertraits: vec![],
            methods: vec![],
            associated_types: vec![
                ResolvedAssociatedType {
                    symbol: assoc_type_symbol,
                    name: "Item".to_string(),
                    bounds: vec![struct_symbol],
                    span: dummy_span(),
                }
            ],
            span: dummy_span(),
        });
        let mut checker = setup_checker(definitions);
        // Add the struct def to the type context so get_definition_info works
         checker.type_ctx.add_type(struct_symbol, "NotATrait".to_string(), TypeDef::Struct(StructDef { name: "NotATrait".to_string(), symbol: struct_symbol, generic_params: vec![], fields: vec![], span: dummy_span() }));

        let result = checker.check_trait_definition(&checker.definitions.traits[0]);
        assert!(result.is_ok());
        assert_eq!(checker.errors.len(), 1, "Expected 1 error, found {:?}", checker.errors);
        assert!(matches!(checker.errors[0], TypeError::NotATrait { ref found, .. } if found == "NotATrait"),
                "Expected NotATrait error, found {:?}", checker.errors[0]);

        let trait_def_opt = checker.trait_repo.get_trait_def_by_symbol(trait_symbol);
        assert!(trait_def_opt.is_some());
        let trait_def = trait_def_opt.unwrap();
        let assoc_ty_def_opt = trait_def.associated_types.get(&assoc_type_symbol);
        assert!(assoc_ty_def_opt.is_some());
        let assoc_ty_def = assoc_ty_def_opt.unwrap();
        assert!(assoc_ty_def.bounds.is_empty());
    }

    #[test]
    fn test_check_trait_def_assoc_type_bound_unknown_trait() {
        let unknown_trait_symbol = Symbol::new(700);
        let trait_symbol = Symbol::new(220);
        let assoc_type_symbol = Symbol::new(221);
        let resolved_trait = ResolvedTrait {
            symbol: trait_symbol,
            name: "MyOtherProblematicIterable".to_string(),
            module_symbol: Symbol::new(0),
            is_public: true,
            generic_params: vec![],
            supertraits: vec![],
            methods: vec![],
            associated_types: vec![
                ResolvedAssociatedType {
                    symbol: assoc_type_symbol,
                    name: "Item".to_string(),
                    bounds: vec![unknown_trait_symbol],
                    span: dummy_span(),
                }
            ],
            span: dummy_span(),
        };
        let mut definitions = ResolvedDefinitions::default();
        definitions.traits.push(resolved_trait);
        let mut checker = setup_checker(definitions);

        let result = checker.check_trait_definition(&checker.definitions.traits[0]);
        assert!(result.is_ok());
        assert_eq!(checker.errors.len(), 1, "Expected 1 error for unknown trait bound, found {:?}", checker.errors);
        assert!(matches!(checker.errors[0], TypeError::UnknownTrait { ref name, .. } if name.contains("symbol_700")),
                "Expected UnknownTrait error for symbol_700, found {:?}", checker.errors[0]);

        let trait_def = checker.trait_repo.get_trait_def_by_symbol(trait_symbol).unwrap();
        let assoc_ty_def = trait_def.associated_types.get(&assoc_type_symbol).unwrap();
        assert!(assoc_ty_def.bounds.is_empty());
    }

    #[test]
    fn test_check_trait_def_assoc_type_multiple_bounds() {
        let trait_symbol = Symbol::new(230);
        let assoc_type_symbol = Symbol::new(231);
        let resolved_trait = ResolvedTrait {
            symbol: trait_symbol,
            name: "MyCloneableIterable".to_string(),
            module_symbol: Symbol::new(0),
            is_public: true,
            generic_params: vec![],
            supertraits: vec![],
            methods: vec![],
            associated_types: vec![
                ResolvedAssociatedType {
                    symbol: assoc_type_symbol,
                    name: "Item".to_string(),
                    bounds: vec![DEBUG_TRAIT_SYMBOL, CLONE_TRAIT_SYMBOL],
                    span: dummy_span(),
                }
            ],
            span: dummy_span(),
         };
         let mut definitions = ResolvedDefinitions::default();
         definitions.traits.push(resolved_trait);
         let mut checker = setup_checker(definitions);
         add_dummy_trait(&mut checker, "Debug", DEBUG_TRAIT_SYMBOL, DEBUG_TRAIT_ID);
         add_dummy_trait(&mut checker, "Clone", CLONE_TRAIT_SYMBOL, CLONE_TRAIT_ID);

         let result = checker.check_trait_definition(&checker.definitions.traits[0]);
         assert!(result.is_ok(), "check_trait_definition failed: {:?}", result.err());
         assert!(checker.errors.is_empty(), "Checker errors found: {:?}", checker.errors);

         let trait_def = checker.trait_repo.get_trait_def_by_symbol(trait_symbol).unwrap();
         let assoc_ty_def = trait_def.associated_types.get(&assoc_type_symbol).unwrap();
         assert_eq!(assoc_ty_def.bounds.len(), 2);
         let bound_ids: std::collections::HashSet<TraitId> = assoc_ty_def.bounds.iter().map(|b| b.trait_id).collect();
         assert!(bound_ids.contains(&DEBUG_TRAIT_ID));
         assert!(bound_ids.contains(&CLONE_TRAIT_ID));
         assert!(assoc_ty_def.bounds.iter().all(|b| b.type_arguments.is_empty()));
     }
}