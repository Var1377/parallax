use std::collections::HashMap;
use std::collections::VecDeque;
use std::sync::Arc;

use miette::SourceSpan;
use parallax_resolve::{
    definitions::DefinitionKind,
    types::{
        PrimitiveType as ResolverPrimitiveType, ResolvedDefinitions, ResolvedEnum,
        ResolvedEnumVariant, ResolvedFunction, ResolvedGenericParamDef, ResolvedImpl,
        ResolvedModuleStructure, ResolvedStruct, ResolvedTrait, ResolvedType, Symbol,
    },
};

// Add dummy_span function instead of a const
pub(crate) fn dummy_span() -> SourceSpan {
    SourceSpan::new(0.into(), 0)
}

use crate::{
    error::{display_type, TypeError, TypeResult},
    inference::{InferenceContext, Substitution, TypeEnvironment},
    traits::{ImplId, TraitId, TraitRepository},
    types::{
        FunctionSignature, GenericParamDef, ParamType, PrimitiveType as ParallaxPrimitiveType,
        TraitDef as CheckerTraitDef, TraitRef as CheckerTraitRef, Ty, TyKind, TypeContext, TypeDef,
        TypeId, TypedDefinitions, TypedExpr, TypedExprKind, TypedModule, TypedVariant,
    },
    TypeDatabase,
};

pub mod aggregates;
pub mod control_flow;
pub mod expressions;
pub mod helpers;
pub mod invocations;
pub mod operators;
pub mod patterns;

pub enum CheckItem<'a> {
    Struct(&'a ResolvedStruct),
    Enum(&'a ResolvedEnum),
    Function(&'a ResolvedFunction),
    Trait(&'a ResolvedTrait),
    Impl(&'a ResolvedImpl),
}

pub(crate) struct TypeChecker<'a> {
    _db: &'a dyn TypeDatabase,
    resolved_defs: &'a ResolvedDefinitions,
    _core_traits: &'a Vec<(String, Symbol)>,
    _intrinsics: &'a Vec<(String, Symbol)>,
    pub(crate) inference_ctx: InferenceContext,
    pub(crate) type_ctx: TypeContext,
    pub(crate) trait_repo: TraitRepository,
    pub(crate) errors: Vec<TypeError>,
    pub(crate) typed_definitions: TypedDefinitions,
    generic_scopes: Vec<HashMap<String, TypeId>>,
    current_definition_context: Option<(DefinitionKind, Symbol)>,
    pub(crate) _warnings: Vec<String>,
    _current_module_symbol: Option<Symbol>,
    _current_function_return_type: Option<Ty>,
    pub(crate) _type_env: Arc<TypeEnvironment>,
}

impl<'a> TypeChecker<'a> {
    pub(crate) fn new(
        db: &'a dyn TypeDatabase,
        resolved_defs: &'a ResolvedDefinitions,
        core_traits: &'a Vec<(String, Symbol)>,
        intrinsics: &'a Vec<(String, Symbol)>,
    ) -> Self {
        let mut checker = Self {
            _db: db,
            resolved_defs,
            _core_traits: core_traits,
            _intrinsics: intrinsics,
            inference_ctx: InferenceContext::new(),
            type_ctx: TypeContext::new(),
            trait_repo: TraitRepository::new(),
            errors: Vec::new(),
            typed_definitions: TypedDefinitions::default(),
            generic_scopes: Vec::new(),
            current_definition_context: None,
            _warnings: Vec::new(),
            _current_module_symbol: None,
            _current_function_return_type: None,
            _type_env: Arc::new(TypeEnvironment::new()),
        };

        // --- Populate TypeContext and TraitRepository from ResolvedDefinitions --- 
        // Order matters: Traits must be added before impls that use them.
        
        // 1. Add all non-impl definitions first (structs, enums, functions, traits)
        for r#struct in &resolved_defs.structs {
            match checker.resolve_and_add_struct_def(r#struct) {
                Ok(_) => {},
                Err(e) => checker.errors.push(e),
            }
        }
        for r#enum in &resolved_defs.enums {
            match checker.resolve_and_add_enum_def(r#enum) {
                Ok(_) => {},
                Err(e) => checker.errors.push(e),
            }
        }
        for r#trait in &resolved_defs.traits {
             match checker.resolve_and_add_trait_def(r#trait) {
                 Ok(_) => {},
                 Err(e) => checker.errors.push(e),
             }
         }
        // Add function signatures later, potentially after all types are known?
        // Or add placeholder Function TypeDefs now?
        // Let's add placeholders now.
         for func in &resolved_defs.functions {
             match checker.resolve_and_add_function_signature(func, None) { // None for parent context initially
                 Ok(_) => {}, 
                 Err(e) => checker.errors.push(e),
             }
         }

        // 2. Add impls (needs traits and types to be known)
        for r#impl in &resolved_defs.impls {
            match checker.resolve_and_add_impl_def(r#impl) {
                Ok(_) => {},
                Err(e) => checker.errors.push(e),
            }
        }
        
        checker
    }

    // Placeholder methods for population logic (to be implemented or moved)
    fn resolve_and_add_struct_def(&mut self, r_struct: &ResolvedStruct) -> TypeResult<()> {
        let original_context = self.current_definition_context.clone();
        self.current_definition_context = Some((DefinitionKind::Struct, r_struct.symbol));

        let mut generic_params = Vec::with_capacity(r_struct.generic_params.len());
        let mut struct_generic_scope = HashMap::new();
        for gp in &r_struct.generic_params {
            let resolved_gp = self.resolve_single_generic_param(gp, self.current_definition_context)?; // Pass context
            struct_generic_scope.insert(resolved_gp.name.clone(), resolved_gp.id);
            generic_params.push(resolved_gp);
        }

        self.generic_scopes.push(struct_generic_scope);

        let mut fields = Vec::with_capacity(r_struct.fields.len());
        for field in &r_struct.fields {
            let field_ty = self.resolve_type_to_ty(&field.field_type)?; 
            fields.push(crate::types::Field { // Use types::Field
                name: field.name.clone(),
                symbol: field.symbol,
                ty: field_ty,
                span: field.span,
            });
        }

        self.generic_scopes.pop();

        let struct_def = crate::types::StructDef {
            name: r_struct.name.clone(),
            symbol: r_struct.symbol,
            generic_params,
            fields,
            span: r_struct.span,
        };

        let type_def = TypeDef::Struct(struct_def);
        self.type_ctx.add_type(r_struct.symbol, r_struct.name.clone(), type_def);

        self.current_definition_context = original_context;
        Ok(())
    }
    fn resolve_and_add_enum_def(&mut self, r_enum: &ResolvedEnum) -> TypeResult<()> {
        let mut typed_variants = Vec::new();
        for variant in &r_enum.variants {
            let typed_variant: TypedVariant = match variant {
                ResolvedEnumVariant::Unit { name, symbol, span } => {
                    TypedVariant::Unit {
                        name: name.clone(),
                        symbol: *symbol,
                        span: *span,
                    }
                },
                ResolvedEnumVariant::Tuple { name, symbol, fields: resolved_tuple_fields, span } => {
                    let mut types = Vec::new();
                    for resolved_ty in resolved_tuple_fields.iter() {
                        let ty = self.resolve_type_to_ty(resolved_ty)?;
                        types.push(ty);
                    }
                    TypedVariant::Tuple {
                        name: name.clone(),
                        symbol: *symbol,
                        types,
                        span: *span,
                    }
                },
                ResolvedEnumVariant::Struct { name, symbol, fields: resolved_fields, span } => {
                    let mut fields = Vec::new();
                    for resolved_field in resolved_fields {
                        let ty = self.resolve_type_to_ty(&resolved_field.field_type)?;
                        fields.push(crate::types::TypedField {
                            name: resolved_field.name.clone(),
                            symbol: resolved_field.symbol,
                            ty,
                            is_public: resolved_field.is_public,
                            span: resolved_field.span,
                        });
                    }
                    TypedVariant::Struct {
                        name: name.clone(),
                        symbol: *symbol,
                        fields,
                        span: *span,
                    }
                },
            };
            typed_variants.push(typed_variant);
        }

        let generic_params = r_enum
            .generic_params
            .iter()
            .map(|p| self.resolve_single_generic_param(p, Some((DefinitionKind::Enum, r_enum.symbol)))) // Pass context
            .collect::<TypeResult<Vec<_>>>()?;

        let typed_enum = crate::types::TypedEnum {
            name: r_enum.name.clone(),
            symbol: r_enum.symbol,
            variants: typed_variants,
            generic_params,
            span: r_enum.span,
        };
        
        self.typed_definitions.add_enum(r_enum.symbol, typed_enum);

        Ok(())
    }
    fn resolve_and_add_trait_def(&mut self, r_trait: &ResolvedTrait) -> TypeResult<()> {
        let original_context = self.current_definition_context.clone();
        self.current_definition_context = Some((DefinitionKind::Trait, r_trait.symbol));

        let trait_id = self.trait_repo.next_trait_id();

        // Resolve generic parameters defined on the trait itself
        let mut generic_params = Vec::with_capacity(r_trait.generic_params.len());
        let mut trait_generic_scope = HashMap::new();
        for gp_def in &r_trait.generic_params {
            let resolved_gp = self.resolve_single_generic_param(gp_def, self.current_definition_context)?;
            trait_generic_scope.insert(resolved_gp.name.clone(), resolved_gp.id);
            generic_params.push(resolved_gp);
        }
        self.generic_scopes.push(trait_generic_scope);

        // --- Associated Types - Placeholder --- 
        // TODO: Resolve associated types properly once ResolvedTrait includes them.
        let associated_types = HashMap::new(); // Empty for now
        /* 
        let mut associated_types = HashMap::new();
        for assoc_type_def in &r_trait.associated_types { // This field doesn't exist yet
            let at_def = crate::types::AssociatedTypeDef {
                name: assoc_type_def.name.clone(),
                symbol: assoc_type_def.symbol,
                // bounds: vec![], // TODO: Resolve bounds for associated types
                default: if let Some(resolved_default) = &assoc_type_def.default_type {
                    Some(self.resolve_type_to_ty(resolved_default)?)
                } else {
                    None
                },
                span: assoc_type_def.span,
            };
            associated_types.insert(at_def.symbol, at_def);
        }
        */

        // Resolve method signatures defined within the trait
        let mut methods = HashMap::new();
        for resolved_assoc_func in &r_trait.methods { // Iterate through ResolvedAssociatedFunction
             let method_symbol = resolved_assoc_func.func_symbol; // Get the actual function symbol
             
             // Find the full ResolvedFunction definition for the method symbol
            let resolved_method = self.find_resolved_function(method_symbol)?; // Use helper to find it

             // Resolve the signature within the trait's context (Trait Definition)
            let signature = self.resolve_function_signature(&resolved_method, self.current_definition_context)?;

             let trait_method = crate::types::TraitMethod {
                 name: signature.name.clone(), // Use name from resolved signature
                 method_symbol, // The symbol of the function itself
                 signature,
             };
             methods.insert(method_symbol, trait_method); // Use the function symbol as key
        }
        self.generic_scopes.pop();

        // Create the CheckerTraitDef
        let checker_trait_def = crate::types::TraitDef { // Explicit path
            id: trait_id,
            trait_symbol: r_trait.symbol,
            name: r_trait.name.clone(),
            generic_params,
            // supertraits: vec![], // TODO: Resolve supertraits
            methods, // This now correctly uses Symbol as key
            associated_types, // Use the empty map for now
            span: r_trait.span,
        };

        // Add to TraitRepository
        self.trait_repo.add_trait(checker_trait_def);

        // Add symbol->id mapping to TypeContext
        self.type_ctx.add_trait_symbol(r_trait.symbol, r_trait.name.clone(), trait_id);

        self.current_definition_context = original_context;
        Ok(())
    }
    fn resolve_and_add_function_signature(&mut self, r_func: &ResolvedFunction, context: Option<(DefinitionKind, Symbol)>) -> TypeResult<()> {
        let original_context = self.current_definition_context.clone();
        self.current_definition_context = context.or_else(|| Some((DefinitionKind::Function, r_func.symbol))); // Set context if not provided

        // Check if the signature for this symbol already exists (e.g., from trait processing)
        if self.type_ctx.get_type_by_symbol(&r_func.symbol).is_some() {
             println!(" Signature for func {} ({:?}) already exists, skipping.", r_func.name, r_func.symbol);
            self.current_definition_context = original_context; // Restore context before returning
            return Ok(());
        }
        
        // Resolve the signature itself using the helper function
        // The helper manages pushing/popping generic scopes internally
        let signature = self.resolve_function_signature(r_func, self.current_definition_context)?;
        
        // Add the resolved signature to the TypeContext
        let type_def = TypeDef::Function(signature);
        self.type_ctx.add_type(r_func.symbol, r_func.name.clone(), type_def);

        self.current_definition_context = original_context; // Restore original context
        Ok(()) 
    }
    fn resolve_and_add_impl_def(&mut self, r_impl: &ResolvedImpl) -> TypeResult<()> {
        let original_context = self.current_definition_context.clone();
        self.current_definition_context = Some((DefinitionKind::Impl, r_impl.impl_symbol));

        // Resolve generic parameters defined on the impl block itself
        let mut impl_generic_params = Vec::with_capacity(r_impl.generic_params.len());
        let mut impl_generic_scope = HashMap::new();
        for gp_def in &r_impl.generic_params {
            let resolved_gp = self.resolve_single_generic_param(gp_def, self.current_definition_context)?;
            impl_generic_scope.insert(resolved_gp.name.clone(), resolved_gp.id);
            impl_generic_params.push(resolved_gp);
        }
        self.generic_scopes.push(impl_generic_scope);

        // Resolve the implementing type (`Self`)
        let implementing_ty = self.resolve_type_to_ty(&r_impl.implementing_type)?;
        // We need to substitute any impl-level generics into the Self type
        let mut impl_subst = Substitution::new();
        for gp in &impl_generic_params {
             impl_subst.insert(gp.id, Ty::new(TyKind::Var(gp.id))); // Map generic ID to itself initially
        }
        let concrete_implementing_ty = implementing_ty.apply_subst(&impl_subst);

        // Resolve trait reference, if applicable
        let mut checker_trait_ref: Option<CheckerTraitRef> = None;
        let mut expected_trait_methods: HashMap<Symbol, FunctionSignature> = HashMap::new();
        let mut trait_name_for_error: String = String::new();

        if let Some(trait_symbol) = r_impl.trait_symbol {
            let trait_id = self.lookup_trait_id_by_symbol(trait_symbol)?;
            // TODO: Resolve trait type arguments properly
            checker_trait_ref = Some(CheckerTraitRef {
                trait_id,
                type_arguments: vec![], // Placeholder
                span: r_impl.span, // Use impl span for now
            });

            // Get expected method signatures from the trait definition
            let trait_def = self.lookup_trait_by_symbol(trait_symbol)?;
            trait_name_for_error = trait_def.name.clone();
            for (method_sym, trait_method) in &trait_def.methods {
                // Substitute Self and trait generics into the expected signature
                 let mut trait_subst = Substitution::new();
                 trait_subst.insert_self(&concrete_implementing_ty);
                // TODO: Substitute trait generics based on checker_trait_ref.type_arguments
                let concrete_expected_sig = self.manual_substitute_signature(&trait_method.signature, &trait_subst)?;
                expected_trait_methods.insert(*method_sym, concrete_expected_sig);
            }
        }

        // Resolve methods defined in the impl block
        let mut impl_methods = HashMap::new(); // Map: Trait Method Symbol -> Impl Method Symbol
        let mut inherent_methods_for_type = HashMap::new(); // For inherent impls

        for assoc_func in &r_impl.methods {
            let impl_method_symbol = assoc_func.func_symbol;
            // Find the full ResolvedFunction for this method
             let resolved_method = self.find_resolved_function(impl_method_symbol)?;
            // Resolve its signature within the impl context (Self=implementing_ty)
            let impl_method_sig = self.resolve_function_signature(&resolved_method, self.current_definition_context)?;

            // Store the resolved signature in the main context if not already there
            if !self.type_ctx.get_type_by_symbol(&impl_method_symbol).is_some() {
                self.type_ctx.add_type(impl_method_symbol, impl_method_sig.name.clone(), TypeDef::Function(impl_method_sig.clone()));
            }

            if let Some(trait_method_symbol) = assoc_func.trait_method_symbol {
                // This method corresponds to a trait method
                if let Some(expected_sig) = expected_trait_methods.get(&trait_method_symbol) {
                     // Compare signatures (this performs unification and returns errors)
                     self.compare_function_signatures(&impl_method_sig, expected_sig, &trait_name_for_error)?;
                     impl_methods.insert(trait_method_symbol, impl_method_symbol);
                } else {
                    // Method in impl claims to implement a trait method, but not found in trait def
                    self.errors.push(TypeError::InternalError {
                         message: format!("Impl method {:?} claims to implement trait method symbol {:?}, but it was not found in trait definition", impl_method_symbol, trait_method_symbol),
                         span: Some(impl_method_sig.span)
                    });
                 }
            } else {
                // This is an inherent method (impl Type { fn method... })
                inherent_methods_for_type.insert(impl_method_symbol, impl_method_sig);
            }
        }

        // Check if all required trait methods were implemented
        if r_impl.trait_symbol.is_some() {
            for expected_method_symbol in expected_trait_methods.keys() {
                if !impl_methods.contains_key(expected_method_symbol) {
                    let expected_sig = expected_trait_methods.get(expected_method_symbol).unwrap(); // Safe unwrap
                    self.errors.push(TypeError::MissingMethodImpl {
                        trait_name: trait_name_for_error.clone(),
                        method_name: expected_sig.name.clone(),
                        span: r_impl.span,
                    });
                }
            }
        }

        // Store inherent methods in TypeContext
        if !inherent_methods_for_type.is_empty() {
            // Need the symbol for the implementing type if it's a named type
            if let TyKind::Named{ name: self_type_name, .. } = &concrete_implementing_ty.kind {
                if let Some(self_type_symbol) = self.type_ctx.get_symbol_for_name(self_type_name) {
                     for (method_symbol, sig) in inherent_methods_for_type {
                         self.type_ctx.add_inherent_method(self_type_symbol, method_symbol, sig);
                     }
                } else {
                     self.errors.push(TypeError::InternalError {
                         message: format!("Could not find symbol for inherent impl type name '{}'", self_type_name),
                         span: Some(r_impl.span)
                     });
                 }
            } else {
                // Implementing inherent methods on non-nominal types (like primitives, tuples)?
                // This might be an error depending on language rules.
                self.errors.push(TypeError::InternalError {
                     message: format!("Cannot define inherent methods directly on non-nominal type '{}'", display_type(&concrete_implementing_ty)),
                     span: Some(r_impl.span)
                 });
            }
        }

        // --- Associated Type Bindings - Placeholder --- 
        // TODO: Resolve associated type bindings once ResolvedImpl includes them
        let associated_type_bindings = HashMap::new();

        // Create the CheckerImplDef
        let checker_impl_def = crate::types::ImplDef {
            id: ImplId(0), // ID will be assigned by add_impl
            impl_symbol: r_impl.impl_symbol,
            trait_ref: checker_trait_ref,
            implementing_type: concrete_implementing_ty, // Store the potentially generic Self type
            generic_params: impl_generic_params,
            methods: impl_methods,
            associated_type_bindings,
            span: r_impl.span,
        };

        // Add to TraitRepository (which assigns the ID)
        self.trait_repo.add_impl(checker_impl_def);

        self.generic_scopes.pop(); // Pop impl generic scope
        self.current_definition_context = original_context; // Restore original context
        Ok(())
    }

    /// Retrieves the definition kind, name, and span associated with a symbol.
    fn get_definition_info(&self, symbol: Symbol) -> Option<(DefinitionKind, String, SourceSpan)> {
        // Need a way to get DefinitionInfo back from ResolvedDefinitions efficiently.
        // Maybe store a Symbol -> ResolvedX map or require ResolvedX to hold DefinitionInfo?
        // For now, iterate (inefficient):
        if let Some(func) = self.resolved_defs.functions.iter().find(|f| f.symbol == symbol) {
            Some((DefinitionKind::Function, func.name.clone(), func.span))
        } else if let Some(s) = self.resolved_defs.structs.iter().find(|s| s.symbol == symbol) {
            Some((DefinitionKind::Struct, s.name.clone(), s.span))
        } else if let Some(e) = self.resolved_defs.enums.iter().find(|e| e.symbol == symbol) {
             Some((DefinitionKind::Enum, e.name.clone(), e.span))
        } else if let Some(t) = self.resolved_defs.traits.iter().find(|t| t.symbol == symbol) {
            Some((DefinitionKind::Trait, t.name.clone(), t.span))
        } else if let Some(i) = self.resolved_defs.impls.iter().find(|i| i.impl_symbol == symbol) {
             Some((DefinitionKind::Impl, "<impl>".to_string(), i.span)) // Impl name needs resolution
        } else {
            None
        }
    }

    /// Resolves a `ResolvedType` into a `Ty`.
    pub(crate) fn resolve_type_to_ty(
        &mut self,
        resolved_ty: &ResolvedType,
    ) -> Result<Ty, TypeError> {
        // Use a dummy span initially, try to get a better one from context if possible
        // TODO: Thread spans through resolution better.
        let span = dummy_span(); // Placeholder span

        match resolved_ty {
            ResolvedType::Primitive(prim) => {
                // Map ResolverPrimitiveType to ParallaxPrimitiveType
                let parallax_prim = resolver_primitive_to_parallax(*prim);
                Ok(Ty::with_span(TyKind::Primitive(parallax_prim), span))
            }
            ResolvedType::Unknown => {
                // Represent Unknown as Error type internally
                Ok(Ty::with_span(TyKind::Error, span))
            }
            ResolvedType::Never => Ok(Ty::with_span(TyKind::Never, span)),
            ResolvedType::Tuple(types) => {
                let mut inner_tys = Vec::with_capacity(types.len());
                for rt in types {
                    inner_tys.push(self.resolve_type_to_ty(rt)?);
                }
                Ok(Ty::with_span(TyKind::Tuple(inner_tys), span))
            }
            ResolvedType::Function {
                param_types,
                return_type,
            } => {
                let mut param_tys = Vec::with_capacity(param_types.len());
                for rt in param_types {
                    param_tys.push(self.resolve_type_to_ty(rt)?);
                }
                let ret_ty = self.resolve_type_to_ty(return_type)?;
                Ok(Ty::with_span(
                    TyKind::Function(param_tys, Arc::new(ret_ty)),
                    span,
                ))
            }
            ResolvedType::Array { element_type, size } => {
                let elem_ty = self.resolve_type_to_ty(element_type)?;
                let array_size = size.unwrap_or(0);
                Ok(Ty::with_span(
                    TyKind::Array(Arc::new(elem_ty), array_size),
                    span,
                ))
            }
            ResolvedType::UserDefined { symbol, type_args } => {
                let type_symbol = *symbol;
                let type_name = match self.get_name_for_symbol(type_symbol) {
                    Ok(name) => name,
                    Err(_) => {
                        self.errors.push(TypeError::InternalError {
                            message: format!(
                                "Failed to find name for resolved user type symbol {:?}",
                                type_symbol
                            ),
                            span: Some(span),
                        });
                        return Ok(Ty::with_span(TyKind::Error, span));
                    }
                };

                // Use explicit loop for type arguments
                let mut resolved_args = Vec::new();
                if let Some(args) = type_args {
                    resolved_args.reserve(args.len());
                    for rt in args {
                        resolved_args.push(self.resolve_type_to_ty(rt)?);
                    }
                }

                Ok(Ty::with_span(
                    TyKind::Named {
                        name: type_name,
                        args: resolved_args,
                    },
                    span,
                ))
            }
            ResolvedType::GenericParam(name) => {
                // Search active generic scopes (innermost first)
                for scope in self.generic_scopes.iter().rev() {
                    if let Some(type_id) = scope.get(name) {
                        return Ok(Ty::with_span(TyKind::Var(*type_id), span));
                    }
                }
                let err = TypeError::UnknownIdentifier {
                    name: name.clone(),
                    span,
                };
                self.errors.push(err.clone());
                Ok(Ty::with_span(TyKind::Error, span))
            }
            ResolvedType::SelfType => {
                // Return the dedicated SelfType kind
                Ok(Ty::with_span(TyKind::SelfType, span))
            }
        }
    }

    fn unify(&mut self, ty1: &Ty, ty2: &Ty) -> TypeResult<Substitution> {
        self.inference_ctx.unify(ty1, ty2)
    }

    fn resolve_type(&self, ty: &Ty) -> Ty {
        self.inference_ctx.resolve_type(ty)
    }

    fn get_name_for_symbol(&self, symbol: Symbol) -> TypeResult<String> {
        self.type_ctx
            .get_name_for_symbol(&symbol)
            .cloned()
            .ok_or_else(|| TypeError::InternalError {
                message: format!("Name not found for symbol {:?}", symbol),
                span: None,
            })
    }

    fn check_item(&mut self, item: CheckItem) {
        // Placeholder for dispatching to specific checking functions
        match item {
            CheckItem::Struct(s) => println!("Checking Struct: {}", s.name),
            CheckItem::Enum(e) => println!("Checking Enum: {}", e.name),
            CheckItem::Function(f) => println!("Checking Function: {}", f.name),
            CheckItem::Trait(t) => println!("Checking Trait: {}", t.name),
            CheckItem::Impl(_i) => println!("Checking Impl"),
        }
    }

    fn finalize(self) -> (TypedDefinitions, TraitRepository, Vec<TypeError>) {
        (self.typed_definitions, self.trait_repo, self.errors)
    }

    fn lookup_trait_id_by_symbol(&mut self, symbol: Symbol) -> TypeResult<TraitId> {
        self.trait_repo.get_trait_id_by_symbol(symbol).ok_or_else(|| {
            let name = self.resolved_defs.traits.iter()
                .find(|t| t.symbol == symbol)
                .map_or_else(|| format!("symbol_{}", symbol.id()), |t| t.name.clone());
            TypeError::UnknownTrait {
                name,
                span: self.resolved_defs.traits.iter().find(|t| t.symbol == symbol).map_or(dummy_span(), |t| t.span)
            }
        })
    }

    fn lookup_trait_by_symbol(&mut self, symbol: Symbol) -> TypeResult<CheckerTraitDef> {
        let trait_id = self.lookup_trait_id_by_symbol(symbol)?;
        self.trait_repo.traits.get(&trait_id)
            .cloned()
            .ok_or_else(|| TypeError::InternalError {
                message: format!("Trait definition not found for ID {:?} (symbol {:?})", trait_id, symbol),
                span: None, // TODO: Span?
            })
    }

    fn find_resolved_function(&self, symbol: Symbol) -> TypeResult<Arc<ResolvedFunction>> {
        self.resolved_defs
            .functions
            .iter()
            .find(|f| f.symbol == symbol)
            .cloned() // Clone the ResolvedFunction first
            .map(Arc::new) // Then put it in an Arc
            .ok_or_else(|| TypeError::InternalError {
                message: format!("Resolved function not found for symbol {:?}", symbol),
                span: None,
            })
    }

    /// Helper to resolve a single generic parameter definition.
    pub(crate) fn resolve_single_generic_param(
        &mut self,
        param_def: &ResolvedGenericParamDef,
        _context: Option<(DefinitionKind, Symbol)>,
    ) -> TypeResult<GenericParamDef> {
        let fresh_var_ty = self.inference_ctx.fresh_var_with_span(param_def.span);
        let type_id = if let TyKind::Var(id) = fresh_var_ty.kind {
            id
        } else {
            panic!("Internal Error: Fresh var was not TyKind::Var")
        };

        let mut bounds = Vec::new();
        for bound_symbol in &param_def.bounds {
            if let Ok(trait_id) = self.lookup_trait_id_by_symbol(*bound_symbol) {
                bounds.push(crate::types::TraitRef {
                    trait_id,
                    type_arguments: Vec::new(),
                    span: param_def.span,
                });
            }
        }

        Ok(GenericParamDef {
            name: param_def.name.clone(),
            symbol: Symbol::fresh(),
            id: type_id,
            bounds,
            span: param_def.span,
        })
    }

    fn resolve_function_signature(
        &mut self,
        func_def: &ResolvedFunction,
        _context: Option<(DefinitionKind, Symbol)>,
    ) -> TypeResult<FunctionSignature> {
        let mut func_generic_defs = Vec::new();
        let mut current_func_generic_scope = HashMap::<String, TypeId>::new();

        for param_def in &func_def.generic_params {
            let fresh_var_ty = self.inference_ctx.fresh_var_with_span(param_def.span);
            let type_id = if let TyKind::Var(id) = fresh_var_ty.kind {
                id
            } else {
                panic!("Internal Error: Fresh var was not TyKind::Var")
            };

            let bound_trait_ids = Vec::new();

            func_generic_defs.push(GenericParamDef {
                name: param_def.name.clone(),
                symbol: Symbol::fresh(),
                id: type_id,
                bounds: bound_trait_ids,
                span: param_def.span,
            });
            current_func_generic_scope.insert(param_def.name.clone(), type_id);
        }

        self.generic_scopes.push(current_func_generic_scope);

        let mut params = Vec::new();
        for param in &func_def.parameters {
            let param_ty = self.resolve_type_to_ty(&param.param_type)?;
            params.push(ParamType {
                name: param.name.clone(),
                ty: param_ty,
                span: param.span,
            });
        }

        let return_ty = self.resolve_type_to_ty(&func_def.return_type)?;

        let signature = FunctionSignature {
            name: func_def.name.clone(),
            self_param: None,
            generic_params: func_generic_defs,
            params,
            return_type: return_ty,
            span: func_def.span,
        };

        self.generic_scopes.pop();

        Ok(signature)
    }

    fn manual_substitute_signature(
        &self,
        signature: &FunctionSignature,
        substitution: &Substitution,
    ) -> TypeResult<FunctionSignature> {
        let substituted_params = signature
            .params
            .iter()
            .map(|param| ParamType {
                name: param.name.clone(),
                ty: param.ty.apply_subst(substitution),
                span: param.span,
            })
            .collect();

        let substituted_return_type = signature.return_type.apply_subst(substitution);

        let substituted_generic_params = signature.generic_params.clone();

        Ok(FunctionSignature {
            params: substituted_params,
            return_type: substituted_return_type,
            generic_params: substituted_generic_params,
            name: signature.name.clone(),
            self_param: signature.self_param.clone(),
            span: signature.span,
        })
    }

    fn compare_function_signatures(
        &mut self,
        impl_sig: &FunctionSignature,
        expected_sig: &FunctionSignature,
        trait_name: &str,
    ) -> TypeResult<()> {
        if impl_sig.params.len() != expected_sig.params.len() {
            return Err(TypeError::ParamCountMismatch {
                name: impl_sig.name.clone(),
                expected: expected_sig.params.len(),
                found: impl_sig.params.len(),
                span: impl_sig.span,
            });
        }

        let impl_ret = &impl_sig.return_type;
        let expected_ret = &expected_sig.return_type;

        if let Err(e) = self.unify(impl_ret, expected_ret) {
            return Err(TypeError::MethodReturnTypeMismatch {
                method_name: impl_sig.name.clone(),
                trait_name: trait_name.to_string(),
                expected: display_type(expected_ret),
                found: display_type(impl_ret),
                span: impl_sig.span,
                error: Box::new(e),
            });
        }

        for (i, (impl_param, expected_param)) in impl_sig
            .params
            .iter()
            .zip(expected_sig.params.iter())
            .enumerate()
        {
            if let Err(e) = self.unify(&impl_param.ty, &expected_param.ty) {
                return Err(TypeError::MethodParamMismatch {
                    trait_name: trait_name.to_string(),
                    method_name: impl_sig.name.clone(),
                    param_index: i,
                    expected: display_type(&expected_param.ty),
                    found: display_type(&impl_param.ty),
                    span: impl_param.span,
                    error: Box::new(e),
                });
            }
        }

        Ok(())
    }

    fn verify_expr_concrete(&mut self, expr: &TypedExpr) {
        // Use a queue for breadth-first or depth-first traversal (stack)
        let mut queue = VecDeque::new();
        queue.push_back(expr);

        while let Some(current_expr) = queue.pop_front() {
            // Check the type of the current expression itself
            self.verify_type_concrete(&current_expr.ty);

            // Add sub-expressions to the queue based on kind
            match &current_expr.kind {
                TypedExprKind::Block(exprs) => {
                    for sub_expr in exprs {
                        queue.push_back(sub_expr);
                    }
                }
                TypedExprKind::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    queue.push_back(condition);
                    queue.push_back(then_branch);
                    if let Some(else_b) = else_branch {
                        queue.push_back(else_b);
                    }
                }
                TypedExprKind::Call { func, args } => {
                    queue.push_back(func);
                    for arg in args {
                        queue.push_back(&arg.value);
                    }
                }
                TypedExprKind::Lambda { params: _, body } => {
                    // Don't check param types here, they are part of signature check
                    queue.push_back(body);
                }
                TypedExprKind::Variable { .. }
                | TypedExprKind::Literal(_)
                | TypedExprKind::Error => {
                    // No sub-expressions to check
                }
                TypedExprKind::Field { object, .. } => {
                    queue.push_back(object);
                }
                TypedExprKind::Array(elements) | TypedExprKind::Tuple(elements) => {
                    for elem in elements {
                        queue.push_back(elem);
                    }
                }
                TypedExprKind::Let { pattern: _, value } => {
                    // Don't check pattern type here, part of signature check
                    queue.push_back(value);
                }
                TypedExprKind::Struct { fields, base, .. } => {
                    for (_, field_expr) in fields {
                        queue.push_back(field_expr);
                    }
                    if let Some(base_expr) = base {
                        queue.push_back(base_expr);
                    }
                }
                TypedExprKind::Paren(inner_expr) => {
                    queue.push_back(inner_expr);
                }
                TypedExprKind::VariantConstructor { args, .. } => {
                    for arg in args {
                        queue.push_back(&arg.value);
                    }
                }
                TypedExprKind::Match { scrutinee, arms } => {
                    queue.push_back(scrutinee);
                    for arm in arms {
                        // Don't check pattern type here
                        queue.push_back(&arm.body);
                    }
                }
                TypedExprKind::LogicalAnd { left, right }
                | TypedExprKind::LogicalOr { left, right } => {
                    queue.push_back(left);
                    queue.push_back(right);
                }
                TypedExprKind::Map(entries) => {
                    for (key_expr, value_expr) in entries {
                        queue.push_back(key_expr);
                        queue.push_back(value_expr);
                    }
                }
                TypedExprKind::HashSet(elements) => {
                    for elem_expr in elements {
                        queue.push_back(elem_expr);
                    }
                }
            }
        }
    }

    /// Checks if a single type resolves to a concrete type (not TyKind::Var).
    /// Adds an error if an unresolved variable is found.
    fn verify_type_concrete(&mut self, ty: &Ty) {
        let resolved_ty = self.resolve_type(ty);
        if let TyKind::Var(id) = &resolved_ty.kind {
            self.errors.push(TypeError::InternalError {
                message: format!(
                    "Unresolved type variable '{}' remaining after type checking.",
                    id
                ),
                span: resolved_ty.span, // Use span from the resolved type if available
            });
            return; // Don't recurse into unresolved vars
        }
        // Recursively check nested types - explicitly match all variants
        match &resolved_ty.kind {
            TyKind::Named { name: enum_name, args } => {
                // Verify generic arguments
                for arg_ty in args {
                    self.verify_type_concrete(arg_ty);
                }

                // Get symbol from name - needed to look up in typed_definitions
                let symbol_opt = self.type_ctx.get_symbol_for_name(enum_name);

                // If it's an enum, verify variant field types
                if let Some(symbol) = symbol_opt {
                    if let Some(enum_def) = self.typed_definitions.enums.get(&symbol).cloned() {
                        let substitution = self.inference_ctx.get_substitution();
                        let resolved_args = args.iter().map(|a| self.resolve_type(a)).collect::<Vec<_>>();

                        if resolved_args.len() != enum_def.generic_params.len() {
                             self.errors.push(TypeError::GenericArgCountMismatch {
                                 kind: "Enum".to_string(),
                                 name: enum_def.name.clone(),
                                 expected: enum_def.generic_params.len(),
                                 found: resolved_args.len(),
                                 span: resolved_ty.span.unwrap_or_else(dummy_span),
                             });
                             return;
                        }

                        let mut variant_subst = Substitution::default();
                        for (param, arg) in enum_def.generic_params.iter().zip(resolved_args.iter()) {
                            variant_subst.insert(param.id, arg.clone());
                        }
                        let combined_subst = substitution.compose(&variant_subst);

                        // Correctly match on TypedVariant enum variants
                        for variant in &enum_def.variants {
                            match variant {
                                TypedVariant::Unit { .. } => {
                                    // No fields to check
                                },
                                TypedVariant::Tuple { types, .. } => {
                                    for ty in types {
                                        let substituted_type = ty.apply_subst(&combined_subst);
                                        self.verify_type_concrete(&substituted_type);
                                    }
                                },
                                TypedVariant::Struct { fields, .. } => {
                                    for field in fields {
                                        let substituted_field_ty = field.ty.apply_subst(&combined_subst);
                                        self.verify_type_concrete(&substituted_field_ty);
                                    }
                                }
                            }
                        }
                    } // TODO: Verify struct field types similarly?
                }
            },
            TyKind::Array(elem_ty, _) => {
                self.verify_type_concrete(elem_ty);
            },
            TyKind::Tuple(tys) => {
                for inner_ty in tys {
                    self.verify_type_concrete(inner_ty);
                }
            },
            TyKind::Function(params, ret) => {
                for param_ty in params {
                    self.verify_type_concrete(param_ty);
                }
                self.verify_type_concrete(ret);
            },
            TyKind::Map(key_ty, value_ty) => {
                self.verify_type_concrete(key_ty);
                self.verify_type_concrete(value_ty);
            },
            TyKind::Set(elem_ty) => {
                self.verify_type_concrete(elem_ty);
            },
            // Base cases - include all remaining types including Var (already handled above)
            TyKind::Var(_) | TyKind::Primitive(_) | TyKind::Error | TyKind::Never | TyKind::SelfType => {
                // Already concrete or not verifiable further
            }
        }
    }

    fn is_associated_function(&self, function_symbol: Symbol) -> bool {
        // This also requires the original definition map
        false // Placeholder
    }

    // Restore fresh_infer_var method
    pub(crate) fn fresh_infer_var(&mut self, span: SourceSpan) -> Ty {
        self.inference_ctx.fresh_var_with_span(span)
    }
}

/// Type check a module's definitions.
pub fn type_check_definitions(
    db: &dyn TypeDatabase,
    resolved_module: ResolvedModuleStructure,
) -> TypedModule {
    println!("Starting type checking...");
    let mut checker = TypeChecker::new(
        db,
        &resolved_module.definitions(db),
        &resolved_module.core_traits(db),
        &resolved_module.intrinsics(db),
    );

    // Pass 1: Check declarations and signatures
    for resolved_struct in &resolved_module.definitions(db).structs {
        checker.check_item(CheckItem::Struct(resolved_struct));
    }
    for resolved_enum in &resolved_module.definitions(db).enums {
        checker.check_item(CheckItem::Enum(resolved_enum));
    }
    for resolved_func in &resolved_module.definitions(db).functions {
        let func_symbol = resolved_func.symbol;
        if !checker.is_associated_function(func_symbol) {
            // Uses &self
            // let _ = declarations::type_check_declaration_signature(&mut checker, resolved_func);
            // TODO: Integrate signature checking properly later
        }
    }
    for resolved_trait in &resolved_module.definitions(db).traits {
        checker.check_item(CheckItem::Trait(resolved_trait));
    }
    for resolved_impl in &resolved_module.definitions(db).impls {
        checker.check_item(CheckItem::Impl(resolved_impl));
    }

    // Pass 2: Check function bodies
    for resolved_func in &resolved_module.definitions(db).functions {
        // let _ = declarations::type_check_declaration_body(&mut checker, resolved_func);
        // TODO: Integrate body checking properly later
    }

    // --- Phase 3: Verification ---
    println!("Starting Type Verification...");

    // Collect data to verify separately to avoid borrow checker issues
    let functions_to_verify: Vec<_> = checker
        .typed_definitions
        .functions
        .values()
        .cloned()
        .collect();

    let structs_to_verify: Vec<_> = checker
        .typed_definitions
        .structs
        .values()
        .cloned()
        .collect();
    let enums_to_verify: Vec<_> = checker.typed_definitions.enums.values().cloned().collect();

    // Now iterate over the collected data and call mutable methods on checker

    // Check function signatures and bodies
    for func in functions_to_verify {
        // Verify return type
        checker.verify_type_concrete(&func.return_type);
        // Verify parameter types
        for param in &func.params {
            checker.verify_type_concrete(&param.ty);
        }
        // Verify body expressions
        if let Some(body) = &func.body {
            checker.verify_expr_concrete(body);
        }
    }

    // Check struct fields
    for def in structs_to_verify {
        for field in &def.fields {
            checker.verify_type_concrete(&field.ty);
        }
        // Also check generic param bounds if they store types
        // for gen_param in &def.generic_params { ... }
    }

    // Check enum variants
    for def in enums_to_verify {
        for variant in &def.variants {
            match variant {
                TypedVariant::Unit { .. } => {
                    // No fields to check for unit variants
                },
                TypedVariant::Tuple { types, .. } => {
                    for ty in types {
                        checker.verify_type_concrete(ty);
                    }
                },
                TypedVariant::Struct { fields, .. } => {
                    for field in fields {
                        checker.verify_type_concrete(&field.ty);
                    }
                },
            }
        }
        // Also check generic param bounds if they store types
        for param in &def.generic_params {
            for bound in &param.bounds {
                // Check type arguments in bounds if any
                for arg in &bound.type_arguments {
                    checker.verify_type_concrete(arg);
                }
            }
        }
    }
    println!("Type Verification Complete.");
    // --- End Phase 3 ---

    let (typed_definitions, trait_repo, errors) = checker.finalize();

    // Combine parse/resolve errors with type errors
    let mut all_errors = resolved_module
        .errors(db)
        .iter()
        .map(|e| format!("{:?}", e))
        .collect::<Vec<_>>();
    all_errors.extend(errors.into_iter().map(|e| format!("{}", e)));

    TypedModule {
        definitions: typed_definitions,
        trait_repo,
        entry_point: resolved_module.entry_point(db),
        intrinsics: resolved_module.intrinsics(db).clone(),
        errors: all_errors,
    }
}

fn resolver_primitive_to_parallax(prim: ResolverPrimitiveType) -> ParallaxPrimitiveType {
    match prim {
        // Map all specific integer types to their equivalent
        ResolverPrimitiveType::I8   => ParallaxPrimitiveType::I8,
        ResolverPrimitiveType::I16  => ParallaxPrimitiveType::I16,
        ResolverPrimitiveType::I32  => ParallaxPrimitiveType::I32,
        ResolverPrimitiveType::I64  => ParallaxPrimitiveType::I64,
        ResolverPrimitiveType::I128 => ParallaxPrimitiveType::I128,
        ResolverPrimitiveType::U8   => ParallaxPrimitiveType::U8,
        ResolverPrimitiveType::U16  => ParallaxPrimitiveType::U16,
        ResolverPrimitiveType::U32  => ParallaxPrimitiveType::U32,
        ResolverPrimitiveType::U64  => ParallaxPrimitiveType::U64,
        ResolverPrimitiveType::U128 => ParallaxPrimitiveType::U128,
        ResolverPrimitiveType::F32  => ParallaxPrimitiveType::F32,
        ResolverPrimitiveType::F64  => ParallaxPrimitiveType::F64,
        ResolverPrimitiveType::Bool => ParallaxPrimitiveType::Bool,
        ResolverPrimitiveType::Char => ParallaxPrimitiveType::Char,
        ResolverPrimitiveType::String => ParallaxPrimitiveType::String,
        ResolverPrimitiveType::Unit => ParallaxPrimitiveType::Unit,
    }
}
