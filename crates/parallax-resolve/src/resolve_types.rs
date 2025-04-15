// Pass 3: Resolve type references in signatures.

use std::collections::HashMap;
use parallax_syntax::ast::{self, types::{Type, TypeKind}};
use parallax_syntax::SyntaxDatabase; // Import SyntaxDatabase
// use parallax_syntax::ast::common::DUMMY_SP; // Import DUMMY_SP
use crate::definitions::{DefinitionInfo, DefinitionKind};
use crate::scopes::ModuleScope;
use crate::types::{
    Symbol, ResolvedType, PrimitiveType, ResolvedStruct, ResolvedEnum, 
    ResolvedFunction, ResolvedParameter, ResolvedField, ResolvedEnumVariant, 
    ResolvedDefinitions, ResolvedGenericParamDef, ResolvedAssociatedFunction, 
    ResolvedTrait, ResolvedImpl
};
use crate::error::ResolutionError;
use miette::SourceSpan; // Remove DUMMY_SP from this import

/// Pass 3: Resolve type references in item signatures (struct fields, enum variants,
/// function parameters/return types, trait/impl types).
///
/// This pass iterates through the definitions collected in Pass 1 (`definitions_map`)
/// and uses the scopes built in Pass 2 (`module_scopes`) to resolve all type references
/// found in the signatures of structs, enums, functions, traits, and impls.
///
/// It populates the `resolved_defs` structure with `ResolvedStruct`, `ResolvedEnum`,
/// `ResolvedFunction`, `ResolvedTrait`, and `ResolvedImpl` instances containing
/// fully resolved types (`ResolvedType`).
///
/// Errors during type resolution (e.g., type not found, private type access) are collected.
///
/// # Arguments
///
/// * `db`: Database access.
/// * `definitions_map`: Map of all collected `DefinitionInfo`.
/// * `module_scopes`: Map of resolved `ModuleScope` for each module.
/// * `prelude_scope`: Map of resolved symbols for predefined paths.
/// * `resolved_defs`: Mutable reference to the `ResolvedDefinitions` structure to be populated.
/// * `errors`: Mutable vector to collect `ResolutionError`s.
pub fn resolve_signatures<'db>(
    db: &'db dyn SyntaxDatabase, // Pass db directly
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>, // Updated to use Symbol
    module_scopes: &HashMap<Symbol, ModuleScope>,
    prelude_scope: &HashMap<String, Symbol>, // Added prelude scope
    resolved_defs: &mut ResolvedDefinitions, // Output structure
    errors: &mut Vec<ResolutionError>,
) {
    // First, resolve signatures for items that define types (structs, enums, traits)
    // and standalone functions. Impls are deferred as they might depend on traits.
    for (symbol, def_info) in definitions_map {
        // Determine the module scope to use for resolving types within this definition.
        // For items within modules, it's their parent. For the root module itself, use its own symbol.
        let module_symbol = def_info.parent_symbol.unwrap_or(*symbol);
        let definition_context = Some((def_info.kind, *symbol)); // Context for resolving `Self`

        // --- Intrinsic Function Check ---
        if def_info.kind == DefinitionKind::Function &&
           def_info.name.starts_with("__intrinsic_") &&
           def_info.full_path.starts_with("std::")
        {
            // This is an intrinsic function from the standard library.
            // Add it to the dedicated lookup list.
            resolved_defs.intrinsics.push((def_info.full_path.clone(), *symbol));
            // We still proceed to resolve its signature below like any other function,
            // as the signature information (params, return type) is still useful.
        }
        // --- End Intrinsic Check ---

        match def_info.kind {
            DefinitionKind::Struct => {
                if let Some(ast::items::ItemKind::Struct(struct_ast)) = def_info.ast_item.map(|i| &i.kind) {
                    match resolve_struct_signature(
                        db, definitions_map, module_scopes, prelude_scope, module_symbol, def_info, struct_ast, definition_context
                    ) {
                        Ok(resolved_struct) => resolved_defs.structs.push(resolved_struct),
                        Err(e) => errors.push(e),
                    }
                } else {
                    // This indicates an inconsistency between DefinitionInfo and the AST item.
                    errors.push(ResolutionError::InternalError {
                         message: format!("AST item mismatch for struct definition: {}", def_info.full_path),
                         span: Some(def_info.span),
                     });
                }
            },
            DefinitionKind::Enum => {
                 if let Some(ast::items::ItemKind::Enum(enum_ast)) = def_info.ast_item.map(|i| &i.kind) {
                    match resolve_enum_signature(
                        db, definitions_map, module_scopes, prelude_scope, module_symbol, def_info, enum_ast, definition_context
                    ) {
                        Ok(resolved_enum) => resolved_defs.enums.push(resolved_enum),
                        Err(e) => errors.push(e),
                    }
                } else {
                    errors.push(ResolutionError::InternalError {
                         message: format!("AST item mismatch for enum definition: {}", def_info.full_path),
                         span: Some(def_info.span),
                     });
                }
            },
            DefinitionKind::Function => {
                 // Check if this function is associated or already resolved *before* trying to get the AST item.
                 let is_associated = def_info.parent_symbol
                     .and_then(|p_sym| definitions_map.get(&p_sym))
                     .map_or(false, |p_info| matches!(p_info.kind, DefinitionKind::Trait | DefinitionKind::Impl));

                 let already_resolved = resolved_defs.functions.iter().any(|rf| rf.symbol == *symbol);

                 if !is_associated && !already_resolved {
                     // Only try to get the AST item and resolve if it's a standalone function
                     // that hasn't been processed yet.
                     if let Some(ast::items::ItemKind::Function(func_ast)) = def_info.ast_item.map(|i| &i.kind) {
                         // Resolve standalone function signature.
                         // Pass empty parent generics as there are none.
                        match resolve_function_signature(
                            db, definitions_map, module_scopes, prelude_scope, module_symbol, def_info, func_ast, &[], definition_context
                        ) {
                            Ok(resolved_func) => resolved_defs.functions.push(resolved_func),
                            Err(e) => errors.push(e),
                        }
                     } else {
                         // If it's not associated/resolved but we still can't get the AST, it's an error.
                         errors.push(ResolutionError::InternalError {
                             message: format!("AST item mismatch for standalone function definition: {}", def_info.full_path),
                             span: Some(def_info.span),
                         });
                     }
                 } 
                 // else: Skip associated or already resolved functions
            },
            DefinitionKind::Trait => {
                if let Some(ast::items::ItemKind::Trait(trait_ast)) = def_info.ast_item.map(|i| &i.kind) {
                    // Resolve trait signature, including associated function signatures within it.
                    match resolve_trait_signature(
                        db, definitions_map, module_scopes, prelude_scope, module_symbol, def_info, trait_ast,
                        resolved_defs, // Pass mutable resolved_defs to add associated functions
                        errors,
                        definition_context
                    ) {
                        Ok(resolved_trait) => resolved_defs.traits.push(resolved_trait),
                        Err(e) => errors.push(e),
                    }
                } else {
                    errors.push(ResolutionError::InternalError {
                         message: format!("AST item mismatch for trait definition: {}", def_info.full_path),
                         span: Some(def_info.span),
                     });
                }
            },
            // Impls are handled in a separate loop after other items are resolved.
            DefinitionKind::Impl => { /* Skip for now */ },
            // Modules don't have signatures in the same way types/functions do.
            DefinitionKind::Module => { /* Skip */ },
            // Enum variant types (like tuple fields or struct fields) are resolved
            // as part of resolving the parent enum's signature in `resolve_enum_signature`.
            DefinitionKind::EnumVariant => { /* Skip */ },
        }
    }

    // Now, resolve signatures for Impl blocks. This pass runs after structs, enums, functions,
    // and traits are processed, ensuring that the types and traits referenced in impl signatures
    // (like the trait being implemented and the `Self` type) are likely already resolved.
    for (symbol, def_info) in definitions_map {
        if def_info.kind == DefinitionKind::Impl {
             let module_symbol = def_info.parent_symbol.unwrap_or(*symbol);
             let definition_context = Some((def_info.kind, *symbol)); // Context for `Self`

            if let Some(ast::items::ItemKind::Impl(impl_ast)) = def_info.ast_item.map(|i| &i.kind) {
                match resolve_impl_signature(
                    db, definitions_map, module_scopes, prelude_scope, module_symbol, def_info, impl_ast,
                    resolved_defs, // Pass mutable resolved_defs to add associated functions
                    errors,
                    definition_context
                ) {
                    Ok(resolved_impl) => resolved_defs.impls.push(resolved_impl),
                    Err(e) => errors.push(e),
                }
            } else {
                errors.push(ResolutionError::InternalError {
                    message: format!("AST item mismatch for impl definition: {}", def_info.full_path),
                    span: Some(def_info.span),
                });
            }
        }
    }
}

/// Resolves the signature of a struct definition.
/// Extracts field types and resolves generic parameters.
fn resolve_struct_signature<'db>(
    db: &'db dyn SyntaxDatabase,
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    module_scopes: &HashMap<Symbol, ModuleScope>,
    prelude_scope: &HashMap<String, Symbol>, // Added prelude
    module_symbol: Symbol, // Symbol of the module containing the struct definition
    def_info: &DefinitionInfo<'db>,
    struct_ast: &ast::items::StructDef,
    definition_context: Option<(DefinitionKind, Symbol)>, // Context for `Self`
) -> Result<ResolvedStruct, ResolutionError> {
    // Resolve generic parameters defined on the struct itself.
    let generic_params = resolve_generic_params(db, definitions_map, module_scopes, prelude_scope, module_symbol, struct_ast.generic_params.as_ref(), definition_context)?;
    // Get names of resolved generics to help resolve types within the struct body.
    let generic_param_names: Vec<String> = generic_params.iter().map(|p| p.name.clone()).collect();

    // Resolve the type of each field.
    let mut resolved_fields = Vec::new();
    for field_ast in &struct_ast.fields {
        // Resolve the field's type using the current module scope and available generic parameters.
        let resolved_type = resolve_type(
            db, definitions_map, module_scopes, prelude_scope, module_symbol,
            &field_ast.ty, &generic_param_names, definition_context, field_ast.ty.span
        )?;
        resolved_fields.push(ResolvedField {
            symbol: Symbol::fresh(), // Assign a fresh symbol to each field
            name: field_ast.name.name.clone(),
            field_type: resolved_type,
            is_public: field_ast.visibility,
            span: field_ast.span,
        });
    }

    Ok(ResolvedStruct {
        symbol: def_info.symbol,
        name: def_info.name.clone(),
        module_symbol, // Store the module where the struct is defined
        fields: resolved_fields,
        generic_params, // Store resolved generic parameters
        is_public: def_info.is_public,
        span: def_info.span,
    })
}

/// Resolves the signature of an enum definition.
/// Resolves generic parameters and the types within each variant.
fn resolve_enum_signature<'db>(
    db: &'db dyn SyntaxDatabase,
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    module_scopes: &HashMap<Symbol, ModuleScope>,
    prelude_scope: &HashMap<String, Symbol>, // Added prelude
    module_symbol: Symbol, // Symbol of the module containing the enum definition
    def_info: &DefinitionInfo<'db>,
    enum_ast: &ast::items::EnumDef,
    definition_context: Option<(DefinitionKind, Symbol)>, // Context for `Self`
) -> Result<ResolvedEnum, ResolutionError> {
    // Resolve generic parameters defined on the enum itself.
    let generic_params = resolve_generic_params(db, definitions_map, module_scopes, prelude_scope, module_symbol, enum_ast.generic_params.as_ref(), definition_context)?;
    let generic_param_names: Vec<String> = generic_params.iter().map(|p| p.name.clone()).collect();

    // Resolve each variant defined within the enum.
    let mut resolved_variants = Vec::new();
    for variant_ast in &enum_ast.variants {
        // Find the DefinitionInfo for this variant (collected in Pass 1)
        // This provides the unique Symbol assigned to the variant.
        let variant_def_info = definitions_map.values()
            .find(|vi| vi.kind == DefinitionKind::EnumVariant && vi.parent_symbol == Some(def_info.symbol) && vi.name == variant_ast.name.name)
            .ok_or_else(|| ResolutionError::InternalError {
                message: format!("DefinitionInfo not found for enum variant {}::{} during signature resolution", def_info.name, variant_ast.name.name),
                span: Some(variant_ast.span)
            })?;

        // Resolve the types associated with the variant based on its kind.
        let resolved_variant = match &variant_ast.kind {
            ast::items::EnumVariantKind::Unit => ResolvedEnumVariant::Unit {
                name: variant_ast.name.name.clone(),
                symbol: variant_def_info.symbol,
                span: variant_ast.span,
            },
            ast::items::EnumVariantKind::Tuple(types) => {
                // Resolve each type within the tuple variant.
                let fields = types.iter()
                    .map(|ty| resolve_type(db, definitions_map, module_scopes, prelude_scope, module_symbol, ty, &generic_param_names, definition_context, ty.span))
                    .collect::<Result<Vec<_>, _>>()?; // Collect results, propagating errors
                ResolvedEnumVariant::Tuple {
                    name: variant_ast.name.name.clone(),
                    symbol: variant_def_info.symbol,
                    fields,
                    span: variant_ast.span,
                }
            },
            ast::items::EnumVariantKind::Struct(fields) => {
                // Resolve each field type within the struct variant.
                let resolved_fields = fields.iter()
                    .map(|field| {
                        resolve_type(db, definitions_map, module_scopes, prelude_scope, module_symbol, &field.ty, &generic_param_names, definition_context, field.ty.span).map(|ty| ResolvedField {
                            symbol: Symbol::fresh(), // Assign a fresh symbol here too
                            name: field.name.name.clone(),
                            field_type: ty,
                            is_public: field.visibility, // Struct variant fields have visibility
                            span: field.span,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?; // Collect results, propagating errors
                ResolvedEnumVariant::Struct {
                    name: variant_ast.name.name.clone(),
                    symbol: variant_def_info.symbol,
                    fields: resolved_fields,
                    span: variant_ast.span,
                }
            },
        };
        resolved_variants.push(resolved_variant);
    }

    Ok(ResolvedEnum {
        symbol: def_info.symbol,
        name: def_info.name.clone(),
        module_symbol,
        variants: resolved_variants,
        generic_params, // Store resolved generic params
        is_public: def_info.is_public,
        span: def_info.span,
    })
}

/// Resolves the signature of a function (standalone or associated).
/// Resolves generic parameters, parameter types, and the return type.
///
/// # Arguments
/// * `parent_generic_params`: Generic parameters from the surrounding context (e.g., from an `impl<T>` block).
fn resolve_function_signature<'db>(
    db: &'db dyn SyntaxDatabase,
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    module_scopes: &HashMap<Symbol, ModuleScope>,
    prelude_scope: &HashMap<String, Symbol>, // Added prelude
    module_symbol: Symbol, // Module where the function's source (AST) is defined
    def_info: &DefinitionInfo<'db>,
    func_ast: &ast::items::Function,
    _parent_generic_params: &[ResolvedGenericParamDef], // Prefix unused variable
    definition_context: Option<(DefinitionKind, Symbol)>, // Context for `Self`
) -> Result<ResolvedFunction, ResolutionError> {
    let generic_param_names_in_scope: Vec<String> = func_ast.generic_params.as_ref()
        .map(|gp| gp.iter().map(|p| p.name.name.clone()).collect())
        .unwrap_or_default();

    // Simple helper to extract the name from common pattern kinds
    fn resolve_pattern_name(pattern: &ast::pattern::Pattern) -> String {
        match &pattern.kind {
            ast::pattern::PatternKind::Identifier(ident) => ident.name.clone(),
            // Add more cases as needed, e.g., for `_` or more complex patterns
            _ => "_".to_string(), // Default to underscore for unhandled patterns
        }
    }

    // Resolve parameter types.
    let mut params = Vec::new();
    for param_ast in &func_ast.params {
        let param_name = resolve_pattern_name(&param_ast.pattern); // Corrected: use pattern

        // Special handling for 'self' parameter in methods
        let param_type = if param_name == "self" {
            if let Some((DefinitionKind::Impl, _impl_symbol)) = definition_context {
                // If we are inside an impl block, 'self' refers to the type being implemented.
                // We need to fetch the ResolvedImpl to get its `implementing_type`.
                // This requires access to the `resolved_defs` which are being built.
                // This indicates a potential ordering issue or the need to pass more context.
                // For now, let's mark it as SelfType and resolve it later or during body checking.
                // TODO: Revisit Self type resolution during signature pass.
                 ResolvedType::SelfType
            } else if let Some((DefinitionKind::Trait, _)) = definition_context {
                 // Inside a trait definition, `self` also refers to `Self`
                 ResolvedType::SelfType
            } else {
                // 'self' used outside of an impl/trait context (invalid)
                 return Err(ResolutionError::InternalError {
                    message: "'self' parameter encountered outside of impl or trait context".to_string(),
                    span: Some(param_ast.span),
                });
            }
        } else {
            // Regular parameter: resolve type from AST annotation
            let param_type_ast = param_ast.ty.as_ref().ok_or_else(|| ResolutionError::InternalError {
                message: format!("Parameter '{}' is missing type annotation in AST", param_name),
                span: Some(param_ast.span),
            })?;
            resolve_type(
                db,
                definitions_map,
                module_scopes,
                prelude_scope,
                module_symbol,
                param_type_ast,
                &generic_param_names_in_scope,
                definition_context,
                param_ast.span,
            )?
        };

        // Assign a fresh symbol to the parameter
        let param_symbol = Symbol::fresh();

        params.push(ResolvedParameter {
            symbol: param_symbol, // Add the symbol here
            name: param_name,
            param_type: param_type,
            is_variadic: false, // TODO: Determine from AST pattern/type if possible
            has_default: false, // TODO: Determine from AST if possible
            span: param_ast.span,
        });
    }

    // Resolve return type.
    let return_ty = match &func_ast.return_type {
        Some(ty) => resolve_type(
            db,
            definitions_map,
            module_scopes,
            prelude_scope,
            module_symbol,
            ty, // Pass the &Type directly
            &generic_param_names_in_scope,
            definition_context,
            ty.span, // Use the type's span
        )?,
        // If no return type is specified, it implicitly returns unit.
        None => ResolvedType::Primitive(PrimitiveType::Unit),
    };

    // Resolve generic parameters defined directly on the function.
    let func_generic_params = resolve_generic_params(db, definitions_map, module_scopes, prelude_scope, module_symbol, func_ast.generic_params.as_ref(), definition_context)?;

    Ok(ResolvedFunction {
        symbol: def_info.symbol,
        name: def_info.name.clone(),
        module_symbol, // Module where the function source is defined
        parameters: params,
        return_type: return_ty,
        body: None, // Body (ResolvedExpr) is resolved in Pass 4
        generic_params: func_generic_params, // Store the resolved function's own generics here
        is_public: def_info.is_public,
        span: def_info.span,
        is_effectful: def_info.is_effectful, // Added
    })
}

/// Resolves the signature of a trait definition.
/// Resolves generic parameters, supertraits, and signatures of associated items (methods, types).
fn resolve_trait_signature<'db>(
    db: &'db dyn SyntaxDatabase,
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    module_scopes: &HashMap<Symbol, ModuleScope>,
    prelude_scope: &HashMap<String, Symbol>, // Added prelude
    module_symbol: Symbol, // Module where the trait is defined
    def_info: &DefinitionInfo<'db>,
    trait_ast: &ast::items::TraitDef,
    resolved_defs: &mut ResolvedDefinitions, // Mutable access to add resolved associated functions
    errors: &mut Vec<ResolutionError>,
    definition_context: Option<(DefinitionKind, Symbol)>, // Context for `Self` (within the trait)
) -> Result<ResolvedTrait, ResolutionError> {
    // Resolve generic parameters defined on the trait.
    let generic_params = resolve_generic_params(db, definitions_map, module_scopes, prelude_scope, module_symbol, trait_ast.generic_params.as_ref(), definition_context)?;
    let generic_param_names: Vec<String> = generic_params.iter().map(|p| p.name.clone()).collect();

    // TODO: Resolve supertrait references.
    // let resolved_supertraits = trait_ast.supertraits.iter()
    //     .map(|st_ast| resolve_type(db, definitions_map, module_scopes, module_symbol, st_ast, &generic_param_names, definition_context, st_ast.span))
    //     .collect::<Result<Vec<_>, _>>()?;
    // Need to ensure resolved supertraits are actually traits.

    // Resolve signatures of associated items (methods, types) defined within the trait.
    let mut resolved_methods = Vec::new();
    for item in &trait_ast.items {
        match item {
            ast::items::TraitItem::Method { function: func_ast, .. } => {
                // Find the DefinitionInfo for this associated function (created in Pass 1).
                let assoc_func_name = &func_ast.name.name;
                let assoc_func_symbol_opt = definitions_map.iter()
                    .find(|(_, d)| d.kind == DefinitionKind::Function && d.parent_symbol == Some(def_info.symbol) && &d.name == assoc_func_name)
                    .map(|(s, _)| *s);
                
                let assoc_func_symbol = match assoc_func_symbol_opt {
                    Some(s) => s,
                    None => {
                        errors.push(ResolutionError::InternalError {
                            message: format!("DefinitionInfo symbol not found for trait method {}::{} during signature resolution", def_info.name, assoc_func_name),
                            span: Some(func_ast.span)
                        });
                        continue; // Skip this method if its DefInfo wasn't found
                    }
                };
                
                // Get the DefinitionInfo using the found symbol
                // This unwrap is safe because we just found the symbol above.
                let assoc_func_def_info = definitions_map.get(&assoc_func_symbol).unwrap();

                // Resolve the function signature, passing the trait's generics as parent context.
                match resolve_function_signature(
                    db, definitions_map, module_scopes, prelude_scope,
                    module_symbol,
                    assoc_func_def_info, // Pass the correct DefinitionInfo
                    func_ast, // Pass the Function AST node from the TraitItem
                    &generic_params, // Trait generics are the parent generics for the method
                    definition_context, // Pass the trait context
                ) {
                    Ok(mut resolved_func) => {
                        // Trait methods don't have bodies defined in the trait block.
                        resolved_func.body = None;
                        // Add the fully resolved function to the main collection.
                        resolved_defs.functions.push(resolved_func);
                        // Store a reference (Symbol) to this method in the trait's definition.
                        resolved_methods.push(ResolvedAssociatedFunction {
                            func_symbol: assoc_func_def_info.symbol,
                            trait_method_symbol: None, // This *is* the trait method definition
                        });
                    }
                    Err(e) => errors.push(e),
                }
            },
            ast::items::TraitItem::AssociatedType { name, .. } => {
                // TODO: Resolve associated type declarations.
                // Need to store information about the associated type (name, bounds, etc.)
                // Might need a `ResolvedAssociatedType` struct in `types.rs`.
                 errors.push(ResolutionError::InternalError{
                    message: format!("Associated type {} resolution not yet implemented", name.name),
                    span: Some(name.span)
                });
            }
        }
    }

    Ok(ResolvedTrait {
        symbol: def_info.symbol,
        name: def_info.name.clone(),
        module_symbol,
        generic_params,
        methods: resolved_methods,
        // supertraits: resolved_supertraits, // TODO: Add field
        // associated_types: resolved_associated_types, // TODO: Add field
        is_public: def_info.is_public,
        span: def_info.span,
    })
}

/// Resolves the signature of an impl block (`impl Trait for Type` or `impl Type`).
/// Resolves generics, the implemented trait (if any), the implementing type (`Self`), and signatures of associated items.
fn resolve_impl_signature<'db>(
    db: &'db dyn SyntaxDatabase,
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    module_scopes: &HashMap<Symbol, ModuleScope>,
    prelude_scope: &HashMap<String, Symbol>, // Added prelude
    module_symbol: Symbol, // Module where the impl block is defined
    def_info: &DefinitionInfo<'db>,
    impl_ast: &ast::items::ImplDef,
    resolved_defs: &mut ResolvedDefinitions, // Mutable access to add resolved associated functions
    errors: &mut Vec<ResolutionError>,
    definition_context: Option<(DefinitionKind, Symbol)>, // Context for `Self` (within the impl)
) -> Result<ResolvedImpl, ResolutionError> {
    // Resolve generic parameters defined on the impl block itself.
    let generic_params = resolve_generic_params(db, definitions_map, module_scopes, prelude_scope, module_symbol, impl_ast.generic_params.as_ref(), definition_context)?;
    let generic_param_names: Vec<String> = generic_params.iter().map(|p| p.name.clone()).collect();

    // Resolve the optional trait being implemented.
    let resolved_trait_ref = match &impl_ast.trait_type {
        Some(trait_ty_ast) => {
            // Resolve the type path specified for the trait, including potential generics.
            match resolve_type(db, definitions_map, module_scopes, prelude_scope, module_symbol, trait_ty_ast, &generic_param_names, definition_context, trait_ty_ast.span)? {
                // Expecting it to resolve to a UserDefined type (which could be a Trait).
                ResolvedType::UserDefined { symbol: trait_symbol, .. } => {
                    // Verify that the resolved symbol actually corresponds to a Trait definition.
                    if definitions_map.get(&trait_symbol).map_or(false, |info| info.kind == DefinitionKind::Trait) {
                         Some(trait_symbol) // Return the base trait symbol
                    } else {
                         // Resolved to something, but it wasn't a trait.
                         errors.push(ResolutionError::TypeMismatch {
                             expected: "Trait".to_string(),
                             found: format!("{:?}", definitions_map.get(&trait_symbol).map(|i| i.kind)),
                             span: trait_ty_ast.span,
                             context: Some("in impl block trait reference".to_string()),
                         });
                         None // Treat as error, no trait symbol associated
                    }
                }
                // If the trait path resolved to something other than a UserDefined type.
                ty => {
                    errors.push(ResolutionError::TypeMismatch {
                        expected: "Trait".to_string(),
                        found: format!("{:?}", ty),
                        span: trait_ty_ast.span,
                        context: Some("in impl block trait reference".to_string()),
                    });
                    None
                }
            }
        }
        None => None, // This is an inherent impl (no trait specified).
    };

    // Resolve the implementing type (the `Self` type for this impl block).
    let resolved_implementing_type = resolve_type(db, definitions_map, module_scopes, prelude_scope, module_symbol, &impl_ast.self_type, &generic_param_names, definition_context, impl_ast.self_type.span)?;

    // Resolve signatures of associated items (methods, types) defined within the impl.
    let mut resolved_methods = Vec::new();
    for item in &impl_ast.items {
        match item {
            ast::items::ImplItem::Method(func_ast) => {
                // Find the DefinitionInfo for this associated function (created in Pass 1).
                let assoc_func_name = &func_ast.name.name;
                 let assoc_func_def_info = definitions_map.values()
                    .find(|d| d.kind == DefinitionKind::Function && d.parent_symbol == Some(def_info.symbol) && &d.name == assoc_func_name)
                    .ok_or_else(|| ResolutionError::InternalError {
                        message: format!("DefinitionInfo not found for impl method {} during signature resolution", assoc_func_name),
                        span: Some(func_ast.span)
                    })?;

                // Resolve the function signature, passing the impl's generics as parent context.
                 match resolve_function_signature(
                    db, definitions_map, module_scopes, prelude_scope,
                    module_symbol,
                    assoc_func_def_info, // Use the DefinitionInfo specific to the method
                    func_ast,
                    &generic_params, // Impl generics are the parent generics for the method
                    definition_context, // Pass the impl context for `Self` resolution
                ) {
                    Ok(resolved_func) => {
                        // Add the fully resolved function (signature only for now) to the main collection.
                        resolved_defs.functions.push(resolved_func);
                        // Store a reference to this method in the impl's definition.
                        // TODO: Link `trait_method_symbol` if this implements a trait method.
                        resolved_methods.push(ResolvedAssociatedFunction { 
                            func_symbol: assoc_func_def_info.symbol, 
                            trait_method_symbol: None, // Initialize as None, link later if trait impl
                        });
                    }
                    Err(e) => errors.push(e),
                }
            },
            ast::items::ImplItem::AssociatedType { name, ty: _, span } => {
                // TODO: Resolve associated type bindings.
                // 1. Resolve the provided `ty`.
                // 2. Store the binding (e.g., `type Output = i32;`) in the ResolvedImpl.
                // 3. Ensure the associated type exists in the implemented trait (if applicable).
                 errors.push(ResolutionError::InternalError{
                    message: format!("Associated type binding {} resolution not yet implemented", name.name),
                    span: Some(*span)
                });
            }
        }
    }

    // TODO: After resolving all methods, if this is a trait impl (`resolved_trait_ref.is_some()`),
    // iterate through the methods in the `ResolvedTrait` definition and try to match them
    // with the methods resolved here. Populate `trait_method_symbol` in `ResolvedAssociatedFunction`.
    // Also check for missing/extra methods compared to the trait definition.

    Ok(ResolvedImpl {
        impl_symbol: def_info.symbol,
        generic_params,
        trait_symbol: resolved_trait_ref,
        implementing_type: resolved_implementing_type,
        methods: resolved_methods,
        // associated_type_bindings: resolved_bindings, // TODO
        span: def_info.span,
    })
}

/// Helper to resolve generic parameters from an AST `GenericParam` list.
/// Currently resolves names only; bounds resolution requires where clauses or direct bounds.
///
/// # Arguments
/// * `_db`, `_module_scopes`, `_current_module_symbol`: Potentially needed if bounds involve resolving types.
/// * `generic_params_ast`: The optional `Vec<ast::expr::GenericParam>` from the AST.
/// * `definition_context`: Used to get a fallback span if the param AST node doesn't have one.
pub(crate) fn resolve_generic_params<'db>(
    _db: &'db dyn SyntaxDatabase, // Keep for potential future use with bounds
    _definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>, // Prefix unused variable
    _module_scopes: &HashMap<Symbol, ModuleScope>, // Keep for potential future use with bounds
    _prelude_scope: &HashMap<String, Symbol>, // Added prelude (unused for now)
    _current_module_symbol: Symbol, // Keep for potential future use with bounds
    generic_params_ast: Option<&Vec<ast::expr::GenericParam>>,
    _definition_context: Option<(DefinitionKind, Symbol)>, // Prefix unused variable // Context primarily for fallback span
) -> Result<Vec<ResolvedGenericParamDef>, ResolutionError> {
    let mut resolved_params = Vec::new();
    if let Some(params_ast) = generic_params_ast {
        for param_ast in params_ast {
            // TODO: Resolve bounds specified directly on the param or via where clauses.
            // For now, assume bounds are empty.
            let resolved_bounds = Vec::new();

            // Determine the span. Use the parameter's span if available, otherwise fallback.
            // Assuming ast::expr::GenericParam has a span field, otherwise use context.
            let span = param_ast.name.span;
             // Fallback span logic (if param_ast doesn't have a direct span):
             // let span = definition_context
             //     .and_then(|ctx| definitions_map.get(&ctx.1))
             //     .map_or(SourceSpan::from((0, 0)), |info| info.span);

            resolved_params.push(ResolvedGenericParamDef {
                name: param_ast.name.name.clone(),
                bounds: resolved_bounds,
                span, // Use span from the AST identifier
            });
        }
    }
    Ok(resolved_params)
}

/// Core helper to resolve an AST type reference (`ast::types::Type`) to a `ResolvedType`.
///
/// Handles resolving paths, primitive types, function types, tuples, arrays, generics,
/// type applications (`KindApp`), and the `Self` type.
///
/// # Arguments
///
/// * `db`: Database access.
/// * `definitions_map`: Map of all known definitions.
/// * `module_scopes`: Map of resolved scopes for each module.
/// * `prelude_scope`: Map of resolved symbols for predefined paths.
/// * `current_module_symbol`: The symbol of the module scope from which resolution should start.
/// * `ast_type`: The `ast::types::Type` node to resolve.
/// * `generic_params_in_scope`: Names of generic parameters currently in scope (e.g., from surrounding function or impl).
/// * `definition_context`: Context for resolving `Self` (e.g., `Some((DefinitionKind::Trait, trait_symbol))`).
/// * `type_span`: The source span of the `ast_type` node, used for error reporting.
pub(crate) fn resolve_type<'db>(
    db: &'db dyn SyntaxDatabase,
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    module_scopes: &HashMap<Symbol, ModuleScope>,
    prelude_scope: &HashMap<String, Symbol>, // Added prelude scope
    current_module_symbol: Symbol,
    ast_type: &Type,
    generic_params_in_scope: &[String], // Names of generics visible in this scope
    definition_context: Option<(DefinitionKind, Symbol)>,
    type_span: SourceSpan, // Use the span passed from the caller (e.g., ty.span)
) -> Result<ResolvedType, ResolutionError> {
    // TODO: Consider if prelude_scope should be part of a larger context struct.
    let resolve_path_with_prelude = |segments: &[ast::common::Ident], span: SourceSpan| -> Result<Symbol, ResolutionError> {
        // This closure needs access to definitions_map, module_scopes, current_module_symbol
        // It needs the prelude_scope which is now passed to resolve_type.
        resolve_path(definitions_map, module_scopes, prelude_scope, current_module_symbol, segments, span)
    };

    match &ast_type.kind {
        TypeKind::Path(segments) => {
            // --- Path Resolution --- 
            if segments.is_empty() {
                 return Err(ResolutionError::InternalError { message: "Attempted to resolve empty type path".to_string(), span: Some(type_span) });
            }

            // Check for single-segment paths which might be primitives, generics, or Self.
            if segments.len() == 1 {
                let name = &segments[0].name;
                let segment_span = segments[0].span;

                // 1. Check for `Self` type keyword.
                if name == "Self" {
                    // `Self` is only valid within a Trait or Impl context.
                    if let Some((kind, _symbol)) = definition_context {
                        if kind == DefinitionKind::Trait || kind == DefinitionKind::Impl {
                             return Ok(ResolvedType::SelfType);
                        } else {
                             return Err(ResolutionError::InternalError {
                                 message: format!("'Self' type used outside of trait or impl context (in {:?})", kind),
                                 span: Some(segment_span)
                             });
                        }
                    } else {
                        return Err(ResolutionError::InternalError {
                            message: "'Self' type used outside of trait or impl context".to_string(),
                            span: Some(segment_span)
                        });
                    }
                }

                // 2. Check if it's a generic parameter in the current scope.
                if generic_params_in_scope.contains(name) {
                    return Ok(ResolvedType::GenericParam(name.clone()));
                }

                // 3. Check for specific primitive type names.
                match name.as_str() {
                    // --- Standard Primitive Keywords ---
                    "bool" => return Ok(ResolvedType::Primitive(PrimitiveType::Bool)),
                    "char" => return Ok(ResolvedType::Primitive(PrimitiveType::Char)),
                    "string" => return Ok(ResolvedType::Primitive(PrimitiveType::String)),
                    // Sized Integers
                    "i8"   => return Ok(ResolvedType::Primitive(PrimitiveType::I8)),
                    "i16"  => return Ok(ResolvedType::Primitive(PrimitiveType::I16)),
                    "i32"  => return Ok(ResolvedType::Primitive(PrimitiveType::I32)),
                    "i64"  => return Ok(ResolvedType::Primitive(PrimitiveType::I64)),
                    "i128" => return Ok(ResolvedType::Primitive(PrimitiveType::I128)),
                    "u8"   => return Ok(ResolvedType::Primitive(PrimitiveType::U8)),
                    "u16"  => return Ok(ResolvedType::Primitive(PrimitiveType::U16)),
                    "u32"  => return Ok(ResolvedType::Primitive(PrimitiveType::U32)),
                    "u64"  => return Ok(ResolvedType::Primitive(PrimitiveType::U64)),
                    "u128" => return Ok(ResolvedType::Primitive(PrimitiveType::U128)),
                    // Sized Floats
                    "f32"  => return Ok(ResolvedType::Primitive(PrimitiveType::F32)),
                    "f64"  => return Ok(ResolvedType::Primitive(PrimitiveType::F64)),
                    // Special case: Unit type often written as `()`
                    // Handle this in Tuple resolution? Or add a dedicated keyword `unit`?
                    // For now, assume `()` is handled by TypeKind::Tuple([]) which maps to PrimitiveType::Unit
                    // during function signature resolution (implicit return).
                    // If a specific keyword like `unit` is needed, add it here.
                    _ => { /* Not a primitive, continue to general path resolution below */ }
                }
            }

            // 4. Resolve as a potentially multi-segment path (e.g., `my_mod::MyStruct`).
            // This block is now only reached if it's not Self, a generic, or a primitive.
            match resolve_path_with_prelude(segments, type_span) { // Use the closure
                Ok(resolved_symbol) => {
                    // Path resolved to a symbol. Check if this symbol represents a type.
                    if let Some(def_info) = definitions_map.get(&resolved_symbol) {
                        match def_info.kind {
                            // Structs, Enums, and Traits can be used as types.
                            DefinitionKind::Struct | DefinitionKind::Enum | DefinitionKind::Trait => {
                                // TODO: Should Trait have its own ResolvedType variant, e.g., `ResolvedType::Trait(symbol)`?
                                // For now, treat it like other UserDefined types.
                                Ok(ResolvedType::UserDefined { symbol: resolved_symbol, type_args: None })
                            },
                            // Other kinds (Function, Module, Impl, Variant) are not types themselves.
                            _ => Err(ResolutionError::TypeMismatch {
                                expected: "Type (Struct, Enum, or Trait)".to_string(),
                                found: format!("{:?}", def_info.kind),
                                span: type_span,
                                context: Some(format!("path '{}' resolved to a non-type definition", def_info.full_path)),
                            }),
                        }
                    } else {
                         // Should be unreachable if resolve_path succeeded and returned a valid Symbol.
                         Err(ResolutionError::InternalError {
                             message: format!("Resolved path symbol {:?} not found in definitions map", resolved_symbol),
                             span: Some(type_span),
                         })
                    }
                }
                Err(e) => Err(e), // Propagate path resolution error
            }
        }
        TypeKind::Function(param_ty, ret_ty) => {
            // --- Function Type Resolution --- 
            // The AST `TypeKind::Function` represents `Param -> Ret`.
            // `Param` can be a single type or a tuple type for multiple parameters.

            // Resolve the return type.
            let resolved_ret = resolve_type(db, definitions_map, module_scopes, prelude_scope, current_module_symbol, ret_ty, generic_params_in_scope, definition_context, ret_ty.span)?;

            // Resolve the parameter type(s).
            let resolved_params = match &param_ty.kind {
                // If `Param` is explicitly a tuple `(T1, T2)`, resolve each element.
                TypeKind::Tuple(param_types) => {
                    param_types.iter()
                        .map(|ty| resolve_type(db, definitions_map, module_scopes, prelude_scope, current_module_symbol, ty, generic_params_in_scope, definition_context, ty.span))
                        .collect::<Result<Vec<_>, _>>()?
                },
                 // If `Param` is a single type (e.g., `unit` for no params, `T` for one param).
                _ => {
                     let single_param_type = resolve_type(db, definitions_map, module_scopes, prelude_scope, current_module_symbol, param_ty, generic_params_in_scope, definition_context, param_ty.span)?;
                     // If the single type resolved to Unit, it represents a function with no parameters.
                     if single_param_type == ResolvedType::Primitive(PrimitiveType::Unit) {
                         Vec::new()
                     } else {
                         // Otherwise, it's a function with one parameter of this type.
                         vec![single_param_type]
                     }
                 }
            };

            Ok(ResolvedType::Function {
                param_types: resolved_params,
                return_type: Box::new(resolved_ret),
            })
        }
        TypeKind::Tuple(types) => {
            // --- Tuple Type Resolution --- 
            let resolved_types = types.iter()
                .map(|ty| resolve_type(db, definitions_map, module_scopes, prelude_scope, current_module_symbol, ty, generic_params_in_scope, definition_context, ty.span))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(ResolvedType::Tuple(resolved_types))
        }
        TypeKind::Array(elem_ty, size) => {
            // --- Array Type Resolution --- 
            let resolved_elem = resolve_type(db, definitions_map, module_scopes, prelude_scope, current_module_symbol, elem_ty, generic_params_in_scope, definition_context, elem_ty.span)?;
            Ok(ResolvedType::Array {
                element_type: Box::new(resolved_elem),
                size: Some(*size), // Assuming size is always known from AST?
            })
        }
        TypeKind::KindApp(base_ty, arg_types) => {
            // --- Type Application (Generics) Resolution --- 
            // e.g., `List<i32>`

            // 1. Resolve the base type (e.g., `List`).
            let resolved_base = resolve_type(db, definitions_map, module_scopes, prelude_scope, current_module_symbol, base_ty, generic_params_in_scope, definition_context, base_ty.span)?;

            // 2. Resolve the type arguments (e.g., `i32`).
             let resolved_args = arg_types.iter()
                .map(|ty| resolve_type(db, definitions_map, module_scopes, prelude_scope, current_module_symbol, ty, generic_params_in_scope, definition_context, ty.span))
                .collect::<Result<Vec<_>, _>>()?;

            // 3. Combine them. Expect the base to be a UserDefined type that is generic.
            if let ResolvedType::UserDefined { symbol, type_args: None } = resolved_base {
                 // Base type resolved successfully and doesn't already have args applied.
                 // TODO: Check arity of generics from DefinitionInfo
                 Ok(ResolvedType::UserDefined { symbol, type_args: Some(resolved_args) })
            } else {
                 Err(ResolutionError::TypeMismatch {
                     expected: "Generic Base Type (Struct, Enum, Trait)".to_string(),
                     found: format!("{:?}", resolved_base),
                     span: base_ty.span,
                     context: Some("applying type arguments".to_string()),
                 })
            }
        }
    }
}

/// Helper to resolve a path relative to a module scope.
/// Tries current scope, then parent scopes recursively.
pub(crate) fn resolve_path<'db>(
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    module_scopes: &HashMap<Symbol, ModuleScope>,
    prelude_scope: &HashMap<String, Symbol>, // Added prelude scope
    current_module_symbol: Symbol,
    segments: &[ast::common::Ident],
    path_span: SourceSpan, // Add span parameter
) -> Result<Symbol, ResolutionError> {
    let _path_str = segments.iter().map(|s|s.name.as_str()).collect::<Vec<_>>().join("::"); // Prefix unused variable
    // println!("DEBUG: Resolving path: {} from module symbol {:?}", path_str, current_module_symbol);

    if segments.is_empty() {
        return Err(ResolutionError::InternalError { 
            message: "Attempted to resolve empty path".to_string(), 
            span: None
        });
    }

    let start_segment_ident = &segments[0];
    let start_segment = &start_segment_ident.name;
    
    // Handle special path segments
    if start_segment == "crate" {
        // Find the crate root module
        let crate_root = find_crate_root(definitions_map, current_module_symbol)?;
        
        if segments.len() == 1 {
            // Just "crate" - return the crate root module
            return Ok(crate_root);
        } else {
            // "crate::something::..." - resolve the rest relative to crate root
            return resolve_path_from(definitions_map, module_scopes, prelude_scope, crate_root, &segments[1..], path_span);
        }
    } else if start_segment == "super" {
        // Find the parent module
        let parent = find_parent_module(definitions_map, current_module_symbol)?;
        
        if segments.len() == 1 {
            // Just "super" - return the parent module
            return Ok(parent);
        } else {
            // "super::something::..." - resolve the rest relative to parent
            return resolve_path_from(definitions_map, module_scopes, prelude_scope, parent, &segments[1..], path_span);
        }
    } else if start_segment == "self" {
        if segments.len() == 1 {
            // Just "self" - return the current module
            return Ok(current_module_symbol);
        } else {
            // "self::something::..." - resolve the rest relative to current module
            return resolve_path_from(definitions_map, module_scopes, prelude_scope, current_module_symbol, &segments[1..], path_span);
        }
    }
    
    // Helper function to search scopes recursively
    fn find_in_scopes(
        module_scopes: &HashMap<Symbol, ModuleScope>,
        definitions_map: &HashMap<Symbol, DefinitionInfo>,
        module_symbol: Symbol,
        name: &str,
    ) -> Option<Symbol> {
        // DEBUG LOG: Entering find_in_scopes
        // println!("  DEBUG [find_in_scopes]: Searching for '{}' starting from module {:?}", name, module_symbol);

        // Check current scope
        if let Some(scope) = module_scopes.get(&module_symbol) {
            // println!("    DEBUG [find_in_scopes]: Found scope for {:?}", module_symbol);
            if let Some(entry) = scope.items.get(name) {
                // println!("    DEBUG [find_in_scopes]: Found '{}' in scope of {:?} -> {:?}", name, module_symbol, entry.symbol);
                // TODO: Mark import used here? entry.mark_used();
                return Some(entry.symbol);
            }
        } else {
            // println!("    DEBUG [find_in_scopes]: Scope *not* found for module {:?}", module_symbol);
        }
        
        // Check parent scope if this module has a parent
        // DEBUG LOG: Check parent lookup
        // println!("    DEBUG [find_in_scopes]: Looking up parent for {:?} in definitions_map (size: {})", module_symbol, definitions_map.len());
        if let Some(def_info) = definitions_map.get(&module_symbol) {
            // println!("      -> Found def_info for {:?}: name='{}', parent={:?}", module_symbol, def_info.name, def_info.parent_symbol);
            if let Some(parent_symbol) = def_info.parent_symbol {
                // println!("      -> Recursively searching parent: {:?}", parent_symbol);
                find_in_scopes(module_scopes, definitions_map, parent_symbol, name)
            } else {
                // println!("      -> No parent symbol found for {:?}. Stopping search.", module_symbol);
                None // No parent scope
            }
        } else {
            // println!("      -> Def_info *not* found for module symbol {:?} in definitions map!", module_symbol);
            None // Module not found in definitions
        }
    }

    // 1. Look up the first segment in the current scope and parent scopes
    let mut initial_resolved_symbol = find_in_scopes(module_scopes, definitions_map, current_module_symbol, start_segment);

    // 2. If not found in scopes, check the prelude for the *first segment*.
    if initial_resolved_symbol.is_none() {
        if let Some(prelude_symbol) = prelude_scope.get(start_segment) {
            // println!("DEBUG: Found '{}' in prelude.", start_segment);
            initial_resolved_symbol = Some(*prelude_symbol);
        }
    }

    // 3. Handle not found after checking scopes and prelude
    if initial_resolved_symbol.is_none() {
        // println!("DEBUG: Initial segment '{}' not found in scopes or prelude starting from {:?}", start_segment, current_module_symbol); // Log not found
        return Err(ResolutionError::NameNotFound {
            name: start_segment.clone(),
            span: path_span, // Use overall path span
            help: Some(format!("Searched in module `{}` and prelude", 
                definitions_map.get(&current_module_symbol)
                    .map_or("unknown".to_string(), |info| info.full_path.clone())
            )),
        });
    }
    
    let mut current_resolved_symbol = initial_resolved_symbol.unwrap();

    // 4. Resolve subsequent segments
    // println!("DEBUG: Initial segment '{}' resolved to {:?}. Resolving remaining segments...", start_segment, current_resolved_symbol);
    for segment_ident in segments.iter().skip(1) {
        let segment_name = &segment_ident.name;
        // println!("DEBUG: Resolving next segment: '{}' from current symbol {:?}", segment_name, current_resolved_symbol);
        
        if let Some(def_info) = definitions_map.get(&current_resolved_symbol) {
            // We can only traverse into Modules or Enums (for variants)
            match def_info.kind {
                DefinitionKind::Module => {
                    // Find a child definition (item or submodule) within this module
                    let found_child = find_in_module_children(definitions_map, current_resolved_symbol, segment_name);
                    
                    if let Some(child_symbol) = found_child {
                        // Check visibility before resolving further
                        if let Some(child_def_info) = definitions_map.get(&child_symbol) {
                            // Visibility check: item is public OR we are accessing from within the same module or a child module
                            if child_def_info.is_public || crate::scopes::is_accessible(definitions_map, current_module_symbol, child_symbol) {
                                current_resolved_symbol = child_symbol;
                                continue; // Move to next segment
                            } else {
                                // println!("DEBUG: Access denied to private item: {}", child_def_info.full_path);
                                return Err(ResolutionError::PrivateItemAccess {
                                    path: segment_name.clone(),
                                    span: path_span, // Use overall path span
                                });
                            }
                        } // else: child symbol not in map (internal error)
                    } // else: child name not found in module
                },
                DefinitionKind::Enum => {
                    // Find an enum variant with this name
                    let found_variant = find_enum_variant(definitions_map, current_resolved_symbol, segment_name);
                    if let Some(variant_symbol) = found_variant {
                        // Variants inherit enum visibility, already checked for the enum.
                        // Check if this is the *last* segment
                        if segment_ident as *const _ == segments.last().unwrap() as *const _ { // Pointer comparison to check if it's the last segment
                            current_resolved_symbol = variant_symbol;
                            continue; // Successfully resolved to variant
                        } else {
                            // Cannot path further through an enum variant
                            return Err(ResolutionError::InvalidPathSegment {
                                segment: segment_name.clone(),
                                path: segments.iter().map(|s| s.name.as_str()).collect::<Vec<_>>().join("::"),
                                span: path_span, // Use overall path span
                            });
                        }
                    } // else: variant name not found in enum
                },
                 _ => { // Cannot path through Struct, Function, Trait, Impl, etc.
                    //  println!("DEBUG: Segment '{}' not found as child/variant of {:?}", segment_name, current_resolved_symbol);
                     return Err(ResolutionError::InvalidPathSegment {
                        segment: segment_name.clone(),
                        path: segments.iter().map(|s| s.name.as_str()).collect::<Vec<_>>().join("::"),
                        span: path_span, // Use overall path span
                    });
                 }
            }
            // If code reaches here, the segment name was not found as a valid child/variant
            return Err(ResolutionError::InvalidPathSegment {
                segment: segment_name.clone(),
                path: segments.iter().map(|s| s.name.as_str()).collect::<Vec<_>>().join("::"),
                span: path_span, // Use overall path span
            });

        } else {
            return Err(ResolutionError::InternalError { 
                message: format!("Path segment resolution failed: intermediate symbol not found in definitions map"),
                span: Some(path_span), // Use overall path span
            });
        }
    }

    // Final check to ensure the symbol is in the definitions map
    if definitions_map.contains_key(&current_resolved_symbol) {
        Ok(current_resolved_symbol)
    } else {
        // This should be unreachable if the logic above is correct.
        Err(ResolutionError::InternalError { 
            message: format!("Final resolved symbol not found in definitions map"),
            span: None, 
        })
    }
}

/// Resolve a path starting from a specific module
pub(crate) fn resolve_path_from<'db>(
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    _module_scopes: &HashMap<Symbol, ModuleScope>, // Prefix unused variable
    _prelude_scope: &HashMap<String, Symbol>, // Prefix unused variable // Added prelude scope
    start_module: Symbol,
    segments: &[ast::common::Ident],
    path_span: SourceSpan, // Add span parameter
) -> Result<Symbol, ResolutionError> {
    if segments.is_empty() {
        return Ok(start_module); // Resolved to the start module itself
    }
    
    // Use the general resolve_path logic, but force it to start 
    // by looking directly within the children of start_module for the first segment.
    let first_segment_ident = &segments[0];
    let first_segment_name = &first_segment_ident.name;
    
    let mut current_symbol = find_in_module_children(definitions_map, start_module, first_segment_name)
        .ok_or_else(|| ResolutionError::InvalidPathSegment { 
            segment: first_segment_name.clone(), 
            path: segments.iter().map(|s| s.name.as_str()).collect::<Vec<_>>().join("::"),
            span: path_span,
        })?;

    // Check visibility of the first segment relative to the *original* calling module
    // NOTE: We pass start_module here because visibility is checked from the module *containing* the item, 
    // not the original module initiating the path resolution, when resolving relative paths.
    if !crate::scopes::is_accessible(definitions_map, start_module, current_symbol) {
         return Err(ResolutionError::PrivateItemAccess {
            path: first_segment_name.clone(),
            span: path_span,
        });
    }

    // Resolve subsequent segments
    for segment_ident in segments.iter().skip(1) {
        let segment_name = &segment_ident.name;
        
        if let Some(def_info) = definitions_map.get(&current_symbol) {
            match def_info.kind {
                DefinitionKind::Module => {
                    let found_child = find_in_module_children(definitions_map, current_symbol, segment_name);
                    if let Some(child_symbol) = found_child {
                        // Check visibility relative to the *module containing the child*
                        if crate::scopes::is_accessible(definitions_map, current_symbol, child_symbol) {
                             current_symbol = child_symbol;
                             continue;
                        } else {
                             return Err(ResolutionError::PrivateItemAccess { path: segment_name.clone(), span: path_span });
                        }
                    }
                },
                DefinitionKind::Enum => {
                    let found_variant = find_enum_variant(definitions_map, current_symbol, segment_name);
                    if let Some(variant_symbol) = found_variant {
                        if segment_ident as *const _ == segments.last().unwrap() as *const _ {
                            current_symbol = variant_symbol;
                            continue; 
                        } else {
                             return Err(ResolutionError::InvalidPathSegment { segment: segment_name.clone(), path: segments.iter().map(|s| s.name.as_str()).collect::<Vec<_>>().join("::"), span: path_span });
                        }
                    } 
                },
                _ => { 
                    return Err(ResolutionError::InvalidPathSegment { segment: segment_name.clone(), path: segments.iter().map(|s| s.name.as_str()).collect::<Vec<_>>().join("::"), span: path_span });
                }
            }
             return Err(ResolutionError::InvalidPathSegment { segment: segment_name.clone(), path: segments.iter().map(|s| s.name.as_str()).collect::<Vec<_>>().join("::"), span: path_span });
        } else {
             return Err(ResolutionError::InternalError { message: format!("Intermediate symbol {:?} not found during path resolution", current_symbol), span: Some(path_span) });
        }
    }
    
    Ok(current_symbol)
}

/// Find the crate root module symbol starting from any module.
fn find_crate_root<'db>(
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    current_module: Symbol,
) -> Result<Symbol, ResolutionError> {
    let mut module = current_module;
    
    // Travel up through parent modules until we reach a module with no parent
    loop {
        if let Some(def_info) = definitions_map.get(&module) {
            if let Some(parent) = def_info.parent_symbol {
                module = parent;
            } else {
                // No parent means we've found the crate root
                return Ok(module);
            }
        } else {
            return Err(ResolutionError::InternalError {
                message: format!("Module {} not found in definitions map while finding crate root", module.id()),
                span: None,
            });
        }
    }
}

/// Find the parent module of a given module
fn find_parent_module<'db>(
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    current_module: Symbol,
) -> Result<Symbol, ResolutionError> {
    if let Some(def_info) = definitions_map.get(&current_module) {
        if let Some(parent) = def_info.parent_symbol {
            Ok(parent)
        } else {
            Err(ResolutionError::InternalError { // Or maybe a specific error like CannotGoSuperFromRoot?
                message: format!("Cannot access parent module: current module `{}` is the crate root", def_info.full_path),
                span: None,
            })
        }
    } else {
        Err(ResolutionError::InternalError {
            message: format!("Module {} not found in definitions map while finding parent", current_module.id()),
            span: None,
        })
    }
}

/// Helper to find a child definition (item or submodule) in a module by name.
/// Excludes enum variants, which are handled by `find_enum_variant`.
///
/// # Arguments
/// * `definitions_map`: Map containing all definition info.
/// * `module_symbol`: The symbol of the parent module to search within.
/// * `child_name`: The simple name of the child item to find.
///
/// # Returns
/// `Some(Symbol)` if a direct, non-variant child with the given name is found,
/// `None` otherwise.
fn find_in_module_children<'db>(
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    module_symbol: Symbol,
    child_name: &str,
) -> Option<Symbol> {
    // Check if the module_symbol is an enum
    let is_enum = definitions_map.get(&module_symbol)
        .map_or(false, |info| info.kind == DefinitionKind::Enum);
    
    if is_enum {
        // If it's an enum, we need to find an enum variant with the given name
        definitions_map.iter()
            .find(|(_, def_info)| 
                def_info.parent_symbol == Some(module_symbol) && 
                def_info.name == child_name &&
                def_info.kind == DefinitionKind::EnumVariant
            )
            .map(|(symbol, _)| *symbol)
    } else {
        // For regular modules, find any child except enum variants
        definitions_map.iter()
            .find(|(_, def_info)| 
                def_info.parent_symbol == Some(module_symbol) && 
                def_info.name == child_name &&
                def_info.kind != DefinitionKind::EnumVariant // Exclude variants for regular modules
            )
            .map(|(symbol, _)| *symbol)
    }
}

/// Helper to find an enum variant by name within a specific enum definition.
///
/// # Arguments
/// * `definitions_map`: Map containing all definition info.
/// * `enum_symbol`: The symbol of the enum definition to search within.
/// * `variant_name`: The simple name of the variant to find.
///
/// # Returns
/// `Some(Symbol)` if the `enum_symbol` corresponds to an Enum definition and
/// a variant with the given name exists within it. `None` otherwise.
fn find_enum_variant<'db>(
    definitions_map: &HashMap<Symbol, DefinitionInfo<'db>>,
    enum_symbol: Symbol,
    variant_name: &str,
) -> Option<Symbol> {
    // First check if enum_symbol is actually an enum
    if definitions_map.get(&enum_symbol).map_or(false, |info| info.kind == DefinitionKind::Enum) {
         for (symbol, def_info) in definitions_map {
            if def_info.kind == DefinitionKind::EnumVariant && def_info.parent_symbol == Some(enum_symbol) && def_info.name == variant_name {
                return Some(*symbol);
            }
        }
    }
    None
}

/// Builds the prelude scope by resolving predefined paths against the stdlib.
/// This should be called once after the stdlib frame is loaded and definitions collected.
pub(crate) fn build_prelude_scope(
    definitions_map: &HashMap<Symbol, DefinitionInfo>,
    module_scopes: &HashMap<Symbol, ModuleScope>, // Needed to get the prelude module's scope
) -> HashMap<String, Symbol> {
    let mut prelude = HashMap::new();
    let dummy_span = miette::SourceSpan::from(0..0); // For path resolution

    // 1. Define the path to the prelude module
    let prelude_path_segments: Vec<ast::common::Ident> = vec!["std", "prelude"]
        .into_iter()
        .map(|s| ast::common::Ident { name: s.to_string(), span: dummy_span })
        .collect();

    // 2. Find the root 'std' module symbol
    let std_root_symbol = match definitions_map.iter().find(|(_, info)| info.name == "std" && info.parent_symbol.is_none()) {
        Some((symbol, _)) => *symbol,
        None => {
            println!("WARNING: Standard library root 'std' not found. Prelude will be empty.");
            return prelude; // Return empty scope
        }
    };

    // 3. Resolve the path std::prelude starting from the std root
    // Note: Pass an empty prelude scope for resolving the prelude module path itself.
    let empty_prelude_scope = HashMap::new();
    match crate::resolve_types::resolve_path_from(definitions_map, module_scopes, &empty_prelude_scope, std_root_symbol, &prelude_path_segments[1..], dummy_span) {
        Ok(prelude_module_symbol) => {
            // 4. Get the scope of the resolved prelude module
            if let Some(prelude_scope) = module_scopes.get(&prelude_module_symbol) {
                println!("DEBUG: Found prelude module scope for symbol: {:?}", prelude_module_symbol);
                // 5. Iterate through items in the prelude module's scope and add them to our prelude map
                for (name, scope_entry) in &prelude_scope.items {
                    // Check if the item itself is public using definitions_map
                    if let Some(def_info) = definitions_map.get(&scope_entry.symbol) {
                        if def_info.is_public {
                            println!("  -> Adding prelude item: '{}' -> {:?}", name, scope_entry.symbol);
                            prelude.insert(name.clone(), scope_entry.symbol);
                        }
                    }
                }
            } else {
                 println!("WARNING: Scope for prelude module symbol {:?} not found. Prelude may be incomplete.", prelude_module_symbol);
            }
        }
        Err(e) => {
            println!("WARNING: Failed to resolve prelude module path 'std::prelude': {:?}. Prelude will be empty.", e);
        }
    }

    prelude
}

#[cfg(test)]
mod tests {
    use super::*; // Import items from parent module
    use crate::definitions::{DefinitionInfo, DefinitionKind};
    use crate::types::{Symbol, ResolvedType, PrimitiveType};
    use crate::scopes::ModuleScope;
    use parallax_syntax::ast::common::Ident;
    use miette::SourceSpan;
    use std::collections::HashMap;

    // Helper to create a dummy span
    fn dummy_span() -> SourceSpan {
        SourceSpan::from((0, 0))
    }

    // Helper to create simple DefinitionInfo for testing path resolution
    fn create_def_info(symbol: Symbol, parent: Option<Symbol>, name: &str, kind: DefinitionKind, is_public: bool) -> DefinitionInfo<'static> {
        DefinitionInfo {
            symbol,
            parent_symbol: parent,
            kind,
            name: name.to_string(),
            full_path: format!("test::{}", name), // Simple path for testing
            is_public,
            span: dummy_span(),
            ast_item: None,
            generic_params: None,
            variant_kind: None,
            impl_trait_ast: None,
            impl_type_ast: None,
            supertrait_asts: None,
            special_kind: None,
            is_effectful: false, // Add default for tests
        }
    }

    // Setup a common definitions map for path tests
    fn setup_defs_for_path_tests() -> HashMap<Symbol, DefinitionInfo<'static>> {
        let mut defs = HashMap::new();
        let root = Symbol::fresh();        // Module
        let mod_a = Symbol::fresh();       // Module
        let item_x = Symbol::fresh();      // Function in mod_a
        let enum_y = Symbol::fresh();      // Enum in mod_a
        let variant_z = Symbol::fresh();   // Variant in enum_y
        let mod_b = Symbol::fresh();       // Module

        defs.insert(root, create_def_info(root, None, "root", DefinitionKind::Module, true));
        defs.insert(mod_a, create_def_info(mod_a, Some(root), "mod_a", DefinitionKind::Module, true));
        defs.insert(item_x, create_def_info(item_x, Some(mod_a), "item_x", DefinitionKind::Function, true));
        defs.insert(enum_y, create_def_info(enum_y, Some(mod_a), "EnumY", DefinitionKind::Enum, true));
        defs.insert(variant_z, create_def_info(variant_z, Some(enum_y), "VariantZ", DefinitionKind::EnumVariant, true));
        defs.insert(mod_b, create_def_info(mod_b, Some(root), "mod_b", DefinitionKind::Module, true));
        
        defs
    }

    #[test]
    fn test_find_crate_root() {
        let defs = setup_defs_for_path_tests();
        let root_symbol = defs.values().find(|d| d.parent_symbol.is_none()).unwrap().symbol;
        let mod_a_symbol = defs.values().find(|d| d.name == "mod_a").unwrap().symbol;
        let item_x_symbol = defs.values().find(|d| d.name == "item_x").unwrap().symbol;
        let enum_y_symbol = defs.values().find(|d| d.name == "EnumY").unwrap().symbol;
        let variant_z_symbol = defs.values().find(|d| d.name == "VariantZ").unwrap().symbol;

        assert_eq!(find_crate_root(&defs, root_symbol), Ok(root_symbol));
        assert_eq!(find_crate_root(&defs, mod_a_symbol), Ok(root_symbol));
        assert_eq!(find_crate_root(&defs, item_x_symbol), Ok(root_symbol));
        assert_eq!(find_crate_root(&defs, enum_y_symbol), Ok(root_symbol));
        assert_eq!(find_crate_root(&defs, variant_z_symbol), Ok(root_symbol));
    }

    #[test]
    fn test_find_parent_module() {
        let defs = setup_defs_for_path_tests();
        let root_symbol = defs.values().find(|d| d.parent_symbol.is_none()).unwrap().symbol;
        let mod_a_symbol = defs.values().find(|d| d.name == "mod_a").unwrap().symbol;
        let item_x_symbol = defs.values().find(|d| d.name == "item_x").unwrap().symbol;
        let enum_y_symbol = defs.values().find(|d| d.name == "EnumY").unwrap().symbol;
        let variant_z_symbol = defs.values().find(|d| d.name == "VariantZ").unwrap().symbol;

        assert!(find_parent_module(&defs, root_symbol).is_err(), "Root should have no parent");
        assert_eq!(find_parent_module(&defs, mod_a_symbol), Ok(root_symbol));
        assert_eq!(find_parent_module(&defs, item_x_symbol), Ok(mod_a_symbol));
        assert_eq!(find_parent_module(&defs, enum_y_symbol), Ok(mod_a_symbol));
        assert_eq!(find_parent_module(&defs, variant_z_symbol), Ok(enum_y_symbol));
    }

    #[test]
    fn test_find_in_module_children() {
        let defs = setup_defs_for_path_tests();
        let root_symbol = defs.values().find(|d| d.parent_symbol.is_none()).unwrap().symbol;
        let mod_a_symbol = defs.values().find(|d| d.name == "mod_a").unwrap().symbol;
        let item_x_symbol = defs.values().find(|d| d.name == "item_x").unwrap().symbol;
        let enum_y_symbol = defs.values().find(|d| d.name == "EnumY").unwrap().symbol;
        let variant_z_symbol = defs.values().find(|d| d.name == "VariantZ").unwrap().symbol;
        let mod_b_symbol = defs.values().find(|d| d.name == "mod_b").map(|i| i.symbol).unwrap();

        // Test direct children of root
        assert_eq!(find_in_module_children(&defs, root_symbol, "mod_a"), Some(mod_a_symbol));
        assert_eq!(find_in_module_children(&defs, root_symbol, "mod_b"), Some(mod_b_symbol));
        
        // These are NOT direct children of root
        assert_eq!(find_in_module_children(&defs, root_symbol, "item_x"), None);
        assert_eq!(find_in_module_children(&defs, root_symbol, "EnumY"), None);
        assert_eq!(find_in_module_children(&defs, root_symbol, "VariantZ"), None);
        
        // Test direct children of mod_a
        assert_eq!(find_in_module_children(&defs, mod_a_symbol, "item_x"), Some(item_x_symbol));
        assert_eq!(find_in_module_children(&defs, mod_a_symbol, "EnumY"), Some(enum_y_symbol));
        // VariantZ is not a direct child of mod_a, it's a child of EnumY
        assert_eq!(find_in_module_children(&defs, mod_a_symbol, "VariantZ"), None);
        
        // Test direct children of EnumY
        assert_eq!(find_in_module_children(&defs, enum_y_symbol, "VariantZ"), Some(variant_z_symbol));
    }

    #[test]
    fn test_find_enum_variant() {
        let defs = setup_defs_for_path_tests();
        let root_symbol = defs.values().find(|d| d.parent_symbol.is_none()).unwrap().symbol;
        let mod_a_symbol = defs.values().find(|d| d.name == "mod_a").unwrap().symbol;
        let item_x_symbol = defs.values().find(|d| d.name == "item_x").unwrap().symbol;
        let enum_y_symbol = defs.values().find(|d| d.name == "EnumY").unwrap().symbol;
        let variant_z_symbol = defs.values().find(|d| d.name == "VariantZ").unwrap().symbol;

        assert_eq!(find_enum_variant(&defs, enum_y_symbol, "VariantZ"), Some(variant_z_symbol));
        assert_eq!(find_enum_variant(&defs, enum_y_symbol, "NonExistentVariant"), None);

        assert_eq!(find_enum_variant(&defs, root_symbol, "mod_a"), None);
        assert_eq!(find_enum_variant(&defs, mod_a_symbol, "item_x"), None);
        assert_eq!(find_enum_variant(&defs, item_x_symbol, "anything"), None);
    }
} 