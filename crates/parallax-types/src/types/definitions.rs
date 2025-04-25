use std::collections::{HashMap};
use miette::SourceSpan;
use parallax_resolve::types::Symbol;

// TODO: Update these imports after splitting types.rs and moving traits.rs
use crate::types::core::{Ty, TypeId};
use crate::context::trait_repo::{TraitId, ImplId}; // Updated path

/// Information about a typed function
#[derive(Debug, Clone)]
pub struct TypedFunction {
    /// The name of the function
    pub name: String,
    /// Parameters with their types
    pub params: Vec<TypedParameter>,
    /// Return type
    pub return_type: Ty,
    /// Generic parameters on the function
    pub generic_params: Vec<String>,
    /// Original span of the function declaration
    pub span: SourceSpan,
    /// Optional typed body of the function
    // TODO: Update this when TypedExpr is defined
    pub body: Option<crate::types::typed_ast::TypedExpr>,
    /// Whether the function has side effects
    pub is_effectful: bool,
}

/// Information about a typed parameter
#[derive(Debug, Clone)]
pub struct TypedParameter {
    /// The name of the parameter
    pub name: String,
    /// The resolved symbol for the parameter
    pub symbol: Symbol,
    /// The type of the parameter
    pub ty: Ty,
    /// Whether the parameter is variadic
    pub is_variadic: bool,
    /// Whether the parameter has a default value
    pub has_default: bool,
    /// Original span of the parameter declaration
    pub span: SourceSpan,
}

/// Definition of a generic parameter including its bounds.
/// This version is used internally by the type checker.
#[derive(Debug, Clone)]
pub struct GenericParamDef {
    pub name: String,      // Original name from source
    pub symbol: Symbol,    // Symbol from resolver
    pub id: TypeId,        // TypeId assigned by typechecker (usually a TyKind::Var)
    pub bounds: Vec<TraitRef>, // Use the checker's TraitRef
    pub span: SourceSpan,
}

/// Information about a typed struct
#[derive(Debug, Clone)]
pub struct TypedStruct {
    /// The name of the struct
    pub name: String,
    /// The symbol associated with the struct definition
    pub symbol: Symbol,
    /// Fields with their types
    pub fields: Vec<TypedField>,
    /// Generic parameters on the struct
    pub generic_params: Vec<GenericParamDef>,
    /// Original span of the struct declaration
    pub span: SourceSpan,
}

/// Information about a typed field
#[derive(Debug, Clone)]
pub struct TypedField {
    /// The name of the field
    pub name: String,
    /// The resolved symbol for the field (added for HIR lowering)
    pub symbol: Symbol,
    /// The type of the field
    pub ty: Ty,
    /// Whether the field is public
    pub is_public: bool,
    /// Original span of the field declaration
    pub span: SourceSpan,
}

/// Information about a typed enum
#[derive(Debug, Clone)]
pub struct TypedEnum {
    /// The name of the enum
    pub name: String,
    /// The symbol associated with the enum definition
    pub symbol: Symbol,
    /// Variants of the enum
    pub variants: Vec<TypedVariant>,
    /// Generic parameters on the enum
    pub generic_params: Vec<GenericParamDef>,
    /// Original span of the enum declaration
    pub span: SourceSpan,
}

/// Information about a typed enum variant
#[derive(Debug, Clone)]
pub enum TypedVariant {
    /// A simple variant with no data
    Unit {
        /// The name of the variant
        name: String,
        /// The symbol associated with the variant definition
        symbol: Symbol,
        /// Original span of the variant declaration
        span: SourceSpan,
    },
    
    /// A tuple variant with ordered fields
    Tuple {
        /// The name of the variant
        name: String,
        /// The symbol associated with the variant definition
        symbol: Symbol,
        /// Types of the tuple fields
        types: Vec<Ty>,
        /// Original span of the variant declaration
        span: SourceSpan,
    },
    
    /// A struct variant with named fields
    Struct {
        /// The name of the variant
        name: String,
        /// The symbol associated with the variant definition
        symbol: Symbol,
        /// Fields of the struct variant
        fields: Vec<TypedField>,
        /// Original span of the variant declaration
        span: SourceSpan,
    },
}

/// A context for type checking and type information
#[derive(Debug, Clone, Default)]
pub struct TypeContext {
    /// Map from type names to type definitions
    types: HashMap<String, TypeDef>,
    /// Map from symbol to original name (temporary solution)
    symbol_names: HashMap<Symbol, String>,
    /// Map from type Symbol -> method Symbol -> method signature for inherent impls
    #[allow(dead_code)]
    inherent_methods: HashMap<Symbol, HashMap<Symbol, FunctionSignature>>,
    /// Map from Trait Symbol to TraitId
    trait_ids: HashMap<Symbol, TraitId>,
    /// Map from Type Symbol to TypeDef
    type_defs_by_symbol: HashMap<Symbol, TypeDef>,
}

impl TypeContext {
    /// Create a new empty type context
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
            symbol_names: HashMap::new(), 
            inherent_methods: HashMap::new(), 
            trait_ids: HashMap::new(), // Initialize
            type_defs_by_symbol: HashMap::new(), // Initialize
        }
    }
    
    /// Add a type definition and its symbol-name mapping
    pub fn add_type(&mut self, symbol: Symbol, name: String, def: TypeDef) {
        self.symbol_names.insert(symbol, name.clone());
        self.type_defs_by_symbol.insert(symbol, def.clone()); // Store by symbol too
        self.types.insert(name, def);
    }

    /// Add just a symbol-name mapping, e.g., for generic parameters.
    pub fn add_symbol_name(&mut self, symbol: Symbol, name: String) {
        self.symbol_names.insert(symbol, name);
    }

    /// Get the original name for a symbol (temporary solution)
    pub fn get_name_for_symbol(&self, symbol: &Symbol) -> Option<&String> {
        self.symbol_names.get(symbol)
    }

    /// Get the symbol for a type name
    pub fn get_symbol_for_name(&self, name: &str) -> Option<Symbol> {
        // Inefficient reverse lookup - consider adding name->symbol map if needed often
        self.symbol_names.iter()
            .find(|(_, n)| *n == name)
            .map(|(s, _)| *s)
    }
    
    /// Get a type definition by name
    pub fn get_type(&self, name: &str) -> Option<&TypeDef> {
        self.types.get(name)
    }

    /// Get a type definition by symbol
    pub fn get_type_by_symbol(&self, symbol: &Symbol) -> Option<&TypeDef> {
        self.type_defs_by_symbol.get(symbol)
    }
    
    /// Check if a type exists
    pub fn has_type(&self, name: &str) -> bool {
        self.types.contains_key(name)
    }
    
    /// Get all type definitions
    pub fn all_types(&self) -> &HashMap<String, TypeDef> {
        &self.types
    }

    /// Add a mapping from a trait symbol to its TraitId
    pub fn add_trait_symbol(&mut self, symbol: Symbol, _name: String, id: TraitId) {
        self.trait_ids.insert(symbol, id);
    }

    /// Add an inherent method signature associated with a type symbol.
    #[allow(dead_code)]
    pub(crate) fn add_inherent_method(&mut self, type_symbol: Symbol, method_symbol: Symbol, signature: FunctionSignature) {
        self.inherent_methods
            .entry(type_symbol)
            .or_default()
            .insert(method_symbol, signature);
    }

    /// Get the inherent methods for a given type symbol.
    #[allow(dead_code)]
    pub(crate) fn get_inherent_methods(&self, type_symbol: &Symbol) -> Option<&HashMap<Symbol, FunctionSignature>> {
        self.inherent_methods.get(type_symbol)
    }
}

/// A type definition stored in the TypeContext
#[derive(Debug, Clone)]
pub enum TypeDef {
    /// A struct definition
    Struct(StructDef),
    /// An enum definition
    Enum(EnumDef),
    /// A function signature
    Function(FunctionSignature),
    // Note: Trait definitions are primarily stored in TraitRepository,
    // but TypeContext might hold a mapping from Symbol -> TraitId/Name.
}

/// A field in a struct type
#[derive(Debug, Clone)]
pub struct Field {
    /// Field name
    pub name: String,
    /// The resolved symbol for the field
    pub symbol: Symbol,
    /// Field type
    pub ty: Ty,
    /// Source span
    pub span: SourceSpan,
}

/// A struct type definition
#[derive(Debug, Clone)]
pub struct StructDef {
    /// Struct name
    pub name: String,
    /// The symbol associated with the struct definition
    pub symbol: Symbol,
    /// Generic parameters
    pub generic_params: Vec<GenericParamDef>,
    /// Fields
    pub fields: Vec<Field>,
    /// Source span
    pub span: SourceSpan,
}

/// A variant in an enum
#[derive(Debug, Clone)]
pub struct EnumVariant {
    /// Variant name
    pub name: String,
    /// Symbol for the variant itself
    pub symbol: Symbol,
    /// Variant fields (empty for unit variants)
    pub fields: Vec<Field>,
    /// Source span
    pub span: SourceSpan,
}

/// An enum type definition
#[derive(Debug, Clone)]
pub struct EnumDef {
    /// Enum name
    pub name: String,
    /// The symbol associated with the enum definition
    pub symbol: Symbol,
    /// Generic parameters
    pub generic_params: Vec<GenericParamDef>,
    /// Variants
    pub variants: Vec<EnumVariant>,
    /// Source span
    pub span: SourceSpan,
}

/// A type annotation for a parameter
#[derive(Debug, Clone)]
pub struct ParamType {
    /// Parameter name
    pub name: String,
    /// Parameter type
    pub ty: Ty,
    /// Source span
    pub span: SourceSpan,
}

/// Defines different kinds of self parameter for a function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SelfParamKind {
    /// Self parameter is passed by value. 
    /// In Rust, this would be `self`.
    Value,
    /// No self parameter is used.
    None,
}

/// A function signature
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    /// Function name
    pub name: String,
    /// Optional 'self' parameter kind (None for free functions)
    pub self_param: Option<SelfParamKind>,
    /// Generic parameters
    pub generic_params: Vec<GenericParamDef>,
    /// Parameters (excluding self)
    pub params: Vec<ParamType>,
    /// Return type
    pub return_type: Ty,
    /// Source span
    pub span: SourceSpan,
}

/// Represents a resolved trait reference (e.g., a bound `MyTrait<String>`).
/// This version is used internally by the type checker.
#[derive(Debug, Clone)]
pub struct TraitRef {
    pub trait_id: TraitId, // The ID of the referenced trait
    pub type_arguments: Vec<Ty>, // Concrete types for the trait's generic parameters
    pub span: SourceSpan,
}

/// Definition of an associated type within a trait.
#[derive(Debug, Clone)]
pub struct AssociatedTypeDef {
    pub name: String,
    pub symbol: Symbol, // Symbol for the associated type itself
    pub bounds: Vec<TraitRef>, // Store resolved TraitRef bounds
    pub default: Option<Ty>, // TODO: Add default type later if needed
    pub span: SourceSpan,
}

/// Information about a trait definition stored by the typechecker.
#[derive(Debug, Clone)]
pub struct TraitDef {
    pub id: TraitId, // ID assigned by the TraitRepository
    pub trait_symbol: Symbol, // Original symbol from resolver
    pub name: String,
    pub generic_params: Vec<GenericParamDef>,
    // pub supertraits: Vec<TraitRef>, // Placeholder for resolved supertrait references
    pub methods: HashMap<Symbol, TraitMethod>, // Map method Symbol -> TraitMethod signature
    pub associated_types: HashMap<Symbol, AssociatedTypeDef>, // Added associated types
    pub span: SourceSpan,
}

/// Information about a method signature within a trait definition.
#[derive(Debug, Clone)]
pub struct TraitMethod {
    pub name: String,
    pub method_symbol: Symbol, // Original symbol from resolver
    pub signature: FunctionSignature, // Use the existing FunctionSignature
}

/// Information about a trait implementation stored by the typechecker.
#[derive(Debug, Clone)]
pub struct ImplDef {
    pub id: ImplId, // ID assigned by the TraitRepository
    pub impl_symbol: Symbol, // Original symbol from resolver
    pub trait_ref: Option<TraitRef>, // Use the checker's TraitRef
    pub implementing_type: Ty, // The concrete type implementing the trait
    pub generic_params: Vec<GenericParamDef>, // Generic parameters defined *on the impl block itself*.
    pub methods: HashMap<Symbol, Symbol>, // Map: Trait Method Symbol -> Impl Method Symbol
    pub associated_type_bindings: HashMap<Symbol, Ty>, // Added associated type bindings
    pub span: SourceSpan,
}

#[cfg(test)]
mod tests {
    use super::*; // Import items from parent module
    use crate::types::core::{Ty, TyKind, PrimitiveType};
    use crate::context::trait_repo::TraitId; 
    use parallax_resolve::types::Symbol;
    use miette::SourceSpan;
    use std::collections::HashMap;
    use std::sync::Arc;

    // Helper for dummy span
    fn dummy_span() -> SourceSpan {
        SourceSpan::from((0, 0))
    }

    // Helper for primitive type
    fn ty_prim(prim: PrimitiveType) -> Ty {
        Ty::new(TyKind::Primitive(prim))
    }

    // Helper to create a dummy struct def
    fn dummy_struct_def(name: &str, symbol: Symbol) -> StructDef {
        StructDef {
            name: name.to_string(),
            symbol,
            generic_params: vec![],
            fields: vec![],
            span: dummy_span(),
        }
    }

    // Helper to create a dummy function signature
    fn dummy_func_sig(name: &str) -> FunctionSignature {
        FunctionSignature {
            name: name.to_string(),
            self_param: None,
            generic_params: vec![],
            params: vec![],
            return_type: ty_prim(PrimitiveType::Unit),
            span: dummy_span(),
        }
    }

    #[test]
    fn test_type_context_add_get() {
        let mut ctx = TypeContext::new();
        let struct_symbol = Symbol::new(1);
        let struct_name = "MyStruct".to_string();
        let struct_def = dummy_struct_def(&struct_name, struct_symbol);
        let type_def = TypeDef::Struct(struct_def.clone());

        ctx.add_type(struct_symbol, struct_name.clone(), type_def.clone());

        // Test get_type by name
        assert!(ctx.has_type(&struct_name));
        assert!(ctx.get_type(&struct_name).is_some());
        match ctx.get_type(&struct_name).unwrap() {
            TypeDef::Struct(s) => assert_eq!(s.name, struct_name),
            _ => panic!("Expected StructDef"),
        }

        // Test get_type_by_symbol
        assert!(ctx.get_type_by_symbol(&struct_symbol).is_some());
         match ctx.get_type_by_symbol(&struct_symbol).unwrap() {
            TypeDef::Struct(s) => assert_eq!(s.symbol, struct_symbol),
            _ => panic!("Expected StructDef"),
        }

        // Test get_name_for_symbol
        assert_eq!(ctx.get_name_for_symbol(&struct_symbol), Some(&struct_name));

        // Test get_symbol_for_name
        assert_eq!(ctx.get_symbol_for_name(&struct_name), Some(struct_symbol));

        // Test non-existent items
        assert!(!ctx.has_type("OtherStruct"));
        assert!(ctx.get_type("OtherStruct").is_none());
        assert!(ctx.get_type_by_symbol(&Symbol::new(99)).is_none());
        assert!(ctx.get_name_for_symbol(&Symbol::new(99)).is_none());
        assert!(ctx.get_symbol_for_name("OtherStruct").is_none());

        // Test all_types
        assert_eq!(ctx.all_types().len(), 1);
    }

    #[test]
    fn test_type_context_symbol_name_only() {
        let mut ctx = TypeContext::new();
        let generic_symbol = Symbol::new(5);
        let generic_name = "T".to_string();

        ctx.add_symbol_name(generic_symbol, generic_name.clone());

        assert_eq!(ctx.get_name_for_symbol(&generic_symbol), Some(&generic_name));
        assert_eq!(ctx.get_symbol_for_name(&generic_name), Some(generic_symbol));
        // Ensure it didn't accidentally add a TypeDef
        assert!(ctx.get_type(&generic_name).is_none());
        assert!(ctx.get_type_by_symbol(&generic_symbol).is_none());
    }

    #[test]
    fn test_type_context_trait_symbol() {
        let mut ctx = TypeContext::new();
        let trait_symbol = Symbol::new(10);
        let trait_name = "MyTrait".to_string();
        let trait_id = TraitId(0);

        ctx.add_trait_symbol(trait_symbol, trait_name.clone(), trait_id);

        // Test internal map (though not directly exposed)
        assert_eq!(ctx.trait_ids.get(&trait_symbol), Some(&trait_id));
        // Ensure it didn't add a TypeDef
        assert!(ctx.get_type(&trait_name).is_none());
        assert!(ctx.get_type_by_symbol(&trait_symbol).is_none());
    }

    #[test]
    fn test_type_context_inherent_methods() {
        let mut ctx = TypeContext::new();
        let type_symbol = Symbol::new(20);
        let method_symbol = Symbol::new(21);
        let method_name = "do_stuff".to_string();
        let signature = dummy_func_sig(&method_name);

        ctx.add_inherent_method(type_symbol, method_symbol, signature.clone());

        let methods = ctx.get_inherent_methods(&type_symbol);
        assert!(methods.is_some());
        let methods_map = methods.unwrap();
        assert_eq!(methods_map.len(), 1);
        assert!(methods_map.contains_key(&method_symbol));
        assert_eq!(methods_map.get(&method_symbol).unwrap().name, method_name);

        // Test non-existent type
        assert!(ctx.get_inherent_methods(&Symbol::new(99)).is_none());
        // Test type with no methods
        let other_type_symbol = Symbol::new(30);
        ctx.add_symbol_name(other_type_symbol, "OtherType".to_string()); // Add symbol so it exists
        assert!(ctx.get_inherent_methods(&other_type_symbol).is_none());
    }
} 