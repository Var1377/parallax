use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::io;
use regex::Regex;

/// Scanner for finding implemented parser features by analyzing source code
pub struct ParserScanner {
    /// The base directory to scan
    base_dir: PathBuf,
    /// Set of found node kinds
    node_kinds: HashSet<String>,
}

impl ParserScanner {
    /// Create a new parser scanner
    pub fn new<P: AsRef<Path>>(base_dir: P) -> Self {
        Self {
            base_dir: base_dir.as_ref().to_path_buf(),
            node_kinds: HashSet::new(),
        }
    }
    
    /// Scan the codebase to find node.kind() patterns
    pub fn scan(&mut self) -> io::Result<()> {
        let base_dir = self.base_dir.clone();
        self.scan_directory(&base_dir)
    }
    
    /// Scan a directory recursively for Rust files
    fn scan_directory(&mut self, dir: &Path) -> io::Result<()> {
        if dir.is_dir() {
            for entry in fs::read_dir(dir)? {
                let entry = entry?;
                let path = entry.path();
                
                if path.is_dir() {
                    self.scan_directory(&path)?;
                } else if let Some(ext) = path.extension() {
                    if ext == "rs" {
                        self.scan_file(&path)?;
                    }
                }
            }
        }
        
        Ok(())
    }
    
    /// Scan a single file for node.kind() patterns
    fn scan_file(&mut self, file: &Path) -> io::Result<()> {
        let content = fs::read_to_string(file)?;
        
        // Match patterns like `node.kind() == "pattern"` or `case "pattern":`
        let re_eq = Regex::new(r#"node\.kind\(\s*\)\s*==\s*"([^"]+)""#).unwrap();
        let re_match = Regex::new(r#"match\s+[^{]+\{\s*"([^"]+)"\s*=>"#).unwrap();
        let re_match_node_kind = Regex::new(r#"match\s+node\.kind\(\s*\)\s*\{(?s:.*?)"([^"]+)"\s*=>"#).unwrap();
        
        // Find all matches for the first pattern
        for cap in re_eq.captures_iter(&content) {
            if let Some(kind) = cap.get(1) {
                self.node_kinds.insert(kind.as_str().to_string());
            }
        }
        
        // Find all matches for the second pattern
        for cap in re_match.captures_iter(&content) {
            if let Some(kind) = cap.get(1) {
                self.node_kinds.insert(kind.as_str().to_string());
            }
        }
        
        // Find all matches for the third pattern
        for cap in re_match_node_kind.captures_iter(&content) {
            if let Some(kind) = cap.get(1) {
                self.node_kinds.insert(kind.as_str().to_string());
            }
        }
        
        Ok(())
    }
    
    /// Get all found node kinds
    pub fn get_node_kinds(&self) -> &HashSet<String> {
        &self.node_kinds
    }
    
    /// Map node kinds to EBNF grammar elements
    pub fn map_to_grammar_elements(&self) -> HashSet<String> {
        let mut grammar_elements = HashSet::new();
        
        // Mapping from tree-sitter node kinds to EBNF grammar elements
        let mapping = self.create_node_to_grammar_mapping();
        
        for node_kind in &self.node_kinds {
            if let Some(element) = mapping.get(node_kind.as_str()) {
                grammar_elements.insert(element.to_string());
            }
        }
        
        grammar_elements
    }
    
    /// Create a mapping from tree-sitter node kinds to EBNF grammar elements
    fn create_node_to_grammar_mapping(&self) -> std::collections::HashMap<&'static str, &'static str> {
        let mut mapping = std::collections::HashMap::new();
        
        // Top-level constructs
        mapping.insert("source_file", "SourceFile");
        mapping.insert("item", "Item");
        mapping.insert("block_item", "BlockItem");
        mapping.insert("module", "Module");
        
        // Use statements
        mapping.insert("use", "Use");
        mapping.insert("use_item", "UseItem");
        
        // Basic elements
        mapping.insert("identifier", "Identifier");
        mapping.insert("visibility", "Visibility");
        
        // Functions
        mapping.insert("function_sig", "FunctionSig");
        mapping.insert("function", "Function");
        
        // Type definitions
        mapping.insert("type_def", "TypeDef");
        mapping.insert("enum", "Enum");
        mapping.insert("struct", "Struct");
        mapping.insert("trait", "Trait");
        mapping.insert("impl", "Impl");
        
        // Generics and constraints
        mapping.insert("generic_parameters", "GenericParameters");
        mapping.insert("generic_param", "GenericParam");
        mapping.insert("where_clause", "WhereClause");
        mapping.insert("where_pred", "WherePred");
        
        // Kinds and types
        mapping.insert("kind", "Kind");
        mapping.insert("function_kind", "FunctionKind");
        mapping.insert("tuple_kind", "TupleKind");
        mapping.insert("type", "Type");
        mapping.insert("function_type", "FunctionType");
        mapping.insert("tuple_type", "TupleType");
        mapping.insert("kind_app", "KindApp");
        mapping.insert("array_type", "ArrayType");
        
        // Data structure components
        mapping.insert("enum_variants", "EnumVariants");
        mapping.insert("enum_variant", "EnumVariant");
        mapping.insert("struct_body", "StructBody");
        mapping.insert("struct_field", "StructField");
        mapping.insert("tuple_body", "TupleBody");
        
        // Struct and enum creation
        mapping.insert("struct_expr", "StructExpr");
        
        // Trait and implementation items
        mapping.insert("trait_items", "TraitItems");
        mapping.insert("impl_items", "ImplItems");
        mapping.insert("trait_item", "TraitItem");
        mapping.insert("impl_item", "ImplItem");
        
        // Parameters
        mapping.insert("parameters", "Parameters");
        mapping.insert("parameter", "Parameter");
        
        // Expression structure components
        mapping.insert("expr_struct_body", "ExprStructBody");
        mapping.insert("field_init", "FieldInit");
        mapping.insert("base_struct", "BaseStruct");
        mapping.insert("expr_tuple_body", "ExprTupleBody");
        
        // Collections
        mapping.insert("array_expr", "ArrayExpr");
        mapping.insert("tuple_expr", "TupleExpr");
        mapping.insert("map_expr", "MapExpr");
        mapping.insert("hash_set_expr", "HashSetExpr");
        mapping.insert("map_entry", "MapEntry");
        
        // Expressions
        mapping.insert("expression", "Expression");
        mapping.insert("block", "Block");
        mapping.insert("if_expr", "IfExpr");
        mapping.insert("match_expr", "MatchExpr");
        mapping.insert("match_arms", "MatchArms");
        mapping.insert("match_arm", "MatchArm");
        mapping.insert("lambda_expr", "LambdaExpr");
        mapping.insert("binary_expr", "BinaryExpr");
        mapping.insert("unary_expr", "UnaryExpr");
        mapping.insert("let_expr", "LetExpr");
        mapping.insert("field_access", "FieldAccess");
        
        // Literals and basic types
        mapping.insert("literal", "Literal");
        mapping.insert("integer_literal", "IntegerLiteral");
        mapping.insert("decimal_literal", "IntegerLiteral");
        mapping.insert("hex_literal", "IntegerLiteral");
        mapping.insert("octal_literal", "IntegerLiteral");
        mapping.insert("binary_literal", "IntegerLiteral");
        mapping.insert("float_literal", "FloatLiteral");
        mapping.insert("decimal_float", "FloatLiteral");
        mapping.insert("exponent_float", "FloatLiteral");
        mapping.insert("string_literal", "StringLiteral");
        mapping.insert("raw_string_literal", "StringLiteral");
        mapping.insert("normal_string_literal", "StringLiteral");
        mapping.insert("byte_string_literal", "StringLiteral");
        mapping.insert("character_literal", "CharacterLiteral");
        mapping.insert("boolean_literal", "BooleanLiteral");
        
        // Operators
        mapping.insert("binary_op", "BinaryOp");
        mapping.insert("unary_op", "UnaryOp");
        
        // Function calls
        mapping.insert("call_expr", "CallExpr");
        mapping.insert("call_args", "CallArgs");
        mapping.insert("argument", "Argument");
        
        // Patterns
        mapping.insert("pattern", "Pattern");
        mapping.insert("constructor", "Constructor");
        mapping.insert("tuple_pattern", "TuplePattern");
        mapping.insert("array_pattern", "ArrayPattern");
        mapping.insert("struct_pattern", "StructPattern");
        mapping.insert("field_pattern", "FieldPattern");
        mapping.insert("rest_pattern", "RestPattern");
        mapping.insert("wildcard_pattern", "WildcardPattern");
        mapping.insert("or_pattern", "OrPattern");
        
        // Paths
        mapping.insert("path", "Path");
        mapping.insert("path_segment", "PathSegment");
        mapping.insert("generic_args", "GenericArgs");
        
        mapping
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_regex_patterns() {
        // Test the pattern matching for node.kind() == "pattern"
        let re_eq = Regex::new(r#"node\.kind\(\s*\)\s*==\s*"([^"]+)""#).unwrap();
        let test_str = r#"if node.kind() == "pattern" {"#;
        let caps = re_eq.captures(test_str).unwrap();
        assert_eq!(caps.get(1).unwrap().as_str(), "pattern");
        
        // Test the pattern matching for match ... { "pattern" => ... }
        let re_match = Regex::new(r#"match\s+[^{]+\{\s*"([^"]+)"\s*=>"#).unwrap();
        let test_str = r#"match some_var { "pattern" => foo, "#;
        let caps = re_match.captures(test_str).unwrap();
        assert_eq!(caps.get(1).unwrap().as_str(), "pattern");
        
        // Test the pattern matching for match node.kind() { "pattern" => ... }
        let re_match_node_kind = Regex::new(r#"match\s+node\.kind\(\s*\)\s*\{(?s:.*?)"([^"]+)"\s*=>"#).unwrap();
        let test_str = r#"match node.kind() { 
            "pattern" => foo,
        "#;
        let caps = re_match_node_kind.captures(test_str).unwrap();
        assert_eq!(caps.get(1).unwrap().as_str(), "pattern");
    }
} 