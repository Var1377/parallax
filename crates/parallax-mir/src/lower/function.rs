//! Function lowering from HIR to MIR.
//!
//! This module implements the lowering of HIR functions to MIR functions.

#[cfg(test)]
mod tests {
    use crate::db::MirDatabase;
    use crate::ir::function::MirType;
    use crate::ir::statement::{Constant, Operand};
    use crate::ir::terminator::MirTerminator;
    use crate::tests::{create_test_db, create_test_function};
    
    #[test]
    fn test_lower_empty_function() {
        let db = create_test_db();
        let hir_func_id = create_test_function("empty");
        
        let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
        
        assert_eq!(mir_func.name, "empty");
        assert!(mir_func.signature.params.is_empty());
        assert!(matches!(&mir_func.signature.return_type, MirType::Unit));
    }
    
    #[test]
    fn test_lower_function_with_return_value() {
        let db = create_test_db();
        let hir_func_id = create_test_function("return_42");
        
        let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
        
        assert_eq!(mir_func.name, "return_42");
        assert!(mir_func.signature.params.is_empty());
        assert!(matches!(&mir_func.signature.return_type, MirType::Int));
        
        let entry_block = mir_func.body.blocks.get(&mir_func.body.entry_block)
            .expect("Entry block not found");
            
        match &entry_block.terminator {
            MirTerminator::Return { value: Some(Operand::Constant(Constant::Int(val))) } => {
                assert_eq!(*val, 42);
            }
            _ => panic!("Expected return terminator with int constant 42"),
        }
    }
    
    #[test]
    fn test_lower_function_with_parameters() {
        let db = create_test_db();
        let hir_func_id = create_test_function("add");
        
        let mir_func = db.mir_function(hir_func_id).expect("Failed to lower function");
        
        assert_eq!(mir_func.name, "add");
        assert_eq!(mir_func.signature.params.len(), 2);
        
        // Check that params have the right types
        assert!(matches!(&mir_func.signature.params[0], MirType::Int));
        assert!(matches!(&mir_func.signature.params[1], MirType::Int));
        
        // Check that it has locals for parameters and result variable
        assert_eq!(mir_func.locals.len(), 3);
        assert!(matches!(&mir_func.locals[0].ty, MirType::Int));
        assert!(matches!(&mir_func.locals[1].ty, MirType::Int));
        assert!(matches!(&mir_func.locals[2].ty, MirType::Int));
    }
} 