// use miette::Diagnostic;
// use thiserror::Error;

// mod llvm;
// mod inet;

// #[derive(Debug, Error, Diagnostic)]
// pub enum CodegenError {
//     #[error("LLVM codegen error: {0}")]
//     LLVMError(String),

//     #[error("Interaction net codegen error: {0}")]
//     INetError(String),

//     #[error("Type error during codegen: {0}")]
//     TypeError(String),
// }

// pub type CodegenResult<T> = Result<T, CodegenError>;

// /// The main code generation context that manages both LLVM and interaction net output
// pub struct CodeGenerator {
//     llvm_context: llvm::LLVMContext,
//     inet_context: inet::INetContext,
// }

// impl CodeGenerator {
//     pub fn new() -> Self {
//         Self {
//             llvm_context: llvm::LLVMContext::new(),
//             inet_context: inet::INetContext::new(),
//         }
//     }

//     /// Generate both LLVM IR and interaction net representation for a module
//     pub fn generate_module(&mut self, ast: &parallax_lang::oldast::Program) -> CodegenResult<()> {
//         // Generate LLVM IR
//         self.llvm_context.generate_module(ast)?;
        
//         // Generate interaction nets
//         self.inet_context.generate_module(ast)?;
        
//         Ok(())
//     }
// } 