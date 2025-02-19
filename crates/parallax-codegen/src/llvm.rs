// use inkwell::context::Context;
// use inkwell::module::Module;
// use inkwell::builder::Builder;
// use inkwell::values::{FunctionValue, BasicValueEnum};
// use inkwell::types::BasicTypeEnum;
// use parallax_lang::ast;

// use crate::{CodegenResult, CodegenError};

// pub struct LLVMContext<'ctx> {
//     context: Context,
//     module: Module<'ctx>,
//     builder: Builder<'ctx>,
// }

// impl<'ctx> LLVMContext<'ctx> {
//     pub fn new() -> Self {
//         let context = Context::create();
//         let module = context.create_module("parallax_module");
//         let builder = context.create_builder();

//         Self {
//             context,
//             module,
//             builder,
//         }
//     }

//     pub fn generate_module(&mut self, program: &oldast::Program) -> CodegenResult<()> {
//         // Generate LLVM IR for each item in the program
//         for item in &program.items {
//             match item {
//                 oldast::Item::Function(func) => self.generate_function(func)?,
//                 oldast::Item::TypeDef(_) => (), // Type definitions don't generate code
//                 oldast::Item::Struct(_) => (), // Struct definitions are handled during type generation
//                 oldast::Item::Enum(_) => (), // Enum definitions are handled during type generation
//             }
//         }
//         Ok(())
//     }

//     fn generate_function(&self, func: &oldast::Function) -> CodegenResult<FunctionValue> {
//         // Convert function type to LLVM type
//         let return_type = self.convert_type(&func.return_type)?;
//         let param_types: Vec<BasicTypeEnum> = func.params
//             .iter()
//             .map(|param| self.convert_type(&param.ty))
//             .collect::<Result<_, _>>()?;

//         // Create function type
//         let fn_type = return_type.fn_type(&param_types, false);
        
//         // Create function
//         let function = self.module.add_function(
//             func.name,
//             fn_type,
//             None,
//         );

//         // Create entry basic block
//         let basic_block = self.context.append_basic_block(function, "entry");
//         self.builder.position_at_end(basic_block);

//         // Generate function body
//         self.generate_expr(&func.body)?;

//         Ok(function)
//     }

//     fn generate_expr(&self, expr: &oldast::Expr) -> CodegenResult<BasicValueEnum> {
//         match expr {
//             oldast::Expr::Literal { value, .. } => self.generate_literal(value),
//             oldast::Expr::Path { .. } => todo!("Generate path expression"),
//             oldast::Expr::Block(block) => self.generate_block(block),
//             oldast::Expr::If { condition, then_branch, else_branch, .. } => {
//                 self.generate_if(condition, then_branch, else_branch.as_deref())
//             }
//             oldast::Expr::Match { .. } => todo!("Generate match expression"),
//             oldast::Expr::Binary { .. } => todo!("Generate binary expression"),
//             oldast::Expr::Unary { .. } => todo!("Generate unary expression"),
//             oldast::Expr::Call { .. } => todo!("Generate function call"),
//             oldast::Expr::StructLit { .. } => todo!("Generate struct literal"),
//         }
//     }

//     fn generate_literal(&self, literal: &oldast::Literal) -> CodegenResult<BasicValueEnum> {
//         match literal {
//             oldast::Literal::Int(n) => Ok(self.context
//                 .i64_type()
//                 .const_int(*n as u64, false)
//                 .as_basic_value_enum()),
//             oldast::Literal::Float(x) => Ok(self.context
//                 .f64_type()
//                 .const_float(*x)
//                 .as_basic_value_enum()),
//             oldast::Literal::String(s) => {
//                 let string = self.builder.build_global_string_ptr(s, "string");
//                 Ok(string.as_basic_value_enum())
//             }
//             oldast::Literal::Bool(b) => Ok(self.context
//                 .bool_type()
//                 .const_int(*b as u64, false)
//                 .as_basic_value_enum()),
//         }
//     }

//     fn generate_block(&self, block: &oldast::Block) -> CodegenResult<BasicValueEnum> {
//         // Generate code for each statement
//         for stmt in &block.stmts {
//             match stmt {
//                 oldast::Stmt::Let { pattern, init, .. } => {
//                     self.generate_let(pattern, init)?;
//                 }
//                 oldast::Stmt::Expr { expr, .. } => {
//                     self.generate_expr(expr)?;
//                 }
//             }
//         }

//         // Generate final expression if it exists
//         if let Some(expr) = &block.expr {
//             self.generate_expr(expr)
//         } else {
//             // Return void if no final expression
//             Ok(self.context.void_type().const_zero().as_basic_value_enum())
//         }
//     }

//     fn generate_if(
//         &self,
//         condition: &oldast::Expr,
//         then_branch: &oldast::Expr,
//         else_branch: Option<&oldast::Expr>,
//     ) -> CodegenResult<BasicValueEnum> {
//         let cond_value = self.generate_expr(condition)?;
//         let cond = self.builder.build_int_compare(
//             inkwell::IntPredicate::NE,
//             cond_value.into_int_value(),
//             self.context.bool_type().const_zero(),
//             "ifcond",
//         );

//         let function = self.builder.get_insert_block().unwrap().get_parent().unwrap();
//         let then_block = self.context.append_basic_block(function, "then");
//         let else_block = self.context.append_basic_block(function, "else");
//         let merge_block = self.context.append_basic_block(function, "ifcont");

//         self.builder.build_conditional_branch(cond, then_block, else_block);

//         // Generate then branch
//         self.builder.position_at_end(then_block);
//         let then_value = self.generate_expr(then_branch)?;
//         self.builder.build_unconditional_branch(merge_block);

//         // Generate else branch
//         self.builder.position_at_end(else_block);
//         let else_value = if let Some(else_expr) = else_branch {
//             self.generate_expr(else_expr)?
//         } else {
//             self.context.void_type().const_zero().as_basic_value_enum()
//         };
//         self.builder.build_unconditional_branch(merge_block);

//         // Generate phi node
//         self.builder.position_at_end(merge_block);
//         let phi = self.builder.build_phi(then_value.get_type(), "iftmp");
//         phi.add_incoming(&[
//             (&then_value, then_block),
//             (&else_value, else_block),
//         ]);

//         Ok(phi.as_basic_value())
//     }

//     fn generate_let(&self, pattern: &oldast::Pattern, init: &oldast::Expr) -> CodegenResult<()> {
//         let value = self.generate_expr(init)?;
//         self.bind_pattern(pattern, value)
//     }

//     fn bind_pattern(&self, pattern: &oldast::Pattern, value: BasicValueEnum) -> CodegenResult<()> {
//         match pattern {
//             oldast::Pattern::Variable { name, .. } => {
//                 // Allocate space for the variable
//                 let alloca = self.builder.build_alloca(value.get_type(), name);
//                 // Store the value
//                 self.builder.build_store(alloca, value);
//                 Ok(())
//             }
//             _ => Err(CodegenError::LLVMError(
//                 "Complex patterns not yet supported".to_string(),
//             )),
//         }
//     }

//     fn convert_type(&self, ty: &oldast::Type) -> CodegenResult<BasicTypeEnum> {
//         match ty {
//             oldast::Type::Path { path, .. } => {
//                 // Handle built-in types
//                 match path.segments[0].to_string().as_str() {
//                     // Integer types
//                     "i8" => Ok(self.context.i8_type().as_basic_type_enum()),
//                     "i16" => Ok(self.context.i16_type().as_basic_type_enum()),
//                     "i32" => Ok(self.context.i32_type().as_basic_type_enum()),
//                     "i64" => Ok(self.context.i64_type().as_basic_type_enum()),
//                     "i128" => Ok(self.context.i128_type().as_basic_type_enum()),
//                     "isize" => Ok(self.context.i64_type().as_basic_type_enum()), // Platform dependent
//                     "u8" => Ok(self.context.i8_type().as_basic_type_enum()),
//                     "u16" => Ok(self.context.i16_type().as_basic_type_enum()),
//                     "u32" => Ok(self.context.i32_type().as_basic_type_enum()),
//                     "u64" => Ok(self.context.i64_type().as_basic_type_enum()),
//                     "u128" => Ok(self.context.i128_type().as_basic_type_enum()),
//                     "usize" => Ok(self.context.i64_type().as_basic_type_enum()), // Platform dependent
//                     // Floating point types
//                     "f32" => Ok(self.context.f32_type().as_basic_type_enum()),
//                     "f64" => Ok(self.context.f64_type().as_basic_type_enum()),
//                     // Other primitive types
//                     "bool" => Ok(self.context.bool_type().as_basic_type_enum()),
//                     "char" => Ok(self.context.i32_type().as_basic_type_enum()), // Unicode codepoint
//                     "String" => Ok(self.context.i8_type().ptr_type(inkwell::AddressSpace::Generic).as_basic_type_enum()),
//                     "unit" => Ok(self.context.void_type().ptr_type(inkwell::AddressSpace::Generic).as_basic_type_enum()),
//                     _ => Err(CodegenError::TypeError(
//                         format!("Unknown type: {}", path.segments[0]),
//                     )),
//                 }
//             }
//             oldast::Type::Function { .. } => Err(CodegenError::TypeError(
//                 "Function types not yet supported".to_string(),
//             )),
//             oldast::Type::Generic { .. } => Err(CodegenError::TypeError(
//                 "Generic types not yet supported".to_string(),
//             )),
//         }
//     }
// } 