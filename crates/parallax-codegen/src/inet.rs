// use parallax_lang::ast;
// use crate::{CodegenResult, CodegenError};

// /// A port in an interaction net
// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
// pub struct Port(usize);

// /// An agent in an interaction net
// #[derive(Debug, Clone)]
// pub struct Agent {
//     /// The name/symbol of the agent
//     name: String,
//     /// The ports of the agent, in order
//     ports: Vec<Port>,
// }

// /// A connection between two ports in an interaction net
// #[derive(Debug, Clone)]
// pub struct Connection {
//     from: Port,
//     to: Port,
// }

// /// An interaction net
// #[derive(Debug, Default)]
// pub struct InteractionNet {
//     /// The agents in the net
//     agents: Vec<Agent>,
//     /// The connections between ports
//     connections: Vec<Connection>,
//     /// The next available port number
//     next_port: usize,
// }

// impl InteractionNet {
//     fn new() -> Self {
//         Self::default()
//     }

//     fn new_port(&mut self) -> Port {
//         let port = Port(self.next_port);
//         self.next_port += 1;
//         port
//     }

//     fn add_agent(&mut self, name: String, arity: usize) -> Vec<Port> {
//         let ports: Vec<_> = (0..arity).map(|_| self.new_port()).collect();
//         self.agents.push(Agent {
//             name,
//             ports: ports.clone(),
//         });
//         ports
//     }

//     fn connect(&mut self, from: Port, to: Port) {
//         self.connections.push(Connection { from, to });
//     }
// }

// /// The context for generating interaction nets
// pub struct INetContext {
//     /// The current interaction net being built
//     current_net: InteractionNet,
// }

// impl INetContext {
//     pub fn new() -> Self {
//         Self {
//             current_net: InteractionNet::new(),
//         }
//     }

//     pub fn generate_module(&mut self, program: &oldast::Program) -> CodegenResult<()> {
//         // Generate interaction net for each item in the program
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

//     fn generate_function(&mut self, func: &oldast::Function) -> CodegenResult<()> {
//         // Create an agent for the function
//         let ports = self.current_net.add_agent(
//             func.name.to_string(),
//             func.params.len() + 1, // +1 for return value
//         );

//         // Generate the function body
//         self.generate_expr(&func.body, ports[0])?;

//         // Connect parameter ports
//         for (i, param) in func.params.iter().enumerate() {
//             let param_port = ports[i + 1];
//             self.generate_pattern(&param.pattern, param_port)?;
//         }

//         Ok(())
//     }

//     fn generate_expr(&mut self, expr: &oldast::Expr, output: Port) -> CodegenResult<()> {
//         match expr {
//             oldast::Expr::Literal { value, .. } => self.generate_literal(value, output),
//             oldast::Expr::Path { .. } => todo!("Generate path expression"),
//             oldast::Expr::Block(block) => self.generate_block(block, output),
//             oldast::Expr::If { condition, then_branch, else_branch, .. } => {
//                 self.generate_if(condition, then_branch, else_branch.as_deref(), output)
//             }
//             oldast::Expr::Match { .. } => todo!("Generate match expression"),
//             oldast::Expr::Binary { .. } => todo!("Generate binary expression"),
//             oldast::Expr::Unary { .. } => todo!("Generate unary expression"),
//             oldast::Expr::Call { .. } => todo!("Generate function call"),
//             oldast::Expr::StructLit { .. } => todo!("Generate struct literal"),
//         }
//     }

//     fn generate_literal(&mut self, literal: &oldast::Literal, output: Port) -> CodegenResult<()> {
//         // Create a constant agent for the literal
//         let ports = self.current_net.add_agent(
//             format!("Const_{:?}", literal),
//             1,
//         );
//         self.current_net.connect(ports[0], output);
//         Ok(())
//     }

//     fn generate_block(&mut self, block: &oldast::Block, output: Port) -> CodegenResult<()> {
//         // Generate code for each statement
//         for stmt in &block.stmts {
//             match stmt {
//                 oldast::Stmt::Let { pattern, init, .. } => {
//                     let value_port = self.current_net.new_port();
//                     self.generate_expr(init, value_port)?;
//                     self.generate_pattern(pattern, value_port)?;
//                 }
//                 oldast::Stmt::Expr { expr, .. } => {
//                     let expr_port = self.current_net.new_port();
//                     self.generate_expr(expr, expr_port)?;
//                 }
//             }
//         }

//         // Generate final expression if it exists
//         if let Some(expr) = &block.expr {
//             self.generate_expr(expr, output)?;
//         }

//         Ok(())
//     }

//     fn generate_if(
//         &mut self,
//         condition: &oldast::Expr,
//         then_branch: &oldast::Expr,
//         else_branch: Option<&oldast::Expr>,
//         output: Port,
//     ) -> CodegenResult<()> {
//         // Create IF agent
//         let if_ports = self.current_net.add_agent(
//             "If".to_string(),
//             4, // condition, then, else, output
//         );

//         // Connect condition
//         self.generate_expr(condition, if_ports[0])?;

//         // Connect then branch
//         self.generate_expr(then_branch, if_ports[1])?;

//         // Connect else branch
//         if let Some(else_expr) = else_branch {
//             self.generate_expr(else_expr, if_ports[2])?;
//         }

//         // Connect output
//         self.current_net.connect(if_ports[3], output);

//         Ok(())
//     }

//     fn generate_pattern(&mut self, pattern: &oldast::Pattern, value: Port) -> CodegenResult<()> {
//         match pattern {
//             oldast::Pattern::Variable { name, .. } => {
//                 // Create a variable agent
//                 let var_ports = self.current_net.add_agent(
//                     format!("Var_{}", name),
//                     1,
//                 );
//                 self.current_net.connect(var_ports[0], value);
//                 Ok(())
//             }
//             _ => Err(CodegenError::INetError(
//                 "Complex patterns not yet supported".to_string(),
//             )),
//         }
//     }
// } 