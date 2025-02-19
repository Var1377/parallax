use crate::ast::Book;

mod variable_disconnection;
mod variable_removal;
mod runtime_gen;

impl<'a> From<Book<'a>> for Program {
    fn from(book: Book<'a>) -> Self {
        // Stage 1: Disconnect variable nodes
        let disconnect_variable_nodes = variable_disconnection::DisconnectedVariables::from(book);
        
        // Stage 2: Remove variable nodes and rewire connections
        let removed_variable_nodes = variable_removal::RemovedVariables::from(disconnect_variable_nodes);
        
        // Stage 5: Generate final program structure
        runtime_gen::generate_program(memory_layout)
    }
}