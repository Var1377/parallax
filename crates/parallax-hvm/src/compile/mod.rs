use crate::ast::Book;
use crate::{IRResult, IRError};
use parallax_net::{CompiledNet, Symbol};
use std::collections::HashMap;

mod variable_disconnection;
mod variable_removal;
mod net_gen;

pub fn compile_book_to_net<'a>(book: Book<'a>) -> IRResult<CompiledNet> {
    // Stage 1: Disconnect variable nodes
    let disconnect_variable_nodes = variable_disconnection::DisconnectedVariables::from(book);
    
    // Stage 2: Remove variable nodes and rewire connections
    let removed_variable_nodes = variable_removal::RemovedVariables::from(disconnect_variable_nodes);
    
    // Stage 3: Generate CompiledNet structure
    net_gen::generate_compiled_net(removed_variable_nodes)
}

// Potential future implementation if direct From trait is desired
// impl<'a> TryFrom<Book<'a>> for CompiledNet {
//     type Error = IRError;
//
//     fn try_from(book: Book<'a>) -> IRResult<Self> {
//         compile_book_to_net(book)
//     }
// }