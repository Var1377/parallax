use std::time::Duration;

#[derive(Debug, Clone)]
pub struct Config {
    pub vm: VMConfig,
    pub compiler: CompilerConfig,
    pub debug: DebugConfig,
}

#[derive(Debug, Clone)]
pub struct VMConfig {
    pub string_interner_capacity: usize,
    pub debug_logging: bool,
    pub max_reduction_steps: usize,
    pub max_stack_size: usize,
    pub reduction_timeout: Option<Duration>,
}

#[derive(Debug, Clone)]
pub struct CompilerConfig {
    pub debug_info: bool,
    pub verbose: bool,
    pub max_inline_depth: usize,
    pub opt_level: OptLevel,
}

#[derive(Debug, Clone)]
pub struct DebugConfig {
    pub step_mode: bool,
    pub max_steps: Option<usize>,
    pub verbose: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptLevel {
    None,
    Basic,
    Advanced,
}

enum Node {
    Var(String),
    
}