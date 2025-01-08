use parallax_ir::Book;

pub struct VM<'a> {
    book: Book<'a>,
}

pub struct DebugVM<'a> {
    book: Book<'a>,
    config: DebugConfig,
}

#[derive(Debug, Clone)]
pub struct DebugConfig {
    /// Run in single-step mode
    pub step_mode: bool,
    /// Maximum number of reduction steps
    pub max_steps: Option<usize>,
}

impl<'a> VM<'a> {
    pub fn new(book: Book<'a>) -> Self {
        Self { book }
    }

    pub fn run(&mut self) -> Result<(), String> {
        todo!("Implement VM execution")
    }
}

impl<'a> DebugVM<'a> {
    pub fn new(book: Book<'a>, config: DebugConfig) -> Self {
        Self { book, config }
    }

    pub fn run(&mut self) -> Result<(), String> {
        if self.config.step_mode {
            todo!("Implement step-by-step debugging")
        } else {
            todo!("Implement debug mode with max_steps={:?}", self.config.max_steps)
        }
    }
}

pub fn run_file<'a>(book: Book<'a>) -> Result<(), String> {
    VM::new(book).run()
}

pub fn debug_file<'a>(book: Book<'a>, config: DebugConfig) -> Result<(), String> {
    DebugVM::new(book, config).run()
}