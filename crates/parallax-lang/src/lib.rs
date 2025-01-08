use std::path::Path;

pub fn compile(source: &Path, output: &Path) -> Result<(), String> {
    todo!("Implement language compilation")
}

pub fn check(source: &Path) -> Result<(), String> {
    todo!("Implement type checking")
}

pub fn format(source: &Path) -> Result<(), String> {
    todo!("Implement source formatting")
}