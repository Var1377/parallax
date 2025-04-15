use std::path::PathBuf;

#[salsa::interned]
pub struct Path {
    #[return_ref]
    pub inner: PathBuf,
}

