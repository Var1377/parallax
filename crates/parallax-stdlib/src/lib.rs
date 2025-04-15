//! Standard library for the Parallax programming language.
//!
//! This crate defines the standard library types, traits, functions, and intrinsics
//! that are built into the language. 

#![allow(non_upper_case_globals)]

use parallax_source::{Dir, Frame, FrameConfig, FrameConfigInner, PackageInfo, SourceDatabase, SourceFile};
use fxhash::FxHashMap;

macro_rules! load_stdlib_files {
    ($($name:ident),*) => {
        $(
            const $name: (&str, &str) = (concat!(stringify!($name), ".plx"), include_str!(concat!(stringify!($name), ".plx")));
        )*
        const STDLIBS: &[(&str, &str)] = &[$($name),*];
    };
}

load_stdlib_files!(
    lib, prelude, ops
);

/// Salsa query to load the embedded standard library as a Frame.
/// This avoids filesystem access at runtime for the standard library source.
pub fn load_stdlib_frame<'db>(db: &'db dyn SourceDatabase) -> Frame<'db> {
    let stdlib_config_inner = FrameConfigInner {
        package: PackageInfo {
            name: "std".to_string(),
            version: "0.1.0".to_string(),
            entry_point: "lib.plx".to_string(),
            authors: vec!["Parallax Contributors".to_string()],
            ..Default::default()
        },
        dependencies: FxHashMap::default(),
    };
    let stdlib_config = FrameConfig::new(db, stdlib_config_inner);

    let mut stdlib_files = Vec::new();

    for (filename, content) in STDLIBS {
        let location = format!("std/{}", filename);
        stdlib_files.push(SourceFile::new(db, location, content.to_string()));
    }

    let stdlib_root_dir = Dir::new(db, "std".to_string(), stdlib_files, vec![]);

    Frame::new(db, stdlib_config, None, stdlib_root_dir, FxHashMap::default())
}

/// Get the standard library frame.
pub fn stdlib_frame(db: &dyn SourceDatabase) -> Frame {
    let stdlib_config_inner = FrameConfigInner {
        package: PackageInfo {
            name: "std".to_string(),
            version: "0.1.0".to_string(),
            entry_point: "lib.plx".to_string(),
            authors: vec!["Parallax Contributors".to_string()],
            ..Default::default()
        },
        dependencies: FxHashMap::default(),
    };
    let stdlib_config = FrameConfig::new(db, stdlib_config_inner);

    let mut stdlib_files = Vec::new();

    for (filename, content) in STDLIBS {
        let location = format!("std/{}", filename);
        stdlib_files.push(SourceFile::new(db, location, content.to_string()));
    }

    let stdlib_root_dir = Dir::new(db, "std".to_string(), stdlib_files, vec![]);

    Frame::new(db, stdlib_config, None, stdlib_root_dir, FxHashMap::default())
}

// Expose the runtime functions
// pub mod runtime; // Removed




