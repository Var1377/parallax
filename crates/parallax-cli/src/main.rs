#![allow(dead_code)]

use clap::Parser;
use parallax_ir;
use parallax_lang;
use parallax_vm::{self, DebugConfig};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(name = "parallax")]
#[command(about = "Parallax interaction reduction machine compiler and VM", long_about = None)]
struct Args {
    #[clap(subcommand)]
    command: Command,
}

#[derive(clap::Subcommand, Debug)]
enum Command {
    /// Run a source file
    Run {
        /// Source file to run
        #[arg(value_name = "FILE")]
        file: PathBuf,
        /// Keep intermediate IR file
        #[arg(short = 'i', long)]
        keep_ir: bool,
    },

    /// Debug a source file
    Debug {
        /// Source file to debug
        #[arg(value_name = "FILE")]
        file: PathBuf,
        /// Run in single-step mode
        #[arg(short, long)]
        step: bool,
        /// Maximum number of reduction steps
        #[arg(short = 'n', long, value_name = "STEPS")]
        max_steps: Option<usize>,
        /// Keep intermediate IR file
        #[arg(short = 'i', long)]
        keep_ir: bool,
    },

    /// Check and format source code
    Check {
        /// Source file to check
        #[arg(value_name = "FILE")]
        file: PathBuf,
        /// Format source code in-place
        #[arg(short, long)]
        fmt: bool,
    },

    /// Compile source to IR
    Build {
        /// Source file to compile
        #[arg(value_name = "FILE")]
        file: PathBuf,
        /// Output file (defaults to source name with .hvm extension)
        #[arg(short, long, value_name = "OUT")]
        output: Option<PathBuf>,
    },

    /// Low-level IR tools
    #[command(subcommand)]
    Ir(IrCommand),
}

#[derive(clap::Subcommand, Debug)]
enum IrCommand {
    /// Run an IR file directly
    Run {
        /// IR file to execute
        #[arg(value_name = "FILE")]
        file: PathBuf,
    },

    /// Debug an IR file
    Debug {
        /// IR file to debug
        #[arg(value_name = "FILE")]
        file: PathBuf,
        /// Run in single-step mode
        #[arg(short, long)]
        step: bool,
        /// Maximum number of reduction steps
        #[arg(short = 'n', long, value_name = "STEPS")]
        max_steps: Option<usize>,
    },

    /// Analyze IR file
    Show {
        /// IR file to analyze
        #[arg(value_name = "FILE")]
        file: PathBuf,
        /// What to display
        #[arg(value_enum, default_value_t = ShowFormat::Ast)]
        format: ShowFormat,
    },
}

#[derive(clap::ValueEnum, Clone, Debug)]
enum ShowFormat {
    /// Display parsed AST
    Ast,
    /// Display network graph
    Graph,
    /// Display statistics
    Stats,
}

fn main() -> Result<(), String> {
    let args = Args::parse();
    match args.command {
        Command::Run { file, keep_ir } => {
            // First compile to IR
            let ir_file = file.with_extension("hvm");
            parallax_lang::compile(&file, &ir_file)?;
            
            // Then run the IR
            let contents = std::fs::read_to_string(&ir_file)
                .map_err(|e| format!("Failed to read IR file {}: {}", ir_file.display(), e))?;
            let book = parallax_ir::parse(&contents)?;
            
            // Run in VM
            parallax_vm::run_file(book)?;

            // Clean up IR unless requested to keep it
            if !keep_ir {
                if let Err(e) = std::fs::remove_file(&ir_file) {
                    eprintln!("Warning: Failed to remove IR file {}: {}", ir_file.display(), e);
                }
            }
            Ok(())
        }
        Command::Debug { file, step, max_steps, keep_ir } => {
            // First compile to IR
            let ir_file = file.with_extension("hvm");
            parallax_lang::compile(&file, &ir_file)?;
            
            // Then debug the IR
            let contents = std::fs::read_to_string(&ir_file)
                .map_err(|e| format!("Failed to read IR file {}: {}", ir_file.display(), e))?;
            let book = parallax_ir::parse(&contents)?;
            
            // Debug in VM
            let config = DebugConfig {
                step_mode: step,
                max_steps,
            };
            parallax_vm::debug_file(book, config)?;

            // Clean up IR unless requested to keep it
            if !keep_ir {
                if let Err(e) = std::fs::remove_file(&ir_file) {
                    eprintln!("Warning: Failed to remove IR file {}: {}", ir_file.display(), e);
                }
            }
            Ok(())
        }
        Command::Check { file, fmt } => {
            parallax_lang::check(&file)?;
            if fmt {
                parallax_lang::format(&file)?;
            }
            Ok(())
        }
        Command::Build { file, output } => {
            let output = output.unwrap_or_else(|| file.with_extension("hvm"));
            parallax_lang::compile(&file, &output)?;
            Ok(())
        }
        Command::Ir(cmd) => match cmd {
            IrCommand::Run { file } => {
                let contents = std::fs::read_to_string(&file)
                    .map_err(|e| format!("Failed to read {}: {}", file.display(), e))?;
                let book = parallax_ir::parse(&contents)?;
                parallax_vm::run_file(book)
            }
            IrCommand::Debug { file, step, max_steps } => {
                let contents = std::fs::read_to_string(&file)
                    .map_err(|e| format!("Failed to read {}: {}", file.display(), e))?;
                let book = parallax_ir::parse(&contents)?;
                let config = DebugConfig {
                    step_mode: step,
                    max_steps,
                };
                parallax_vm::debug_file(book, config)
            }
            IrCommand::Show { file, format } => {
                let contents = std::fs::read_to_string(&file)
                    .map_err(|e| format!("Failed to read {}: {}", file.display(), e))?;
                let book = parallax_ir::parse(&contents)?;
                
                match format {
                    ShowFormat::Ast => {
                        println!("{:#?}", book);
                        Ok(())
                    }
                    ShowFormat::Graph => {
                        let graph = parallax_ir::generate_graph(book.clone())?;
                        println!("{}", graph);
                        Ok(())
                    }
                    ShowFormat::Stats => {
                        let stats = parallax_ir::analyze_stats(book)?;
                        println!("{}", stats);
                        Ok(())
                    }
                }
            }
        }
    }
}