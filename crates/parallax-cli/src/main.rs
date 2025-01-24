use clap::Parser;
use indicatif::{ProgressBar, ProgressStyle};
use std::path::PathBuf;
use std::time::Duration;

mod error;
use error::{CliError, ErrorContext, convert_ir_error};

mod io;

#[derive(Parser, Debug)]
#[command(name = "parallax")]
#[command(about = "Parallax interaction network compiler and VM", long_about = None)]
struct Args {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Parser, Debug)]
enum Command {
    /// Run a Parallax program
    Run {
        /// The file to run
        file: PathBuf,
        /// Keep the intermediate IR file
        #[arg(long)]
        keep_ir: bool,
        /// Show progress during execution
        #[arg(long)]
        progress: bool,
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
        /// Show detailed diagnostics
        #[arg(short = 'd', long)]
        diagnostics: bool,
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
        /// Show progress bar
        #[arg(short = 'p', long)]
        progress: bool,
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

fn run_ir_file(path: PathBuf, progress: bool) -> Result<(), CliError> {
    let contents = io::read_file(path)?;

    let ctx = ErrorContext {
        source: &contents,
    };

    let tokens = parallax_ir::lexer::lex(&contents)
        .map_err(|e| convert_ir_error(e, ctx))?;

    let book = parallax_ir::parser::parse_book(&tokens)
        .map_err(|e| convert_ir_error(e, ctx))?;

    // Set up progress bar if requested
    let pb = if progress {
        let pb = ProgressBar::new_spinner();
        pb.set_style(
            ProgressStyle::default_spinner()
                .tick_chars("⠁⠂⠄⡀⢀⠠⠐⠈")
                .template("{spinner:.green} {msg}")
                .unwrap()
        );
        pb.enable_steady_tick(Duration::from_millis(100)); // TODO: Have a better solution for this
        Some(pb)
    } else {
        None
    };

    // let net = parallax_vm::compile::compile(&book).map_err(CliError::from)?;
    // let result = parallax_vm::run(&net)
    //     .map_err(CliError::from)?;
    
    if let Some(pb) = pb {
        pb.finish_and_clear();
    }
    
    Ok(())
}

fn main() -> Result<(), CliError> {
    let args = Args::parse();

    match args.command {
        Command::Ir(IrCommand::Run { file, progress }) => run_ir_file(file, progress),
        _ => {
            todo!("Command not yet implemented")
        }
    }
}