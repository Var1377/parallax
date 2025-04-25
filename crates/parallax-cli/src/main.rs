use clap::Parser;
use std::path::PathBuf;
use std::env;
use clap::ValueEnum;

mod error;
use error::CliError;

mod utils; // Contains find_frame_root
mod commands; // Contains handlers

use crate::utils::find_frame_root;

#[derive(Parser, Debug)]
#[command(name = "plx")]
#[command(version = env!("CARGO_PKG_VERSION"))]
#[command(about = "Parallax Interaction Network Compiler", long_about = None)]
struct Args {
    #[clap(subcommand)]
    command: Command,

    /// Set verbosity level (v: info, vv: debug, vvv: trace)
    #[clap(flatten)]
    verbose: clap_verbosity_flag::Verbosity, // Assuming this crate is added
}

#[derive(ValueEnum, Clone, Debug, PartialEq, Eq)]
enum Backend {
    Native,
    Inet,
    Hybrid,
}

#[derive(Parser, Debug)]
enum Command {
    /// Create a new Parallax frame (project)
    New {
        /// Path to create the new frame in
        path: PathBuf,
        /// Create a library frame
        #[arg(long, short = 'l')]
        lib: bool,
    },

    /// Initialize a Parallax frame in an existing directory
    Init {
        /// Path to initialize the frame in (defaults to current directory)
        path: Option<PathBuf>,
        /// Create a library frame
        #[arg(long, short)]
        lib: bool,
    },

    /// Compile the current frame
    Build {
        /// Optional path to the frame to build
        #[arg(value_name = "PATH")]
        path: Option<PathBuf>,
        /// Build optimized artifacts
        #[arg(long, short = 'r')]
        release: bool,
        /// Build with profiling information
        #[arg(long, short = 'p')]
        profile: bool,
    },

    /// Compile and run the current frame's binary executable
    Run {
        /// Optional path to the frame to run
        #[arg(value_name = "PATH")]
        path: Option<PathBuf>,
        /// Build and run optimized artifacts
        #[arg(long, short = 'r')]
        release: bool,
        /// Build and run with profiling information
        #[arg(long, short = 'p')]
        profile: bool,
        /// Select the execution backend
        #[arg(long, short, value_enum, default_value_t = Backend::Hybrid)]
        backend: Backend,
        /// Arguments to pass to the executable
        #[arg(last = true)]
        args: Vec<String>,
    },

    /// Analyze the current frame and report errors, without building
    Check {
        /// Show detailed diagnostics
        #[arg(short = 'd', long, default_value_t = true)]
        diagnostics: bool,
    },

    /// Compile and run tests
    Test {
         /// Build and run optimized artifacts
        #[arg(long)]
        release: bool,
        /// Filter tests by name
        test_filter: Option<String>,
    },

    /// Add a dependency to frame.toml
    Add {
        /// Dependency specification (e.g., name or name@version)
        dep_spec: String,
        /// Add as a development dependency
        #[arg(long)]
        dev: bool,
        /// Specify a path dependency
        #[arg(long, value_name="PATH")]
        path: Option<PathBuf>,
    },

    /// Remove a dependency from frame.toml
    Remove {
        /// Name of the dependency to remove
        dep_name: String,
    },

     /// Update dependencies
    Update {
        /// Specific dependency to update
        dep_name: Option<String>,
    },

    /// Format Parallax source code
    Fmt {
        /// Check if files are formatted without making changes
        #[arg(long)]
        check: bool,
    },

    /// Inspect compiler artifacts and stages
    #[command(subcommand)]
    Show(ShowCommand),

    /// Remove build artifacts
    Clean,
}

#[derive(clap::Subcommand, Debug)]
enum ShowCommand {
    /// Show the resolved dependency tree
    Dependencies,
    /// Show the Abstract Syntax Tree for a file (Placeholder)
    Ast { file: PathBuf },
    /// Show the Intermediate Representation (Placeholder)
    Ir,
    /// Show the network graph (Placeholder)
    NetGraph,
}

fn main() -> miette::Result<()> {
    let args = Args::parse();

    // TODO: Initialize logging based on args.verbose (e.g., using env_logger or tracing_subscriber)
    // Example: simple_logger::init_with_level(args.verbose.log_level().unwrap_or(log::Level::Info));

    // Need to potentially find frame root outside the match for commands that need it
    let current_dir = env::current_dir().map_err(|e| CliError::IoError {
        path: PathBuf::from("."),
        operation: "getting current directory".to_string(),
        source: e,
    });

    let result = match args.command {
        Command::Check { diagnostics } => commands::check::handle_check(diagnostics),
        Command::New { path, lib } => commands::new::handle_new(path, lib),
        Command::Build { path, release, profile } => {
            // Determine starting path based on optional arg or current dir
            let start_path = match path {
                Some(p) => p,
                None => current_dir? // Use the current_dir found earlier
            };
            let root = find_frame_root(&start_path)?;
            commands::build::handle_build(root, release, profile)
        },
        Command::Run { path, release, profile, backend, args } => commands::run::handle_run(path, release, profile, backend, args),
        Command::Clean => commands::clean::handle_clean(),
        // --- TODO: Add dispatch for other commands ---
        Command::Init { .. } => todo!("Implement dispatch for `init` command"),
        Command::Test { .. } => todo!("Implement dispatch for `test` command"),
        Command::Add { .. } => todo!("Implement dispatch for `add` command"),
        Command::Remove { .. } => todo!("Implement dispatch for `remove` command"),
        Command::Update { .. } => todo!("Implement dispatch for `update` command"),
        Command::Fmt { .. } => todo!("Implement dispatch for `fmt` command"),
        Command::Show(subcommand) => match subcommand {
             ShowCommand::Dependencies => todo!("Implement dispatch for `show dependencies`"),
             ShowCommand::Ast { .. } => todo!("Implement dispatch for `show ast`"),
             ShowCommand::Ir => todo!("Implement dispatch for `show ir`"),
             ShowCommand::NetGraph => todo!("Implement dispatch for `show net-graph`"),
         },
    };

    // Convert CliError to miette::Report for final display
    result.map_err(|cli_error| miette::Report::new(cli_error))
}