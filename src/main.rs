use clap::Parser;

#[macro_use]
extern crate clap;

mod ir;
mod vm;
mod hvm;

#[derive(Parser, Debug)]
struct Args {
    #[clap(subcommand)]
    subcmd: SubCmd,
}

#[derive(Subcommand, Debug)]
enum SubCmd {
    #[clap(name = "check-ir")]
    CheckIR {
        filename: String,
    }
}

fn main() {
    let args = Args::parse();
    match args.subcmd {
        SubCmd::CheckIR { filename } => {
            // read the file
            let contents = std::fs::read_to_string(filename).expect("File not found");

            ir::CoreParser::new(&contents).parse_book().expect("Parsing failed");
        }
    }
}
