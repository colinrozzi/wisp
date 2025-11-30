mod compiler;

use std::path::{Path, PathBuf};

use anyhow::Result;
use clap::{Parser, Subcommand};

use crate::compiler::CompileArtifacts;

#[derive(Parser)]
#[command(name = "tinyc", version, about = "Tiny Lisp-to-Wasm compiler")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Compile an S-expression source file to WAT/WASM/WIT artifacts.
    Compile {
        /// Path to the input Lisp file.
        #[arg(value_name = "SOURCE")]
        source: PathBuf,
        /// Basename for the generated artifacts.
        #[arg(value_name = "OUT_STEM")]
        out: String,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Command::Compile { source, out } => run_compile(&source, &out)?,
    }

    Ok(())
}

fn run_compile(source: &Path, out: &str) -> Result<()> {
    let artifacts = compiler::compile(source, out)?;
    print_artifacts(&artifacts);
    Ok(())
}

fn print_artifacts(artifacts: &CompileArtifacts) {
    println!("Wrote:");
    println!("  {}", artifacts.wat.display());
    println!("  {}", artifacts.wasm.display());
    println!("  {}", artifacts.wit.display());
}
