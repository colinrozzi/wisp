mod compiler;

use std::path::{Path, PathBuf};

use anyhow::{Context, Result, bail};
use clap::{Parser, Subcommand};
use wasmtime::{Engine, Instance, Module, Store, Val, ValType};

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
    /// Run a function exported from a compiled wasm module.
    Run {
        /// Path to the wasm module produced by `tinyc compile`.
        #[arg(value_name = "WASM")]
        module: PathBuf,
        /// Name of the exported function to invoke.
        #[arg(value_name = "FUNC")]
        func: String,
        /// Integer arguments to pass to the function.
        #[arg(value_name = "ARGS")]
        args: Vec<i32>,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Command::Compile { source, out } => run_compile(&source, &out)?,
        Command::Run { module, func, args } => run_module(&module, &func, &args)?,
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

fn run_module(module_path: &Path, func: &str, args: &[i32]) -> Result<()> {
    let engine = Engine::default();
    let module = Module::from_file(&engine, module_path)
        .with_context(|| format!("failed to load module {}", module_path.display()))?;
    let mut store = Store::new(&engine, ());
    let instance =
        Instance::new(&mut store, &module, &[]).context("failed to instantiate wasm module")?;
    let func_ref = instance
        .get_func(&mut store, func)
        .with_context(|| format!("export '{}' not found", func))?;
    let ty = func_ref.ty(&store);

    if ty.params().len() != args.len() {
        bail!(
            "function '{}' expects {} arguments but {} were provided",
            func,
            ty.params().len(),
            args.len()
        );
    }
    if ty.results().len() > 1 {
        bail!("functions with more than one result are not supported yet");
    }

    let mut wasm_args = Vec::new();
    for (arg, param_ty) in args.iter().zip(ty.params()) {
        match param_ty {
            ValType::I32 => wasm_args.push(Val::I32(*arg)),
            other => bail!("unsupported parameter type {:?} in '{}'", other, func),
        }
    }

    let mut results = vec![Val::I32(0); ty.results().len()];
    func_ref
        .call(&mut store, &wasm_args, &mut results)
        .with_context(|| format!("failed to invoke '{}'", func))?;

    if let Some(result) = results.first() {
        match result {
            Val::I32(n) => println!("{}", n),
            other => bail!("unsupported return type {:?} from '{}'", other, func),
        }
    }

    Ok(())
}
