mod compiler;

use std::path::{Path, PathBuf};

use anyhow::{Context, Result, bail};
use clap::{Parser, Subcommand};
use wasmtime::{
    Engine, Store,
    component::{Component, Linker, Type, Val},
};

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
    /// Run a function exported from a compiled WebAssembly component.
    Run {
        /// Path to the wasm component produced by `tinyc compile`.
        #[arg(value_name = "WASM")]
        component: PathBuf,
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
        Command::Run {
            component,
            func,
            args,
        } => run_component(&component, &func, &args)?,
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
    println!("  {}", artifacts.component.display());
    println!("  {}", artifacts.wit.display());
}

fn run_component(component_path: &Path, func: &str, args: &[i32]) -> Result<()> {
    let engine = Engine::default();
    let component = Component::from_file(&engine, component_path)
        .with_context(|| format!("failed to load component {}", component_path.display()))?;
    let mut store = Store::new(&engine, ());
    let linker = Linker::new(&engine);
    let instance = linker
        .instantiate(&mut store, &component)
        .context("failed to instantiate component")?;
    let func_ref = instance
        .get_func(&mut store, func)
        .with_context(|| format!("export '{}' not found", func))?;
    let param_types = func_ref.params(&store);
    if param_types.len() != args.len() {
        bail!(
            "function '{}' expects {} arguments but {} were provided",
            func,
            param_types.len(),
            args.len()
        );
    }
    let params = encode_params(&param_types, args, func)?;
    let result_types = func_ref.results(&store);
    if result_types.len() > 1 {
        bail!("functions with more than one result are not supported yet");
    }
    let mut results = vec![Val::Bool(false); result_types.len()];
    func_ref
        .call(&mut store, &params, &mut results)
        .with_context(|| format!("failed to invoke '{}'", func))?;
    func_ref
        .post_return(&mut store)
        .context("failed to complete component call cleanup")?;

    if let Some((ty, value)) = result_types.into_vec().into_iter().zip(results).next() {
        match (ty, value) {
            (Type::S32, Val::S32(n)) => println!("{}", n),
            (other_ty, other_val) => bail!(
                "unsupported return combination {:?} / {:?} from '{}'",
                other_ty,
                other_val,
                func
            ),
        }
    }

    Ok(())
}

fn encode_params(param_types: &[Type], args: &[i32], func: &str) -> Result<Vec<Val>> {
    let mut params = Vec::with_capacity(args.len());
    for (ty, value) in param_types.iter().zip(args.iter()) {
        match ty {
            Type::S32 => params.push(Val::S32(*value)),
            other => bail!(
                "unsupported parameter type {:?} encountered while calling '{}'",
                other,
                func
            ),
        }
    }
    Ok(params)
}
