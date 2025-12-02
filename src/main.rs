mod compiler;

use std::path::{Path, PathBuf};

use anyhow::{Context, Result, anyhow, bail};
use clap::{Parser, Subcommand};
use wasmtime::{
    Engine, Store,
    component::{Component, Linker, Type, Val, types::ComponentItem},
};

use crate::compiler::CompileArtifacts;

#[derive(Parser)]
#[command(name = "wisp", version, about = "Tiny Lisp-to-Wasm compiler")]
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
        /// Basename for the generated artifacts (defaults to source stem).
        #[arg(value_name = "OUT_STEM")]
        out: Option<String>,
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
        args: Vec<String>,
        /// Optional dependency to satisfy imports, in the form `module=path.wasm`.
        #[arg(long = "dep", value_name = "MOD=PATH")]
        dep: Option<String>,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Command::Compile { source, out } => run_compile(&source, out.as_deref())?,
        Command::Run {
            component,
            func,
            args,
            dep,
        } => run_component(&component, &func, &args, dep.as_deref())?,
    }

    Ok(())
}

fn run_compile(source: &Path, out: Option<&str>) -> Result<()> {
    let out_stem = match out {
        Some(stem) => stem.to_string(),
        None => derive_out_stem(source)?,
    };

    let artifacts = compiler::compile(source, &out_stem)?;
    print_artifacts(&artifacts);
    Ok(())
}

fn derive_out_stem(source: &Path) -> Result<String> {
    let stem = source
        .file_stem()
        .and_then(|s| s.to_str())
        .with_context(|| format!("{} has no valid file stem", source.display()))?;
    Ok(stem.to_string())
}

fn print_artifacts(artifacts: &CompileArtifacts) {
    println!("Wrote:");
    println!("  {}", artifacts.wat.display());
    println!("  {}", artifacts.component.display());
    println!("  {}", artifacts.wit.display());
}

fn run_component(
    component_path: &Path,
    func: &str,
    args: &[String],
    dep: Option<&str>,
) -> Result<()> {
    let engine = Engine::default();
    let component = Component::from_file(&engine, component_path)
        .with_context(|| format!("failed to load component {}", component_path.display()))?;
    let mut store = Store::new(&engine, ());
    let mut linker = Linker::new(&engine);

    if let Some(dep) = dep {
        let (module, path) = parse_dep_arg(dep)?;
        let dep_component = Component::from_file(&engine, &path)
            .with_context(|| format!("failed to load dependency {}", path.display()))?;
        let dep_instance = Linker::new(&engine)
            .instantiate(&mut store, &dep_component)
            .with_context(|| format!("failed to instantiate dependency {}", path.display()))?;

        let mut ns = linker
            .instance(&module)
            .with_context(|| format!("failed to create namespace '{}'", module))?;

        for (name, item) in dep_component.component_type().exports(&engine) {
            if matches!(item, ComponentItem::ComponentFunc(_)) {
                let func_ref = dep_instance
                    .get_func(&mut store, name)
                    .with_context(|| format!("dependency export '{}' not found", name))?;
                let func_clone = func_ref.clone();
                ns.func_new(name, move |mut cx, params, results| {
                    func_clone.call(&mut cx, params, results)
                })
                .with_context(|| format!("failed to wire dependency export '{}'", name))?;
            }
        }
    }

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
            (Type::S64, Val::S64(n)) => println!("{}", n),
            (Type::Float32, Val::Float32(n)) => println!("{}", n),
            (Type::Float64, Val::Float64(n)) => println!("{}", n),
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

fn encode_params(param_types: &[Type], args: &[String], func: &str) -> Result<Vec<Val>> {
    let mut params = Vec::with_capacity(args.len());
    for (ty, raw) in param_types.iter().zip(args.iter()) {
        match ty {
            Type::S32 => {
                let parsed: i32 = raw
                    .parse()
                    .with_context(|| format!("expected s32 arg, got '{}'", raw))?;
                params.push(Val::S32(parsed));
            }
            Type::S64 => {
                let parsed: i64 = raw
                    .parse()
                    .with_context(|| format!("expected s64 arg, got '{}'", raw))?;
                params.push(Val::S64(parsed));
            }
            Type::Float32 => {
                let parsed: f32 = raw
                    .parse()
                    .with_context(|| format!("expected f32 arg, got '{}'", raw))?;
                params.push(Val::Float32(parsed));
            }
            Type::Float64 => {
                let parsed: f64 = raw
                    .parse()
                    .with_context(|| format!("expected f64 arg, got '{}'", raw))?;
                params.push(Val::Float64(parsed));
            }
            other => bail!(
                "unsupported parameter type {:?} encountered while calling '{}'",
                other,
                func
            ),
        }
    }
    Ok(params)
}

fn parse_dep_arg(dep: &str) -> Result<(String, PathBuf)> {
    let (module, path) = dep
        .split_once('=')
        .ok_or_else(|| anyhow!("--dep expects format module=path.wasm"))?;
    if module.is_empty() {
        bail!("--dep module name cannot be empty");
    }
    if path.is_empty() {
        bail!("--dep path cannot be empty");
    }
    Ok((module.to_string(), PathBuf::from(path)))
}
