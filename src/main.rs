mod compiler;

use std::path::{Path, PathBuf};

use anyhow::{Context, Result, anyhow, bail};
use clap::{Parser, Subcommand};
use wasmtime::{
    Engine, Store, Module, Instance, Func,
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
    /// Compile an S-expression source file to WAT/WASM artifacts.
    Compile {
        /// Path to the input Lisp file.
        #[arg(value_name = "SOURCE")]
        source: PathBuf,
        /// Basename for the generated artifacts (defaults to source stem).
        #[arg(value_name = "OUT_STEM")]
        out: Option<String>,
    },
    /// Run a function exported from a compiled WebAssembly package (component model).
    Run {
        /// Path to the wasm package produced by `wisp compile`.
        #[arg(value_name = "WASM")]
        package: PathBuf,
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
    /// Run a function from a raw WebAssembly module (not component).
    RunModule {
        /// Path to the wasm module.
        #[arg(value_name = "WASM")]
        module: PathBuf,
        /// Name of the exported function to invoke.
        #[arg(value_name = "FUNC")]
        func: String,
        /// Integer arguments to pass to the function.
        #[arg(value_name = "ARGS")]
        args: Vec<i32>,
        /// String input to pass to the function (for CGRF encoding).
        #[arg(long = "input", value_name = "STRING")]
        input: Option<String>,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Command::Compile { source, out } => run_compile(&source, out.as_deref())?,
        Command::Run {
            package,
            func,
            args,
            dep,
        } => run_package(&package, &func, &args, dep.as_deref())?,
        Command::RunModule { module, func, args, input } => run_module(&module, &func, &args, input.as_deref())?,
    }

    Ok(())
}

fn run_compile(source: &Path, out: Option<&str>) -> Result<()> {
    let out_base = derive_out_base(source, out)?;

    let artifacts = compiler::compile(source, &out_base)?;
    print_artifacts(&artifacts);
    Ok(())
}

fn derive_out_base(source: &Path, out: Option<&str>) -> Result<PathBuf> {
    let parent = source
        .parent()
        .map(Path::to_path_buf)
        .unwrap_or_else(|| PathBuf::from("."));

    match out {
        Some(raw) => {
            let candidate = PathBuf::from(raw);
            if candidate.parent().is_none() {
                Ok(parent.join(candidate))
            } else {
                Ok(candidate)
            }
        }
        None => {
            let stem = source
                .file_stem()
                .and_then(|s| s.to_str())
                .with_context(|| format!("{} has no valid file stem", source.display()))?;
            Ok(parent.join(stem))
        }
    }
}

fn print_artifacts(artifacts: &CompileArtifacts) {
    println!("Wrote:");
    println!("  {}", artifacts.wat.display());
    println!("  {}", artifacts.wasm.display());
}

fn run_package(
    package_path: &Path,
    func: &str,
    args: &[String],
    dep: Option<&str>,
) -> Result<()> {
    let engine = Engine::default();
    let component = Component::from_file(&engine, package_path)
        .with_context(|| format!("failed to load package {}", package_path.display()))?;
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
                let func_clone = func_ref;
                ns.func_new(name, move |mut cx, params, results| {
                    func_clone.call(&mut cx, params, results)
                })
                .with_context(|| format!("failed to wire dependency export '{}'", name))?;
            }
        }
    }

    let instance = linker
        .instantiate(&mut store, &component)
        .context("failed to instantiate package")?;
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
        .context("failed to complete package call cleanup")?;

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

fn run_module(module_path: &Path, func: &str, args: &[i32], input: Option<&str>) -> Result<()> {
    let mut config = wasmtime::Config::new();
    config.wasm_tail_call(true);
    let engine = Engine::new(&config)?;
    let module = Module::from_file(&engine, module_path)
        .with_context(|| format!("failed to load module {}", module_path.display()))?;
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[])
        .context("failed to instantiate module")?;
    let func_ref = instance
        .get_func(&mut store, func)
        .with_context(|| format!("export '{}' not found", func))?;

    let ty = func_ref.ty(&store);
    let num_params = ty.params().len();

    // Check if this is a composite-ABI function (4 params: in_ptr, in_len, out_ptr, out_cap)
    if num_params == 4 && args.is_empty() {
        // Composite calling convention - allocate memory for input and output
        let memory = instance
            .get_memory(&mut store, "memory")
            .context("module has no memory export")?;

        // Encode input as CGRF if provided
        let (in_ptr, in_len) = if let Some(input_str) = input {
            // CGRF string format:
            // Offset 0: Magic "CGRF" (4 bytes)
            // Offset 4: Version (2 bytes) = 2
            // Offset 6: Padding (2 bytes)
            // Offset 8: Num values (4 bytes) = 1
            // Offset 12: Reserved (4 bytes) = 0
            // Offset 16: Type tag (1 byte) = 6 (string)
            // Offset 17-19: Padding (3 bytes)
            // Offset 20: Size (4 bytes) = 4 + len
            // Offset 24: String length (4 bytes)
            // Offset 28: String data
            let str_bytes = input_str.as_bytes();
            let header_size = 28usize;
            let total_size = header_size + str_bytes.len();

            let in_ptr = 8192i32; // Use 8KB for input buffer
            let mut in_buf = vec![0u8; total_size];

            // Magic "CGRF"
            in_buf[0..4].copy_from_slice(&0x46524743u32.to_le_bytes());
            // Version 2
            in_buf[4..6].copy_from_slice(&2u16.to_le_bytes());
            // Num values = 1
            in_buf[8..12].copy_from_slice(&1u32.to_le_bytes());
            // Type tag = 6 (string)
            in_buf[16] = 6;
            // Size = 4 + len
            in_buf[20..24].copy_from_slice(&((4 + str_bytes.len()) as u32).to_le_bytes());
            // String length
            in_buf[24..28].copy_from_slice(&(str_bytes.len() as u32).to_le_bytes());
            // String data
            in_buf[28..].copy_from_slice(str_bytes);

            memory.write(&mut store, in_ptr as usize, &in_buf)?;
            (in_ptr, total_size as i32)
        } else {
            (0, 0)
        };

        // Use address 0 for output buffer
        let out_ptr = 0i32;
        let out_cap = 8192i32; // Increase output capacity

        let params = vec![
            wasmtime::Val::I32(in_ptr),  // in_ptr
            wasmtime::Val::I32(in_len),  // in_len
            wasmtime::Val::I32(out_ptr), // out_ptr
            wasmtime::Val::I32(out_cap), // out_cap
        ];
        let mut results = vec![wasmtime::Val::I32(0)];

        func_ref
            .call(&mut store, &params, &mut results)
            .with_context(|| format!("failed to invoke '{}'", func))?;

        // Read the composite result from memory
        // Format (from WAT analysis):
        // - Offset 0: Tag "FCSF" (4 bytes) = 0x46435346
        // - Offset 4: Variant type (2 bytes), 2=s32, 3=s64
        // - Offset 6: Padding (2 bytes)
        // - Offset 8: Num payloads (4 bytes)
        // - Offset 12: Reserved (4 bytes)
        // - Offset 16: Type tag (1 byte)
        // - Offset 17-19: Padding (3 bytes)
        // - Offset 20: Size (4 bytes)
        // - Offset 24: Actual value (4 bytes for s32, 8 bytes for s64)
        let mut buf = [0u8; 32];
        memory.read(&store, out_ptr as usize, &mut buf)?;

        // Check the tag (first 4 bytes should be 0x46524743 = "CGRF")
        let tag = u32::from_le_bytes([buf[0], buf[1], buf[2], buf[3]]);
        if tag == 0x46524743 {
            // Check type tag at offset 16
            let type_tag = buf[16];
            match type_tag {
                2 => {
                    // s32: read 4 bytes at offset 24
                    let value = i32::from_le_bytes([buf[24], buf[25], buf[26], buf[27]]);
                    println!("{}", value);
                }
                3 => {
                    // s64: read 8 bytes at offset 24
                    let value = i64::from_le_bytes([
                        buf[24], buf[25], buf[26], buf[27], buf[28], buf[29], buf[30], buf[31],
                    ]);
                    println!("{}", value);
                }
                6 => {
                    // String: length at offset 24, data starts at offset 28
                    let str_len = u32::from_le_bytes([buf[24], buf[25], buf[26], buf[27]]) as usize;
                    let mut str_buf = vec![0u8; str_len];
                    memory.read(&store, out_ptr as usize + 28, &mut str_buf)?;
                    let s = String::from_utf8_lossy(&str_buf);
                    println!("{}", s);
                }
                _ => {
                    // Just print the raw result code
                    if let wasmtime::Val::I32(n) = results[0] {
                        println!("(composite result, type={}, bytes written={})", type_tag, n);
                    }
                }
            }
        } else {
            // Not a composite result, print raw
            if let wasmtime::Val::I32(n) = results[0] {
                println!("(raw result: {})", n);
            }
        }
    } else {
        // Standard calling convention
        let params: Vec<wasmtime::Val> = args.iter().map(|&n| wasmtime::Val::I32(n)).collect();
        let num_results = ty.results().len();
        let mut results = vec![wasmtime::Val::I32(0); num_results];

        func_ref
            .call(&mut store, &params, &mut results)
            .with_context(|| format!("failed to invoke '{}'", func))?;

        for result in results {
            match result {
                wasmtime::Val::I32(n) => println!("{}", n),
                wasmtime::Val::I64(n) => println!("{}", n),
                wasmtime::Val::F32(n) => println!("{}", f32::from_bits(n)),
                wasmtime::Val::F64(n) => println!("{}", f64::from_bits(n)),
                other => bail!("unsupported result type: {:?}", other),
            }
        }
    }

    Ok(())
}
