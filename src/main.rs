mod compiler;

use std::path::{Path, PathBuf};

use anyhow::{Context, Result, anyhow, bail};
use clap::{Parser, Subcommand};
use wasmtime::{
    Engine, Instance, Module, Store,
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
        /// Dependencies to satisfy imports, in the form `module=path.wasm`.
        #[arg(long = "dep", value_name = "MOD=PATH")]
        dep: Vec<String>,
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
        /// Dependencies to satisfy imports, in the form `module=path.wasm`.
        #[arg(long = "dep", value_name = "MOD=PATH")]
        dep: Vec<String>,
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
        } => run_package(&package, &func, &args, &dep)?,
        Command::RunModule {
            module,
            func,
            args,
            input,
            dep,
        } => run_module(&module, &func, &args, input.as_deref(), &dep)?,
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

fn run_package(package_path: &Path, func: &str, args: &[String], deps: &[String]) -> Result<()> {
    let engine = Engine::default();
    let component = Component::from_file(&engine, package_path)
        .with_context(|| format!("failed to load package {}", package_path.display()))?;
    let mut store = Store::new(&engine, ());
    let mut linker = Linker::new(&engine);

    for dep in deps {
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

fn run_module(
    module_path: &Path,
    func: &str,
    args: &[i32],
    input: Option<&str>,
    deps: &[String],
) -> Result<()> {
    let mut config = wasmtime::Config::new();
    config.wasm_tail_call(true);
    let engine = Engine::new(&config)?;
    let module = Module::from_file(&engine, module_path)
        .with_context(|| format!("failed to load module {}", module_path.display()))?;
    let mut store = Store::new(&engine, ());

    // Load and instantiate dependency modules
    let mut dep_instances: std::collections::HashMap<String, Instance> =
        std::collections::HashMap::new();
    for dep_str in deps {
        let (name, path) = parse_dep_arg(dep_str)?;
        let dep_module = Module::from_file(&engine, &path)
            .with_context(|| format!("failed to load dependency module {}", path.display()))?;
        let dep_instance = Instance::new(&mut store, &dep_module, &[]).with_context(|| {
            format!("failed to instantiate dependency module {}", path.display())
        })?;
        dep_instances.insert(name, dep_instance);
    }

    // Build CGRF bridge functions for each import.
    // Each bridge copies data between the caller's memory and the dep's memory,
    // since each module has its own isolated linear memory.
    let mut imports: Vec<wasmtime::Extern> = Vec::new();
    for import in module.imports() {
        let dep_name = import.module().to_string();
        let func_name = import.name().to_string();
        let dep_instance = *dep_instances.get(&dep_name).ok_or_else(|| {
            anyhow!(
                "missing dependency '{}' for import '{}.{}'",
                dep_name,
                dep_name,
                func_name
            )
        })?;

        // Get the dep's exported function and memory handles
        let dep_func = dep_instance
            .get_func(&mut store, &func_name)
            .ok_or_else(|| anyhow!("dependency '{}' missing export '{}'", dep_name, func_name))?;
        let dep_memory = dep_instance
            .get_memory(&mut store, "memory")
            .ok_or_else(|| anyhow!("dependency '{}' has no memory export", dep_name))?;
        let dep_alloc = dep_instance
            .get_func(&mut store, "__pack_alloc")
            .ok_or_else(|| anyhow!("dependency '{}' missing __pack_alloc", dep_name))?;

        let bridge = wasmtime::Func::new(
            &mut store,
            wasmtime::FuncType::new(
                &engine,
                [
                    wasmtime::ValType::I32,
                    wasmtime::ValType::I32,
                    wasmtime::ValType::I32,
                    wasmtime::ValType::I32,
                ],
                [wasmtime::ValType::I32],
            ),
            move |mut caller: wasmtime::Caller<'_, ()>,
                  params: &[wasmtime::Val],
                  results: &mut [wasmtime::Val]| {
                let in_ptr = params[0].unwrap_i32() as usize;
                let in_len = params[1].unwrap_i32() as usize;
                let out_ptr_ptr = params[2].unwrap_i32() as usize;
                let out_len_ptr = params[3].unwrap_i32() as usize;

                // 1. Read CGRF input from caller's memory
                let caller_memory = caller
                    .get_export("memory")
                    .and_then(|e| e.into_memory())
                    .ok_or_else(|| wasmtime::Error::msg("caller has no memory export"))?;
                let mut in_buf = vec![0u8; in_len];
                if in_len > 0 {
                    caller_memory
                        .read(&caller, in_ptr, &mut in_buf)
                        .map_err(|e| {
                            wasmtime::Error::msg(format!("failed to read caller input: {}", e))
                        })?;
                }

                // 2. Decode, re-encode, and write to dep's memory
                let cgrf_bytes = if in_len > 0 {
                    // Decode from caller, re-encode for dep (same format, but
                    // we need to place bytes in dep's address space)
                    in_buf.clone()
                } else {
                    // No input — encode empty tuple
                    pack::encode(&pack::abi::Value::Tuple(vec![])).map_err(|e| {
                        wasmtime::Error::msg(format!("failed to encode empty input: {}", e))
                    })?
                };

                // Allocate input buffer in dep's memory
                let mut alloc_result = [wasmtime::Val::I32(0)];
                dep_alloc
                    .call(
                        &mut caller,
                        &[wasmtime::Val::I32(cgrf_bytes.len() as i32)],
                        &mut alloc_result,
                    )
                    .map_err(|e| wasmtime::Error::msg(format!("dep __pack_alloc failed: {}", e)))?;
                let dep_in_ptr = alloc_result[0].unwrap_i32();

                dep_memory
                    .write(&mut caller, dep_in_ptr as usize, &cgrf_bytes)
                    .map_err(|e| {
                        wasmtime::Error::msg(format!("failed to write to dep memory: {}", e))
                    })?;

                // Allocate slots for output ptr and len in dep's memory
                dep_alloc
                    .call(&mut caller, &[wasmtime::Val::I32(8)], &mut alloc_result)
                    .map_err(|e| {
                        wasmtime::Error::msg(format!("dep __pack_alloc for slots failed: {}", e))
                    })?;
                let dep_slots_ptr = alloc_result[0].unwrap_i32();
                dep_memory
                    .write(&mut caller, dep_slots_ptr as usize, &[0u8; 8])
                    .map_err(|e| {
                        wasmtime::Error::msg(format!("failed to init dep slots: {}", e))
                    })?;

                // 3. Call the dep's function
                let mut call_result = [wasmtime::Val::I32(0)];
                dep_func
                    .call(
                        &mut caller,
                        &[
                            wasmtime::Val::I32(dep_in_ptr),
                            wasmtime::Val::I32(cgrf_bytes.len() as i32),
                            wasmtime::Val::I32(dep_slots_ptr),
                            wasmtime::Val::I32(dep_slots_ptr + 4),
                        ],
                        &mut call_result,
                    )
                    .map_err(|e| {
                        wasmtime::Error::msg(format!("dep function call failed: {}", e))
                    })?;

                // 4. Read output from dep's memory
                let mut ptr_buf = [0u8; 4];
                let mut len_buf = [0u8; 4];
                dep_memory
                    .read(&caller, dep_slots_ptr as usize, &mut ptr_buf)
                    .map_err(|e| {
                        wasmtime::Error::msg(format!("failed to read dep out_ptr: {}", e))
                    })?;
                dep_memory
                    .read(&caller, (dep_slots_ptr + 4) as usize, &mut len_buf)
                    .map_err(|e| {
                        wasmtime::Error::msg(format!("failed to read dep out_len: {}", e))
                    })?;
                let dep_out_ptr = i32::from_le_bytes(ptr_buf) as usize;
                let dep_out_len = i32::from_le_bytes(len_buf) as usize;

                let mut out_buf = vec![0u8; dep_out_len];
                dep_memory
                    .read(&caller, dep_out_ptr, &mut out_buf)
                    .map_err(|e| {
                        wasmtime::Error::msg(format!("failed to read dep output: {}", e))
                    })?;

                // 5. Allocate output buffer in caller's memory and write result
                let caller_alloc = caller
                    .get_export("__pack_alloc")
                    .and_then(|e| e.into_func())
                    .ok_or_else(|| wasmtime::Error::msg("caller has no __pack_alloc"))?;

                caller_alloc
                    .call(
                        &mut caller,
                        &[wasmtime::Val::I32(out_buf.len() as i32)],
                        &mut alloc_result,
                    )
                    .map_err(|e| {
                        wasmtime::Error::msg(format!("caller __pack_alloc failed: {}", e))
                    })?;
                let caller_out_ptr = alloc_result[0].unwrap_i32() as usize;

                let caller_memory = caller
                    .get_export("memory")
                    .and_then(|e| e.into_memory())
                    .ok_or_else(|| wasmtime::Error::msg("caller has no memory export"))?;
                caller_memory
                    .write(&mut caller, caller_out_ptr, &out_buf)
                    .map_err(|e| {
                        wasmtime::Error::msg(format!("failed to write output to caller: {}", e))
                    })?;

                // 6. Update caller's output pointer and length slots
                caller_memory
                    .write(
                        &mut caller,
                        out_ptr_ptr,
                        &(caller_out_ptr as i32).to_le_bytes(),
                    )
                    .map_err(|e| {
                        wasmtime::Error::msg(format!("failed to write caller out_ptr: {}", e))
                    })?;
                caller_memory
                    .write(
                        &mut caller,
                        out_len_ptr,
                        &(out_buf.len() as i32).to_le_bytes(),
                    )
                    .map_err(|e| {
                        wasmtime::Error::msg(format!("failed to write caller out_len: {}", e))
                    })?;

                results[0] = wasmtime::Val::I32(0);
                Ok(())
            },
        );

        imports.push(bridge.into());
    }

    let instance =
        Instance::new(&mut store, &module, &imports).context("failed to instantiate module")?;
    let func_ref = instance
        .get_func(&mut store, func)
        .with_context(|| format!("export '{}' not found", func))?;

    let ty = func_ref.ty(&store);
    let num_params = ty.params().len();

    // Check if this is a Pack ABI function (4 params: in_ptr, in_len, out_ptr_ptr, out_len_ptr)
    if num_params == 4 && args.is_empty() {
        let memory = instance
            .get_memory(&mut store, "memory")
            .context("module has no memory export")?;
        let pack_alloc = instance
            .get_func(&mut store, "__pack_alloc")
            .context("module has no __pack_alloc export")?;

        // Encode input as CGRF
        let in_bytes = if let Some(input_str) = input {
            pack::encode(&pack::abi::Value::String(input_str.to_string()))
                .context("failed to encode input string")?
        } else {
            pack::encode(&pack::abi::Value::Tuple(vec![]))
                .context("failed to encode empty input")?
        };

        // Allocate input buffer in module's memory
        let mut alloc_result = [wasmtime::Val::I32(0)];
        pack_alloc.call(
            &mut store,
            &[wasmtime::Val::I32(in_bytes.len() as i32)],
            &mut alloc_result,
        )?;
        let in_ptr = alloc_result[0].unwrap_i32();
        memory.write(&mut store, in_ptr as usize, &in_bytes)?;

        // Allocate slots for output pointer and length
        pack_alloc.call(&mut store, &[wasmtime::Val::I32(8)], &mut alloc_result)?;
        let slots_ptr = alloc_result[0].unwrap_i32();
        memory.write(&mut store, slots_ptr as usize, &[0u8; 8])?;

        let params = vec![
            wasmtime::Val::I32(in_ptr),
            wasmtime::Val::I32(in_bytes.len() as i32),
            wasmtime::Val::I32(slots_ptr),     // out_ptr_ptr
            wasmtime::Val::I32(slots_ptr + 4), // out_len_ptr
        ];
        let mut results = vec![wasmtime::Val::I32(0)];

        func_ref
            .call(&mut store, &params, &mut results)
            .with_context(|| format!("failed to invoke '{}'", func))?;

        let status = results[0].unwrap_i32();
        if status != 0 {
            bail!("function '{}' returned error status {}", func, status);
        }

        // Read output pointer and length from the slots
        let mut ptr_buf = [0u8; 4];
        let mut len_buf = [0u8; 4];
        memory.read(&store, slots_ptr as usize, &mut ptr_buf)?;
        memory.read(&store, (slots_ptr + 4) as usize, &mut len_buf)?;
        let out_ptr = i32::from_le_bytes(ptr_buf) as usize;
        let out_len = i32::from_le_bytes(len_buf) as usize;

        // Read and decode the CGRF result
        let mut out_buf = vec![0u8; out_len];
        memory.read(&store, out_ptr, &mut out_buf)?;

        let value = pack::decode(&out_buf)
            .with_context(|| format!("failed to decode CGRF result from '{}'", func))?;

        match value {
            pack::abi::Value::S32(n) => println!("{}", n),
            pack::abi::Value::S64(n) => println!("{}", n),
            pack::abi::Value::F32(n) => println!("{}", n),
            pack::abi::Value::F64(n) => println!("{}", n),
            pack::abi::Value::String(s) => println!("{}", s),
            other => println!("{:?}", other),
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
