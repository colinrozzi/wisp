//! Test runtime for wisp-compiled modules with assembler support.
//!
//! This runtime provides:
//! - theater:simple/runtime (log)
//! - theater:simple/assembler (wat-to-wasm)
//!
//! Usage:
//!   test-runtime <wasm> <func> [arg]           - Call function with optional arg
//!   test-runtime --compile <source>            - Full pipeline: source -> WAT -> WASM
//!   test-runtime --compile-run <source> <func> - Compile and run a function
//!   test-runtime --compose <wrapper> <expr>    - Link wrapper + expression, call init
//!   test-runtime --repl                        - Interactive REPL mode

use std::collections::HashMap;
use std::io::{self, BufRead, Write};
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::RwLock as SyncRwLock;
use std::sync::Mutex;

use anyhow::{Context, Result};
use theater::actor::handle::ActorHandle;
use theater::actor::store::ActorStore;
use theater::chain::StateChain;
use theater::pack_bridge::{AsyncRuntime, PackInstance, Ctx, Value};
use theater::id::TheaterId;
use theater::messages::TheaterCommand;
use theater::ValueType;
use tokio::sync::mpsc;
use tracing::info;
use wasmtime::{Engine, Instance, Module, Store};

// Pack runtime for loading imported packages
use pack::Runtime as PackRuntime;
use pack::abi::Value as PackValue;

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter("info")
        .init();

    let args: Vec<String> = std::env::args().collect();

    // Check for special modes
    if args.len() >= 3 && args[1] == "--compile" {
        return run_compile_pipeline(&args[2]).await;
    }

    if args.len() >= 4 && args[1] == "--compile-run" {
        let func_args: Vec<i32> = args[4..].iter()
            .filter_map(|s| s.parse().ok())
            .collect();
        return run_compile_and_execute(&args[2], &args[3], &func_args).await;
    }

    if args.len() >= 4 && args[1] == "--compose" {
        return run_compose(&args[2], &args[3]).await;
    }

    if args.len() >= 2 && args[1] == "--repl" {
        return run_repl().await;
    }

    let wasm_path = args.get(1)
        .map(|s| s.as_str())
        .unwrap_or("examples/wisp-compiler.wasm");

    let func_name = args.get(2)
        .map(|s| s.as_str())
        .unwrap_or("compile-source");

    // Optional: string argument for the function
    let arg = args.get(3).cloned();

    info!("Loading WASM from: {}", wasm_path);

    let wasm_bytes = std::fs::read(&wasm_path)?;
    info!("Loaded {} bytes", wasm_bytes.len());

    let runtime = AsyncRuntime::new();

    // Create minimal actor store
    let actor_id = TheaterId::generate();
    let (theater_tx, _theater_rx) = mpsc::channel::<TheaterCommand>(10);
    let (operation_tx, _operation_rx) = mpsc::channel(10);
    let (info_tx, _info_rx) = mpsc::channel(10);
    let (control_tx, _control_rx) = mpsc::channel(10);
    let chain = Arc::new(SyncRwLock::new(StateChain::new(
        actor_id.clone(),
        theater_tx.clone(),
    )));
    let actor_handle = ActorHandle::new(operation_tx, info_tx, control_tx);

    let actor_store = ActorStore::new(
        actor_id.clone(),
        theater_tx.clone(),
        actor_handle,
        chain,
    );

    let mut instance = PackInstance::new(
        "wisp-test",
        &wasm_bytes,
        &runtime,
        actor_store,
        |builder| {
            // theater:simple/runtime - log function
            builder
                .interface("theater:simple/runtime")?
                .func_typed("log", |_ctx: &mut Ctx<'_, ActorStore>, input: Value| {
                    let msg = match input {
                        Value::String(s) => s,
                        _ => format!("{:?}", input),
                    };
                    info!("[ACTOR LOG] {}", msg);
                    Value::Tuple(vec![])
                })?;

            // theater:simple/assembler - wat-to-wasm function
            builder
                .interface("theater:simple/assembler")?
                .func_typed("wat-to-wasm", |_ctx: &mut Ctx<'_, ActorStore>, input: Value| {
                    let wat = match input {
                        Value::String(s) => s,
                        _ => {
                            return Value::Result {
                                ok_type: ValueType::List(Box::new(ValueType::U8)),
                                err_type: ValueType::String,
                                value: Err(Box::new(Value::String(
                                    "expected string argument".to_string(),
                                ))),
                            }
                        }
                    };

                    info!("[ASSEMBLER] Converting {} bytes of WAT to WASM", wat.len());

                    match wat::parse_str(&wat) {
                        Ok(wasm_bytes) => {
                            info!("[ASSEMBLER] Success: {} bytes of WASM", wasm_bytes.len());
                            let bytes: Vec<Value> = wasm_bytes.into_iter().map(Value::U8).collect();
                            Value::Result {
                                ok_type: ValueType::List(Box::new(ValueType::U8)),
                                err_type: ValueType::String,
                                value: Ok(Box::new(Value::List {
                                    elem_type: ValueType::U8,
                                    items: bytes,
                                })),
                            }
                        }
                        Err(e) => {
                            info!("[ASSEMBLER] Error: {}", e);
                            Value::Result {
                                ok_type: ValueType::List(Box::new(ValueType::U8)),
                                err_type: ValueType::String,
                                value: Err(Box::new(Value::String(e.to_string()))),
                            }
                        }
                    }
                })?;

            Ok(())
        },
    )
    .await?;

    info!("PackInstance created successfully");
    info!("Calling function: {}", func_name);

    // Build the input value
    let input = if let Some(s) = arg {
        Value::String(s)
    } else {
        Value::Tuple(vec![])
    };

    let result = instance.call_value(&func_name, &input).await?;

    info!("Result: {:?}", result);

    // Pretty print the result
    match result {
        Value::S32(n) => println!("{}", n),
        Value::S64(n) => println!("{}", n),
        Value::String(s) => println!("{}", s),
        Value::Result { value: Ok(inner), .. } => {
            match *inner {
                Value::List { items, .. } => {
                    // If it's a list of bytes, show length
                    println!("Success: {} bytes", items.len());
                }
                other => println!("Success: {:?}", other),
            }
        }
        Value::Result { value: Err(inner), .. } => {
            println!("Error: {:?}", inner);
        }
        other => println!("{:?}", other),
    }

    Ok(())
}

/// Full compile pipeline: source -> WAT -> WASM
async fn run_compile_pipeline(source: &str) -> Result<()> {
    info!("Compile pipeline: source -> WAT -> WASM");
    info!("Source: {}", source);

    // Step 1: Load the self-hosted compiler
    let compiler_wasm = std::fs::read("examples/wisp-compiler.wasm")
        .context("Failed to load wisp-compiler.wasm")?;

    let runtime = AsyncRuntime::new();
    let actor_store = create_actor_store();

    let mut instance = PackInstance::new(
        "compiler",
        &compiler_wasm,
        &runtime,
        actor_store,
        |builder| {
            builder.interface("theater:simple/runtime")?
                .func_typed("log", |_ctx: &mut Ctx<'_, ActorStore>, input: Value| {
                    if let Value::String(s) = input {
                        info!("[COMPILER] {}", s);
                    }
                    Value::Tuple(vec![])
                })?;
            Ok(())
        },
    ).await?;

    // Step 2: Compile source to WAT
    info!("Compiling source to WAT...");
    let result = instance.call_value("compile-source", &Value::String(source.to_string())).await?;

    let wat = match result {
        Value::String(s) => s,
        other => anyhow::bail!("Expected string result, got {:?}", other),
    };

    info!("Generated {} bytes of WAT", wat.len());

    // Step 3: Assemble WAT to WASM
    info!("Assembling WAT to WASM...");
    let wasm_bytes = wat::parse_str(&wat)
        .context("Failed to assemble WAT to WASM")?;

    info!("Generated {} bytes of WASM", wasm_bytes.len());

    // Verify it's valid WASM
    let engine = Engine::default();
    Module::new(&engine, &wasm_bytes)
        .context("Generated WASM is invalid")?;

    info!("WASM validated successfully!");
    println!("Success: {} bytes of valid WASM", wasm_bytes.len());

    Ok(())
}

/// Compile source and run a function from it
async fn run_compile_and_execute(source: &str, func_name: &str, func_args: &[i32]) -> Result<()> {
    info!("Compile and execute: {} -> {} with args {:?}", source, func_name, func_args);

    // Step 1: Load the self-hosted compiler
    let compiler_wasm = std::fs::read("examples/wisp-compiler.wasm")
        .context("Failed to load wisp-compiler.wasm")?;

    let runtime = AsyncRuntime::new();
    let actor_store = create_actor_store();

    let mut instance = PackInstance::new(
        "compiler",
        &compiler_wasm,
        &runtime,
        actor_store,
        |builder| {
            builder.interface("theater:simple/runtime")?
                .func_typed("log", |_ctx: &mut Ctx<'_, ActorStore>, _input: Value| {
                    Value::Tuple(vec![])
                })?;
            Ok(())
        },
    ).await?;

    // Step 2: Compile source to WAT
    let result = instance.call_value("compile-source", &Value::String(source.to_string())).await?;

    let wat = match result {
        Value::String(s) => s,
        other => anyhow::bail!("Expected string result, got {:?}", other),
    };

    // Step 3: Assemble WAT to WASM
    let wasm_bytes = wat::parse_str(&wat)
        .context("Failed to assemble WAT")?;

    info!("Compiled {} bytes of source to {} bytes of WASM", source.len(), wasm_bytes.len());

    // Step 4: Load and run the compiled module
    let mut config = wasmtime::Config::new();
    config.wasm_tail_call(true);
    let engine = Engine::new(&config)?;
    let module = Module::new(&engine, &wasm_bytes)?;
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[])?;

    let func = instance.get_func(&mut store, func_name)
        .with_context(|| format!("Function '{}' not found", func_name))?;

    let ty = func.ty(&store);
    let mut results = vec![wasmtime::Val::I32(0); ty.results().len()];

    let params: Vec<wasmtime::Val> = func_args.iter().map(|&n| wasmtime::Val::I32(n)).collect();
    func.call(&mut store, &params, &mut results)?;

    // Print results
    for result in results {
        match result {
            wasmtime::Val::I32(n) => println!("{}", n),
            wasmtime::Val::I64(n) => println!("{}", n),
            wasmtime::Val::F32(n) => println!("{}", f32::from_bits(n)),
            wasmtime::Val::F64(n) => println!("{}", f64::from_bits(n)),
            other => println!("{:?}", other),
        }
    }

    Ok(())
}

fn create_actor_store() -> ActorStore {
    let actor_id = TheaterId::generate();
    let (theater_tx, _) = mpsc::channel::<TheaterCommand>(10);
    let (operation_tx, _) = mpsc::channel(10);
    let (info_tx, _) = mpsc::channel(10);
    let (control_tx, _) = mpsc::channel(10);
    let chain = Arc::new(SyncRwLock::new(StateChain::new(
        actor_id.clone(),
        theater_tx.clone(),
    )));
    let actor_handle = ActorHandle::new(operation_tx, info_tx, control_tx);

    ActorStore::new(actor_id, theater_tx, actor_handle, chain)
}

/// Compose wrapper + expression modules and call init
async fn run_compose(wrapper_path: &str, expr_path: &str) -> Result<()> {
    info!("Composing {} + {}", wrapper_path, expr_path);

    let wrapper_bytes = std::fs::read(wrapper_path)
        .with_context(|| format!("Failed to load wrapper: {}", wrapper_path))?;
    let expr_bytes = std::fs::read(expr_path)
        .with_context(|| format!("Failed to load expression: {}", expr_path))?;

    let mut config = wasmtime::Config::new();
    config.wasm_tail_call(true);
    let engine = Engine::new(&config)?;

    // Load both modules
    let wrapper_module = Module::new(&engine, &wrapper_bytes)?;
    let expr_module = Module::new(&engine, &expr_bytes)?;

    let mut store = Store::new(&engine, ());

    // First instantiate the expression module (no imports)
    let expr_instance = Instance::new(&mut store, &expr_module, &[])?;

    // Get the eval function from expression module
    let eval_func = expr_instance.get_func(&mut store, "eval")
        .context("Expression module must export 'eval'")?;

    // Instantiate wrapper with the eval import
    let wrapper_instance = Instance::new(&mut store, &wrapper_module, &[
        eval_func.into(),
    ])?;

    // Call init on the wrapper
    let init_func = wrapper_instance.get_func(&mut store, "init")
        .context("Wrapper must export 'init'")?;

    // Allocate buffers for CGRF calling convention
    let memory = wrapper_instance.get_memory(&mut store, "memory")
        .context("Wrapper must export 'memory'")?;

    let in_ptr = 32768i32;
    let out_ptr = 36864i32;
    let out_cap = 4096i32;

    // Write empty input (CGRF header for empty tuple)
    let in_buf: [u8; 24] = [
        0x43, 0x47, 0x52, 0x46,  // Magic "CGRF"
        0x02, 0x00,              // Version 2
        0x00, 0x00,              // Padding
        0x01, 0x00, 0x00, 0x00,  // Num values: 1
        0x00, 0x00, 0x00, 0x00,  // Reserved
        0x08,                    // Type: tuple
        0x00, 0x00, 0x00,        // Padding
        0x00, 0x00, 0x00, 0x00,  // Size: 0 (empty tuple)
    ];
    memory.write(&mut store, in_ptr as usize, &in_buf)?;

    let mut results = vec![wasmtime::Val::I32(0)];
    init_func.call(&mut store, &[
        wasmtime::Val::I32(in_ptr),
        wasmtime::Val::I32(24),
        wasmtime::Val::I32(out_ptr),
        wasmtime::Val::I32(out_cap),
    ], &mut results)?;

    // Read result from output buffer
    let mut out_buf = [0u8; 32];
    memory.read(&store, out_ptr as usize, &mut out_buf)?;

    // Parse CGRF result
    let tag = u32::from_le_bytes([out_buf[0], out_buf[1], out_buf[2], out_buf[3]]);
    if tag == 0x46524743 {  // "CGRF"
        let type_tag = out_buf[16];
        match type_tag {
            2 => {  // s32
                let value = i32::from_le_bytes([out_buf[24], out_buf[25], out_buf[26], out_buf[27]]);
                println!("{}", value);
            }
            3 => {  // s64
                let value = i64::from_le_bytes([
                    out_buf[24], out_buf[25], out_buf[26], out_buf[27],
                    out_buf[28], out_buf[29], out_buf[30], out_buf[31],
                ]);
                println!("{}", value);
            }
            _ => {
                println!("(result type {})", type_tag);
            }
        }
    } else {
        println!("(raw result)");
    }

    Ok(())
}

/// Represents where an import comes from
#[derive(Debug, Clone)]
enum ImportSource {
    /// Import from the host runtime
    Host,
    /// Import from a WASM component file
    Component(PathBuf),
}

/// Represents a WASM value type
#[derive(Debug, Clone, PartialEq)]
enum WasmType {
    I32,
    I64,
    F32,
    F64,
}

impl WasmType {
    fn from_wasmtime(ty: wasmtime::ValType) -> Option<Self> {
        match ty {
            wasmtime::ValType::I32 => Some(WasmType::I32),
            wasmtime::ValType::I64 => Some(WasmType::I64),
            wasmtime::ValType::F32 => Some(WasmType::F32),
            wasmtime::ValType::F64 => Some(WasmType::F64),
            _ => None, // We don't support other types yet
        }
    }

    fn to_wat(&self) -> &'static str {
        match self {
            WasmType::I32 => "i32",
            WasmType::I64 => "i64",
            WasmType::F32 => "f32",
            WasmType::F64 => "f64",
        }
    }

    fn to_wisp(&self) -> &'static str {
        match self {
            WasmType::I32 => "s32",
            WasmType::I64 => "s64",
            WasmType::F32 => "f32",
            WasmType::F64 => "f64",
        }
    }
}

/// Function signature: parameter types and return type
#[derive(Debug, Clone)]
struct FunctionSig {
    params: Vec<WasmType>,
    results: Vec<WasmType>,
}

/// An exported function with its signature
#[derive(Debug, Clone)]
struct ExportedFunction {
    name: String,
    sig: FunctionSig,
}

/// Tracks a loaded interface and its exports
#[derive(Debug, Clone)]
struct LoadedInterface {
    /// Full interface name (e.g., "colin:math/ops")
    interface: String,
    /// Where it's loaded from
    source: ImportSource,
    /// Exported functions with their signatures
    exports: Vec<ExportedFunction>,
}

/// A loaded Pack package instance
struct LoadedPackage {
    /// Path to the package file
    path: PathBuf,
    /// The Pack instance (wrapped for sharing across closures)
    instance: Arc<Mutex<pack::Instance<()>>>,
}

/// Parse an import statement: (import <interface> from <source>)
/// Returns (interface, source) or None if invalid
fn parse_import(line: &str) -> Option<(String, ImportSource)> {
    // Strip (import and trailing )
    let inner = line.strip_prefix("(import ")?.strip_suffix(')')?;

    // Split on " from "
    let parts: Vec<&str> = inner.splitn(2, " from ").collect();
    if parts.len() != 2 {
        return None;
    }

    let interface = parts[0].trim().to_string();
    let source_str = parts[1].trim();

    let source = if source_str == "host" {
        ImportSource::Host
    } else if source_str.starts_with('"') && source_str.ends_with('"') {
        // It's a file path
        let path = source_str[1..source_str.len()-1].to_string();
        ImportSource::Component(PathBuf::from(path))
    } else {
        return None;
    };

    Some((interface, source))
}

/// Load an interface from a source
///
/// For Pack packages, this loads the package with pack::Runtime and stores
/// the instance for later use. Functions are discovered and assumed to have
/// Graph ABI signatures internally, but are exposed with their logical signatures.
fn load_interface(
    interface: &str,
    source: &ImportSource,
    pack_runtime: &PackRuntime,
    loaded_packages: &mut HashMap<PathBuf, Arc<Mutex<pack::Instance<()>>>>,
) -> Result<LoadedInterface> {
    match source {
        ImportSource::Host => {
            // Known host interfaces with their signatures
            let exports = match interface {
                "theater:simple/runtime" => vec![
                    ExportedFunction {
                        name: "log".to_string(),
                        sig: FunctionSig {
                            params: vec![], // Takes a string via CGRF, not supported yet
                            results: vec![],
                        },
                    },
                ],
                "theater:simple/assembler" => vec![
                    ExportedFunction {
                        name: "wat-to-wasm".to_string(),
                        sig: FunctionSig {
                            params: vec![], // Takes a string via CGRF
                            results: vec![], // Returns result<list<u8>, string>
                        },
                    },
                ],
                "wisp:repl/debug" => vec![
                    ExportedFunction {
                        name: "print-i32".to_string(),
                        sig: FunctionSig {
                            params: vec![WasmType::I32],
                            results: vec![WasmType::I32], // Returns the value (useful for chaining)
                        },
                    },
                    ExportedFunction {
                        name: "print-i64".to_string(),
                        sig: FunctionSig {
                            params: vec![WasmType::I64],
                            results: vec![WasmType::I64],
                        },
                    },
                    ExportedFunction {
                        name: "print-f32".to_string(),
                        sig: FunctionSig {
                            params: vec![WasmType::F32],
                            results: vec![WasmType::F32],
                        },
                    },
                    ExportedFunction {
                        name: "print-f64".to_string(),
                        sig: FunctionSig {
                            params: vec![WasmType::F64],
                            results: vec![WasmType::F64],
                        },
                    },
                ],
                _ => anyhow::bail!("Unknown host interface: {}", interface),
            };
            Ok(LoadedInterface {
                interface: interface.to_string(),
                source: source.clone(),
                exports,
            })
        }
        ImportSource::Component(path) => {
            // Load the Pack package if not already loaded
            if !loaded_packages.contains_key(path) {
                let bytes = std::fs::read(path)
                    .with_context(|| format!("Failed to read Pack package: {}", path.display()))?;

                // Load with pack::Runtime
                let module = pack_runtime.load_module(&bytes)
                    .with_context(|| format!("Failed to load Pack package: {}", path.display()))?;

                let instance = module.instantiate()
                    .with_context(|| format!("Failed to instantiate Pack package: {}", path.display()))?;

                loaded_packages.insert(path.clone(), Arc::new(Mutex::new(instance)));
            }

            // For Pack packages, exports use Graph ABI: (in_ptr, in_len, out_ptr, out_cap) -> out_len
            // The logical signature is encoded in wit+ metadata (not yet parsed)
            //
            // For now, we provide common function signatures that work with the
            // multi-param-test.wasm example. The bridge functions handle Graph ABI.
            //
            // TODO: Parse wit+ to discover actual signatures from the package

            // Common math functions matching multi-param-test.wisp
            let exports = vec![
                ExportedFunction {
                    name: "add".to_string(),
                    sig: FunctionSig {
                        params: vec![WasmType::I32, WasmType::I32],
                        results: vec![WasmType::I32],
                    },
                },
                ExportedFunction {
                    name: "sub".to_string(),
                    sig: FunctionSig {
                        params: vec![WasmType::I32, WasmType::I32],
                        results: vec![WasmType::I32],
                    },
                },
                ExportedFunction {
                    name: "mul".to_string(),
                    sig: FunctionSig {
                        params: vec![WasmType::I32, WasmType::I32],
                        results: vec![WasmType::I32],
                    },
                },
                ExportedFunction {
                    name: "sum3".to_string(),
                    sig: FunctionSig {
                        params: vec![WasmType::I32, WasmType::I32, WasmType::I32],
                        results: vec![WasmType::I32],
                    },
                },
            ];

            info!("Loaded Pack package: {} with Graph ABI exports", path.display());

            Ok(LoadedInterface {
                interface: interface.to_string(),
                source: source.clone(),
                exports,
            })
        }
    }
}

/// Interactive REPL
/// - Maintains bindings (x=42) and functions
/// - Compiles expressions with inlined values using self-hosted compiler
/// - Loads and uses Pack packages via (import <interface> from <source>)
/// - Executes and prints results
async fn run_repl() -> Result<()> {
    println!("Wisp REPL (self-hosted compiler)");
    println!("Commands: (define x 42), (fn name ...), (import <interface> from <source>)");
    println!("Type 'quit' to exit\n");

    // REPL state
    let mut bindings: HashMap<String, i32> = HashMap::new();
    let mut functions: Vec<String> = Vec::new();
    let mut imports: Vec<LoadedInterface> = Vec::new();
    // Loaded Pack packages: path -> LoadedPackage
    let mut loaded_packages: HashMap<PathBuf, Arc<Mutex<pack::Instance<()>>>> = HashMap::new();
    // Pack runtime for loading packages
    let pack_runtime = PackRuntime::new();

    // Load the self-hosted compiler once
    let compiler_wasm = std::fs::read("examples/wisp-compiler.wasm")
        .context("Failed to load wisp-compiler.wasm")?;

    let runtime = AsyncRuntime::new();

    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        // Print prompt
        print!("wisp> ");
        stdout.flush()?;

        // Read line
        let mut line = String::new();
        if stdin.lock().read_line(&mut line)? == 0 {
            break; // EOF
        }
        let line = line.trim();

        if line.is_empty() {
            continue;
        }
        if line == "quit" || line == "exit" {
            break;
        }

        // Check for special forms
        if line.starts_with("(define ") {
            // Parse (define name value)
            if let Some(rest) = line.strip_prefix("(define ") {
                if let Some(rest) = rest.strip_suffix(')') {
                    let parts: Vec<&str> = rest.splitn(2, ' ').collect();
                    if parts.len() == 2 {
                        let name = parts[0].to_string();
                        if let Ok(value) = parts[1].parse::<i32>() {
                            bindings.insert(name.clone(), value);
                            println!("defined {} = {}", name, value);
                            continue;
                        }
                    }
                }
            }
            println!("error: invalid define syntax");
            continue;
        }

        if line.starts_with("(fn ") {
            // Store function definition
            functions.push(line.to_string());
            // Extract function name for display
            if let Some(name) = line.strip_prefix("(fn ").and_then(|s| s.split_whitespace().next()) {
                println!("defined function {}", name);
            }
            continue;
        }

        if line == "(list)" {
            println!("bindings: {:?}", bindings);
            println!("functions: {} defined", functions.len());
            println!("imports: {} loaded", imports.len());
            for imp in &imports {
                let source_name = match &imp.source {
                    ImportSource::Host => "host".to_string(),
                    ImportSource::Component(p) => p.display().to_string(),
                };
                println!("  {} from {} ({} exports)", imp.interface, source_name, imp.exports.len());
                for func in &imp.exports {
                    let params: Vec<&str> = func.sig.params.iter().map(|t| t.to_wisp()).collect();
                    let results: Vec<&str> = func.sig.results.iter().map(|t| t.to_wisp()).collect();
                    let result_str = if results.is_empty() { "()".to_string() } else { results.join(", ") };
                    println!("    {}({}) -> {}", func.name, params.join(", "), result_str);
                }
            }
            continue;
        }

        if line == "(clear)" {
            bindings.clear();
            functions.clear();
            imports.clear();
            loaded_packages.clear();
            println!("cleared");
            continue;
        }

        if line.starts_with("(import ") {
            match parse_import(line) {
                Some((interface, source)) => {
                    match load_interface(&interface, &source, &pack_runtime, &mut loaded_packages) {
                        Ok(loaded) => {
                            let source_name = match &loaded.source {
                                ImportSource::Host => "host".to_string(),
                                ImportSource::Component(p) => p.display().to_string(),
                            };
                            println!("loaded interface {} from {}", loaded.interface, source_name);
                            if !loaded.exports.is_empty() {
                                let export_names: Vec<&str> = loaded.exports.iter()
                                    .map(|e| e.name.as_str())
                                    .collect();
                                println!("  exports: {}", export_names.join(", "));
                            }
                            imports.push(loaded);
                        }
                        Err(e) => println!("error loading import: {}", e),
                    }
                }
                None => println!("error: invalid import syntax. Use: (import <interface> from <source>)"),
            }
            continue;
        }

        // Compile and evaluate expression
        match eval_expression(&compiler_wasm, &runtime, line, &bindings, &functions, &imports, &loaded_packages).await {
            Ok(result) => println!("{}", result),
            Err(e) => println!("error: {}", e),
        }
    }

    println!("\nGoodbye!");
    Ok(())
}

/// Compile and evaluate a single expression
///
/// For Pack package imports, creates bridge functions that:
/// 1. Accept simple signature (i32 args)
/// 2. Encode to PackValue
/// 3. Call Pack instance via call_with_value (Graph ABI)
/// 4. Decode result back to i32
async fn eval_expression(
    compiler_wasm: &[u8],
    runtime: &AsyncRuntime,
    expr: &str,
    bindings: &HashMap<String, i32>,
    functions: &[String],
    imports: &[LoadedInterface],
    loaded_packages: &HashMap<PathBuf, Arc<Mutex<pack::Instance<()>>>>,
) -> Result<i32> {
    // Find which imported functions are used in the expression
    let mut used_imports: Vec<(&LoadedInterface, &ExportedFunction)> = Vec::new();
    for imp in imports {
        for export in &imp.exports {
            // Check if this function name appears in the expression
            // Simple heuristic: look for (funcname or funcname)
            if expr.contains(&format!("({}", export.name)) || expr.contains(&format!(" {}", export.name)) {
                used_imports.push((imp, export));
            }
        }
    }

    // Generate source with all functions and an eval wrapper
    let mut source = String::new();

    // Add stub function definitions for imported functions so the compiler knows their types.
    // These will be replaced with actual imports when we post-process the WAT.
    for (_imp, func) in &used_imports {
        // Generate parameter list with proper types
        let params: Vec<String> = func.sig.params.iter()
            .enumerate()
            .map(|(i, ty)| format!("(p{} {})", i, ty.to_wisp()))
            .collect();
        let params_str = params.join(" ");

        // Generate return type (default to s32 if no results)
        let return_type = func.sig.results.first()
            .map(|t| t.to_wisp())
            .unwrap_or("s32");

        // Generate stub body that matches return type
        let stub_body = match func.sig.results.first() {
            Some(WasmType::I32) | None => "(i32.const 0)",
            Some(WasmType::I64) => "(i64.const 0)",
            Some(WasmType::F32) => "(f32.const 0)",
            Some(WasmType::F64) => "(f64.const 0)",
        };

        source.push_str(&format!(
            "(fn {} ({}) {} {})\n",
            func.name, params_str, return_type, stub_body
        ));
    }

    // Add all function definitions
    for func in functions {
        source.push_str(func);
        source.push('\n');
    }

    // Inline bindings into the expression
    let mut inlined_expr = expr.to_string();
    for (name, value) in bindings {
        // Simple string replacement (not perfect but works for basic cases)
        inlined_expr = inlined_expr.replace(
            &format!(" {} ", name),
            &format!(" (i32.const {}) ", value),
        );
        inlined_expr = inlined_expr.replace(
            &format!(" {})", name),
            &format!(" (i32.const {}))", value),
        );
        inlined_expr = inlined_expr.replace(
            &format!("({} ", name),
            &format!("((i32.const {}) ", value),
        );
    }

    // Wrap expression in eval function
    source.push_str(&format!("(export (fn eval () s32 {}))", inlined_expr));

    // Compile using self-hosted compiler
    let actor_store = create_actor_store();
    let mut compiler = PackInstance::new(
        "compiler",
        compiler_wasm,
        runtime,
        actor_store,
        |builder| {
            builder.interface("theater:simple/runtime")?
                .func_typed("log", |_ctx: &mut Ctx<'_, ActorStore>, _input: Value| {
                    Value::Tuple(vec![])
                })?;
            Ok(())
        },
    ).await?;

    let result = compiler.call_value("compile-source", &Value::String(source.clone())).await?;

    let wat = match result {
        Value::String(s) => s,
        other => anyhow::bail!("Expected WAT string, got {:?}", other),
    };

    // Post-process WAT to inject import declarations
    // The self-hosted compiler doesn't support (import ...) yet, so we:
    // 1. Add stub functions to the source (done above)
    // 2. Post-process WAT to remove stubs and add real imports
    let wat = if !used_imports.is_empty() {
        // Collect the names of stub functions we need to remove
        let stub_names: Vec<&str> = used_imports.iter().map(|(_, f)| f.name.as_str()).collect();

        // Filter out stub function definitions and error lines
        // We need to track when we're inside a stub function to remove multi-line bodies
        let mut in_stub_func = false;
        let mut paren_depth = 0;

        let lines: Vec<&str> = wat.lines()
            .filter(|line| {
                // Remove error lines
                if line.contains("(error:") {
                    return false;
                }

                // Check if this starts a stub function
                for name in &stub_names {
                    if line.contains(&format!("(func ${} ", name)) && !line.contains("(call") {
                        in_stub_func = true;
                        // Count opening parens
                        paren_depth = line.chars().filter(|c| *c == '(').count() as i32
                                    - line.chars().filter(|c| *c == ')').count() as i32;
                        return false;
                    }
                }

                // If we're in a stub function, track parens until we close
                if in_stub_func {
                    paren_depth += line.chars().filter(|c| *c == '(').count() as i32
                                 - line.chars().filter(|c| *c == ')').count() as i32;
                    if paren_depth <= 0 {
                        in_stub_func = false;
                    }
                    return false;
                }

                true
            })
            .collect();

        // Generate import declarations with correct signatures
        let mut import_wat = String::new();
        for (imp, func) in &used_imports {
            // Generate param types
            let params: Vec<&str> = func.sig.params.iter()
                .map(|t| t.to_wat())
                .collect();
            let params_str = params.iter()
                .map(|t| format!("(param {})", t))
                .collect::<Vec<_>>()
                .join(" ");

            // Generate result type
            let results_str = func.sig.results.iter()
                .map(|t| format!("(result {})", t.to_wat()))
                .collect::<Vec<_>>()
                .join(" ");

            import_wat.push_str(&format!(
                "  (import \"{}\" \"{}\" (func ${} {} {}))\n",
                imp.interface, func.name, func.name, params_str, results_str
            ));
        }

        // Build result, inserting imports after (module
        let mut result = String::new();
        for line in lines {
            result.push_str(line);
            result.push('\n');
            if line.trim().starts_with("(module") {
                result.push_str(&import_wat);
            }
        }
        result
    } else {
        wat
    };

    // Check for compile errors
    if wat.contains("(error:") || wat.contains("ERROR") {
        anyhow::bail!("Compile error in generated WAT:\n{}", wat);
    }

    // Assemble WAT to WASM
    let wasm_bytes = wat::parse_str(&wat)
        .context("Failed to assemble WAT")?;

    // Load and run
    let mut config = wasmtime::Config::new();
    config.wasm_tail_call(true);
    let engine = Engine::new(&config)?;
    let module = Module::new(&engine, &wasm_bytes)?;
    let mut store = Store::new(&engine, ());

    // Build imports list by instantiating imported components
    let mut extern_imports: Vec<wasmtime::Extern> = Vec::new();

    for (imp, exported_func) in &used_imports {
        match &imp.source {
            ImportSource::Host => {
                // Create host functions for known interfaces
                match (imp.interface.as_str(), exported_func.name.as_str()) {
                    ("wisp:repl/debug", "print-i32") => {
                        let func = wasmtime::Func::wrap(&mut store, |value: i32| -> i32 {
                            println!("[debug] {}", value);
                            value // Return the value for chaining
                        });
                        extern_imports.push(func.into());
                    }
                    ("wisp:repl/debug", "print-i64") => {
                        let func = wasmtime::Func::wrap(&mut store, |value: i64| -> i64 {
                            println!("[debug] {}", value);
                            value
                        });
                        extern_imports.push(func.into());
                    }
                    ("wisp:repl/debug", "print-f32") => {
                        let func = wasmtime::Func::wrap(&mut store, |value: f32| -> f32 {
                            println!("[debug] {}", value);
                            value
                        });
                        extern_imports.push(func.into());
                    }
                    ("wisp:repl/debug", "print-f64") => {
                        let func = wasmtime::Func::wrap(&mut store, |value: f64| -> f64 {
                            println!("[debug] {}", value);
                            value
                        });
                        extern_imports.push(func.into());
                    }
                    _ => {
                        anyhow::bail!(
                            "Host function {}:{} not implemented for expressions (complex types require CGRF)",
                            imp.interface, exported_func.name
                        );
                    }
                }
            }
            ImportSource::Component(path) => {
                // Get the loaded Pack instance
                let pack_instance = loaded_packages.get(path)
                    .with_context(|| format!("Pack package not loaded: {}", path.display()))?
                    .clone();

                // Create a bridge function that:
                // 1. Accepts simple signature (i32, i32) -> i32
                // 2. Converts to PackValue tuple
                // 3. Calls Pack instance via call_with_value (Graph ABI)
                // 4. Converts result back to i32

                let func_name = exported_func.name.clone();
                let num_params = exported_func.sig.params.len();

                // Create bridge based on number of parameters
                // For now, support 0, 1, or 2 i32 parameters returning i32
                match num_params {
                    0 => {
                        let func = wasmtime::Func::wrap(&mut store, move || -> i32 {
                            let mut instance = pack_instance.lock().unwrap();
                            // Call with empty tuple
                            let input = PackValue::Tuple(vec![]);
                            match instance.call_with_value(&func_name, &input, 0) {
                                Ok(PackValue::S32(n)) => n,
                                Ok(PackValue::Tuple(items)) if items.is_empty() => 0,
                                Ok(other) => {
                                    eprintln!("[Pack bridge] unexpected result: {:?}", other);
                                    0
                                }
                                Err(e) => {
                                    eprintln!("[Pack bridge] error: {}", e);
                                    0
                                }
                            }
                        });
                        extern_imports.push(func.into());
                    }
                    1 => {
                        let func = wasmtime::Func::wrap(&mut store, move |a: i32| -> i32 {
                            let mut instance = pack_instance.lock().unwrap();
                            // Call with single s32 value
                            let input = PackValue::S32(a);
                            match instance.call_with_value(&func_name, &input, 0) {
                                Ok(PackValue::S32(n)) => n,
                                Ok(other) => {
                                    eprintln!("[Pack bridge] unexpected result: {:?}", other);
                                    0
                                }
                                Err(e) => {
                                    eprintln!("[Pack bridge] error: {}", e);
                                    0
                                }
                            }
                        });
                        extern_imports.push(func.into());
                    }
                    2 => {
                        let func = wasmtime::Func::wrap(&mut store, move |a: i32, b: i32| -> i32 {
                            let mut instance = pack_instance.lock().unwrap();
                            // Call with tuple of two s32 values
                            let input = PackValue::Tuple(vec![PackValue::S32(a), PackValue::S32(b)]);
                            match instance.call_with_value(&func_name, &input, 0) {
                                Ok(PackValue::S32(n)) => n,
                                Ok(other) => {
                                    eprintln!("[Pack bridge] unexpected result: {:?}", other);
                                    0
                                }
                                Err(e) => {
                                    eprintln!("[Pack bridge] error: {}", e);
                                    0
                                }
                            }
                        });
                        extern_imports.push(func.into());
                    }
                    3 => {
                        let func = wasmtime::Func::wrap(&mut store, move |a: i32, b: i32, c: i32| -> i32 {
                            let mut instance = pack_instance.lock().unwrap();
                            // Call with tuple of three s32 values
                            let input = PackValue::Tuple(vec![PackValue::S32(a), PackValue::S32(b), PackValue::S32(c)]);
                            match instance.call_with_value(&func_name, &input, 0) {
                                Ok(PackValue::S32(n)) => n,
                                Ok(other) => {
                                    eprintln!("[Pack bridge] unexpected result: {:?}", other);
                                    0
                                }
                                Err(e) => {
                                    eprintln!("[Pack bridge] error: {}", e);
                                    0
                                }
                            }
                        });
                        extern_imports.push(func.into());
                    }
                    _ => {
                        anyhow::bail!(
                            "Pack function {} has {} parameters, only 0-3 supported currently",
                            exported_func.name, num_params
                        );
                    }
                }
            }
        }
    }

    let instance = Instance::new(&mut store, &module, &extern_imports)?;

    let eval_func = instance.get_func(&mut store, "eval")
        .context("eval function not found")?;

    let mut results = vec![wasmtime::Val::I32(0)];
    eval_func.call(&mut store, &[], &mut results)?;

    match results.into_iter().next() {
        Some(wasmtime::Val::I32(n)) => Ok(n),
        other => anyhow::bail!("Expected i32 result, got {:?}", other),
    }
}
