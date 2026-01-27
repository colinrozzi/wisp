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
use std::sync::Arc;
use std::sync::RwLock as SyncRwLock;

use anyhow::{Context, Result};
use theater::actor::handle::ActorHandle;
use theater::actor::store::ActorStore;
use theater::chain::StateChain;
use theater::composite_bridge::{AsyncRuntime, CompositeInstance, Ctx, Value};
use theater::id::TheaterId;
use theater::messages::TheaterCommand;
use theater::ValueType;
use tokio::sync::mpsc;
use tracing::info;
use wasmtime::{Engine, Instance, Module, Store};

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

    let mut instance = CompositeInstance::new(
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

    info!("CompositeInstance created successfully");
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

    let mut instance = CompositeInstance::new(
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

    let mut instance = CompositeInstance::new(
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

/// Interactive REPL
/// - Maintains bindings (x=42) and functions
/// - Compiles expressions with inlined values using self-hosted compiler
/// - Executes and prints results
async fn run_repl() -> Result<()> {
    println!("Wisp REPL (self-hosted compiler)");
    println!("Commands: (define x 42), (fn name ...), expressions");
    println!("Type 'quit' to exit\n");

    // REPL state
    let mut bindings: HashMap<String, i32> = HashMap::new();
    let mut functions: Vec<String> = Vec::new();

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
            continue;
        }

        if line == "(clear)" {
            bindings.clear();
            functions.clear();
            println!("cleared");
            continue;
        }

        // Compile and evaluate expression
        match eval_expression(&compiler_wasm, &runtime, line, &bindings, &functions).await {
            Ok(result) => println!("{}", result),
            Err(e) => println!("error: {}", e),
        }
    }

    println!("\nGoodbye!");
    Ok(())
}

/// Compile and evaluate a single expression
async fn eval_expression(
    compiler_wasm: &[u8],
    runtime: &AsyncRuntime,
    expr: &str,
    bindings: &HashMap<String, i32>,
    functions: &[String],
) -> Result<i32> {
    // Generate source with all functions and an eval wrapper
    let mut source = String::new();

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
    let mut compiler = CompositeInstance::new(
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

    // Check for compile errors
    if wat.contains("(error:") {
        anyhow::bail!("Compile error in generated WAT");
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
    let instance = Instance::new(&mut store, &module, &[])?;

    let eval_func = instance.get_func(&mut store, "eval")
        .context("eval function not found")?;

    let mut results = vec![wasmtime::Val::I32(0)];
    eval_func.call(&mut store, &[], &mut results)?;

    match results.into_iter().next() {
        Some(wasmtime::Val::I32(n)) => Ok(n),
        other => anyhow::bail!("Expected i32 result, got {:?}", other),
    }
}
