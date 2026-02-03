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
use std::sync::Mutex;
use std::sync::RwLock as SyncRwLock;

use anyhow::{Context, Result};
use theater::actor::handle::ActorHandle;
use theater::actor::store::ActorStore;
use theater::chain::StateChain;
use theater::id::TheaterId;
use theater::messages::TheaterCommand;
use theater::pack_bridge::{AsyncRuntime, Ctx, PackInstance, Value};
use theater::ValueType;
use tokio::sync::mpsc;
use tracing::{info, warn};
use wasmtime::{Engine, Instance, Module, Store};

// Pack runtime for loading imported packages
use pack::Runtime as PackRuntime;

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt().with_env_filter("info").init();

    let args: Vec<String> = std::env::args().collect();

    // Check for special modes
    if args.len() >= 3 && args[1] == "--compile" {
        return run_compile_pipeline(&args[2]).await;
    }

    if args.len() >= 4 && args[1] == "--compile-run" {
        let func_args: Vec<i32> = args[4..].iter().filter_map(|s| s.parse().ok()).collect();
        return run_compile_and_execute(&args[2], &args[3], &func_args).await;
    }

    if args.len() >= 4 && args[1] == "--compose" {
        return run_compose(&args[2], &args[3]).await;
    }

    if args.len() >= 2 && args[1] == "--repl" {
        return run_repl().await;
    }

    if args.len() >= 2 && args[1] == "--test-messaging" {
        let wasm_path = args
            .get(2)
            .map(|s| s.as_str())
            .unwrap_or("examples/actors/messaging-actor.wasm");
        return run_test_messaging(wasm_path).await;
    }

    let wasm_path = args
        .get(1)
        .map(|s| s.as_str())
        .unwrap_or("examples/wisp-compiler.wasm");

    let func_name = args.get(2).map(|s| s.as_str()).unwrap_or("compile-source");

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

    let actor_store = ActorStore::new(actor_id.clone(), theater_tx.clone(), actor_handle, chain);

    let mut instance =
        PackInstance::new("wisp-test", &wasm_bytes, &runtime, actor_store, |builder| {
            // theater:simple/runtime - log function
            builder.interface("theater:simple/runtime")?.func_typed(
                "log",
                |_ctx: &mut Ctx<'_, ActorStore>, input: Value| {
                    let msg = match input {
                        Value::String(s) => s,
                        _ => format!("{:?}", input),
                    };
                    info!("[ACTOR LOG] {}", msg);
                    Value::Tuple(vec![])
                },
            )?;

            // theater:simple/assembler - wat-to-wasm function
            builder.interface("theater:simple/assembler")?.func_typed(
                "wat-to-wasm",
                |_ctx: &mut Ctx<'_, ActorStore>, input: Value| {
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
                },
            )?;

            Ok(())
        })
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
        Value::Result {
            value: Ok(inner), ..
        } => {
            match *inner {
                Value::List { items, .. } => {
                    // If it's a list of bytes, show length
                    println!("Success: {} bytes", items.len());
                }
                other => println!("Success: {:?}", other),
            }
        }
        Value::Result {
            value: Err(inner), ..
        } => {
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
            builder.interface("theater:simple/runtime")?.func_typed(
                "log",
                |_ctx: &mut Ctx<'_, ActorStore>, input: Value| {
                    if let Value::String(s) = input {
                        info!("[COMPILER] {}", s);
                    }
                    Value::Tuple(vec![])
                },
            )?;
            Ok(())
        },
    )
    .await?;

    // Step 2: Compile source to WAT
    info!("Compiling source to WAT...");
    let result = instance
        .call_value("compile-source", &Value::String(source.to_string()))
        .await?;

    let wat = match result {
        Value::String(s) => s,
        other => anyhow::bail!("Expected string result, got {:?}", other),
    };

    info!("Generated {} bytes of WAT", wat.len());

    // Step 3: Assemble WAT to WASM
    info!("Assembling WAT to WASM...");
    let wasm_bytes = wat::parse_str(&wat).context("Failed to assemble WAT to WASM")?;

    info!("Generated {} bytes of WASM", wasm_bytes.len());

    // Verify it's valid WASM
    let engine = Engine::default();
    Module::new(&engine, &wasm_bytes).context("Generated WASM is invalid")?;

    info!("WASM validated successfully!");
    println!("Success: {} bytes of valid WASM", wasm_bytes.len());

    Ok(())
}

/// Compile source and run a function from it
async fn run_compile_and_execute(source: &str, func_name: &str, func_args: &[i32]) -> Result<()> {
    info!(
        "Compile and execute: {} -> {} with args {:?}",
        source, func_name, func_args
    );

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
            builder
                .interface("theater:simple/runtime")?
                .func_typed("log", |_ctx: &mut Ctx<'_, ActorStore>, _input: Value| {
                    Value::Tuple(vec![])
                })?;
            Ok(())
        },
    )
    .await?;

    // Step 2: Compile source to WAT
    let result = instance
        .call_value("compile-source", &Value::String(source.to_string()))
        .await?;

    let wat = match result {
        Value::String(s) => s,
        other => anyhow::bail!("Expected string result, got {:?}", other),
    };

    // Step 3: Assemble WAT to WASM
    let wasm_bytes = wat::parse_str(&wat).context("Failed to assemble WAT")?;

    info!(
        "Compiled {} bytes of source to {} bytes of WASM",
        source.len(),
        wasm_bytes.len()
    );

    // Step 4: Load and run the compiled module
    let mut config = wasmtime::Config::new();
    config.wasm_tail_call(true);
    let engine = Engine::new(&config)?;
    let module = Module::new(&engine, &wasm_bytes)?;
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[])?;

    let func = instance
        .get_func(&mut store, func_name)
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

/// Build an Option<List<U8>> value from an Option<Vec<u8>>.
fn state_to_value(state: &Option<Vec<u8>>) -> Value {
    Value::Option {
        inner_type: ValueType::List(Box::new(ValueType::U8)),
        value: state
            .as_ref()
            .map(|bytes| Box::new(Value::List {
                elem_type: ValueType::U8,
                items: bytes.iter().copied().map(Value::U8).collect(),
            })),
    }
}

/// Build a List<U8> value from a byte slice.
fn bytes_to_value(bytes: &[u8]) -> Value {
    Value::List {
        elem_type: ValueType::U8,
        items: bytes.iter().copied().map(Value::U8).collect(),
    }
}

/// Decode a Result variant returned by actor functions.
///
/// Actor functions return: result<tuple<...>, string>
/// The Ok payload is a tuple whose first element is the new state (option<list<u8>>)
/// and remaining elements are function-specific return values.
///
/// Returns (new_state, extra_values) on Ok, or the error string on Err.
fn decode_actor_result(value: &Value) -> Result<(Option<Vec<u8>>, Vec<Value>)> {
    match value {
        Value::Result { value: Ok(inner), .. } => {
            // inner is a Tuple
            match inner.as_ref() {
                Value::Tuple(fields) => {
                    // First field is the new state: Option<List<U8>>
                    let new_state = match fields.first() {
                        Some(Value::Option { value: Some(list_val), .. }) => {
                            match list_val.as_ref() {
                                Value::List { items, .. } => {
                                    let bytes: Vec<u8> = items.iter().map(|v| match v {
                                        Value::U8(b) => *b,
                                        _ => 0,
                                    }).collect();
                                    Some(bytes)
                                }
                                _ => None,
                            }
                        }
                        Some(Value::Option { value: None, .. }) => None,
                        _ => None,
                    };
                    let extras = fields[1..].to_vec();
                    Ok((new_state, extras))
                }
                other => anyhow::bail!("Expected tuple in Ok result, got: {:?}", other),
            }
        }
        Value::Result { value: Err(inner), .. } => {
            anyhow::bail!("Actor returned error: {:?}", inner)
        }
        other => anyhow::bail!("Expected Result value, got: {:?}", other),
    }
}

/// Format an optional state for display.
fn format_state(state: &Option<Vec<u8>>) -> String {
    match state {
        None => "None".to_string(),
        Some(bytes) => format!("Some({} bytes)", bytes.len()),
    }
}

/// Test harness for messaging actors.
///
/// Loads a messaging actor WASM and calls init, handle-send, and handle-request
/// in sequence, threading state between calls and recording events to the chain.
async fn run_test_messaging(wasm_path: &str) -> Result<()> {
    use theater::events::{ChainEventData, ChainEventPayload};
    use theater::events::wasm::WasmEventData;

    println!("=== Messaging Actor Test ===");
    println!();

    let wasm_bytes = std::fs::read(wasm_path)
        .with_context(|| format!("Failed to read {}", wasm_path))?;
    info!("Loaded {} bytes from {}", wasm_bytes.len(), wasm_path);

    let runtime = AsyncRuntime::new();
    let actor_store = create_actor_store();

    let mut instance = PackInstance::new(
        "messaging-actor-test",
        &wasm_bytes,
        &runtime,
        actor_store,
        |builder| {
            builder.interface("theater:simple/runtime")?.func_typed(
                "log",
                |_ctx: &mut Ctx<'_, ActorStore>, input: Value| {
                    let msg = match input {
                        Value::String(s) => s,
                        _ => format!("{:?}", input),
                    };
                    info!("[ACTOR LOG] {}", msg);
                    Value::Tuple(vec![])
                },
            )?;
            Ok(())
        },
    )
    .await?;

    // --- 1/3: Call init ---
    println!("[1/3] Calling init...");
    let init_func = "theater:simple/actor.init";

    // Record WasmCall event
    instance.actor_store.chain.write().unwrap().add_typed_event(ChainEventData {
        event_type: "wasm".to_string(),
        data: ChainEventPayload::Wasm(WasmEventData::WasmCall {
            function_name: init_func.to_string(),
            params: vec![],
        }),
    })?;

    let init_state: Option<Vec<u8>> = None;
    let init_input = Value::Tuple(vec![state_to_value(&init_state), Value::Tuple(vec![])]);
    let init_result = instance.call_value(init_func, &init_input).await?;

    let (state, _) = decode_actor_result(&init_result)?;

    // Record WasmResult event
    instance.actor_store.chain.write().unwrap().add_typed_event(ChainEventData {
        event_type: "wasm".to_string(),
        data: ChainEventPayload::Wasm(WasmEventData::WasmResult {
            function_name: init_func.to_string(),
            result: (None, vec![]),
        }),
    })?;

    println!("  init returned Ok, state: {}", format_state(&state));
    println!();

    // --- 2/3: Call handle-send ---
    let send_msg = b"Hello from test!";
    println!("[2/3] Calling handle-send with \"Hello from test!\"...");
    let send_func = "theater:simple/message-server-client.handle-send";

    // Record WasmCall event
    instance.actor_store.chain.write().unwrap().add_typed_event(ChainEventData {
        event_type: "wasm".to_string(),
        data: ChainEventPayload::Wasm(WasmEventData::WasmCall {
            function_name: send_func.to_string(),
            params: vec![],
        }),
    })?;

    // handle-send params: (state, tuple(list<u8>))
    let send_input = Value::Tuple(vec![
        state_to_value(&state),
        Value::Tuple(vec![bytes_to_value(send_msg)]),
    ]);
    let send_result = instance.call_value(send_func, &send_input).await?;

    let (state, _) = decode_actor_result(&send_result)?;

    // Record WasmResult event
    instance.actor_store.chain.write().unwrap().add_typed_event(ChainEventData {
        event_type: "wasm".to_string(),
        data: ChainEventPayload::Wasm(WasmEventData::WasmResult {
            function_name: send_func.to_string(),
            result: (None, vec![]),
        }),
    })?;

    println!("  handle-send returned Ok, state: {}", format_state(&state));
    println!();

    // --- 3/3: Call handle-request ---
    let request_id = "test-request-1";
    let request_body = b"Ping";
    println!("[3/3] Calling handle-request with \"Ping\"...");
    let request_func = "theater:simple/message-server-client.handle-request";

    // Record WasmCall event
    instance.actor_store.chain.write().unwrap().add_typed_event(ChainEventData {
        event_type: "wasm".to_string(),
        data: ChainEventPayload::Wasm(WasmEventData::WasmCall {
            function_name: request_func.to_string(),
            params: vec![],
        }),
    })?;

    // handle-request params: (state, tuple(string, list<u8>))
    let request_input = Value::Tuple(vec![
        state_to_value(&state),
        Value::Tuple(vec![
            Value::String(request_id.to_string()),
            bytes_to_value(request_body),
        ]),
    ]);
    let request_result = instance.call_value(request_func, &request_input).await?;

    let (state, extras) = decode_actor_result(&request_result)?;

    // Extract the response from extras: tuple(option<list<u8>>)
    let response_desc = if let Some(Value::Tuple(response_fields)) = extras.first() {
        match response_fields.first() {
            Some(Value::Option { value: Some(list_val), .. }) => {
                match list_val.as_ref() {
                    Value::List { items, .. } => format!("Some({} bytes)", items.len()),
                    _ => "Some(?)".to_string(),
                }
            }
            Some(Value::Option { value: None, .. }) => "None".to_string(),
            _ => format!("{:?}", response_fields),
        }
    } else {
        "N/A".to_string()
    };

    // Record WasmResult event
    instance.actor_store.chain.write().unwrap().add_typed_event(ChainEventData {
        event_type: "wasm".to_string(),
        data: ChainEventPayload::Wasm(WasmEventData::WasmResult {
            function_name: request_func.to_string(),
            result: (None, vec![]),
        }),
    })?;

    println!(
        "  handle-request returned Ok, state: {}, response: {}",
        format_state(&state),
        response_desc
    );
    println!();

    // --- Print event chain ---
    let chain = instance.actor_store.chain.read().unwrap();
    let events = chain.get_events();
    println!("=== Event Chain ({} events) ===", events.len());
    for event in events {
        let hash_prefix = hex::encode(&event.hash[..std::cmp::min(4, event.hash.len())]);
        // Try to deserialize the event data for a readable summary
        let summary = if let Ok(payload) = serde_json::from_slice::<ChainEventPayload>(&event.data)
        {
            match payload {
                ChainEventPayload::Wasm(WasmEventData::WasmCall { function_name, .. }) => {
                    format!("WasmCall {{ function: \"{}\" }}", function_name)
                }
                ChainEventPayload::Wasm(WasmEventData::WasmResult {
                    function_name, ..
                }) => {
                    format!("WasmResult {{ function: \"{}\" }}", function_name)
                }
                ChainEventPayload::Wasm(WasmEventData::WasmError {
                    function_name,
                    message,
                }) => {
                    format!(
                        "WasmError {{ function: \"{}\", message: \"{}\" }}",
                        function_name, message
                    )
                }
                other => format!("{:?}", other),
            }
        } else {
            format!("(raw: {} bytes)", event.data.len())
        };
        println!("  [{}] {} — {}", hash_prefix, event.event_type, summary);
    }
    println!();

    let verified = chain.verify();
    println!("Chain verified: {}", verified);

    Ok(())
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
    let eval_func = expr_instance
        .get_func(&mut store, "eval")
        .context("Expression module must export 'eval'")?;

    // Instantiate wrapper with the eval import
    let wrapper_instance = Instance::new(&mut store, &wrapper_module, &[eval_func.into()])?;

    // Call init on the wrapper
    let init_func = wrapper_instance
        .get_func(&mut store, "init")
        .context("Wrapper must export 'init'")?;

    // Allocate buffers for CGRF calling convention
    let memory = wrapper_instance
        .get_memory(&mut store, "memory")
        .context("Wrapper must export 'memory'")?;

    let in_ptr = 32768i32;
    let out_ptr = 36864i32;
    let out_cap = 4096i32;

    // Write empty input (CGRF header for empty tuple)
    let in_buf: [u8; 24] = [
        0x43, 0x47, 0x52, 0x46, // Magic "CGRF"
        0x02, 0x00, // Version 2
        0x00, 0x00, // Padding
        0x01, 0x00, 0x00, 0x00, // Num values: 1
        0x00, 0x00, 0x00, 0x00, // Reserved
        0x08, // Type: tuple
        0x00, 0x00, 0x00, // Padding
        0x00, 0x00, 0x00, 0x00, // Size: 0 (empty tuple)
    ];
    memory.write(&mut store, in_ptr as usize, &in_buf)?;

    let mut results = vec![wasmtime::Val::I32(0)];
    init_func.call(
        &mut store,
        &[
            wasmtime::Val::I32(in_ptr),
            wasmtime::Val::I32(24),
            wasmtime::Val::I32(out_ptr),
            wasmtime::Val::I32(out_cap),
        ],
        &mut results,
    )?;

    // Read result from output buffer
    let mut out_buf = [0u8; 32];
    memory.read(&store, out_ptr as usize, &mut out_buf)?;

    // Parse CGRF result
    let tag = u32::from_le_bytes([out_buf[0], out_buf[1], out_buf[2], out_buf[3]]);
    if tag == 0x46524743 {
        // "CGRF"
        let type_tag = out_buf[16];
        match type_tag {
            2 => {
                // s32
                let value =
                    i32::from_le_bytes([out_buf[24], out_buf[25], out_buf[26], out_buf[27]]);
                println!("{}", value);
            }
            3 => {
                // s64
                let value = i64::from_le_bytes([
                    out_buf[24],
                    out_buf[25],
                    out_buf[26],
                    out_buf[27],
                    out_buf[28],
                    out_buf[29],
                    out_buf[30],
                    out_buf[31],
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
    #[allow(dead_code)]
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

/// Rich type information from Pack metadata
#[derive(Debug, Clone)]
struct RichSignature {
    params: Vec<pack::ParamSignature>,
    results: Vec<pack::TypeDesc>,
}

/// An exported function with its signature
#[derive(Debug, Clone)]
struct ExportedFunction {
    name: String,
    sig: FunctionSig,
    /// Rich type info from Pack metadata (None for host functions)
    rich_sig: Option<RichSignature>,
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

/// A field in a user-defined record type
#[derive(Debug, Clone)]
struct ReplRecordField {
    name: String,
    #[allow(dead_code)]
    ty: String,
}

/// A user-defined record type
#[derive(Debug, Clone)]
struct ReplRecordDef {
    name: String,
    fields: Vec<ReplRecordField>,
    /// Original source text for inclusion in compilation
    original_source: String,
}

/// A case in a user-defined variant type
#[derive(Debug, Clone)]
struct ReplVariantCase {
    name: String,
    has_payload: bool,
}

/// A user-defined variant type
#[derive(Debug, Clone)]
struct ReplVariantDef {
    name: String,
    cases: Vec<ReplVariantCase>,
    /// Original source text for inclusion in compilation
    original_source: String,
}

/// What kind of return type an expression has
enum ReplReturnType {
    Scalar,
    NativeString,
    NativeRecord(String),
    NativeVariant(String),
    PackCompound,
}

/// Result of evaluating an expression in the REPL
enum EvalResult {
    /// Simple scalar result (i32)
    Scalar(i32),
    /// Compound result decoded from CGRF (string, list, record, etc.)
    Compound(pack::abi::Value),
    /// Native string from WASM linear memory
    NativeString(String),
    /// Native record from WASM linear memory
    NativeRecord {
        type_name: String,
        fields: Vec<(String, i32)>,
    },
    /// Native variant from WASM linear memory
    NativeVariant {
        type_name: String,
        case_name: String,
        payload: Option<i32>,
    },
}

/// Check if a Pack TypeDesc is a compound type (not a scalar)
fn is_compound_type(td: &pack::TypeDesc) -> bool {
    matches!(
        td,
        pack::TypeDesc::String
            | pack::TypeDesc::List(_)
            | pack::TypeDesc::Option(_)
            | pack::TypeDesc::Result { .. }
            | pack::TypeDesc::Record { .. }
            | pack::TypeDesc::Variant { .. }
            | pack::TypeDesc::Tuple(_)
            | pack::TypeDesc::Value
    )
}

/// Pretty-print a pack::Value for REPL display
fn format_value(value: &pack::abi::Value) -> String {
    match value {
        pack::abi::Value::Bool(b) => {
            if *b {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        pack::abi::Value::U8(n) => n.to_string(),
        pack::abi::Value::U16(n) => n.to_string(),
        pack::abi::Value::U32(n) => n.to_string(),
        pack::abi::Value::U64(n) => n.to_string(),
        pack::abi::Value::S8(n) => n.to_string(),
        pack::abi::Value::S16(n) => n.to_string(),
        pack::abi::Value::S32(n) => n.to_string(),
        pack::abi::Value::S64(n) => n.to_string(),
        pack::abi::Value::F32(n) => n.to_string(),
        pack::abi::Value::F64(n) => n.to_string(),
        pack::abi::Value::Char(c) => format!("'{}'", c),
        pack::abi::Value::String(s) => format!("\"{}\"", s),
        pack::abi::Value::List { items, .. } => {
            let inner: Vec<String> = items.iter().map(format_value).collect();
            format!("[{}]", inner.join(", "))
        }
        pack::abi::Value::Option { value: Some(v), .. } => format!("some({})", format_value(v)),
        pack::abi::Value::Option { value: None, .. } => "none".to_string(),
        pack::abi::Value::Result { value: Ok(v), .. } => format!("ok({})", format_value(v)),
        pack::abi::Value::Result { value: Err(v), .. } => format!("err({})", format_value(v)),
        pack::abi::Value::Record {
            type_name, fields, ..
        } => {
            let field_strs: Vec<String> = fields
                .iter()
                .map(|(name, val)| format!("{}: {}", name, format_value(val)))
                .collect();
            format!("{}{{ {} }}", type_name, field_strs.join(", "))
        }
        pack::abi::Value::Variant {
            case_name, payload, ..
        } => {
            if payload.is_empty() {
                case_name.clone()
            } else {
                let inner: Vec<String> = payload.iter().map(format_value).collect();
                format!("{}({})", case_name, inner.join(", "))
            }
        }
        pack::abi::Value::Tuple(items) => {
            let inner: Vec<String> = items.iter().map(format_value).collect();
            format!("({})", inner.join(", "))
        }
        pack::abi::Value::Flags(n) => format!("flags(0x{:x})", n),
    }
}

/// Preprocess string literals in an expression.
/// Replaces `(str.const "...")` with `(i32.const <addr>)` and collects string data.
/// Strings are stored in memory at addresses starting from 0x100 in format [len:u32][utf8_bytes...].
fn preprocess_string_literals(source: &str) -> (String, Vec<(i32, String)>) {
    let mut result = source.to_string();
    let mut strings = Vec::new();
    let mut addr: i32 = 0x100; // Start at offset 256

    while let Some(start) = result.find("(str.const \"") {
        let content_start = start + "(str.const \"".len();
        if let Some(quote_end) = result[content_start..].find('"') {
            let content_end = content_start + quote_end;
            let paren_end = content_end + 1; // ')' after closing quote
            if paren_end < result.len() && result.as_bytes()[paren_end] == b')' {
                let string_value = result[content_start..content_end].to_string();
                let replacement = format!("(i32.const {})", addr);
                result.replace_range(start..=paren_end, &replacement);

                // Allocate: 4 bytes length + string bytes, aligned to 4
                let total = 4 + string_value.len() as i32;
                strings.push((addr, string_value));
                addr += (total + 3) & !3;
            } else {
                break; // Malformed, stop
            }
        } else {
            break;
        }
    }

    (result, strings)
}

/// Encode a string into WAT data segment format: length prefix + UTF-8 bytes
fn encode_string_data_segment(s: &str) -> String {
    let len = s.len() as u32;
    let len_bytes = len.to_le_bytes();
    let mut result = String::new();
    // Encode length prefix as hex escapes
    for b in &len_bytes {
        result.push_str(&format!("\\{:02x}", b));
    }
    // Encode string bytes
    for b in s.as_bytes() {
        if *b >= 0x20 && *b < 0x7f && *b != b'\\' && *b != b'"' {
            result.push(*b as char);
        } else {
            result.push_str(&format!("\\{:02x}", b));
        }
    }
    result
}

/// Display a Pack TypeDesc as a human-readable string
fn type_desc_display(td: &pack::TypeDesc) -> String {
    match td {
        pack::TypeDesc::Bool => "bool".to_string(),
        pack::TypeDesc::U8 => "u8".to_string(),
        pack::TypeDesc::U16 => "u16".to_string(),
        pack::TypeDesc::U32 => "u32".to_string(),
        pack::TypeDesc::U64 => "u64".to_string(),
        pack::TypeDesc::S8 => "s8".to_string(),
        pack::TypeDesc::S16 => "s16".to_string(),
        pack::TypeDesc::S32 => "s32".to_string(),
        pack::TypeDesc::S64 => "s64".to_string(),
        pack::TypeDesc::F32 => "f32".to_string(),
        pack::TypeDesc::F64 => "f64".to_string(),
        pack::TypeDesc::Char => "char".to_string(),
        pack::TypeDesc::String => "string".to_string(),
        pack::TypeDesc::Flags => "flags".to_string(),
        pack::TypeDesc::List(inner) => format!("list<{}>", type_desc_display(inner)),
        pack::TypeDesc::Option(inner) => format!("option<{}>", type_desc_display(inner)),
        pack::TypeDesc::Result { ok, err } => format!(
            "result<{}, {}>",
            type_desc_display(ok),
            type_desc_display(err)
        ),
        pack::TypeDesc::Record { name, .. } => name.clone(),
        pack::TypeDesc::Variant { name, .. } => name.clone(),
        pack::TypeDesc::Tuple(elems) => {
            let inner: Vec<String> = elems.iter().map(|e| type_desc_display(e)).collect();
            format!("tuple<{}>", inner.join(", "))
        }
        pack::TypeDesc::Value => "value".to_string(),
    }
}

/// Convert a Pack TypeDesc to a WASM-level type
fn type_desc_to_wasm(td: &pack::TypeDesc) -> WasmType {
    match td {
        pack::TypeDesc::S32
        | pack::TypeDesc::U32
        | pack::TypeDesc::Bool
        | pack::TypeDesc::U8
        | pack::TypeDesc::U16
        | pack::TypeDesc::S8
        | pack::TypeDesc::S16
        | pack::TypeDesc::Char
        | pack::TypeDesc::Flags => WasmType::I32,
        pack::TypeDesc::S64 | pack::TypeDesc::U64 => WasmType::I64,
        pack::TypeDesc::F32 => WasmType::F32,
        pack::TypeDesc::F64 => WasmType::F64,
        // Compound types are all i32 pointers/handles at the WASM level
        pack::TypeDesc::String
        | pack::TypeDesc::List(_)
        | pack::TypeDesc::Option(_)
        | pack::TypeDesc::Result { .. }
        | pack::TypeDesc::Record { .. }
        | pack::TypeDesc::Variant { .. }
        | pack::TypeDesc::Tuple(_)
        | pack::TypeDesc::Value => WasmType::I32,
    }
}

// CGRF v2 constants
const CGRF_MAGIC: u32 = 0x46524743; // "CGRF" in little-endian
const CGRF_VERSION: u16 = 2;

/// Type information for CGRF encoding/decoding of a single value
struct CgrfTypeInfo {
    /// CGRF node type tag
    tag: u8,
    /// Payload size in bytes (0 for dynamic types like String)
    payload_size: usize,
    /// WAT instruction to store the value (e.g., "i32.store", "i64.store")
    store_instr: &'static str,
    /// WAT instruction to load the value (e.g., "i32.load", "i64.load")
    load_instr: &'static str,
    /// Whether this type has dynamic size (e.g., String, List)
    is_dynamic: bool,
}

/// Get CGRF encoding info for a Pack TypeDesc
fn cgrf_type_info(td: &pack::TypeDesc) -> CgrfTypeInfo {
    match td {
        pack::TypeDesc::Bool => CgrfTypeInfo {
            tag: 0x01,
            payload_size: 1,
            store_instr: "i32.store8",
            load_instr: "i32.load8_u",
            is_dynamic: false,
        },
        pack::TypeDesc::S32 => CgrfTypeInfo {
            tag: 0x02,
            payload_size: 4,
            store_instr: "i32.store",
            load_instr: "i32.load",
            is_dynamic: false,
        },
        pack::TypeDesc::S64 => CgrfTypeInfo {
            tag: 0x03,
            payload_size: 8,
            store_instr: "i64.store",
            load_instr: "i64.load",
            is_dynamic: false,
        },
        pack::TypeDesc::F32 => CgrfTypeInfo {
            tag: 0x04,
            payload_size: 4,
            store_instr: "f32.store",
            load_instr: "f32.load",
            is_dynamic: false,
        },
        pack::TypeDesc::F64 => CgrfTypeInfo {
            tag: 0x05,
            payload_size: 8,
            store_instr: "f64.store",
            load_instr: "f64.load",
            is_dynamic: false,
        },
        pack::TypeDesc::String => CgrfTypeInfo {
            tag: 0x06,
            payload_size: 0, // dynamic: 4 + string length
            store_instr: "",
            load_instr: "",
            is_dynamic: true,
        },
        pack::TypeDesc::U8 => CgrfTypeInfo {
            tag: 0x0C,
            payload_size: 1,
            store_instr: "i32.store8",
            load_instr: "i32.load8_u",
            is_dynamic: false,
        },
        pack::TypeDesc::S8 => CgrfTypeInfo {
            tag: 0x10,
            payload_size: 1,
            store_instr: "i32.store8",
            load_instr: "i32.load8_s",
            is_dynamic: false,
        },
        pack::TypeDesc::U16 => CgrfTypeInfo {
            tag: 0x0D,
            payload_size: 2,
            store_instr: "i32.store16",
            load_instr: "i32.load16_u",
            is_dynamic: false,
        },
        pack::TypeDesc::S16 => CgrfTypeInfo {
            tag: 0x11,
            payload_size: 2,
            store_instr: "i32.store16",
            load_instr: "i32.load16_s",
            is_dynamic: false,
        },
        pack::TypeDesc::U32 => CgrfTypeInfo {
            tag: 0x0E,
            payload_size: 4,
            store_instr: "i32.store",
            load_instr: "i32.load",
            is_dynamic: false,
        },
        pack::TypeDesc::U64 => CgrfTypeInfo {
            tag: 0x0F,
            payload_size: 8,
            store_instr: "i64.store",
            load_instr: "i64.load",
            is_dynamic: false,
        },
        pack::TypeDesc::Char => CgrfTypeInfo {
            tag: 0x12,
            payload_size: 4,
            store_instr: "i32.store",
            load_instr: "i32.load",
            is_dynamic: false,
        },
        pack::TypeDesc::Flags => CgrfTypeInfo {
            tag: 0x13,
            payload_size: 4,
            store_instr: "i32.store",
            load_instr: "i32.load",
            is_dynamic: false,
        },
        // For other compound types, fall back to S32 (pointer/handle)
        _ => CgrfTypeInfo {
            tag: 0x02,
            payload_size: 4,
            store_instr: "i32.store",
            load_instr: "i32.load",
            is_dynamic: false,
        },
    }
}

/// Generate a CGRF wrapper function for a Pack import.
///
/// The wrapper has the logical signature matching the Pack function but internally:
/// 1. Encodes arguments to CGRF in a buffer (type-aware using RichSignature)
/// 2. Calls the raw import (guest-allocates ABI: in_ptr, in_len, out_ptr_ptr, out_len_ptr -> status)
/// 3. Reads ptr/len from the slots, stores in globals for host compound result reading
/// 4. Decodes the result from CGRF (scalar) or returns placeholder (compound)
fn generate_cgrf_wrapper(func: &ExportedFunction) -> String {
    let mut out = String::new();
    let wrapper_name = &func.name;
    let raw_name = format!("$__raw_{}", func.name);

    // Buffer locations (fixed offsets in linear memory)
    // Use low offsets that fit within a single 64KB memory page
    let in_buf: i32 = 0x2000; // Input buffer at 8KB (up to 8KB for input CGRF)
    let out_ptr_slot: i32 = 0x4000; // Slot for callee to write output ptr
    let out_len_slot: i32 = 0x4004; // Slot for callee to write output len

    // Get rich type info for params and result
    let param_infos: Vec<CgrfTypeInfo> = if let Some(ref rich) = func.rich_sig {
        rich.params.iter().map(|p| cgrf_type_info(&p.ty)).collect()
    } else {
        func.sig
            .params
            .iter()
            .map(|_| cgrf_type_info(&pack::TypeDesc::S32))
            .collect()
    };

    let has_dynamic_params = param_infos.iter().any(|ti| ti.is_dynamic);

    let result_info = if let Some(ref rich) = func.rich_sig {
        rich.results
            .first()
            .map(|td| cgrf_type_info(td))
            .unwrap_or_else(|| cgrf_type_info(&pack::TypeDesc::S32))
    } else {
        cgrf_type_info(&pack::TypeDesc::S32)
    };

    let result_is_compound = func
        .rich_sig
        .as_ref()
        .and_then(|r| r.results.first())
        .map(|td| is_compound_type(td))
        .unwrap_or(false);

    // Start function with original signature
    out.push_str(&format!("  (func ${} ", wrapper_name));
    for (i, ty) in func.sig.params.iter().enumerate() {
        out.push_str(&format!("(param $p{} {}) ", i, ty.to_wat()));
    }

    let result_type = func
        .sig
        .results
        .first()
        .map(|t| t.to_wat())
        .unwrap_or("i32");
    out.push_str(&format!("(result {})\n", result_type));

    // Local variables
    out.push_str("    (local $in_len i32)\n");
    out.push_str("    (local $out_ptr i32)\n");
    out.push_str("    (local $out_len i32)\n");
    out.push_str("    (local $status i32)\n");
    if has_dynamic_params {
        out.push_str("    (local $write_pos i32)\n");
        // Add string length locals for each dynamic param
        for (i, ti) in param_infos.iter().enumerate() {
            if ti.is_dynamic {
                out.push_str(&format!("    (local $str_len_{} i32)\n", i));
            }
        }
    }

    // Encode arguments to CGRF
    let num_params = func.sig.params.len();
    if num_params == 0 {
        // Empty tuple
        emit_cgrf_header(&mut out, in_buf, 1, 0);
        emit_node_header(&mut out, in_buf + 16, 0x0B, 4); // Tuple tag, count-only payload
                                                          // Tuple count = 0
        out.push_str(&format!("    i32.const {}\n", in_buf + 24));
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store\n");
        // Total: 16 (header) + 8 (node header) + 4 (count) = 28 bytes
        out.push_str("    i32.const 28\n");
        out.push_str("    local.set $in_len\n");
    } else if num_params == 1 && !param_infos[0].is_dynamic {
        // Single scalar value (not wrapped in tuple)
        let ti = &param_infos[0];
        let node_size = 8 + ti.payload_size;
        let total_size = 16 + node_size;

        out.push_str(&format!("    ;; Encode single {} value\n", ti.store_instr));
        emit_cgrf_header(&mut out, in_buf, 1, 0);
        emit_node_header(&mut out, in_buf + 16, ti.tag, ti.payload_size as u32);

        // Write value with type-appropriate store instruction
        out.push_str(&format!("    i32.const {}\n", in_buf + 24));
        out.push_str("    local.get $p0\n");
        out.push_str(&format!("    {}\n", ti.store_instr));

        out.push_str(&format!("    i32.const {}\n", total_size));
        out.push_str("    local.set $in_len\n");
    } else if num_params == 1 && param_infos[0].is_dynamic {
        // Single dynamic value (string) - not wrapped in tuple
        let ti = &param_infos[0];
        out.push_str("    ;; Encode single string value\n");

        // Read string length from Wisp memory format [len:u32][utf8_bytes...]
        out.push_str("    local.get $p0\n");
        out.push_str("    i32.load\n");
        out.push_str("    local.set $str_len_0\n");

        // CGRF header (16 bytes) - num_nodes=1
        emit_cgrf_header(&mut out, in_buf, 1, 0);

        // String node header - payload_len is dynamic: 4 + str_len
        // Type tag (1 byte)
        out.push_str(&format!("    i32.const {}\n", in_buf + 16));
        out.push_str(&format!("    i32.const {}\n", ti.tag));
        out.push_str("    i32.store8\n");
        // Padding (1 byte)
        out.push_str(&format!("    i32.const {}\n", in_buf + 17));
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store8\n");
        // Padding (2 bytes)
        out.push_str(&format!("    i32.const {}\n", in_buf + 18));
        out.push_str("    i32.const 0\n");
        out.push_str("    i32.store16\n");
        // Payload length = 4 + str_len
        out.push_str(&format!("    i32.const {}\n", in_buf + 20));
        out.push_str("    i32.const 4\n");
        out.push_str("    local.get $str_len_0\n");
        out.push_str("    i32.add\n");
        out.push_str("    i32.store\n");

        // String payload: length prefix
        out.push_str(&format!("    i32.const {}\n", in_buf + 24));
        out.push_str("    local.get $str_len_0\n");
        out.push_str("    i32.store\n");

        // Copy string bytes from [p0+4] to [in_buf+28]
        out.push_str(&format!("    i32.const {}\n", in_buf + 28)); // dest
        out.push_str("    local.get $p0\n");
        out.push_str("    i32.const 4\n");
        out.push_str("    i32.add\n"); // src = p0 + 4
        out.push_str("    local.get $str_len_0\n"); // len
        out.push_str("    memory.copy\n");

        // in_len = 28 + str_len
        out.push_str("    i32.const 28\n");
        out.push_str("    local.get $str_len_0\n");
        out.push_str("    i32.add\n");
        out.push_str("    local.set $in_len\n");
    } else if !has_dynamic_params {
        // Tuple of scalar values (2 or more) - original fixed-offset code
        let tuple_payload = 4 + 4 * num_params; // 4 bytes count + 4 bytes per child index
        let total_nodes = 1 + num_params;

        // Calculate child node sizes (each has 8-byte header + variable payload)
        let child_node_sizes: Vec<usize> =
            param_infos.iter().map(|ti| 8 + ti.payload_size).collect();
        let total_child_bytes: usize = child_node_sizes.iter().sum();
        let total_size = 16 + 8 + tuple_payload + total_child_bytes;

        out.push_str(&format!(
            "    ;; Encode tuple of {} scalar values\n",
            num_params
        ));
        emit_cgrf_header(&mut out, in_buf, total_nodes as u32, 0);
        emit_node_header(&mut out, in_buf + 16, 0x0B, tuple_payload as u32);

        // Tuple count (at offset 24)
        out.push_str(&format!("    i32.const {}\n", in_buf + 24));
        out.push_str(&format!("    i32.const {}\n", num_params));
        out.push_str("    i32.store\n");

        // Tuple child indices (at offset 28)
        for i in 0..num_params {
            out.push_str(&format!("    i32.const {}\n", in_buf + 28 + (i as i32 * 4)));
            out.push_str(&format!("    i32.const {}\n", i + 1)); // child node indices start at 1
            out.push_str("    i32.store\n");
        }

        // Child nodes (after tuple node) - each with its correct type
        let mut node_offset = in_buf + 24 + tuple_payload as i32; // after tuple header + payload
        for i in 0..num_params {
            let ti = &param_infos[i];
            emit_node_header(&mut out, node_offset, ti.tag, ti.payload_size as u32);
            // Write value
            out.push_str(&format!("    i32.const {}\n", node_offset + 8));
            out.push_str(&format!("    local.get $p{}\n", i));
            out.push_str(&format!("    {}\n", ti.store_instr));

            node_offset += 8 + ti.payload_size as i32;
        }

        out.push_str(&format!("    i32.const {}\n", total_size));
        out.push_str("    local.set $in_len\n");
    } else {
        // Tuple with mixed types (some dynamic) - use $write_pos tracking
        let tuple_payload = 4 + 4 * num_params;
        let total_nodes = 1 + num_params;

        out.push_str(&format!(
            "    ;; Encode tuple of {} mixed values (has dynamic types)\n",
            num_params
        ));

        // Read string lengths first
        for (i, ti) in param_infos.iter().enumerate() {
            if ti.is_dynamic {
                out.push_str(&format!("    ;; Read string length for param {}\n", i));
                out.push_str(&format!("    local.get $p{}\n", i));
                out.push_str("    i32.load\n");
                out.push_str(&format!("    local.set $str_len_{}\n", i));
            }
        }

        // CGRF header
        emit_cgrf_header(&mut out, in_buf, total_nodes as u32, 0);

        // Tuple node header - payload_len is fixed (count + indices)
        emit_node_header(&mut out, in_buf + 16, 0x0B, tuple_payload as u32);

        // Tuple count
        out.push_str(&format!("    i32.const {}\n", in_buf + 24));
        out.push_str(&format!("    i32.const {}\n", num_params));
        out.push_str("    i32.store\n");

        // Tuple child indices (stable: 1, 2, 3, ...)
        for i in 0..num_params {
            out.push_str(&format!("    i32.const {}\n", in_buf + 28 + (i as i32 * 4)));
            out.push_str(&format!("    i32.const {}\n", i + 1));
            out.push_str("    i32.store\n");
        }

        // Initialize write_pos after tuple node
        let child_start = in_buf + 24 + tuple_payload as i32;
        out.push_str(&format!("    i32.const {}\n", child_start));
        out.push_str("    local.set $write_pos\n");

        // Write child nodes using $write_pos
        for (i, ti) in param_infos.iter().enumerate() {
            if ti.is_dynamic {
                // Dynamic child (string)
                out.push_str(&format!("    ;; Child {} (string)\n", i));
                // Node header - tag
                out.push_str("    local.get $write_pos\n");
                out.push_str(&format!("    i32.const {}\n", ti.tag));
                out.push_str("    i32.store8\n");
                // Padding byte
                out.push_str("    local.get $write_pos\n");
                out.push_str("    i32.const 1\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.const 0\n");
                out.push_str("    i32.store8\n");
                // Padding 2 bytes
                out.push_str("    local.get $write_pos\n");
                out.push_str("    i32.const 2\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.const 0\n");
                out.push_str("    i32.store16\n");
                // Payload length = 4 + str_len
                out.push_str("    local.get $write_pos\n");
                out.push_str("    i32.const 4\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.const 4\n");
                out.push_str(&format!("    local.get $str_len_{}\n", i));
                out.push_str("    i32.add\n");
                out.push_str("    i32.store\n");
                // String length in payload
                out.push_str("    local.get $write_pos\n");
                out.push_str("    i32.const 8\n");
                out.push_str("    i32.add\n");
                out.push_str(&format!("    local.get $str_len_{}\n", i));
                out.push_str("    i32.store\n");
                // Copy string bytes
                out.push_str("    local.get $write_pos\n");
                out.push_str("    i32.const 12\n");
                out.push_str("    i32.add\n"); // dest = write_pos + 12
                out.push_str(&format!("    local.get $p{}\n", i));
                out.push_str("    i32.const 4\n");
                out.push_str("    i32.add\n"); // src = p_i + 4
                out.push_str(&format!("    local.get $str_len_{}\n", i));
                out.push_str("    memory.copy\n");
                // Advance: write_pos += 12 + str_len
                out.push_str("    local.get $write_pos\n");
                out.push_str("    i32.const 12\n");
                out.push_str("    i32.add\n");
                out.push_str(&format!("    local.get $str_len_{}\n", i));
                out.push_str("    i32.add\n");
                out.push_str("    local.set $write_pos\n");
            } else {
                // Scalar child - fixed size
                let node_size = 8 + ti.payload_size;
                out.push_str(&format!(
                    "    ;; Child {} (scalar, {} bytes)\n",
                    i, node_size
                ));
                // Node header - tag
                out.push_str("    local.get $write_pos\n");
                out.push_str(&format!("    i32.const {}\n", ti.tag));
                out.push_str("    i32.store8\n");
                // Padding byte
                out.push_str("    local.get $write_pos\n");
                out.push_str("    i32.const 1\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.const 0\n");
                out.push_str("    i32.store8\n");
                // Padding 2 bytes
                out.push_str("    local.get $write_pos\n");
                out.push_str("    i32.const 2\n");
                out.push_str("    i32.add\n");
                out.push_str("    i32.const 0\n");
                out.push_str("    i32.store16\n");
                // Payload length
                out.push_str("    local.get $write_pos\n");
                out.push_str("    i32.const 4\n");
                out.push_str("    i32.add\n");
                out.push_str(&format!("    i32.const {}\n", ti.payload_size));
                out.push_str("    i32.store\n");
                // Write value
                out.push_str("    local.get $write_pos\n");
                out.push_str("    i32.const 8\n");
                out.push_str("    i32.add\n");
                out.push_str(&format!("    local.get $p{}\n", i));
                out.push_str(&format!("    {}\n", ti.store_instr));
                // Advance: write_pos += 8 + payload_size
                out.push_str("    local.get $write_pos\n");
                out.push_str(&format!("    i32.const {}\n", node_size));
                out.push_str("    i32.add\n");
                out.push_str("    local.set $write_pos\n");
            }
        }

        // in_len = write_pos - in_buf
        out.push_str("    local.get $write_pos\n");
        out.push_str(&format!("    i32.const {}\n", in_buf));
        out.push_str("    i32.sub\n");
        out.push_str("    local.set $in_len\n");
    }

    // Call raw import with guest-allocates ABI
    out.push_str("    ;; Call raw import (guest-allocates ABI)\n");
    out.push_str(&format!("    i32.const {}\n", in_buf));
    out.push_str("    local.get $in_len\n");
    out.push_str(&format!("    i32.const {}\n", out_ptr_slot));
    out.push_str(&format!("    i32.const {}\n", out_len_slot));
    out.push_str(&format!("    call {}\n", raw_name));
    out.push_str("    local.set $status\n");

    // Read output ptr/len from slots
    out.push_str("    ;; Read output ptr/len from slots\n");
    out.push_str(&format!("    i32.const {}\n", out_ptr_slot));
    out.push_str("    i32.load\n");
    out.push_str("    local.set $out_ptr\n");
    out.push_str(&format!("    i32.const {}\n", out_len_slot));
    out.push_str("    i32.load\n");
    out.push_str("    local.set $out_len\n");

    // Store output ptr/len in globals for host to read compound results
    out.push_str("    ;; Store output ptr/len in globals for host\n");
    out.push_str("    local.get $out_ptr\n");
    out.push_str("    global.set $__result_ptr\n");
    out.push_str("    local.get $out_len\n");
    out.push_str("    global.set $__result_len\n");

    if result_is_compound {
        // Compound result: host will read CGRF from memory via globals
        // Return 0 as placeholder
        out.push_str("    ;; Compound result - host reads CGRF via globals\n");
        out.push_str("    i32.const 0\n");
    } else {
        // Scalar result: decode from CGRF output
        // CGRF format: header(16) + node header(8) + payload
        // Value is at out_ptr + 24
        out.push_str(&format!(
            "    ;; Decode {} result\n",
            result_info.load_instr
        ));
        out.push_str("    local.get $out_ptr\n");
        out.push_str("    i32.const 24\n");
        out.push_str("    i32.add\n");
        out.push_str(&format!("    {}\n", result_info.load_instr));
    }

    out.push_str("  )\n");
    out
}

/// Emit a CGRF v2 header at the given offset
fn emit_cgrf_header(out: &mut String, offset: i32, num_nodes: u32, root_index: u32) {
    // Magic: "CGRF" (4 bytes)
    out.push_str(&format!("    i32.const {}\n", offset));
    out.push_str(&format!("    i32.const {}\n", CGRF_MAGIC));
    out.push_str("    i32.store\n");
    // Version: 2 (2 bytes)
    out.push_str(&format!("    i32.const {}\n", offset + 4));
    out.push_str(&format!("    i32.const {}\n", CGRF_VERSION));
    out.push_str("    i32.store16\n");
    // Padding (2 bytes)
    out.push_str(&format!("    i32.const {}\n", offset + 6));
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store16\n");
    // Num nodes (4 bytes)
    out.push_str(&format!("    i32.const {}\n", offset + 8));
    out.push_str(&format!("    i32.const {}\n", num_nodes));
    out.push_str("    i32.store\n");
    // Root index (4 bytes)
    out.push_str(&format!("    i32.const {}\n", offset + 12));
    out.push_str(&format!("    i32.const {}\n", root_index));
    out.push_str("    i32.store\n");
}

/// Emit a CGRF node header at the given offset
fn emit_node_header(out: &mut String, offset: i32, type_tag: u8, payload_len: u32) {
    // Type tag (1 byte)
    out.push_str(&format!("    i32.const {}\n", offset));
    out.push_str(&format!("    i32.const {}\n", type_tag));
    out.push_str("    i32.store8\n");
    // Padding (1 byte)
    out.push_str(&format!("    i32.const {}\n", offset + 1));
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store8\n");
    // Padding (2 bytes)
    out.push_str(&format!("    i32.const {}\n", offset + 2));
    out.push_str("    i32.const 0\n");
    out.push_str("    i32.store16\n");
    // Payload length (4 bytes)
    out.push_str(&format!("    i32.const {}\n", offset + 4));
    out.push_str(&format!("    i32.const {}\n", payload_len));
    out.push_str("    i32.store\n");
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
        let path = source_str[1..source_str.len() - 1].to_string();
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
                "theater:simple/runtime" => vec![ExportedFunction {
                    name: "log".to_string(),
                    sig: FunctionSig {
                        params: vec![], // Takes a string via CGRF, not supported yet
                        results: vec![],
                    },
                    rich_sig: None,
                }],
                "theater:simple/assembler" => vec![ExportedFunction {
                    name: "wat-to-wasm".to_string(),
                    sig: FunctionSig {
                        params: vec![],  // Takes a string via CGRF
                        results: vec![], // Returns result<list<u8>, string>
                    },
                    rich_sig: None,
                }],
                "wisp:repl/debug" => vec![
                    ExportedFunction {
                        name: "print-i32".to_string(),
                        sig: FunctionSig {
                            params: vec![WasmType::I32],
                            results: vec![WasmType::I32],
                        },
                        rich_sig: None,
                    },
                    ExportedFunction {
                        name: "print-i64".to_string(),
                        sig: FunctionSig {
                            params: vec![WasmType::I64],
                            results: vec![WasmType::I64],
                        },
                        rich_sig: None,
                    },
                    ExportedFunction {
                        name: "print-f32".to_string(),
                        sig: FunctionSig {
                            params: vec![WasmType::F32],
                            results: vec![WasmType::F32],
                        },
                        rich_sig: None,
                    },
                    ExportedFunction {
                        name: "print-f64".to_string(),
                        sig: FunctionSig {
                            params: vec![WasmType::F64],
                            results: vec![WasmType::F64],
                        },
                        rich_sig: None,
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
            // Read the WASM bytes
            let bytes = std::fs::read(path)
                .with_context(|| format!("Failed to read Pack package: {}", path.display()))?;

            // Load the Pack package if not already loaded
            if !loaded_packages.contains_key(path) {
                // Load with pack::Runtime
                let module = pack_runtime
                    .load_module(&bytes)
                    .with_context(|| format!("Failed to load Pack package: {}", path.display()))?;

                let instance = module.instantiate().with_context(|| {
                    format!("Failed to instantiate Pack package: {}", path.display())
                })?;

                loaded_packages.insert(path.clone(), Arc::new(Mutex::new(instance)));
            }

            // Discover exports using Pack type metadata
            let mut exports = Vec::new();
            {
                let mut instance = loaded_packages.get(path).unwrap().lock().unwrap();
                match instance.types() {
                    Ok(metadata) => {
                        for func_sig in &metadata.exports {
                            let wasm_params: Vec<WasmType> = func_sig
                                .params
                                .iter()
                                .map(|p| type_desc_to_wasm(&p.ty))
                                .collect();
                            let wasm_results: Vec<WasmType> = func_sig
                                .results
                                .iter()
                                .map(|td| type_desc_to_wasm(td))
                                .collect();

                            exports.push(ExportedFunction {
                                name: func_sig.name.clone(),
                                sig: FunctionSig {
                                    params: wasm_params,
                                    results: wasm_results,
                                },
                                rich_sig: Some(RichSignature {
                                    params: func_sig.params.clone(),
                                    results: func_sig.results.clone(),
                                }),
                            });
                        }
                        info!(
                            "Loaded Pack package: {} with {} typed exports",
                            path.display(),
                            exports.len()
                        );
                    }
                    Err(pack::MetadataError::NotFound) => {
                        // No __pack_types - fall back to discovering Graph ABI exports
                        warn!(
                            "Pack package {} has no type metadata, falling back to heuristic",
                            path.display()
                        );
                        let engine = wasmtime::Engine::default();
                        let wasm_module =
                            wasmtime::Module::new(&engine, &bytes).with_context(|| {
                                format!("Failed to parse Pack package: {}", path.display())
                            })?;

                        for export in wasm_module.exports() {
                            if let wasmtime::ExternType::Func(func_ty) = export.ty() {
                                let params: Vec<_> = func_ty.params().collect();
                                let results: Vec<_> = func_ty.results().collect();

                                let is_graph_abi = params.len() == 4
                                    && params.iter().all(|p| matches!(p, wasmtime::ValType::I32))
                                    && results.len() == 1
                                    && matches!(results[0], wasmtime::ValType::I32);

                                if is_graph_abi {
                                    let name = export.name();
                                    // Without metadata, default to 1 param
                                    exports.push(ExportedFunction {
                                        name: name.to_string(),
                                        sig: FunctionSig {
                                            params: vec![WasmType::I32],
                                            results: vec![WasmType::I32],
                                        },
                                        rich_sig: None,
                                    });
                                }
                            }
                        }
                        info!(
                            "Loaded Pack package: {} with Graph ABI exports (no metadata)",
                            path.display()
                        );
                    }
                    Err(e) => {
                        warn!(
                            "Failed to read type metadata from {}: {}",
                            path.display(),
                            e
                        );
                    }
                }
            }

            Ok(LoadedInterface {
                interface: interface.to_string(),
                source: source.clone(),
                exports,
            })
        }
    }
}

/// Parse a variant type definition: (variant name (case1) (case2 payload-type) ...)
fn parse_variant_def(line: &str) -> Option<ReplVariantDef> {
    let inner = line.strip_prefix("(variant ")?.strip_suffix(')')?;
    let name_end = inner.find(' ')?;
    let name = inner[..name_end].to_string();
    let rest = inner[name_end..].trim();

    let mut cases = Vec::new();
    let mut pos = 0;
    let bytes = rest.as_bytes();
    while pos < bytes.len() {
        if bytes[pos] == b'(' {
            let start = pos + 1;
            // Find matching close paren
            let end = rest[start..].find(')')? + start;
            let case_str = rest[start..end].trim();
            let parts: Vec<&str> = case_str.split_whitespace().collect();
            if parts.is_empty() {
                return None;
            }
            let case_name = parts[0].to_string();
            let has_payload = parts.len() > 1;
            cases.push(ReplVariantCase {
                name: case_name,
                has_payload,
            });
            pos = end + 1;
        } else {
            pos += 1;
        }
    }

    if cases.is_empty() {
        return None;
    }

    Some(ReplVariantDef {
        name,
        cases,
        original_source: line.to_string(),
    })
}

/// Parse a record type definition: (record name (field1 type1) (field2 type2) ...)
fn parse_record_def(line: &str) -> Option<ReplRecordDef> {
    let inner = line.strip_prefix("(record ")?.strip_suffix(')')?;
    let name_end = inner.find(' ')?;
    let name = inner[..name_end].to_string();
    let rest = inner[name_end..].trim();

    let mut fields = Vec::new();
    let mut pos = 0;
    let bytes = rest.as_bytes();
    while pos < bytes.len() {
        if bytes[pos] == b'(' {
            let start = pos + 1;
            let end = rest[start..].find(')')? + start;
            let field_str = rest[start..end].trim();
            let parts: Vec<&str> = field_str.split_whitespace().collect();
            if parts.len() != 2 {
                return None;
            }
            fields.push(ReplRecordField {
                name: parts[0].to_string(),
                ty: parts[1].to_string(),
            });
            pos = end + 1;
        } else {
            pos += 1;
        }
    }

    if fields.is_empty() {
        return None;
    }

    Some(ReplRecordDef {
        name,
        fields,
        original_source: line.to_string(),
    })
}

/// Infer the return type of an expression based on known type definitions
fn infer_return_type(
    expr: &str,
    record_defs: &HashMap<String, ReplRecordDef>,
    variant_defs: &HashMap<String, ReplVariantDef>,
    used_imports: &[(&LoadedInterface, &ExportedFunction)],
) -> ReplReturnType {
    let trimmed = expr.trim();

    // Check for string-returning operations
    if trimmed.starts_with("(string-append")
        || trimmed.starts_with("(substring")
        || trimmed.starts_with("(str.const")
    {
        return ReplReturnType::NativeString;
    }

    // Check for Pack compound import call
    for (_, func) in used_imports {
        if func
            .rich_sig
            .as_ref()
            .and_then(|r| r.results.first())
            .map(|td| is_compound_type(td))
            .unwrap_or(false)
            && trimmed.starts_with(&format!("({}", func.name))
        {
            return ReplReturnType::PackCompound;
        }
    }

    // Extract the first symbol from the expression
    let first_sym_raw = if trimmed.starts_with('(') {
        trimmed[1..].split_whitespace().next().unwrap_or("")
    } else {
        trimmed
    };
    // Strip trailing parens (e.g., "red)" from "(red)")
    let first_sym = first_sym_raw.trim_end_matches(')');

    // Check if it's a record constructor
    if record_defs.contains_key(first_sym) {
        return ReplReturnType::NativeRecord(first_sym.to_string());
    }

    // Check if it's a variant case constructor
    for (vname, vdef) in variant_defs {
        for case in &vdef.cases {
            if case.name == first_sym {
                return ReplReturnType::NativeVariant(vname.clone());
            }
        }
    }

    // Check if it's a field accessor (e.g., point.x)
    if first_sym.contains('.') {
        // Field accessor returns scalar
        return ReplReturnType::Scalar;
    }

    ReplReturnType::Scalar
}

/// Read a string from WASM linear memory at the given pointer.
/// Format: [len:u32][utf8_bytes...]
fn read_string_from_memory(
    memory: &wasmtime::Memory,
    store: &Store<()>,
    ptr: i32,
) -> Result<String> {
    let mut len_buf = [0u8; 4];
    memory
        .read(store, ptr as usize, &mut len_buf)
        .context("failed to read string length")?;
    let len = u32::from_le_bytes(len_buf) as usize;

    let mut str_buf = vec![0u8; len];
    memory
        .read(store, (ptr as usize) + 4, &mut str_buf)
        .context("failed to read string bytes")?;

    String::from_utf8(str_buf).context("invalid UTF-8 in string")
}

/// Read a record from WASM linear memory at the given pointer.
/// Records are stored as N consecutive i32 fields.
fn read_record_from_memory(
    memory: &wasmtime::Memory,
    store: &Store<()>,
    ptr: i32,
    rec_def: &ReplRecordDef,
) -> Result<Vec<(String, i32)>> {
    let mut fields = Vec::new();
    for (i, field) in rec_def.fields.iter().enumerate() {
        let offset = (i * 4) as usize;
        let mut buf = [0u8; 4];
        memory
            .read(store, (ptr as usize) + offset, &mut buf)
            .context("failed to read record field")?;
        let value = i32::from_le_bytes(buf);
        fields.push((field.name.clone(), value));
    }
    Ok(fields)
}

/// Read a variant from WASM linear memory at the given pointer.
/// Variants are stored as [tag:i32][payload:i32 (optional)]
fn read_variant_from_memory(
    memory: &wasmtime::Memory,
    store: &Store<()>,
    ptr: i32,
    var_def: &ReplVariantDef,
) -> Result<(String, Option<i32>)> {
    let mut tag_buf = [0u8; 4];
    memory
        .read(store, ptr as usize, &mut tag_buf)
        .context("failed to read variant tag")?;
    let tag = i32::from_le_bytes(tag_buf) as usize;

    if tag >= var_def.cases.len() {
        anyhow::bail!(
            "variant tag {} out of range (max {})",
            tag,
            var_def.cases.len() - 1
        );
    }

    let case = &var_def.cases[tag];
    let payload = if case.has_payload {
        let mut payload_buf = [0u8; 4];
        memory
            .read(store, (ptr as usize) + 4, &mut payload_buf)
            .context("failed to read variant payload")?;
        Some(i32::from_le_bytes(payload_buf))
    } else {
        None
    };

    Ok((case.name.clone(), payload))
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
    let mut record_defs: HashMap<String, ReplRecordDef> = HashMap::new();
    let mut variant_defs: HashMap<String, ReplVariantDef> = HashMap::new();
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
            if let Some(name) = line
                .strip_prefix("(fn ")
                .and_then(|s| s.split_whitespace().next())
            {
                println!("defined function {}", name);
            }
            continue;
        }

        if line.starts_with("(variant ") {
            match parse_variant_def(line) {
                Some(vdef) => {
                    println!("defined variant {}", vdef.name);
                    variant_defs.insert(vdef.name.clone(), vdef);
                }
                None => println!("error: invalid variant syntax. Use: (variant name (case1) (case2 payload-type) ...)"),
            }
            continue;
        }

        if line.starts_with("(record ") {
            match parse_record_def(line) {
                Some(rdef) => {
                    println!("defined record {}", rdef.name);
                    record_defs.insert(rdef.name.clone(), rdef);
                }
                None => println!("error: invalid record syntax. Use: (record name (field1 type1) (field2 type2) ...)"),
            }
            continue;
        }

        if line == "(list)" {
            println!("bindings: {:?}", bindings);
            println!("functions: {} defined", functions.len());
            if !variant_defs.is_empty() {
                println!("variants:");
                for (name, vdef) in &variant_defs {
                    let cases: Vec<String> = vdef
                        .cases
                        .iter()
                        .map(|c| {
                            if c.has_payload {
                                format!("{}(_)", c.name)
                            } else {
                                c.name.clone()
                            }
                        })
                        .collect();
                    println!("  {} = {}", name, cases.join(" | "));
                }
            }
            if !record_defs.is_empty() {
                println!("records:");
                for (name, rdef) in &record_defs {
                    let fields: Vec<String> = rdef
                        .fields
                        .iter()
                        .map(|f| format!("{}: {}", f.name, f.ty))
                        .collect();
                    println!("  {} {{ {} }}", name, fields.join(", "));
                }
            }
            println!("imports: {} loaded", imports.len());
            for imp in &imports {
                let source_name = match &imp.source {
                    ImportSource::Host => "host".to_string(),
                    ImportSource::Component(p) => p.display().to_string(),
                };
                println!(
                    "  {} from {} ({} exports)",
                    imp.interface,
                    source_name,
                    imp.exports.len()
                );
                for func in &imp.exports {
                    // Prefer rich type names when available
                    if let Some(ref rich) = func.rich_sig {
                        let params: Vec<String> = rich
                            .params
                            .iter()
                            .map(|p| format!("{}: {}", p.name, type_desc_display(&p.ty)))
                            .collect();
                        let results: Vec<String> = rich
                            .results
                            .iter()
                            .map(|td| type_desc_display(td))
                            .collect();
                        let result_str = if results.is_empty() {
                            "()".to_string()
                        } else {
                            results.join(", ")
                        };
                        println!("    {}({}) -> {}", func.name, params.join(", "), result_str);
                    } else {
                        let params: Vec<&str> =
                            func.sig.params.iter().map(|t| t.to_wisp()).collect();
                        let results: Vec<&str> =
                            func.sig.results.iter().map(|t| t.to_wisp()).collect();
                        let result_str = if results.is_empty() {
                            "()".to_string()
                        } else {
                            results.join(", ")
                        };
                        println!("    {}({}) -> {}", func.name, params.join(", "), result_str);
                    }
                }
            }
            continue;
        }

        if line == "(clear)" {
            bindings.clear();
            functions.clear();
            imports.clear();
            loaded_packages.clear();
            record_defs.clear();
            variant_defs.clear();
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
                                let export_names: Vec<&str> =
                                    loaded.exports.iter().map(|e| e.name.as_str()).collect();
                                println!("  exports: {}", export_names.join(", "));
                            }
                            imports.push(loaded);
                        }
                        Err(e) => println!("error loading import: {}", e),
                    }
                }
                None => println!(
                    "error: invalid import syntax. Use: (import <interface> from <source>)"
                ),
            }
            continue;
        }

        if line.starts_with("(test-actor ") {
            match test_actor_command(line).await {
                Ok(()) => {}
                Err(e) => println!("error: {}", e),
            }
            continue;
        }

        // Compile and evaluate expression
        match eval_expression(
            &compiler_wasm,
            &runtime,
            line,
            &bindings,
            &functions,
            &imports,
            &loaded_packages,
            &record_defs,
            &variant_defs,
        )
        .await
        {
            Ok(EvalResult::Scalar(n)) => println!("{}", n),
            Ok(EvalResult::Compound(v)) => println!("{}", format_value(&v)),
            Ok(EvalResult::NativeString(s)) => println!("\"{}\"", s),
            Ok(EvalResult::NativeRecord { type_name, fields }) => {
                let field_strs: Vec<String> = fields
                    .iter()
                    .map(|(name, val)| format!("{}: {}", name, val))
                    .collect();
                println!("{}{{ {} }}", type_name, field_strs.join(", "));
            }
            Ok(EvalResult::NativeVariant {
                case_name, payload, ..
            }) => match payload {
                Some(v) => println!("{}({})", case_name, v),
                None => println!("{}", case_name),
            },
            Err(e) => println!("error: {}", e),
        }
    }

    println!("\nGoodbye!");
    Ok(())
}

/// Test an actor by loading its WASM module and calling init via Pack
async fn test_actor_command(line: &str) -> Result<()> {
    // Parse path from (test-actor "path.wasm") or (test-actor path.wasm)
    let inner = line
        .strip_prefix("(test-actor ")
        .and_then(|s| s.strip_suffix(')'))
        .ok_or_else(|| anyhow::anyhow!("invalid syntax. Use: (test-actor \"path.wasm\")"))?;
    let path = inner.trim().trim_matches('"');

    println!("Loading actor from {}...", path);

    let wasm_bytes = std::fs::read(path).with_context(|| format!("Failed to read {}", path))?;

    // Load via Pack's AsyncInstance
    let runtime = AsyncRuntime::new();
    let actor_store = create_actor_store();

    let mut instance = PackInstance::new("actor", &wasm_bytes, &runtime, actor_store, |_builder| {
        Ok(())
    })
    .await?;

    // Build init input: Tuple(Option<List<U8>>(None), Tuple([]))
    let state = pack::abi::Value::Option {
        inner_type: ValueType::List(Box::new(ValueType::U8)),
        value: None,
    };
    let params = pack::abi::Value::Tuple(vec![]);
    let input = pack::abi::Value::Tuple(vec![state, params]);

    // Call the init function via Pack's Graph ABI
    let result = instance
        .call_value("theater:simple/actor.init", &input)
        .await;
    match result {
        Ok(value) => println!("init returned: {}", format_value(&value)),
        Err(e) => println!("init failed: {}", e),
    }

    Ok(())
}

/// Compile and evaluate a single expression
///
/// For Pack package imports, creates bridge functions that:
/// 1. Accept simple signature (i32 args)
/// 2. Encode to CGRF via generated wrapper
/// 3. Call Pack instance via Graph ABI bridge
/// 4. Decode result back (scalar or compound)
async fn eval_expression(
    compiler_wasm: &[u8],
    runtime: &AsyncRuntime,
    expr: &str,
    bindings: &HashMap<String, i32>,
    functions: &[String],
    imports: &[LoadedInterface],
    loaded_packages: &HashMap<PathBuf, Arc<Mutex<pack::Instance<()>>>>,
    record_defs: &HashMap<String, ReplRecordDef>,
    variant_defs: &HashMap<String, ReplVariantDef>,
) -> Result<EvalResult> {
    // Preprocess string literals in expression and function definitions.
    // (str.const "hello") -> (i32.const <addr>) with data segments.
    // Process everything in a single pass to get consistent addresses.
    let mut full_text = String::new();
    for f in functions {
        full_text.push_str(f);
        full_text.push('\n');
    }
    full_text.push_str(expr);
    let (processed_full, all_strings) = preprocess_string_literals(&full_text);

    // Split back into functions and expression
    let mut processed_functions = Vec::new();
    let mut remaining = processed_full.as_str();
    for _f in functions {
        if let Some(newline_pos) = remaining.find('\n') {
            processed_functions.push(remaining[..newline_pos].to_string());
            remaining = &remaining[newline_pos + 1..];
        }
    }
    let processed_expr = remaining.to_string();

    // Find which imported functions are used in the expression or user-defined functions
    let mut used_imports: Vec<(&LoadedInterface, &ExportedFunction)> = Vec::new();
    for imp in imports {
        for export in &imp.exports {
            // Check if this function name appears in the expression
            // Simple heuristic: look for (funcname or funcname)
            let mut is_used = processed_expr.contains(&format!("({}", export.name))
                || processed_expr.contains(&format!(" {}", export.name));

            // Also check if any user-defined function uses this import
            if !is_used {
                for func_def in &processed_functions {
                    if func_def.contains(&format!("({}", export.name))
                        || func_def.contains(&format!(" {}", export.name))
                    {
                        is_used = true;
                        break;
                    }
                }
            }

            if is_used {
                used_imports.push((imp, export));
            }
        }
    }

    // Check if any used import has Pack component source (needs wrapper infrastructure)
    let has_pack_imports = used_imports
        .iter()
        .any(|(imp, _)| matches!(imp.source, ImportSource::Component(_)));

    // Infer the return type of the expression
    let return_type = infer_return_type(&processed_expr, record_defs, variant_defs, &used_imports);
    let needs_memory_export = matches!(
        return_type,
        ReplReturnType::NativeString
            | ReplReturnType::NativeRecord(_)
            | ReplReturnType::NativeVariant(_)
    );

    // Generate source with all functions and an eval wrapper
    let mut source = String::new();

    // Add type definitions so the self-hosted compiler can build its context
    for (_, vdef) in variant_defs {
        source.push_str(&vdef.original_source);
        source.push('\n');
    }
    for (_, rdef) in record_defs {
        source.push_str(&rdef.original_source);
        source.push('\n');
    }

    // For host imports: generate native (import ...) declarations that the self-hosted
    // compiler handles directly. For Pack component imports: generate stub functions
    // that will be replaced with Graph ABI imports during WAT post-processing.
    for (imp, func) in &used_imports {
        // Generate parameter list with proper types
        let params: Vec<String> = func
            .sig
            .params
            .iter()
            .enumerate()
            .map(|(i, ty)| format!("(p{} {})", i, ty.to_wisp()))
            .collect();
        let params_str = params.join(" ");

        // Generate return type (default to s32 if no results)
        let return_type = func
            .sig
            .results
            .first()
            .map(|t| t.to_wisp())
            .unwrap_or("s32");

        match &imp.source {
            ImportSource::Host => {
                // Native import declaration — the self-hosted compiler emits WAT import directly
                source.push_str(&format!(
                    "(import {} {} ({}) {})\n",
                    imp.interface, func.name, params_str, return_type
                ));
            }
            ImportSource::Component(_) => {
                // Stub function for Pack imports — will be replaced during WAT post-processing
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
        }
    }

    // Add all function definitions (preprocessed for string literals)
    for func in &processed_functions {
        source.push_str(func);
        source.push('\n');
    }

    // Inline bindings into the expression
    let mut inlined_expr = processed_expr.clone();
    for (name, value) in bindings {
        // Simple string replacement (not perfect but works for basic cases)
        inlined_expr =
            inlined_expr.replace(&format!(" {} ", name), &format!(" (i32.const {}) ", value));
        inlined_expr =
            inlined_expr.replace(&format!(" {})", name), &format!(" (i32.const {}))", value));
        inlined_expr =
            inlined_expr.replace(&format!("({} ", name), &format!("((i32.const {}) ", value));
    }

    // Wrap expression in eval function
    source.push_str(&format!("(export (fn eval () s32 {}))", inlined_expr));

    // Compile using self-hosted compiler
    let actor_store = create_actor_store();
    let mut compiler =
        PackInstance::new("compiler", compiler_wasm, runtime, actor_store, |builder| {
            builder
                .interface("theater:simple/runtime")?
                .func_typed("log", |_ctx: &mut Ctx<'_, ActorStore>, _input: Value| {
                    Value::Tuple(vec![])
                })?;
            Ok(())
        })
        .await?;

    let result = compiler
        .call_value("compile-source", &Value::String(source.clone()))
        .await?;

    let wat = match result {
        Value::String(s) => s,
        other => anyhow::bail!("Expected WAT string, got {:?}", other),
    };

    // Post-process WAT for Pack component imports and data segments.
    // Host imports are now handled natively by the self-hosted compiler (no post-processing needed).
    // Pack component imports still need: stub removal + Graph ABI raw import + CGRF wrappers.
    let has_pack_stubs = used_imports
        .iter()
        .any(|(imp, _)| matches!(imp.source, ImportSource::Component(_)));
    let wat = if has_pack_stubs || !all_strings.is_empty() || needs_memory_export {
        // Collect stub names only for Pack component imports (host imports have no stubs)
        let stub_names: Vec<&str> = used_imports
            .iter()
            .filter(|(imp, _)| matches!(imp.source, ImportSource::Component(_)))
            .map(|(_, f)| f.name.as_str())
            .collect();

        // Filter out stub function definitions and error lines
        let mut in_stub_func = false;
        let mut paren_depth = 0;

        let lines: Vec<&str> = wat
            .lines()
            .filter(|line| {
                // Remove error lines
                if line.contains("(error:") {
                    return false;
                }

                // Check if this starts a stub function (only Pack stubs)
                for name in &stub_names {
                    if line.contains(&format!("(func ${} ", name)) && !line.contains("(call") {
                        in_stub_func = true;
                        paren_depth = line.chars().filter(|c| *c == '(').count() as i32
                            - line.chars().filter(|c| *c == ')').count() as i32;
                        return false;
                    }
                }

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

        // Generate Graph ABI imports and CGRF wrapper functions (Pack component imports only)
        let mut import_wat = String::new();
        let mut wrapper_wat = String::new();

        for (imp, func) in &used_imports {
            if let ImportSource::Component(_) = &imp.source {
                // Pack packages use Graph ABI: generate raw import + wrapper
                import_wat.push_str(&format!(
                    "  (import \"{}\" \"{}\" (func $__raw_{} (param i32 i32 i32 i32) (result i32)))\n",
                    imp.interface, func.name, func.name
                ));
                wrapper_wat.push_str(&generate_cgrf_wrapper(func));
            }
            // Host imports: already in WAT from the self-hosted compiler, no post-processing needed
        }

        // Generate data segments for string literals
        let mut data_segments = String::new();
        for (addr, string_val) in &all_strings {
            let encoded = encode_string_data_segment(string_val);
            data_segments.push_str(&format!("  (data (i32.const {}) \"{}\")\n", addr, encoded));
        }

        // Build result, inserting Pack imports/wrappers and data segments
        let mut result = String::new();
        let mut _memory_exported = false;

        for line in &lines {
            if line.trim() == ")" && !wrapper_wat.is_empty() {
                // Insert __pack_alloc for guest-allocates ABI
                result
                    .push_str("  (func (export \"__pack_alloc\") (param $size i32) (result i32)\n");
                result.push_str("    (local $ptr i32)\n");
                result.push_str("    global.get $__heap_ptr\n");
                result.push_str("    local.set $ptr\n");
                result.push_str("    global.get $__heap_ptr\n");
                result.push_str("    local.get $size\n");
                result.push_str("    i32.add\n");
                result.push_str("    global.set $__heap_ptr\n");
                result.push_str("    local.get $ptr\n");
                result.push_str("  )\n");
                // Insert wrappers before final closing paren
                result.push_str(&wrapper_wat);
            }

            // Ensure memory is exported when needed (Pack imports or native compound results)
            if (!wrapper_wat.is_empty() || needs_memory_export)
                && line.contains("(memory")
                && !line.contains("export")
            {
                let exported_line = line.replace("(memory", "(memory (export \"memory\")");
                result.push_str(&exported_line);
                result.push('\n');
                _memory_exported = true;
                continue;
            }

            result.push_str(line);
            result.push('\n');

            if line.trim().starts_with("(module") {
                // Inject Pack Graph ABI imports after (module
                if !import_wat.is_empty() {
                    result.push_str(&import_wat);
                }
                if !wrapper_wat.is_empty() && !lines.iter().any(|l| l.contains("(memory")) {
                    result.push_str("  (memory (export \"memory\") 1)\n");
                    _memory_exported = true;
                }
                if !wrapper_wat.is_empty() {
                    result.push_str("  (global $__result_ptr (export \"__result_ptr\") (mut i32) (i32.const 0))\n");
                    result.push_str("  (global $__result_len (export \"__result_len\") (mut i32) (i32.const 0))\n");
                }
                if !data_segments.is_empty() {
                    result.push_str(&data_segments);
                }
            }
        }
        result
    } else if needs_memory_export || !all_strings.is_empty() {
        // No Pack imports, but we need memory export for native compound types
        // or data segments for string literals
        let mut result = String::new();
        for line in wat.lines() {
            if needs_memory_export && line.contains("(memory") && !line.contains("export") {
                let exported_line = line.replace("(memory", "(memory (export \"memory\")");
                result.push_str(&exported_line);
                result.push('\n');
                continue;
            }
            result.push_str(line);
            result.push('\n');
            if line.trim().starts_with("(module") {
                // Add data segments for string literals
                for (addr, string_val) in &all_strings {
                    let encoded = encode_string_data_segment(string_val);
                    result.push_str(&format!("  (data (i32.const {}) \"{}\")\n", addr, encoded));
                }
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
    let wasm_bytes =
        wat::parse_str(&wat).with_context(|| format!("Failed to assemble WAT:\n{}", wat))?;

    // Load and run
    let mut config = wasmtime::Config::new();
    config.wasm_tail_call(true);
    let engine = Engine::new(&config)?;
    let module = Module::new(&engine, &wasm_bytes)
        .with_context(|| format!("Failed to compile WASM from WAT:\n{}", wat))?;
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
                let pack_instance = loaded_packages
                    .get(path)
                    .with_context(|| format!("Pack package not loaded: {}", path.display()))?
                    .clone();

                // Create a Graph ABI bridge function (guest-allocates ABI):
                // 1. Accepts signature (in_ptr, in_len, out_ptr_ptr, out_len_ptr) -> status
                // 2. Reads CGRF from expression's memory
                // 3. Decodes to pack::Value
                // 4. Calls Pack instance via call_with_value
                // 5. Encodes result back to CGRF
                // 6. Allocates output buffer in expression's memory via __pack_alloc
                // 7. Writes result to allocated buffer, ptr/len to slots
                // 8. Returns 0 on success

                let func_name = exported_func.name.clone();

                let func = wasmtime::Func::new(
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

                        // Get memory from the expression module
                        let memory = caller
                            .get_export("memory")
                            .and_then(|e| e.into_memory())
                            .ok_or_else(|| wasmtime::Error::msg("no memory export"))?;

                        // Read CGRF input from expression's memory
                        let mut in_buf = vec![0u8; in_len];
                        memory.read(&caller, in_ptr, &mut in_buf).map_err(|e| {
                            wasmtime::Error::msg(format!("failed to read input: {}", e))
                        })?;

                        // Decode CGRF to pack::Value
                        let input = pack::decode(&in_buf).map_err(|e| {
                            wasmtime::Error::msg(format!("failed to decode CGRF: {}", e))
                        })?;

                        // Call Pack instance via call_with_value
                        let mut instance = pack_instance.lock().unwrap();
                        let output = instance.call_with_value(&func_name, &input).map_err(|e| {
                            wasmtime::Error::msg(format!("Pack call failed: {}", e))
                        })?;

                        // Encode result back to CGRF
                        let out_buf = pack::encode(&output).map_err(|e| {
                            wasmtime::Error::msg(format!("failed to encode result: {}", e))
                        })?;

                        // Guest-allocates ABI: allocate output buffer in expression's memory
                        let pack_alloc = caller
                            .get_export("__pack_alloc")
                            .and_then(|e| e.into_func())
                            .ok_or_else(|| wasmtime::Error::msg("no __pack_alloc export"))?;

                        let mut alloc_result = [wasmtime::Val::I32(0)];
                        pack_alloc
                            .call(
                                &mut caller,
                                &[wasmtime::Val::I32(out_buf.len() as i32)],
                                &mut alloc_result,
                            )
                            .map_err(|e| {
                                wasmtime::Error::msg(format!("__pack_alloc failed: {}", e))
                            })?;
                        let out_ptr = alloc_result[0].unwrap_i32() as usize;

                        // Write result to allocated buffer
                        memory.write(&mut caller, out_ptr, &out_buf).map_err(|e| {
                            wasmtime::Error::msg(format!("failed to write output: {}", e))
                        })?;

                        // Write ptr and len to the slots
                        memory
                            .write(&mut caller, out_ptr_ptr, &(out_ptr as i32).to_le_bytes())
                            .map_err(|e| {
                                wasmtime::Error::msg(format!("failed to write out_ptr: {}", e))
                            })?;
                        memory
                            .write(
                                &mut caller,
                                out_len_ptr,
                                &(out_buf.len() as i32).to_le_bytes(),
                            )
                            .map_err(|e| {
                                wasmtime::Error::msg(format!("failed to write out_len: {}", e))
                            })?;

                        // Return 0 for success
                        results[0] = wasmtime::Val::I32(0);
                        Ok(())
                    },
                );

                extern_imports.push(func.into());
            }
        }
    }

    let instance = Instance::new(&mut store, &module, &extern_imports)?;

    let eval_func = instance
        .get_func(&mut store, "eval")
        .context("eval function not found")?;

    let mut results = vec![wasmtime::Val::I32(0)];
    eval_func.call(&mut store, &[], &mut results)?;

    // Determine result type based on inference
    match return_type {
        ReplReturnType::NativeString => {
            let ptr = match results.first() {
                Some(wasmtime::Val::I32(n)) => *n,
                _ => anyhow::bail!("Expected i32 pointer for string result"),
            };
            let memory = instance
                .get_memory(&mut store, "memory")
                .context("memory not found for string result")?;
            let s = read_string_from_memory(&memory, &store, ptr)?;
            Ok(EvalResult::NativeString(s))
        }
        ReplReturnType::NativeRecord(ref type_name) => {
            let ptr = match results.first() {
                Some(wasmtime::Val::I32(n)) => *n,
                _ => anyhow::bail!("Expected i32 pointer for record result"),
            };
            let rec_def = record_defs
                .get(type_name)
                .with_context(|| format!("Record type '{}' not found", type_name))?;
            let memory = instance
                .get_memory(&mut store, "memory")
                .context("memory not found for record result")?;
            let fields = read_record_from_memory(&memory, &store, ptr, rec_def)?;
            Ok(EvalResult::NativeRecord {
                type_name: type_name.clone(),
                fields,
            })
        }
        ReplReturnType::NativeVariant(ref type_name) => {
            let ptr = match results.first() {
                Some(wasmtime::Val::I32(n)) => *n,
                _ => anyhow::bail!("Expected i32 pointer for variant result"),
            };
            let var_def = variant_defs
                .get(type_name)
                .with_context(|| format!("Variant type '{}' not found", type_name))?;
            let memory = instance
                .get_memory(&mut store, "memory")
                .context("memory not found for variant result")?;
            let (case_name, payload) = read_variant_from_memory(&memory, &store, ptr, var_def)?;
            Ok(EvalResult::NativeVariant {
                type_name: type_name.clone(),
                case_name,
                payload,
            })
        }
        ReplReturnType::PackCompound => {
            if has_pack_imports {
                // Read compound result from CGRF via globals
                let result_ptr_global = instance
                    .get_global(&mut store, "__result_ptr")
                    .context("__result_ptr global not found")?;
                let result_len_global = instance
                    .get_global(&mut store, "__result_len")
                    .context("__result_len global not found")?;

                let result_ptr = result_ptr_global.get(&mut store).unwrap_i32() as usize;
                let result_len = result_len_global.get(&mut store).unwrap_i32() as usize;

                if result_ptr > 0 && result_len > 0 {
                    let memory = instance
                        .get_memory(&mut store, "memory")
                        .context("memory not found for compound result")?;

                    let mut cgrf_buf = vec![0u8; result_len];
                    memory
                        .read(&store, result_ptr, &mut cgrf_buf)
                        .context("failed to read compound result CGRF")?;

                    let value =
                        pack::decode(&cgrf_buf).context("failed to decode compound result CGRF")?;

                    return Ok(EvalResult::Compound(value));
                }
            }
            // Fall through to scalar
            match results.into_iter().next() {
                Some(wasmtime::Val::I32(n)) => Ok(EvalResult::Scalar(n)),
                other => anyhow::bail!("Expected i32 result, got {:?}", other),
            }
        }
        ReplReturnType::Scalar => match results.into_iter().next() {
            Some(wasmtime::Val::I32(n)) => Ok(EvalResult::Scalar(n)),
            other => anyhow::bail!("Expected i32 result, got {:?}", other),
        },
    }
}
