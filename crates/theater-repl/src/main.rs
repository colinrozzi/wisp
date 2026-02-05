//! Theater REPL - Interactive Wisp shell powered by Theater actors
//!
//! This runtime provides an interactive REPL where:
//! - The REPL actor handles expression evaluation
//! - The self-hosted compiler is statically composed into the actor (pack-compose)
//! - Host functions provide assembler, logging, and supervisor capabilities
//! - Expression actors can be spawned to evaluate code
//!
//! Architecture (Static Composition):
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │              Static Composition (pack-compose)              │
//! │                                                             │
//! │  repl-actor.wasm + wisp-compiler.wasm                       │
//! │              ↓ StaticComposer                               │
//! │  composed-repl.wasm (single WASM with compiler embedded)    │
//! │                                                             │
//! │  Internal call: compile-source → compiler code (no import!) │
//! │  External imports: log, wat-to-wasm, spawn-and-wait (host)  │
//! └─────────────────────────────────────────────────────────────┘
//! ```
//!
//! Usage:
//!   theater-repl                    - Start interactive REPL
//!   theater-repl --actor <path>     - Use custom REPL actor WASM
//!   theater-repl --compose          - Use spawn-based REPL actor with compiler
//!   theater-repl --static           - Use static composition (pack-compose)
//!   theater-repl --theater          - Use real Theater runtime (NEW!)

mod compose;

use std::io::{self, BufRead, Write};
use std::path::PathBuf;

use anyhow::{Context, Result};
use pack::abi::{decode, encode, Value};
use pack::compose::StaticComposer;
use pack::runtime::CompositionBuilder;
use tracing::{error, info};
use wasmtime::{Engine, Instance, Module, Store};

// Theater integration
use theater::config::actor_manifest::{RuntimeHostConfig, StoreHandlerConfig, SupervisorHostConfig};
use theater::handler::HandlerRegistry;
use theater::messages::{ActorMessage, ActorRequest, MessageCommand, TheaterCommand};
use theater::theater_runtime::TheaterRuntime;
use theater_handler_message_server::{MessageRouter, MessageServerHandler};
use theater_handler_runtime::RuntimeHandler;
use theater_handler_store::StoreHandler;
use theater_handler_supervisor::SupervisorHandler;
use theater_handler_wisp::WispHandler;
use tokio::sync::{mpsc, oneshot};

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt().with_env_filter("info").init();

    let args: Vec<String> = std::env::args().collect();

    // Parse arguments
    let mut actor_path = PathBuf::from("examples/actors/repl-actor.wasm");
    let mut use_compose = false;
    let mut use_static = false;
    let mut use_theater = false;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--actor" if i + 1 < args.len() => {
                actor_path = PathBuf::from(&args[i + 1]);
                i += 2;
            }
            "--compose" => {
                use_compose = true;
                actor_path = PathBuf::from("examples/actors/spawn-repl-actor.wasm");
                i += 1;
            }
            "--static" => {
                use_static = true;
                actor_path = PathBuf::from("examples/actors/spawn-repl-actor.wasm");
                i += 1;
            }
            "--theater" => {
                use_theater = true;
                actor_path = PathBuf::from("examples/actors/spawn-repl-actor.wasm");
                i += 1;
            }
            _ => i += 1,
        }
    }

    if use_theater {
        run_theater_repl(&actor_path).await
    } else if use_static {
        run_static_composed_repl(&actor_path).await
    } else if use_compose {
        run_composed_repl(&actor_path).await
    } else {
        println!("Simple mode not implemented in this version.");
        println!("Use --compose flag for runtime Pack composition mode.");
        println!("Use --static flag for static pack-compose mode.");
        println!("Use --theater flag for real Theater runtime mode.");
        Ok(())
    }
}

/// Run the REPL with real Theater runtime
async fn run_theater_repl(actor_path: &PathBuf) -> Result<()> {
    println!("Theater REPL (Real Theater Runtime)");
    println!("Type 'quit' to exit\n");

    // Load the REPL actor WASM
    let actor_wasm = std::fs::read(actor_path)
        .with_context(|| format!("Failed to load REPL actor from {:?}", actor_path))?;
    info!("Loaded REPL actor: {} bytes", actor_wasm.len());

    // Load the compiler WASM
    let compiler_path = PathBuf::from("examples/wisp-compiler.wasm");
    let compiler_wasm = std::fs::read(&compiler_path)
        .with_context(|| format!("Failed to load compiler from {:?}", compiler_path))?;
    info!("Loaded compiler: {} bytes", compiler_wasm.len());

    // Use StaticComposer to merge them into a single WASM
    info!("Composing WASMs with StaticComposer...");
    let composed_wasm = StaticComposer::new()
        .add_module("compiler", compiler_wasm)
        .context("Failed to add compiler module")?
        .add_module("repl", actor_wasm)
        .context("Failed to add repl module")?
        // Wire repl's import of compile-source to compiler's export
        .wire(
            "repl",
            "wisp:compiler/compiler",
            "compile-source",
            "compiler",
            "compile-source",
        )
        // Export all the actor interfaces from repl
        .export("theater:simple/actor.init", "repl", "theater:simple/actor.init")
        .export(
            "theater:simple/message-server-client.handle-send",
            "repl",
            "theater:simple/message-server-client.handle-send",
        )
        .export(
            "theater:simple/message-server-client.handle-request",
            "repl",
            "theater:simple/message-server-client.handle-request",
        )
        // Export memory from repl
        .export("memory", "repl", "memory")
        .compose()
        .context("Failed to compose WASMs")?;

    info!("Static composition complete: {} bytes", composed_wasm.len());

    // Create the Theater runtime with handlers
    let (theater_tx, theater_rx) = mpsc::channel(100);
    let message_router = MessageRouter::new();
    let handler_registry = create_handler_registry(theater_tx.clone(), message_router.clone());

    let mut runtime = TheaterRuntime::new(theater_tx.clone(), theater_rx, None, handler_registry)
        .await
        .context("Failed to create Theater runtime")?;

    // Start the runtime in a background task
    let runtime_handle = tokio::spawn(async move {
        if let Err(e) = runtime.run().await {
            error!("Theater runtime error: {}", e);
        }
    });

    // Give runtime a moment to start
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

    // Create inline manifest for the composed REPL actor
    // Note: "wisp" handler is not a known type in Theater's manifest parser,
    // but it's registered in the handler registry and will match imports.
    let manifest = r#"
name = "composed-repl"
version = "0.1.0"
package = "inline"
description = "Composed REPL actor with embedded compiler"

[[handler]]
type = "runtime"

[[handler]]
type = "message-server"

[[handler]]
type = "supervisor"

[[handler]]
type = "store"
"#;

    // Spawn the composed REPL actor with inline WASM bytes
    info!("Spawning composed REPL actor...");
    let (response_tx, response_rx) = oneshot::channel();

    theater_tx
        .send(TheaterCommand::SpawnActor {
            manifest_path: manifest.to_string(),
            wasm_bytes: Some(composed_wasm),
            init_bytes: None,
            response_tx,
            parent_id: None,
            supervisor_tx: None,
            subscription_tx: None,
        })
        .await
        .context("Failed to send spawn command")?;

    // Wait for spawn response
    let actor_id = match response_rx.await {
        Ok(Ok(id)) => {
            info!("REPL actor spawned successfully: {}", id);
            id
        }
        Ok(Err(e)) => {
            error!("Failed to spawn actor: {}", e);
            return Err(anyhow::anyhow!("Spawn failed: {}", e));
        }
        Err(_) => {
            error!("Spawn channel closed");
            return Err(anyhow::anyhow!("Spawn channel closed"));
        }
    };

    // HACK: Wait for actor to fully initialize and register with MessageRouter.
    // Theater's SpawnActor returns success when the actor task is spawned, but
    // before handlers (including MessageServerHandler) are started. This means
    // the actor isn't registered with the MessageRouter yet.
    //
    // TODO(theater): SpawnActor should wait until actor is ready to receive messages.
    // See: https://github.com/anthropics/theater/issues/XXX (file issue)
    tokio::time::sleep(tokio::time::Duration::from_millis(1000)).await;

    // REPL loop - send requests via MessageRouter
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        print!("theater> ");
        stdout.flush()?;

        let mut line = String::new();
        if stdin.lock().read_line(&mut line)? == 0 {
            break;
        }
        let line = line.trim();

        if line.is_empty() {
            continue;
        }
        if line == "quit" || line == "exit" {
            break;
        }

        // Send request to the actor via MessageRouter
        let request_data = line.as_bytes().to_vec();
        let (response_tx, response_rx) = oneshot::channel();
        let (cmd_response_tx, cmd_response_rx) = oneshot::channel();

        let command = MessageCommand::SendMessage {
            target_id: actor_id.clone(),
            message: ActorMessage::Request(ActorRequest {
                data: request_data,
                response_tx,
            }),
            response_tx: cmd_response_tx,
        };

        if let Err(e) = message_router.route_message(command).await {
            println!("error: Failed to route message: {}", e);
            continue;
        }

        // Wait for routing confirmation
        match cmd_response_rx.await {
            Ok(Ok(())) => {}
            Ok(Err(e)) => {
                println!("error: {}", e);
                continue;
            }
            Err(e) => {
                println!("error: {}", e);
                continue;
            }
        }

        // Wait for the actual response from the actor
        match response_rx.await {
            Ok(response_bytes) => {
                // Response is a 4-byte little-endian i32
                if response_bytes.len() == 4 {
                    let value = i32::from_le_bytes([
                        response_bytes[0],
                        response_bytes[1],
                        response_bytes[2],
                        response_bytes[3],
                    ]);
                    println!("{}", value);
                } else {
                    // For non-i32 responses, try as string
                    let response = String::from_utf8_lossy(&response_bytes);
                    println!("{}", response);
                }
            }
            Err(e) => {
                println!("error: No response from actor: {}", e);
            }
        }
    }

    println!("\nShutting down...");
    drop(theater_tx);
    let _ = runtime_handle.await;

    println!("Goodbye!");
    Ok(())
}

/// Create handler registry for Theater runtime
fn create_handler_registry(
    theater_tx: mpsc::Sender<TheaterCommand>,
    message_router: MessageRouter,
) -> HandlerRegistry {
    let mut registry = HandlerRegistry::new();

    info!("Registering handlers...");

    // Runtime handler - provides actor runtime information and control
    let runtime_config = RuntimeHostConfig {};
    registry.register(RuntimeHandler::new(runtime_config, theater_tx.clone(), None));

    // Store handler - provides key-value storage for actors
    let store_config = StoreHandlerConfig {};
    registry.register(StoreHandler::new(store_config, None));

    // Supervisor handler - allows actors to spawn and manage child actors
    let supervisor_config = SupervisorHostConfig {};
    registry.register(SupervisorHandler::new(supervisor_config, None));

    // Message server handler - provides inter-actor messaging
    registry.register(MessageServerHandler::new(None, message_router));

    // Wisp handler - provides wisp:assembler and wisp:repl host functions
    registry.register(WispHandler::new());

    info!("5 handlers registered: runtime, store, supervisor, message-server, wisp");

    registry
}

/// Run the REPL with static pack-compose composition
async fn run_static_composed_repl(actor_path: &PathBuf) -> Result<()> {
    println!("Theater REPL (Static composition via pack-compose)");
    println!("Type 'quit' to exit\n");

    // Load the REPL actor WASM
    let actor_wasm = std::fs::read(actor_path)
        .with_context(|| format!("Failed to load REPL actor from {:?}", actor_path))?;
    info!("Loaded REPL actor: {} bytes", actor_wasm.len());

    // Load the compiler WASM
    let compiler_path = PathBuf::from("examples/wisp-compiler.wasm");
    let compiler_wasm = std::fs::read(&compiler_path)
        .with_context(|| format!("Failed to load compiler from {:?}", compiler_path))?;
    info!("Loaded compiler: {} bytes", compiler_wasm.len());

    // Use StaticComposer to merge them into a single WASM
    info!("Composing WASMs with StaticComposer...");
    let composed_wasm = StaticComposer::new()
        .add_module("compiler", compiler_wasm)
        .context("Failed to add compiler module")?
        .add_module("repl", actor_wasm)
        .context("Failed to add repl module")?
        // Wire repl's import of compile-source to compiler's export
        .wire(
            "repl",
            "wisp:compiler/compiler",
            "compile-source",
            "compiler",
            "compile-source",
        )
        // Export all the actor interfaces from repl
        .export("theater:simple/actor.init", "repl", "theater:simple/actor.init")
        .export(
            "theater:simple/message-server-client.handle-send",
            "repl",
            "theater:simple/message-server-client.handle-send",
        )
        .export(
            "theater:simple/message-server-client.handle-request",
            "repl",
            "theater:simple/message-server-client.handle-request",
        )
        // Export memory from repl
        .export("memory", "repl", "memory")
        .compose()
        .context("Failed to compose WASMs")?;

    info!(
        "Static composition complete: {} bytes (was {} + {} = {} bytes)",
        composed_wasm.len(),
        actor_path.metadata().map(|m| m.len()).unwrap_or(0),
        compiler_path.metadata().map(|m| m.len()).unwrap_or(0),
        actor_path.metadata().map(|m| m.len()).unwrap_or(0)
            + compiler_path.metadata().map(|m| m.len()).unwrap_or(0)
    );

    // Save composed WASM for debugging
    let composed_path = PathBuf::from("examples/actors/composed-repl.wasm");
    std::fs::write(&composed_path, &composed_wasm)
        .with_context(|| format!("Failed to write composed WASM to {:?}", composed_path))?;
    info!("Saved composed WASM to {:?}", composed_path);

    // Validate the composed WASM with wasmparser
    info!("Validating composed WASM...");
    let features = wasmparser::WasmFeatures::all();
    let mut validator = wasmparser::Validator::new_with_features(features);
    match validator.validate_all(&composed_wasm) {
        Ok(_) => info!("Composed WASM is valid!"),
        Err(e) => {
            eprintln!("WASM validation error: {}", e);
            eprintln!("Offset: {:?}", e.offset());
            return Err(anyhow::anyhow!("Composed WASM is invalid: {}", e));
        }
    }

    // Now use the composed WASM with a simple PackInstance + host functions
    // For now, use CompositionBuilder with just the composed WASM (no wiring needed!)
    let mut composition = CompositionBuilder::new()
        .add_package("actor", composed_wasm)
        // Add host functions (these are still imports in the composed WASM)
        .add_host_function_typed("theater:simple/runtime", "log", |input| {
            let msg = match input {
                Value::String(s) => s,
                _ => format!("{:?}", input),
            };
            info!("[ACTOR] {}", msg);
            Ok(Value::Tuple(vec![]))
        })
        .add_host_function("wisp:repl/helpers", "wrap-expression", |input_bytes| {
            let input = decode(input_bytes).map_err(|e| e.to_string())?;

            let body_bytes: Vec<u8> = match &input {
                Value::Tuple(items) if items.len() >= 2 => match &items[1] {
                    Value::List { items, .. } => items
                        .iter()
                        .filter_map(|v| if let Value::U8(b) = v { Some(*b) } else { None })
                        .collect(),
                    _ => return Err("expected list<u8> as second tuple element".to_string()),
                },
                _ => return Err("expected tuple with request-id and body".to_string()),
            };

            let expr = String::from_utf8_lossy(&body_bytes).to_string();
            info!("[WRAP] Expression: {}", expr);

            let source = format!(r#"(export (fn eval () s32 {}))"#, expr);
            info!("[WRAP] Wrapped source: {}", source);

            encode(&Value::String(source)).map_err(|e| e.to_string())
        })
        // wat-to-wasm: Assemble WAT to WASM bytes
        .add_host_function("wisp:assembler/runtime", "wat-to-wasm", |input_bytes| {
            let input = decode(input_bytes).map_err(|e| e.to_string())?;
            let wat = match input {
                Value::String(s) => s,
                _ => return Err("expected string argument".to_string()),
            };

            info!("[ASSEMBLER] Converting {} bytes of WAT to WASM", wat.len());

            match wat::parse_str(&wat) {
                Ok(wasm_bytes) => {
                    info!("[ASSEMBLER] Success: {} bytes of WASM", wasm_bytes.len());
                    let bytes: Vec<Value> = wasm_bytes.into_iter().map(Value::U8).collect();
                    // Return option<list<u8>> - Some for success
                    let result = Value::Option {
                        inner_type: pack::abi::ValueType::List(Box::new(pack::abi::ValueType::U8)),
                        value: Some(Box::new(Value::List {
                            elem_type: pack::abi::ValueType::U8,
                            items: bytes,
                        })),
                    };
                    encode(&result).map_err(|e| e.to_string())
                }
                Err(e) => {
                    info!("[ASSEMBLER] Error: {}", e);
                    // Return option<list<u8>> - None for error
                    let result = Value::Option {
                        inner_type: pack::abi::ValueType::List(Box::new(pack::abi::ValueType::U8)),
                        value: None,
                    };
                    encode(&result).map_err(|e| e.to_string())
                }
            }
        })
        // spawn-and-wait: Spawn expression actor and wait for result
        .add_host_function("theater:simple/supervisor", "spawn-and-wait", |input_bytes| {
            info!("[SPAWN-AND-WAIT] Called with {} input bytes", input_bytes.len());
            let input = decode(input_bytes).map_err(|e| {
                info!("[SPAWN-AND-WAIT] Decode error: {}", e);
                e.to_string()
            })?;

            // Input is tuple(string, list<u8>) - (tag, wasm-bytes)
            let wasm_bytes: Vec<u8> = match &input {
                Value::Tuple(items) if items.len() >= 2 => match &items[1] {
                    Value::List { items, .. } => items
                        .iter()
                        .filter_map(|v| if let Value::U8(b) = v { Some(*b) } else { None })
                        .collect(),
                    _ => return Err("expected list<u8> as second tuple element".to_string()),
                },
                _ => return Err("expected tuple(string, list<u8>)".to_string()),
            };

            info!("[SPAWN-AND-WAIT] Spawning expression actor with {} bytes of WASM", wasm_bytes.len());

            // Run the WASM and get the result
            match run_wasm_eval(&wasm_bytes) {
                Ok(value) => {
                    info!("[SPAWN-AND-WAIT] eval() returned: {}", value);
                    let result_str = format!("{}", value);
                    let result_bytes: Vec<Value> = result_str.bytes().map(Value::U8).collect();

                    // Return option<list<u8>> - Some with result
                    let result = Value::Option {
                        inner_type: pack::abi::ValueType::List(Box::new(pack::abi::ValueType::U8)),
                        value: Some(Box::new(Value::List {
                            elem_type: pack::abi::ValueType::U8,
                            items: result_bytes,
                        })),
                    };
                    encode(&result).map_err(|e| e.to_string())
                }
                Err(e) => {
                    info!("[SPAWN-AND-WAIT] Execution error: {}", e);
                    // Return option<list<u8>> - None for error
                    let result = Value::Option {
                        inner_type: pack::abi::ValueType::List(Box::new(pack::abi::ValueType::U8)),
                        value: None,
                    };
                    encode(&result).map_err(|e| e.to_string())
                }
            }
        })
        .build()
        .context("Failed to build composition")?;

    info!("Composed actor ready");

    // Initialize the actor
    let init_input = Value::Option {
        inner_type: pack::abi::ValueType::List(Box::new(pack::abi::ValueType::U8)),
        value: None,
    };

    let init_result = composition
        .call("actor", "theater:simple/actor.init", &init_input)
        .context("Failed to initialize actor")?;
    info!("Actor init result: {:?}", init_result);

    // REPL loop
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        print!("theater> ");
        stdout.flush()?;

        let mut line = String::new();
        if stdin.lock().read_line(&mut line)? == 0 {
            break;
        }
        let line = line.trim();

        if line.is_empty() {
            continue;
        }
        if line == "quit" || line == "exit" {
            break;
        }

        let request_id = format!("req-{}", rand_id());
        let body_bytes: Vec<Value> = line.bytes().map(Value::U8).collect();

        let params = Value::Tuple(vec![
            Value::String(request_id),
            Value::List {
                elem_type: pack::abi::ValueType::U8,
                items: body_bytes,
            },
        ]);

        let state = Value::Option {
            inner_type: pack::abi::ValueType::List(Box::new(pack::abi::ValueType::U8)),
            value: None,
        };

        let input = Value::Tuple(vec![state, params]);

        match composition.call(
            "actor",
            "theater:simple/message-server-client.handle-request",
            &input,
        ) {
            Ok(result) => match extract_response(&result) {
                Some(response_bytes) => {
                    let response = String::from_utf8_lossy(&response_bytes);
                    println!("{}", response);
                }
                None => {
                    println!("(no response)");
                }
            },
            Err(e) => {
                println!("error: {}", e);
            }
        }
    }

    println!("\nGoodbye!");
    Ok(())
}

/// Run the REPL with runtime Pack composition (legacy mode)
async fn run_composed_repl(actor_path: &PathBuf) -> Result<()> {
    println!("Theater REPL (Pack composition: actor + compiler)");
    println!("Type 'quit' to exit\n");

    // Load the REPL actor WASM
    let actor_wasm = std::fs::read(actor_path)
        .with_context(|| format!("Failed to load REPL actor from {:?}", actor_path))?;
    info!("Loaded REPL actor: {} bytes", actor_wasm.len());

    // Load the compiler WASM
    let compiler_path = PathBuf::from("examples/wisp-compiler.wasm");
    let compiler_wasm = std::fs::read(&compiler_path)
        .with_context(|| format!("Failed to load compiler from {:?}", compiler_path))?;
    info!("Loaded compiler: {} bytes", compiler_wasm.len());

    // Build the composition with host functions
    let mut composition = CompositionBuilder::new()
        // Add the compiler package (provider - no imports)
        .add_package("compiler", compiler_wasm)
        // Add the REPL actor package (consumer - has imports)
        .add_package("repl", actor_wasm)
        // Wire compiler export to repl import
        .wire(
            "repl",
            "wisp:compiler/compiler",
            "compile-source",
            "compiler",
            "compile-source",
        )
        // Add host functions
        .add_host_function_typed("theater:simple/runtime", "log", |input| {
            let msg = match input {
                Value::String(s) => s,
                _ => format!("{:?}", input),
            };
            info!("[ACTOR] {}", msg);
            Ok(Value::Tuple(vec![]))
        })
        .add_host_function("wisp:assembler/runtime", "wat-to-wasm", |input_bytes| {
            let input = decode(input_bytes).map_err(|e| e.to_string())?;
            let wat = match input {
                Value::String(s) => s,
                _ => return Err("expected string argument".to_string()),
            };

            info!("[ASSEMBLER] Converting {} bytes of WAT to WASM", wat.len());

            match wat::parse_str(&wat) {
                Ok(wasm_bytes) => {
                    info!("[ASSEMBLER] Success: {} bytes of WASM", wasm_bytes.len());
                    let bytes: Vec<Value> = wasm_bytes.into_iter().map(Value::U8).collect();
                    // Return option<list<u8>> - Some for success
                    let result = Value::Option {
                        inner_type: pack::abi::ValueType::List(Box::new(pack::abi::ValueType::U8)),
                        value: Some(Box::new(Value::List {
                            elem_type: pack::abi::ValueType::U8,
                            items: bytes,
                        })),
                    };
                    encode(&result).map_err(|e| e.to_string())
                }
                Err(e) => {
                    info!("[ASSEMBLER] Error: {}", e);
                    // Return option<list<u8>> - None for error
                    let result = Value::Option {
                        inner_type: pack::abi::ValueType::List(Box::new(pack::abi::ValueType::U8)),
                        value: None,
                    };
                    encode(&result).map_err(|e| e.to_string())
                }
            }
        })
        // wrap-expression: Takes params (tuple string (list u8)), extracts expression,
        // wraps it as a module with eval() function, returns source string
        .add_host_function("wisp:repl/helpers", "wrap-expression", |input_bytes| {
            let input = decode(input_bytes).map_err(|e| e.to_string())?;

            // Input is: (tuple string (list u8)) - request-id and body
            let body_bytes: Vec<u8> = match &input {
                Value::Tuple(items) if items.len() >= 2 => match &items[1] {
                    Value::List { items, .. } => items
                        .iter()
                        .filter_map(|v| if let Value::U8(b) = v { Some(*b) } else { None })
                        .collect(),
                    _ => return Err("expected list<u8> as second tuple element".to_string()),
                },
                _ => return Err("expected tuple with request-id and body".to_string()),
            };

            let expr = String::from_utf8_lossy(&body_bytes).to_string();
            info!("[WRAP] Expression: {}", expr);

            // Wrap expression in a module with an eval function
            let source = format!(r#"(export (fn eval () s32 {}))"#, expr);
            info!("[WRAP] Wrapped source: {}", source);

            // Return the wrapped source string
            encode(&Value::String(source)).map_err(|e| e.to_string())
        })
        // eval-wat: Takes WAT string, assembles to WASM, instantiates, runs eval(), returns result
        .add_host_function("wisp:repl/helpers", "eval-wat", |input_bytes| {
            let input = decode(input_bytes).map_err(|e| e.to_string())?;
            let wat = match input {
                Value::String(s) => s,
                _ => return Err("expected string argument".to_string()),
            };

            info!("[EVAL-WAT] Assembling {} bytes of WAT", wat.len());

            // Step 1: Assemble WAT to WASM
            let wasm_bytes = match wat::parse_str(&wat) {
                Ok(bytes) => {
                    info!("[EVAL-WAT] Assembled to {} bytes of WASM", bytes.len());
                    bytes
                }
                Err(e) => {
                    let error_msg = format!("Assembly error: {}", e);
                    info!("[EVAL-WAT] {}", error_msg);
                    let response_bytes: Vec<Value> = error_msg.bytes().map(Value::U8).collect();
                    let result = Value::Tuple(vec![Value::Option {
                        inner_type: pack::abi::ValueType::List(Box::new(pack::abi::ValueType::U8)),
                        value: Some(Box::new(Value::List {
                            elem_type: pack::abi::ValueType::U8,
                            items: response_bytes,
                        })),
                    }]);
                    return encode(&result).map_err(|e| e.to_string());
                }
            };

            // Step 2: Instantiate WASM module and run eval()
            let result_value = match run_wasm_eval(&wasm_bytes) {
                Ok(value) => {
                    info!("[EVAL-WAT] eval() returned: {}", value);
                    format!("{}", value)
                }
                Err(e) => {
                    info!("[EVAL-WAT] Execution error: {}", e);
                    format!("Execution error: {}", e)
                }
            };

            // Return: (tuple (option (list u8)))
            let response_bytes: Vec<Value> = result_value.bytes().map(Value::U8).collect();
            let result = Value::Tuple(vec![Value::Option {
                inner_type: pack::abi::ValueType::List(Box::new(pack::abi::ValueType::U8)),
                value: Some(Box::new(Value::List {
                    elem_type: pack::abi::ValueType::U8,
                    items: response_bytes,
                })),
            }]);

            encode(&result).map_err(|e| e.to_string())
        })
        // spawn-and-wait: Spawn expression actor and wait for result
        .add_host_function("theater:simple/supervisor", "spawn-and-wait", |input_bytes| {
            info!("[SPAWN-AND-WAIT] Called with {} input bytes", input_bytes.len());
            let input = decode(input_bytes).map_err(|e| {
                info!("[SPAWN-AND-WAIT] Decode error: {}", e);
                e.to_string()
            })?;

            // Input is tuple(string, list<u8>) - (tag, wasm-bytes)
            let wasm_bytes: Vec<u8> = match &input {
                Value::Tuple(items) if items.len() >= 2 => match &items[1] {
                    Value::List { items, .. } => items
                        .iter()
                        .filter_map(|v| if let Value::U8(b) = v { Some(*b) } else { None })
                        .collect(),
                    _ => return Err("expected list<u8> as second tuple element".to_string()),
                },
                _ => return Err("expected tuple(string, list<u8>)".to_string()),
            };

            info!("[SPAWN-AND-WAIT] Spawning expression actor with {} bytes of WASM", wasm_bytes.len());

            // Run the WASM and get the result
            match run_wasm_eval(&wasm_bytes) {
                Ok(value) => {
                    info!("[SPAWN-AND-WAIT] eval() returned: {}", value);
                    let result_str = format!("{}", value);
                    let result_bytes: Vec<Value> = result_str.bytes().map(Value::U8).collect();

                    // Return option<list<u8>> - Some with result
                    let result = Value::Option {
                        inner_type: pack::abi::ValueType::List(Box::new(pack::abi::ValueType::U8)),
                        value: Some(Box::new(Value::List {
                            elem_type: pack::abi::ValueType::U8,
                            items: result_bytes,
                        })),
                    };
                    encode(&result).map_err(|e| e.to_string())
                }
                Err(e) => {
                    info!("[SPAWN-AND-WAIT] Execution error: {}", e);
                    // Return option<list<u8>> - None for error
                    let result = Value::Option {
                        inner_type: pack::abi::ValueType::List(Box::new(pack::abi::ValueType::U8)),
                        value: None,
                    };
                    encode(&result).map_err(|e| e.to_string())
                }
            }
        })
        .build()
        .context("Failed to build composition")?;

    info!("Composition built successfully");
    info!("Packages: {:?}", composition.packages());

    // Initialize the REPL actor
    let init_input = Value::Option {
        inner_type: pack::abi::ValueType::List(Box::new(pack::abi::ValueType::U8)),
        value: None,
    };

    let init_result = composition
        .call("repl", "theater:simple/actor.init", &init_input)
        .context("Failed to initialize REPL actor")?;
    info!("Actor init result: {:?}", init_result);

    // REPL loop
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    loop {
        print!("theater> ");
        stdout.flush()?;

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

        // Build request params: (state, (request-id: string, body: list<u8>))
        let request_id = format!("req-{}", rand_id());
        let body_bytes: Vec<Value> = line.bytes().map(Value::U8).collect();

        let params = Value::Tuple(vec![
            Value::String(request_id),
            Value::List {
                elem_type: pack::abi::ValueType::U8,
                items: body_bytes,
            },
        ]);

        let state = Value::Option {
            inner_type: pack::abi::ValueType::List(Box::new(pack::abi::ValueType::U8)),
            value: None,
        };

        let input = Value::Tuple(vec![state, params]);

        match composition.call(
            "repl",
            "theater:simple/message-server-client.handle-request",
            &input,
        ) {
            Ok(result) => {
                match extract_response(&result) {
                    Some(response_bytes) => {
                        let response = String::from_utf8_lossy(&response_bytes);
                        println!("{}", response);
                    }
                    None => {
                        println!("(no response)");
                    }
                }
            }
            Err(e) => {
                println!("error: {}", e);
            }
        }
    }

    println!("\nGoodbye!");
    Ok(())
}

/// Extract response bytes from the handle-request result
fn extract_response(result: &Value) -> Option<Vec<u8>> {
    match result {
        Value::Result {
            value: Ok(inner), ..
        } => extract_from_tuple(inner),
        Value::Result {
            value: Err(err), ..
        } => {
            if let Value::String(s) = err.as_ref() {
                println!("Actor error: {}", s);
            }
            None
        }
        other => extract_from_tuple(other),
    }
}

fn extract_from_tuple(value: &Value) -> Option<Vec<u8>> {
    if let Value::Tuple(outer) = value {
        if outer.len() >= 2 {
            if let Value::Tuple(response_tuple) = &outer[1] {
                if !response_tuple.is_empty() {
                    if let Value::Option {
                        value: Some(bytes), ..
                    } = &response_tuple[0]
                    {
                        if let Value::List { items, .. } = bytes.as_ref() {
                            return Some(
                                items
                                    .iter()
                                    .filter_map(|v| {
                                        if let Value::U8(b) = v {
                                            Some(*b)
                                        } else {
                                            None
                                        }
                                    })
                                    .collect(),
                            );
                        }
                    }
                }
            }
        }
    }
    None
}

fn rand_id() -> u64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos() as u64
}

/// Instantiate a WASM module and call its eval() function
fn run_wasm_eval(wasm_bytes: &[u8]) -> Result<i32, String> {
    let engine = Engine::default();
    let module = Module::new(&engine, wasm_bytes).map_err(|e| format!("Module error: {}", e))?;
    let mut store = Store::new(&engine, ());
    let instance =
        Instance::new(&mut store, &module, &[]).map_err(|e| format!("Instance error: {}", e))?;

    // Get the eval function
    let eval = instance
        .get_typed_func::<(), i32>(&mut store, "eval")
        .map_err(|e| format!("Failed to get eval function: {}", e))?;

    // Call eval() and get the result
    let result = eval
        .call(&mut store, ())
        .map_err(|e| format!("Eval error: {}", e))?;

    Ok(result)
}
