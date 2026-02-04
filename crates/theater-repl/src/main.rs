//! Theater REPL - Interactive Wisp shell powered by Theater actors
//!
//! This runtime provides an interactive REPL where:
//! - The REPL actor handles expression evaluation
//! - The self-hosted compiler (linked via Pack composition) compiles expressions
//! - Expression actors are spawned to evaluate code
//! - Results are returned via child-exit handlers
//!
//! Usage:
//!   theater-repl                    - Start interactive REPL
//!   theater-repl --actor <path>     - Use custom REPL actor WASM

use std::io::{self, BufRead, Write};
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::{Context, Result};
use theater::actor::handle::ActorHandle;
use theater::actor::store::ActorStore;
use theater::chain::StateChain;
use theater::id::TheaterId;
use theater::messages::TheaterCommand;
use theater::pack_bridge::{AsyncRuntime, PackInstance, Value, ValueType};
use tokio::sync::mpsc;
use tracing::info;

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt().with_env_filter("info").init();

    let args: Vec<String> = std::env::args().collect();

    // Parse optional custom actor path
    let actor_path = if args.len() >= 3 && args[1] == "--actor" {
        PathBuf::from(&args[2])
    } else {
        // Default to composed repl-actor
        PathBuf::from("examples/actors/repl-actor.wasm")
    };

    run_repl(&actor_path).await
}

/// Run the REPL with the given actor WASM
async fn run_repl(actor_path: &PathBuf) -> Result<()> {
    println!("Theater REPL (actor-based evaluation)");
    println!("Type 'quit' to exit\n");

    // Load the REPL actor WASM
    let actor_wasm = std::fs::read(actor_path)
        .with_context(|| format!("Failed to load REPL actor from {:?}", actor_path))?;
    info!("Loaded REPL actor: {} bytes", actor_wasm.len());

    // Create Theater runtime components
    let runtime = AsyncRuntime::new();
    let actor_id = TheaterId::generate();
    let (theater_tx, mut theater_rx) = mpsc::channel::<TheaterCommand>(100);
    let (operation_tx, _operation_rx) = mpsc::channel(10);
    let (info_tx, _info_rx) = mpsc::channel(10);
    let (control_tx, _control_rx) = mpsc::channel(10);
    let chain = Arc::new(std::sync::RwLock::new(StateChain::new(
        actor_id.clone(),
        theater_tx.clone(),
    )));
    let actor_handle = ActorHandle::new(operation_tx, info_tx, control_tx);
    let actor_store = ActorStore::new(actor_id.clone(), theater_tx.clone(), actor_handle, chain);

    // Create handler instances (we'll configure them but run without full Theater)
    // For now, we run a simplified version that doesn't spawn full Theater
    // Instead, we directly use PackInstance with host functions

    let mut instance = PackInstance::new(
        "repl-actor",
        &actor_wasm,
        &runtime,
        actor_store,
        |builder| {
            // Set up host functions that the REPL actor imports

            // theater:simple/runtime - log function
            builder.interface("theater:simple/runtime")?.func_typed(
                "log",
                |ctx: &mut theater::pack_bridge::Ctx<'_, ActorStore>, input: Value| {
                    let msg = match input {
                        Value::String(s) => s,
                        _ => format!("{:?}", input),
                    };
                    let store = ctx.data();
                    info!("[ACTOR] [{}] {}", store.id, msg);
                    Value::Tuple(vec![])
                },
            )?;

            // wisp:assembler/runtime - wat-to-wasm function
            builder
                .interface("wisp:assembler/runtime")?
                .func_typed_result(
                    "wat-to-wasm",
                    |_ctx: &mut theater::pack_bridge::Ctx<'_, ActorStore>, input: Value| {
                        let wat = match input {
                            Value::String(s) => s,
                            _ => {
                                return Err(Value::String("expected string argument".to_string()));
                            }
                        };

                        info!("[ASSEMBLER] Converting {} bytes of WAT to WASM", wat.len());

                        match wat::parse_str(&wat) {
                            Ok(wasm_bytes) => {
                                info!("[ASSEMBLER] Success: {} bytes of WASM", wasm_bytes.len());
                                let bytes: Vec<Value> =
                                    wasm_bytes.into_iter().map(Value::U8).collect();
                                Ok(Value::List {
                                    elem_type: ValueType::U8,
                                    items: bytes,
                                })
                            }
                            Err(e) => {
                                info!("[ASSEMBLER] Error: {}", e);
                                Err(Value::String(e.to_string()))
                            }
                        }
                    },
                )?;

            // wisp:repl/helpers - helper functions for spawn-based REPL
            builder.interface("wisp:repl/helpers")?
                .func_typed(
                    "wrap-expression",
                    |_ctx: &mut theater::pack_bridge::Ctx<'_, ActorStore>, input: Value| {
                        let expr = match input {
                            Value::String(s) => s,
                            _ => return Value::String("expected string argument".to_string()),
                        };

                        // Wrap expression as a minimal actor that evaluates and returns result
                        // The actor exports init which evaluates the expression and exits
                        let actor_source = format!(
                            r#"(export (fn init ((state (option (list u8)))) (result (tuple (option (list u8))) string) (ok (tuple (option (list u8))) string (tuple (none (list u8))))))"#
                        );
                        // TODO: Actually embed the expression result in the actor
                        // For now, return a minimal actor template
                        info!("[HELPERS] wrap-expression: {} -> {} chars", expr.len(), actor_source.len());
                        Value::String(actor_source)
                    },
                )?
                .func_typed(
                    "bytes-to-string",
                    |_ctx: &mut theater::pack_bridge::Ctx<'_, ActorStore>, input: Value| {
                        let bytes: Vec<u8> = match input {
                            Value::List { items, .. } => {
                                items.iter().filter_map(|v| {
                                    if let Value::U8(b) = v { Some(*b) } else { None }
                                }).collect()
                            }
                            _ => return Value::String("expected list<u8> argument".to_string()),
                        };

                        match String::from_utf8(bytes) {
                            Ok(s) => Value::String(s),
                            Err(e) => Value::String(format!("invalid UTF-8: {}", e)),
                        }
                    },
                )?;

            // wisp:evaluator - eval-request (for backwards compatibility with current repl-actor)
            // This provides the old evaluation interface
            builder.interface("wisp:evaluator")?.func_typed(
                "eval-request",
                |_ctx: &mut theater::pack_bridge::Ctx<'_, ActorStore>, input: Value| {
                    // For now, return a placeholder - the actual evaluation
                    // will be done by spawning expression actors
                    info!("[EVALUATOR] eval-request called with: {:?}", input);

                    // Return none for now
                    Value::Tuple(vec![Value::Option {
                        inner_type: ValueType::List(Box::new(ValueType::U8)),
                        value: None,
                    }])
                },
            )?;

            Ok(())
        },
    )
    .await?;

    info!("REPL actor instance created");

    // Initialize the actor
    let init_state = Value::Option {
        inner_type: ValueType::List(Box::new(ValueType::U8)),
        value: None,
    };
    let init_result = instance
        .call_value("theater:simple/actor.init", &init_state)
        .await?;
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

        // Send request to REPL actor
        // Build request params: (request-id: string, body: list<u8>)
        let request_id = format!("req-{}", rand_id());
        let body_bytes: Vec<Value> = line.bytes().map(Value::U8).collect();

        let params = Value::Tuple(vec![
            Value::String(request_id.clone()),
            Value::List {
                elem_type: ValueType::U8,
                items: body_bytes,
            },
        ]);

        // Actor state (from init, we use none for now)
        let state = Value::Option {
            inner_type: ValueType::List(Box::new(ValueType::U8)),
            value: None,
        };

        let input = Value::Tuple(vec![state, params]);

        match instance
            .call_value("theater:simple/message-server-client.handle-request", &input)
            .await
        {
            Ok(result) => {
                // Parse result: result<(state, (response)), string>
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
    // Result is: result<(state, (response: option<list<u8>>)), string>
    // On success: Result { value: Ok(Tuple([state, Tuple([Option(bytes)])])) }
    match result {
        Value::Result { value: Ok(inner), .. } => {
            if let Value::Tuple(outer) = inner.as_ref() {
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
        Value::Result {
            value: Err(err), ..
        } => {
            if let Value::String(s) = err.as_ref() {
                println!("Actor error: {}", s);
            }
            None
        }
        other => {
            // Try to extract directly if not wrapped in Result
            if let Value::Tuple(outer) = other {
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
    }
}

/// Generate a simple random ID
fn rand_id() -> u64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos() as u64
}
