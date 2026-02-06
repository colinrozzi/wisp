//! Theater REPL - Interactive Wisp shell powered by Theater actors
//!
//! This runtime provides an interactive REPL where:
//! - The REPL actor handles expression evaluation
//! - The self-hosted compiler is statically composed into the actor
//! - Host functions provide assembler, logging, and supervisor capabilities
//! - Expression actors can be spawned to evaluate code
//!
//! Architecture:
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │                    Theater Runtime                          │
//! │                                                             │
//! │  spawn-repl-actor.wasm + wisp-compiler.wasm                 │
//! │              ↓ StaticComposer                               │
//! │  composed-repl.wasm (single WASM with compiler embedded)    │
//! │              ↓ TheaterRuntime                               │
//! │  Spawned as Theater actor with handlers                     │
//! │                                                             │
//! │  Handlers: runtime, store, supervisor, message-server, wisp │
//! └─────────────────────────────────────────────────────────────┘
//! ```
//!
//! Usage:
//!   theater-repl                  - Start interactive REPL
//!   theater-repl --actor <path>   - Use custom REPL actor WASM

use std::io::{self, BufRead, Write};
use std::path::PathBuf;

use anyhow::{Context, Result};
use pack::compose::StaticComposer;
use tracing::{error, info};

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
    let mut actor_path = PathBuf::from("examples/actors/spawn-repl-actor.wasm");

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--actor" if i + 1 < args.len() => {
                actor_path = PathBuf::from(&args[i + 1]);
                i += 2;
            }
            "--help" | "-h" => {
                println!("Theater REPL - Interactive Wisp shell powered by Theater actors");
                println!();
                println!("Usage:");
                println!("  theater-repl                  Start interactive REPL");
                println!("  theater-repl --actor <path>   Use custom REPL actor WASM");
                println!("  theater-repl --help           Show this help");
                return Ok(());
            }
            _ => i += 1,
        }
    }

    run_theater_repl(&actor_path).await
}

/// Run the REPL with Theater runtime
async fn run_theater_repl(actor_path: &PathBuf) -> Result<()> {
    println!("Theater REPL");
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
    info!("Composing actor + compiler...");
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

    info!("Composition complete: {} bytes", composed_wasm.len());

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
    info!("Spawning REPL actor...");
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
            info!("REPL actor spawned: {}", id);
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

    // Wait for actor to fully initialize and register with MessageRouter
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
                if response_bytes.is_empty() {
                    println!("(no result)");
                } else {
                    // Response format: [type_tag, ...value_bytes]
                    let type_tag = response_bytes[0];
                    let value_bytes = &response_bytes[1..];

                    match type_tag {
                        0x01 if value_bytes.len() >= 4 => {
                            // s32 (i32)
                            let value = i32::from_le_bytes([
                                value_bytes[0],
                                value_bytes[1],
                                value_bytes[2],
                                value_bytes[3],
                            ]);
                            println!("{}", value);
                        }
                        0x02 if value_bytes.len() >= 8 => {
                            // s64 (i64)
                            let value = i64::from_le_bytes([
                                value_bytes[0],
                                value_bytes[1],
                                value_bytes[2],
                                value_bytes[3],
                                value_bytes[4],
                                value_bytes[5],
                                value_bytes[6],
                                value_bytes[7],
                            ]);
                            println!("{}", value);
                        }
                        0x03 if value_bytes.len() >= 4 => {
                            // f32
                            let value = f32::from_le_bytes([
                                value_bytes[0],
                                value_bytes[1],
                                value_bytes[2],
                                value_bytes[3],
                            ]);
                            println!("{}", value);
                        }
                        0x04 if value_bytes.len() >= 8 => {
                            // f64
                            let value = f64::from_le_bytes([
                                value_bytes[0],
                                value_bytes[1],
                                value_bytes[2],
                                value_bytes[3],
                                value_bytes[4],
                                value_bytes[5],
                                value_bytes[6],
                                value_bytes[7],
                            ]);
                            println!("{}", value);
                        }
                        _ => {
                            // Unknown format, try as string
                            let response = String::from_utf8_lossy(&response_bytes);
                            println!("{}", response);
                        }
                    }
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

    registry
}
