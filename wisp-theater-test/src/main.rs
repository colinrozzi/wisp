//! Test harness for running wisp-compiled modules with Theater's Composite runtime.

use std::sync::Arc;
use std::sync::RwLock as SyncRwLock;

use anyhow::Result;
use theater::actor::handle::ActorHandle;
use theater::actor::store::ActorStore;
use theater::chain::StateChain;
use theater::composite_bridge::{AsyncRuntime, CompositeInstance, Ctx, Value};
use theater::id::TheaterId;
use theater::messages::TheaterCommand;
use tokio::sync::mpsc;
use tracing::info;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter("info")
        .init();

    // Load the wisp-compiled WASM module
    let wasm_path = std::env::args()
        .nth(1)
        .unwrap_or_else(|| "examples/simple-composite.wasm".to_string());

    let func_name = std::env::args()
        .nth(2)
        .unwrap_or_else(|| "hello".to_string());

    info!("Loading WASM from: {}", wasm_path);

    let wasm_bytes = std::fs::read(&wasm_path)?;
    info!("Loaded {} bytes", wasm_bytes.len());

    // Create the Composite runtime
    let runtime = AsyncRuntime::new();

    // Create minimal actor store components
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

    // Create the CompositeInstance with host functions
    let mut instance = CompositeInstance::new(
        "wisp-test",
        &wasm_bytes,
        &runtime,
        actor_store,
        |builder| {
            // Register the log host function (for theater:simple/runtime)
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
            Ok(())
        },
    )
    .await?;

    info!("CompositeInstance created successfully");

    // Call the exported function
    info!("Calling function: {}", func_name);

    // For functions with no input, we pass an empty tuple
    let input = Value::Tuple(vec![]);
    let result = instance.call_value(&func_name, &input).await?;

    info!("Result: {:?}", result);

    // Pretty print the result
    match result {
        Value::S32(n) => println!("Result: {}", n),
        Value::S64(n) => println!("Result: {}", n),
        Value::String(s) => println!("Result: {}", s),
        other => println!("Result: {:?}", other),
    }

    Ok(())
}
