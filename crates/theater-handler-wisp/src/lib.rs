//! # Wisp Handler
//!
//! Theater handler providing Wisp-specific host functions:
//! - `wisp:assembler/runtime.wat-to-wasm` - Assemble WAT to WASM bytes
//! - `wisp:repl/helpers.wrap-expression` - Wrap expression as eval module

use std::future::Future;
use std::pin::Pin;

use tracing::info;

use theater::actor::handle::ActorHandle;
use theater::actor::store::ActorStore;
use theater::handler::{Handler, HandlerContext, SharedActorInstance};
use theater::shutdown::ShutdownReceiver;

// Pack integration
use theater::pack_bridge::{Ctx, HostLinkerBuilder, LinkerError, Value, ValueType};

/// Handler for Wisp-specific host functions
#[derive(Clone, Default)]
pub struct WispHandler;

impl WispHandler {
    pub fn new() -> Self {
        Self
    }
}

impl Handler for WispHandler {
    fn create_instance(
        &self,
        _config: Option<&theater::config::actor_manifest::HandlerConfig>,
    ) -> Box<dyn Handler> {
        Box::new(self.clone())
    }

    fn name(&self) -> &str {
        "wisp"
    }

    fn imports(&self) -> Option<Vec<String>> {
        Some(vec![
            "wisp:assembler/runtime".to_string(),
            "wisp:repl/helpers".to_string(),
        ])
    }

    fn exports(&self) -> Option<Vec<String>> {
        None // No specific exports required
    }

    fn start(
        &mut self,
        _actor_handle: ActorHandle,
        _actor_instance: SharedActorInstance,
        shutdown_receiver: ShutdownReceiver,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<()>> + Send>> {
        info!("Starting Wisp handler");

        Box::pin(async move {
            shutdown_receiver.wait_for_shutdown().await;
            info!("Wisp handler shut down");
            Ok(())
        })
    }

    fn setup_host_functions_composite(
        &mut self,
        builder: &mut HostLinkerBuilder<'_, ActorStore>,
        ctx: &mut HandlerContext,
    ) -> Result<(), LinkerError> {
        info!("Setting up Wisp host functions");

        // Setup wisp:assembler/runtime interface
        if !ctx.is_satisfied("wisp:assembler/runtime") {
            builder
                .interface("wisp:assembler/runtime")?
                // wat-to-wasm: func(wat: string) -> option<list<u8>>
                .func_typed(
                    "wat-to-wasm",
                    |_ctx: &mut Ctx<'_, ActorStore>, input: Value| {
                        let wat = match input {
                            Value::String(s) => s,
                            _ => {
                                info!("[ASSEMBLER] Invalid input type, expected string");
                                return Value::Option {
                                    inner_type: ValueType::List(Box::new(ValueType::U8)),
                                    value: None,
                                };
                            }
                        };

                        info!("[ASSEMBLER] Converting {} bytes of WAT to WASM", wat.len());

                        match wat::parse_str(&wat) {
                            Ok(wasm_bytes) => {
                                info!(
                                    "[ASSEMBLER] Success: {} bytes of WASM",
                                    wasm_bytes.len()
                                );
                                let bytes: Vec<Value> =
                                    wasm_bytes.into_iter().map(Value::U8).collect();
                                Value::Option {
                                    inner_type: ValueType::List(Box::new(ValueType::U8)),
                                    value: Some(Box::new(Value::List {
                                        elem_type: ValueType::U8,
                                        items: bytes,
                                    })),
                                }
                            }
                            Err(e) => {
                                info!("[ASSEMBLER] Error: {}", e);
                                Value::Option {
                                    inner_type: ValueType::List(Box::new(ValueType::U8)),
                                    value: None,
                                }
                            }
                        }
                    },
                )?;
            ctx.mark_satisfied("wisp:assembler/runtime");
        }

        // Setup wisp:repl/helpers interface
        if !ctx.is_satisfied("wisp:repl/helpers") {
            builder
                .interface("wisp:repl/helpers")?
                // wrap-expression: func(params: tuple<string, list<u8>>) -> string
                .func_typed(
                    "wrap-expression",
                    |_ctx: &mut Ctx<'_, ActorStore>, input: Value| {
                        // Input is tuple<string, list<u8>> - (request-id, body-bytes)
                        let body_bytes: Vec<u8> = match &input {
                            Value::Tuple(items) if items.len() >= 2 => match &items[1] {
                                Value::List { items, .. } => items
                                    .iter()
                                    .filter_map(|v| {
                                        if let Value::U8(b) = v {
                                            Some(*b)
                                        } else {
                                            None
                                        }
                                    })
                                    .collect(),
                                _ => {
                                    info!("[WRAP] Expected list<u8> as second tuple element");
                                    return Value::String(String::new());
                                }
                            },
                            _ => {
                                info!("[WRAP] Expected tuple with request-id and body");
                                return Value::String(String::new());
                            }
                        };

                        let expr = String::from_utf8_lossy(&body_bytes).to_string();
                        info!("[WRAP] Expression: {}", expr);

                        // Wrap expression in a module with an eval function
                        let source = format!(r#"(export (fn eval () s32 {}))"#, expr);
                        info!("[WRAP] Wrapped source: {}", source);

                        Value::String(source)
                    },
                )?;
            ctx.mark_satisfied("wisp:repl/helpers");
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_wat_assembly() {
        let wat = r#"(module (func (export "test") (result i32) (i32.const 42)))"#;
        let result = wat::parse_str(wat);
        assert!(result.is_ok());
        let wasm = result.unwrap();
        assert!(!wasm.is_empty());
    }
}
