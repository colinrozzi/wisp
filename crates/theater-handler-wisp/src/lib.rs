//! # Wisp Handler
//!
//! Theater handler providing Wisp-specific host functions:
//! - `wisp:assembler/runtime.wat-to-wasm` - Assemble WAT to WASM bytes
//! - `wisp:repl/helpers.wrap-expression` - Wrap expression as eval module
//! - `wisp:compose/packages.compose-packages` - Compose main + deps into single WASM

use std::future::Future;
use std::pin::Pin;

use tracing::info;

use theater::actor::handle::ActorHandle;
use theater::actor::store::ActorStore;
use theater::handler::{Handler, HandlerContext, SharedActorInstance};
use theater::shutdown::ShutdownReceiver;

// Pack integration
use theater::pack_bridge::{Ctx, HostLinkerBuilder, LinkerError, Value, ValueType};

// Pack composition
use pack::compose::StaticComposer;

// WASM runtime for eval-wasm
use wasmtime::{Engine, Module, Store};

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
            "wisp:compose/packages".to_string(),
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
                )?
                // eval-wasm: func(wasm: list<u8>) -> result<list<u8>, string>
                // Instantiate WASM module, call its `eval` export, return result as bytes
                .func_typed(
                    "eval-wasm",
                    |_ctx: &mut Ctx<'_, ActorStore>, input: Value| {
                        let wasm_bytes: Vec<u8> = match input {
                            Value::List { items, .. } => items
                                .into_iter()
                                .filter_map(|v| {
                                    if let Value::U8(b) = v {
                                        Some(b)
                                    } else {
                                        None
                                    }
                                })
                                .collect(),
                            _ => {
                                return Value::Result {
                                    ok_type: ValueType::List(Box::new(ValueType::U8)),
                                    err_type: ValueType::String,
                                    value: Err(Box::new(Value::String(
                                        "Expected list<u8> for WASM bytes".to_string(),
                                    ))),
                                };
                            }
                        };

                        info!("[EVAL-WASM] Instantiating {} bytes of WASM", wasm_bytes.len());

                        // Create wasmtime engine and store
                        let engine = match Engine::default() {
                            engine => engine,
                        };
                        let mut store = Store::new(&engine, ());

                        // Compile module
                        let module = match Module::new(&engine, &wasm_bytes) {
                            Ok(m) => m,
                            Err(e) => {
                                info!("[EVAL-WASM] Module compilation error: {}", e);
                                return Value::Result {
                                    ok_type: ValueType::List(Box::new(ValueType::U8)),
                                    err_type: ValueType::String,
                                    value: Err(Box::new(Value::String(format!(
                                        "Module compilation failed: {}",
                                        e
                                    )))),
                                };
                            }
                        };

                        // Instantiate module
                        let instance = match wasmtime::Instance::new(&mut store, &module, &[]) {
                            Ok(i) => i,
                            Err(e) => {
                                info!("[EVAL-WASM] Instantiation error: {}", e);
                                return Value::Result {
                                    ok_type: ValueType::List(Box::new(ValueType::U8)),
                                    err_type: ValueType::String,
                                    value: Err(Box::new(Value::String(format!(
                                        "Instantiation failed: {}",
                                        e
                                    )))),
                                };
                            }
                        };

                        // Get the eval function
                        let eval_func = match instance.get_typed_func::<(), i32>(&mut store, "eval") {
                            Ok(f) => f,
                            Err(e) => {
                                info!("[EVAL-WASM] Could not find eval function: {}", e);
                                return Value::Result {
                                    ok_type: ValueType::List(Box::new(ValueType::U8)),
                                    err_type: ValueType::String,
                                    value: Err(Box::new(Value::String(format!(
                                        "Could not find eval function: {}",
                                        e
                                    )))),
                                };
                            }
                        };

                        // Call eval
                        match eval_func.call(&mut store, ()) {
                            Ok(result) => {
                                info!("[EVAL-WASM] Result: {}", result);
                                // Convert i32 to 4 bytes (little-endian)
                                let result_bytes: Vec<Value> = result.to_le_bytes()
                                    .into_iter()
                                    .map(Value::U8)
                                    .collect();
                                Value::Result {
                                    ok_type: ValueType::List(Box::new(ValueType::U8)),
                                    err_type: ValueType::String,
                                    value: Ok(Box::new(Value::List {
                                        elem_type: ValueType::U8,
                                        items: result_bytes,
                                    })),
                                }
                            }
                            Err(e) => {
                                info!("[EVAL-WASM] Execution error: {}", e);
                                Value::Result {
                                    ok_type: ValueType::List(Box::new(ValueType::U8)),
                                    err_type: ValueType::String,
                                    value: Err(Box::new(Value::String(format!(
                                        "Execution failed: {}",
                                        e
                                    )))),
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

                        // Wrap expression as a simple eval function
                        // The REPL will compile this and call eval() directly to get the i32 result
                        // This avoids the self-hosted compiler's bug with u8 type annotations
                        let source = format!(
                            r#"
(export (fn eval () s32
  {}))
"#,
                            expr
                        );
                        info!("[WRAP] Wrapped source (eval function): {}", source.trim());

                        Value::String(source)
                    },
                )?;
            ctx.mark_satisfied("wisp:repl/helpers");
        }

        // Setup wisp:compose/packages interface
        if !ctx.is_satisfied("wisp:compose/packages") {
            builder
                .interface("wisp:compose/packages")?
                // compose-packages: func(main: list<u8>, deps: list<tuple<string, list<u8>>>) -> result<list<u8>, string>
                .func_typed(
                    "compose-packages",
                    |_ctx: &mut Ctx<'_, ActorStore>, input: Value| {
                        // Input is tuple<list<u8>, list<tuple<string, list<u8>>>>
                        // - main WASM bytes
                        // - list of (interface-name, wasm-bytes) dependencies
                        let (main_wasm, deps) = match &input {
                            Value::Tuple(items) if items.len() >= 2 => {
                                // Extract main WASM bytes
                                let main_wasm: Vec<u8> = match &items[0] {
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
                                        return Value::Result {
                                            ok_type: ValueType::List(Box::new(ValueType::U8)),
                                            err_type: ValueType::String,
                                            value: Err(Box::new(Value::String(
                                                "Expected list<u8> for main WASM".to_string(),
                                            ))),
                                        };
                                    }
                                };

                                // Extract dependencies: list<tuple<string, list<u8>>>
                                let deps: Vec<(String, Vec<u8>)> = match &items[1] {
                                    Value::List { items, .. } => {
                                        let mut result = Vec::new();
                                        for item in items {
                                            match item {
                                                Value::Tuple(dep_items) if dep_items.len() >= 2 => {
                                                    let interface = match &dep_items[0] {
                                                        Value::String(s) => s.clone(),
                                                        _ => continue,
                                                    };
                                                    let wasm: Vec<u8> = match &dep_items[1] {
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
                                                        _ => continue,
                                                    };
                                                    result.push((interface, wasm));
                                                }
                                                _ => continue,
                                            }
                                        }
                                        result
                                    }
                                    _ => {
                                        return Value::Result {
                                            ok_type: ValueType::List(Box::new(ValueType::U8)),
                                            err_type: ValueType::String,
                                            value: Err(Box::new(Value::String(
                                                "Expected list of deps".to_string(),
                                            ))),
                                        };
                                    }
                                };

                                (main_wasm, deps)
                            }
                            _ => {
                                return Value::Result {
                                    ok_type: ValueType::List(Box::new(ValueType::U8)),
                                    err_type: ValueType::String,
                                    value: Err(Box::new(Value::String(
                                        "Expected tuple<list<u8>, list<...>>".to_string(),
                                    ))),
                                };
                            }
                        };

                        info!(
                            "[COMPOSE] Composing main ({} bytes) with {} dependencies",
                            main_wasm.len(),
                            deps.len()
                        );

                        // If no dependencies, just return main as-is
                        if deps.is_empty() {
                            let bytes: Vec<Value> = main_wasm.into_iter().map(Value::U8).collect();
                            return Value::Result {
                                ok_type: ValueType::List(Box::new(ValueType::U8)),
                                err_type: ValueType::String,
                                value: Ok(Box::new(Value::List {
                                    elem_type: ValueType::U8,
                                    items: bytes,
                                })),
                            };
                        }

                        // Use StaticComposer to compose packages
                        let mut composer = StaticComposer::new();

                        // Add main module
                        composer = match composer.add_module("main", main_wasm) {
                            Ok(c) => c,
                            Err(e) => {
                                return Value::Result {
                                    ok_type: ValueType::List(Box::new(ValueType::U8)),
                                    err_type: ValueType::String,
                                    value: Err(Box::new(Value::String(format!(
                                        "Failed to add main module: {}",
                                        e
                                    )))),
                                };
                            }
                        };

                        // Add dependency modules
                        for (idx, (interface, wasm)) in deps.iter().enumerate() {
                            let dep_name = format!("dep{}", idx);
                            info!("[COMPOSE] Adding dep '{}' for interface '{}'", dep_name, interface);

                            composer = match composer.add_module(&dep_name, wasm.clone()) {
                                Ok(c) => c,
                                Err(e) => {
                                    return Value::Result {
                                        ok_type: ValueType::List(Box::new(ValueType::U8)),
                                        err_type: ValueType::String,
                                        value: Err(Box::new(Value::String(format!(
                                            "Failed to add dep module: {}",
                                            e
                                        )))),
                                    };
                                }
                            };
                        }

                        // Auto-wire: matches imports to exports by function name
                        composer = match composer.auto_wire() {
                            Ok(c) => c,
                            Err(e) => {
                                return Value::Result {
                                    ok_type: ValueType::List(Box::new(ValueType::U8)),
                                    err_type: ValueType::String,
                                    value: Err(Box::new(Value::String(format!(
                                        "Auto-wire failed: {}",
                                        e
                                    )))),
                                };
                            }
                        };

                        // Export main's exports
                        composer = composer
                            .export("memory", "main", "memory")
                            .export("eval", "main", "eval");

                        // Compose
                        match composer.compose() {
                            Ok(composed_wasm) => {
                                info!("[COMPOSE] Success: {} bytes composed", composed_wasm.len());
                                let bytes: Vec<Value> =
                                    composed_wasm.into_iter().map(Value::U8).collect();
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
                                info!("[COMPOSE] Error: {}", e);
                                Value::Result {
                                    ok_type: ValueType::List(Box::new(ValueType::U8)),
                                    err_type: ValueType::String,
                                    value: Err(Box::new(Value::String(format!(
                                        "Composition failed: {}",
                                        e
                                    )))),
                                }
                            }
                        }
                    },
                )?;
            ctx.mark_satisfied("wisp:compose/packages");
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
