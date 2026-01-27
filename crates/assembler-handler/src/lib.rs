//! # Assembler Handler
//!
//! Provides WAT to WASM assembly capabilities to WebAssembly actors.
//! This allows self-hosted compilers to convert their WAT output to executable WASM.

use std::future::Future;
use std::pin::Pin;
use tracing::info;
use wasmtime::StoreContextMut;

use theater::actor::handle::ActorHandle;
use theater::actor::store::ActorStore;
use theater::handler::{Handler, HandlerContext, SharedActorInstance};
use theater::shutdown::ShutdownReceiver;
use theater::wasm::{ActorComponent, ActorInstance};

// Composite integration
use theater::composite_bridge::{Ctx, HostLinkerBuilder, LinkerError, Value};
use theater::ValueType;

/// Handler for providing WAT to WASM assembly capabilities
#[derive(Clone, Default)]
pub struct AssemblerHandler {}

impl AssemblerHandler {
    pub fn new() -> Self {
        Self {}
    }
}

impl Handler for AssemblerHandler {
    fn create_instance(
        &self,
        _config: Option<&theater::config::actor_manifest::HandlerConfig>,
    ) -> Box<dyn Handler> {
        Box::new(self.clone())
    }

    fn start(
        &mut self,
        _actor_handle: ActorHandle,
        _actor_instance: SharedActorInstance,
        shutdown_receiver: ShutdownReceiver,
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<()>> + Send>> {
        info!("Starting assembler handler");

        Box::pin(async {
            shutdown_receiver.wait_for_shutdown().await;
            info!("Assembler handler shut down");
            Ok(())
        })
    }

    fn setup_host_functions(
        &mut self,
        actor_component: &mut ActorComponent,
        ctx: &mut HandlerContext,
    ) -> anyhow::Result<()> {
        info!("Setting up assembler host functions");

        if ctx.is_satisfied("theater:simple/assembler") {
            info!("theater:simple/assembler already satisfied, skipping");
            return Ok(());
        }

        let mut interface = match actor_component.linker.instance("theater:simple/assembler") {
            Ok(interface) => interface,
            Err(e) => {
                return Err(anyhow::anyhow!(
                    "Could not instantiate theater:simple/assembler: {}",
                    e
                ));
            }
        };

        // wat-to-wasm: func(wat: string) -> result<list<u8>, string>
        interface
            .func_wrap(
                "wat-to-wasm",
                move |_ctx: StoreContextMut<'_, ActorStore>,
                      (wat,): (String,)|
                      -> anyhow::Result<(Result<Vec<u8>, String>,)> {
                    info!("wat-to-wasm called with {} bytes of WAT", wat.len());

                    let result = wat::parse_str(&wat).map_err(|e| e.to_string());

                    Ok((result,))
                },
            )
            .map_err(|e| anyhow::anyhow!("Failed to wrap wat-to-wasm function: {}", e))?;

        ctx.mark_satisfied("theater:simple/assembler");
        Ok(())
    }

    fn add_export_functions(&self, _actor_instance: &mut ActorInstance) -> anyhow::Result<()> {
        // No exports required from actors
        Ok(())
    }

    // Composite Integration

    fn setup_host_functions_composite(
        &mut self,
        builder: &mut HostLinkerBuilder<'_, ActorStore>,
        ctx: &mut HandlerContext,
    ) -> Result<(), LinkerError> {
        info!("Setting up assembler host functions (Composite)");

        if ctx.is_satisfied("theater:simple/assembler") {
            info!("theater:simple/assembler already satisfied, skipping");
            return Ok(());
        }

        builder
            .interface("theater:simple/assembler")?
            .func_typed("wat-to-wasm", |_ctx: &mut Ctx<'_, ActorStore>, input: Value| {
                // Extract WAT string from input
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

                info!("wat-to-wasm called with {} bytes of WAT", wat.len());

                let result = wat::parse_str(&wat);

                match result {
                    Ok(wasm_bytes) => {
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
                    Err(e) => Value::Result {
                        ok_type: ValueType::List(Box::new(ValueType::U8)),
                        err_type: ValueType::String,
                        value: Err(Box::new(Value::String(e.to_string()))),
                    },
                }
            })?;

        ctx.mark_satisfied("theater:simple/assembler");
        Ok(())
    }

    fn supports_composite(&self) -> bool {
        true
    }

    fn name(&self) -> &str {
        "assembler"
    }

    fn imports(&self) -> Option<Vec<String>> {
        Some(vec!["theater:simple/assembler".to_string()])
    }

    fn exports(&self) -> Option<Vec<String>> {
        None
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_wat_to_wasm() {
        let wat = r#"(module
            (func (export "add") (param i32 i32) (result i32)
                local.get 0
                local.get 1
                i32.add)
        )"#;

        let result = wat::parse_str(wat);
        assert!(result.is_ok());

        let wasm = result.unwrap();
        // WASM magic number: \0asm
        assert_eq!(&wasm[0..4], &[0x00, 0x61, 0x73, 0x6d]);
    }
}
