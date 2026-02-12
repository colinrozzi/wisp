//! # Wisp Handler
//!
//! Theater handler providing Wisp-specific host functions:
//! - `wisp:assembler/runtime.wat-to-wasm` - Assemble WAT to WASM bytes
//! - `wisp:repl/helpers.wrap-expression` - Wrap expression as eval module
//! - `wisp:compose/packages.compose-packages` - Compose main + deps into single WASM
//! - `wisp:filesystem/runtime.read-file` - Read file from filesystem
//! - `wisp:imports/metadata.get-package-exports` - Get exports from a Pack package
//!
//! ## Interface Hashing
//!
//! This handler uses `InterfaceImpl` to declare its interfaces, enabling:
//! - Compile-time type extraction from Rust closures
//! - Automatic Merkle-tree hash computation for O(1) compatibility checking
//! - Self-documenting interface declarations

use std::fs;
use std::future::Future;
use std::path::Path;
use std::pin::Pin;

use tracing::info;

use theater::actor::handle::ActorHandle;
use theater::actor::store::ActorStore;
use theater::handler::{Handler, HandlerContext, SharedActorInstance};
use theater::shutdown::ShutdownReceiver;

// Pack integration
use theater::pack_bridge::{Ctx, HostLinkerBuilder, InterfaceImpl, LinkerError, TypeHash, Value, ValueType};

// Pack composition and metadata
use pack::compose::StaticComposer;
use pack::{Runtime, TypeDesc};

// WASM runtime for eval-wasm
use wasmtime::{Engine, Module, Store};

/// Detect the return type of a Wisp expression based on its syntax.
/// Returns "s32", "s64", "f32", or "f64".
fn detect_expr_type(expr: &str) -> &'static str {
    let expr = expr.trim();

    // Check for explicit type cast as outermost expression
    if expr.starts_with("(s64 ") || expr.starts_with("(s64\n") {
        return "s64";
    }
    if expr.starts_with("(f32 ") || expr.starts_with("(f32\n") {
        return "f32";
    }
    if expr.starts_with("(f64 ") || expr.starts_with("(f64\n") {
        return "f64";
    }
    if expr.starts_with("(s32 ") || expr.starts_with("(s32\n") {
        return "s32";
    }

    // Check for WASM instruction prefixes
    if expr.starts_with("(i64.") {
        return "s64";
    }
    if expr.starts_with("(f32.") {
        return "f32";
    }
    if expr.starts_with("(f64.") {
        return "f64";
    }
    if expr.starts_with("(i32.") {
        return "s32";
    }

    // Check for numeric literals with type suffixes
    // e.g., "42s64", "3.14f32"
    if let Some(first_token) = expr.strip_prefix('(').and_then(|s| s.split_whitespace().next()) {
        if first_token.ends_with("s64") || first_token.ends_with("i64") {
            return "s64";
        }
        if first_token.ends_with("f32") {
            return "f32";
        }
        if first_token.ends_with("f64") {
            return "f64";
        }
    }

    // Default to s32
    "s32"
}

// ============================================================================
// Interface Declarations
// ============================================================================

/// Build the interface declaration for `wisp:assembler/runtime`.
///
/// Functions:
/// - `wat-to-wasm`: String -> Option<Vec<u8>>
/// - `eval-wasm`: Vec<u8> -> Result<Vec<u8>, String>
fn assembler_interface() -> InterfaceImpl {
    InterfaceImpl::new("wisp:assembler/runtime")
        .func("wat-to-wasm", |_: String| -> Option<Vec<u8>> { None })
        .func("eval-wasm", |_: Vec<u8>| -> Result<Vec<u8>, String> { Err(String::new()) })
}

/// Build the interface declaration for `wisp:repl/helpers`.
///
/// Functions:
/// - `wrap-expression`: (String, Vec<u8>) -> String
/// - `parse-and-wrap`: (String, Vec<u8>) -> Result<String, String>
fn helpers_interface() -> InterfaceImpl {
    InterfaceImpl::new("wisp:repl/helpers")
        .func("wrap-expression", |_: (String, Vec<u8>)| -> String { String::new() })
        .func("parse-and-wrap", |_: (String, Vec<u8>)| -> Result<String, String> { Err(String::new()) })
}

/// Build the interface declaration for `wisp:compose/packages`.
///
/// Functions:
/// - `compose-packages`: (Vec<u8>, (String, Vec<u8>)) -> Result<Vec<u8>, String>
fn compose_interface() -> InterfaceImpl {
    InterfaceImpl::new("wisp:compose/packages")
        .func("compose-packages", |_: (Vec<u8>, (String, Vec<u8>))| -> Result<Vec<u8>, String> {
            Err(String::new())
        })
}

/// Build the interface declaration for `wisp:filesystem/runtime`.
///
/// Functions:
/// - `read-file`: String -> Result<Vec<u8>, String>
fn filesystem_interface() -> InterfaceImpl {
    InterfaceImpl::new("wisp:filesystem/runtime")
        .func("read-file", |_: String| -> Result<Vec<u8>, String> { Err(String::new()) })
}

/// Build the interface declaration for `wisp:imports/metadata`.
///
/// Functions:
/// - `get-package-exports`: Vec<u8> -> Result<Vec<String>, String>
fn metadata_interface() -> InterfaceImpl {
    InterfaceImpl::new("wisp:imports/metadata")
        .func("get-package-exports", |_: Vec<u8>| -> Result<Vec<String>, String> {
            Err(String::new())
        })
}

// ============================================================================
// Handler Implementation
// ============================================================================

/// Handler for Wisp-specific host functions.
///
/// This handler provides host functions for:
/// - WAT assembly and WASM evaluation
/// - REPL expression wrapping
/// - Package composition
/// - File system access
/// - Package metadata extraction
///
/// ## Interface Hashes
///
/// Each interface has a computed Merkle-tree hash based on its function signatures.
/// Use `interface_hashes()` to get the hashes for compatibility checking.
#[derive(Clone, Default)]
pub struct WispHandler;

impl WispHandler {
    pub fn new() -> Self {
        Self
    }

    /// Get all interface declarations for this handler.
    pub fn interfaces(&self) -> Vec<InterfaceImpl> {
        vec![
            assembler_interface(),
            helpers_interface(),
            compose_interface(),
            filesystem_interface(),
            metadata_interface(),
        ]
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
        // Derive imports from interface declarations
        Some(self.interfaces().iter().map(|i| i.name().to_string()).collect())
    }

    fn exports(&self) -> Option<Vec<String>> {
        None // No specific exports required
    }

    fn interface_hashes(&self) -> Vec<(String, TypeHash)> {
        self.interfaces()
            .iter()
            .map(|i| (i.name().to_string(), i.hash()))
            .collect()
    }

    fn supports_composite(&self) -> bool {
        true
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
        // Using interface_from_impl() to get both the builder and the interface hash
        if !ctx.is_satisfied("wisp:assembler/runtime") {
            let (mut iface, hash) = builder.interface_from_impl(&assembler_interface())?;
            info!("Registering wisp:assembler/runtime with hash: {}", hash);

            iface
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

                        // Type tags for the response
                        const TYPE_S32: u8 = 0x01;
                        const TYPE_S64: u8 = 0x02;
                        const TYPE_F32: u8 = 0x03;
                        const TYPE_F64: u8 = 0x04;

                        // Try to get the eval function with different return types
                        // The type is determined by what the WASM module exports

                        // Helper to build the result
                        let make_result = |type_tag: u8, bytes: Vec<u8>| {
                            let mut result_bytes: Vec<Value> = vec![Value::U8(type_tag)];
                            result_bytes.extend(bytes.into_iter().map(Value::U8));
                            Value::Result {
                                ok_type: ValueType::List(Box::new(ValueType::U8)),
                                err_type: ValueType::String,
                                value: Ok(Box::new(Value::List {
                                    elem_type: ValueType::U8,
                                    items: result_bytes,
                                })),
                            }
                        };

                        let make_error = |msg: String| {
                            Value::Result {
                                ok_type: ValueType::List(Box::new(ValueType::U8)),
                                err_type: ValueType::String,
                                value: Err(Box::new(Value::String(msg))),
                            }
                        };

                        // Try i32 first (most common)
                        if let Ok(func) = instance.get_typed_func::<(), i32>(&mut store, "eval") {
                            match func.call(&mut store, ()) {
                                Ok(result) => {
                                    info!("[EVAL-WASM] Result (i32): {}", result);
                                    return make_result(TYPE_S32, result.to_le_bytes().to_vec());
                                }
                                Err(e) => {
                                    info!("[EVAL-WASM] Execution error: {}", e);
                                    return make_error(format!("Execution failed: {}", e));
                                }
                            }
                        }

                        // Try i64
                        if let Ok(func) = instance.get_typed_func::<(), i64>(&mut store, "eval") {
                            match func.call(&mut store, ()) {
                                Ok(result) => {
                                    info!("[EVAL-WASM] Result (i64): {}", result);
                                    return make_result(TYPE_S64, result.to_le_bytes().to_vec());
                                }
                                Err(e) => {
                                    info!("[EVAL-WASM] Execution error: {}", e);
                                    return make_error(format!("Execution failed: {}", e));
                                }
                            }
                        }

                        // Try f32
                        if let Ok(func) = instance.get_typed_func::<(), f32>(&mut store, "eval") {
                            match func.call(&mut store, ()) {
                                Ok(result) => {
                                    info!("[EVAL-WASM] Result (f32): {}", result);
                                    return make_result(TYPE_F32, result.to_le_bytes().to_vec());
                                }
                                Err(e) => {
                                    info!("[EVAL-WASM] Execution error: {}", e);
                                    return make_error(format!("Execution failed: {}", e));
                                }
                            }
                        }

                        // Try f64
                        if let Ok(func) = instance.get_typed_func::<(), f64>(&mut store, "eval") {
                            match func.call(&mut store, ()) {
                                Ok(result) => {
                                    info!("[EVAL-WASM] Result (f64): {}", result);
                                    return make_result(TYPE_F64, result.to_le_bytes().to_vec());
                                }
                                Err(e) => {
                                    info!("[EVAL-WASM] Execution error: {}", e);
                                    return make_error(format!("Execution failed: {}", e));
                                }
                            }
                        }

                        // No matching eval function found
                        make_error("Could not find eval function with supported return type (i32, i64, f32, f64)".to_string())
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

                        // Detect the return type from the expression
                        let return_type = detect_expr_type(&expr);
                        info!("[WRAP] Detected return type: {}", return_type);

                        // Wrap expression as a simple eval function with the detected type
                        let source = format!(
                            r#"
(export (fn eval () {}
  {}))
"#,
                            return_type, expr
                        );
                        info!("[WRAP] Wrapped source (eval function): {}", source.trim());

                        Value::String(source)
                    },
                )?
                // parse-and-wrap: func(params: tuple<string, list<u8>>) -> result<string, string>
                // Parses input for imports, generates source with import declarations.
                // Just returns the source - compose-packages handles loading deps separately.
                .func_typed(
                    "parse-and-wrap",
                    |_ctx: &mut Ctx<'_, ActorStore>, input: Value| {
                        let ok_type = ValueType::String;
                        let err_type = ValueType::String;

                        // Extract body bytes from input
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
                                    return Value::Result {
                                        ok_type: ok_type.clone(),
                                        err_type: err_type.clone(),
                                        value: Err(Box::new(Value::String(
                                            "Expected list<u8> as second tuple element".to_string(),
                                        ))),
                                    };
                                }
                            },
                            _ => {
                                return Value::Result {
                                    ok_type: ok_type.clone(),
                                    err_type: err_type.clone(),
                                    value: Err(Box::new(Value::String(
                                        "Expected tuple with request-id and body".to_string(),
                                    ))),
                                };
                            }
                        };

                        let input_text = String::from_utf8_lossy(&body_bytes).to_string();
                        info!("[PARSE-AND-WRAP] Input: {}", input_text);

                        // Parse imports and expression
                        let (imports, expression) = parse_imports_and_expression(&input_text);
                        info!("[PARSE-AND-WRAP] Found {} imports", imports.len());

                        // Build import declarations by loading and inspecting each module
                        let mut import_declarations = String::new();

                        for (interface, path) in &imports {
                            info!("[PARSE-AND-WRAP] Processing import: {} from {}", interface, path);

                            // Read the WASM file
                            let resolved_path = if Path::new(path).is_absolute() {
                                path.clone()
                            } else {
                                match std::env::current_dir() {
                                    Ok(cwd) => cwd.join(path).to_string_lossy().to_string(),
                                    Err(e) => {
                                        return Value::Result {
                                            ok_type: ok_type.clone(),
                                            err_type: err_type.clone(),
                                            value: Err(Box::new(Value::String(format!(
                                                "Failed to get CWD: {}",
                                                e
                                            )))),
                                        };
                                    }
                                }
                            };

                            let wasm_bytes = match fs::read(&resolved_path) {
                                Ok(bytes) => bytes,
                                Err(e) => {
                                    return Value::Result {
                                        ok_type: ok_type.clone(),
                                        err_type: err_type.clone(),
                                        value: Err(Box::new(Value::String(format!(
                                            "Failed to read {}: {}",
                                            resolved_path, e
                                        )))),
                                    };
                                }
                            };

                            info!("[PARSE-AND-WRAP] Loaded {} bytes from {}", wasm_bytes.len(), resolved_path);

                            // Get export metadata
                            let runtime = Runtime::new();
                            let module = match runtime.load_module(&wasm_bytes) {
                                Ok(m) => m,
                                Err(e) => {
                                    return Value::Result {
                                        ok_type: ok_type.clone(),
                                        err_type: err_type.clone(),
                                        value: Err(Box::new(Value::String(format!(
                                            "Failed to load module: {}",
                                            e
                                        )))),
                                    };
                                }
                            };

                            let mut instance = match module.instantiate() {
                                Ok(i) => i,
                                Err(e) => {
                                    return Value::Result {
                                        ok_type: ok_type.clone(),
                                        err_type: err_type.clone(),
                                        value: Err(Box::new(Value::String(format!(
                                            "Failed to instantiate module: {}",
                                            e
                                        )))),
                                    };
                                }
                            };

                            let metadata = match instance.types() {
                                Ok(m) => m,
                                Err(e) => {
                                    info!("[PARSE-AND-WRAP] No metadata for {}: {}", path, e);
                                    // Skip this import if no metadata
                                    continue;
                                }
                            };

                            // Generate import declarations for exports matching the requested interface
                            let exports = extract_exports_from_arena(&metadata);
                            for sig in &exports {
                                if sig.interface == *interface {
                                    let params = sig
                                        .params
                                        .iter()
                                        .map(|p| format!("({} {})", p.name, type_desc_to_wisp(&p.ty)))
                                        .collect::<Vec<_>>()
                                        .join(" ");

                                    let result = sig
                                        .results
                                        .first()
                                        .map(|t| type_desc_to_wisp(t))
                                        .unwrap_or_else(|| "unit".to_string());

                                    // Generate Wisp import declaration
                                    let decl = format!(
                                        "(import {} {} ({}) {})\n",
                                        interface, sig.name, params, result
                                    );
                                    info!("[PARSE-AND-WRAP] Adding import: {}", decl.trim());
                                    import_declarations.push_str(&decl);
                                }
                            }
                        }

                        // Detect return type from expression
                        let return_type = detect_expr_type(&expression);
                        info!("[PARSE-AND-WRAP] Detected return type: {}", return_type);

                        // Generate wrapped source with imports
                        let source = if import_declarations.is_empty() {
                            format!(
                                "(export (fn eval () {}\n  {}))",
                                return_type,
                                expression.trim()
                            )
                        } else {
                            format!(
                                "{}\n(export (fn eval () {}\n  {}))",
                                import_declarations.trim(),
                                return_type,
                                expression.trim()
                            )
                        };
                        info!("[PARSE-AND-WRAP] Generated source:\n{}", source);

                        Value::Result {
                            ok_type,
                            err_type,
                            value: Ok(Box::new(Value::String(source))),
                        }
                    },
                )?;
            ctx.mark_satisfied("wisp:repl/helpers");
        }

        // Setup wisp:compose/packages interface
        if !ctx.is_satisfied("wisp:compose/packages") {
            builder
                .interface("wisp:compose/packages")?
                // compose-packages: func(params: tuple<list<u8>, tuple<string, list<u8>>>) -> result<list<u8>, string>
                // Takes (main-wasm-bytes, original-params) where original-params is (request-id, body-bytes)
                .func_typed(
                    "compose-packages",
                    |_ctx: &mut Ctx<'_, ActorStore>, input: Value| {
                        // Input is tuple<list<u8>, tuple<string, list<u8>>>
                        // - main WASM bytes
                        // - original params tuple (request-id, body-bytes)
                        let (main_wasm, original_input) = match &input {
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

                                // Extract original input bytes from params tuple
                                let original_input: Vec<u8> = match &items[1] {
                                    // If it's a tuple (request-id, body-bytes), extract body-bytes
                                    Value::Tuple(param_items) if param_items.len() >= 2 => {
                                        match &param_items[1] {
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
                                                        "Expected list<u8> in params tuple".to_string(),
                                                    ))),
                                                };
                                            }
                                        }
                                    }
                                    // If it's directly a list, use it
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
                                                "Expected tuple or list<u8> for original input".to_string(),
                                            ))),
                                        };
                                    }
                                };

                                (main_wasm, original_input)
                            }
                            _ => {
                                return Value::Result {
                                    ok_type: ValueType::List(Box::new(ValueType::U8)),
                                    err_type: ValueType::String,
                                    value: Err(Box::new(Value::String(
                                        "Expected tuple<list<u8>, ...>".to_string(),
                                    ))),
                                };
                            }
                        };

                        // Parse imports from original input
                        let input_text = String::from_utf8_lossy(&original_input).to_string();
                        let (imports, _expression) = parse_imports_and_expression(&input_text);

                        info!(
                            "[COMPOSE] Composing main ({} bytes) with {} imports",
                            main_wasm.len(),
                            imports.len()
                        );

                        // If no imports, just return main as-is
                        if imports.is_empty() {
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

                        // Load dependency modules from paths
                        let mut deps: Vec<(String, Vec<u8>)> = Vec::new();
                        for (interface, path) in &imports {
                            let resolved_path = if Path::new(path).is_absolute() {
                                path.clone()
                            } else {
                                match std::env::current_dir() {
                                    Ok(cwd) => cwd.join(path).to_string_lossy().to_string(),
                                    Err(e) => {
                                        return Value::Result {
                                            ok_type: ValueType::List(Box::new(ValueType::U8)),
                                            err_type: ValueType::String,
                                            value: Err(Box::new(Value::String(format!(
                                                "Failed to get CWD: {}",
                                                e
                                            )))),
                                        };
                                    }
                                }
                            };

                            let wasm_bytes = match fs::read(&resolved_path) {
                                Ok(bytes) => bytes,
                                Err(e) => {
                                    return Value::Result {
                                        ok_type: ValueType::List(Box::new(ValueType::U8)),
                                        err_type: ValueType::String,
                                        value: Err(Box::new(Value::String(format!(
                                            "Failed to read {}: {}",
                                            resolved_path, e
                                        )))),
                                    };
                                }
                            };

                            info!("[COMPOSE] Loaded {} bytes from {} for interface {}", wasm_bytes.len(), resolved_path, interface);
                            deps.push((interface.clone(), wasm_bytes));
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

        // Setup wisp:filesystem/runtime interface
        if !ctx.is_satisfied("wisp:filesystem/runtime") {
            builder
                .interface("wisp:filesystem/runtime")?
                // read-file: func(path: string) -> result<list<u8>, string>
                .func_typed(
                    "read-file",
                    |_ctx: &mut Ctx<'_, ActorStore>, input: Value| {
                        let path = match input {
                            Value::String(s) => s,
                            _ => {
                                return Value::Result {
                                    ok_type: ValueType::List(Box::new(ValueType::U8)),
                                    err_type: ValueType::String,
                                    value: Err(Box::new(Value::String(
                                        "Expected string for path".to_string(),
                                    ))),
                                };
                            }
                        };

                        info!("[READ-FILE] Reading: {}", path);

                        // Resolve path relative to CWD
                        let resolved_path = if Path::new(&path).is_absolute() {
                            path.clone()
                        } else {
                            match std::env::current_dir() {
                                Ok(cwd) => cwd.join(&path).to_string_lossy().to_string(),
                                Err(e) => {
                                    return Value::Result {
                                        ok_type: ValueType::List(Box::new(ValueType::U8)),
                                        err_type: ValueType::String,
                                        value: Err(Box::new(Value::String(format!(
                                            "Failed to get CWD: {}",
                                            e
                                        )))),
                                    };
                                }
                            }
                        };

                        match fs::read(&resolved_path) {
                            Ok(bytes) => {
                                info!("[READ-FILE] Success: {} bytes from {}", bytes.len(), resolved_path);
                                let values: Vec<Value> = bytes.into_iter().map(Value::U8).collect();
                                Value::Result {
                                    ok_type: ValueType::List(Box::new(ValueType::U8)),
                                    err_type: ValueType::String,
                                    value: Ok(Box::new(Value::List {
                                        elem_type: ValueType::U8,
                                        items: values,
                                    })),
                                }
                            }
                            Err(e) => {
                                info!("[READ-FILE] Error: {}", e);
                                Value::Result {
                                    ok_type: ValueType::List(Box::new(ValueType::U8)),
                                    err_type: ValueType::String,
                                    value: Err(Box::new(Value::String(format!(
                                        "Failed to read {}: {}",
                                        resolved_path, e
                                    )))),
                                }
                            }
                        }
                    },
                )?;
            ctx.mark_satisfied("wisp:filesystem/runtime");
        }

        // Setup wisp:imports/metadata interface
        if !ctx.is_satisfied("wisp:imports/metadata") {
            builder
                .interface("wisp:imports/metadata")?
                // get-package-exports: func(wasm: list<u8>) -> result<list<string>, string>
                // Returns exports as S-expression strings:
                // "(export \"interface\" \"func-name\" \"((param1 type1) (param2 type2))\" \"result-type\")"
                .func_typed(
                    "get-package-exports",
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
                                    ok_type: ValueType::List(Box::new(ValueType::String)),
                                    err_type: ValueType::String,
                                    value: Err(Box::new(Value::String(
                                        "Expected list<u8> for WASM bytes".to_string(),
                                    ))),
                                };
                            }
                        };

                        info!(
                            "[GET-EXPORTS] Extracting metadata from {} bytes of WASM",
                            wasm_bytes.len()
                        );

                        // Use Pack's Runtime to load the module and get types
                        let runtime = Runtime::new();
                        let module = match runtime.load_module(&wasm_bytes) {
                            Ok(m) => m,
                            Err(e) => {
                                info!("[GET-EXPORTS] Failed to load module: {}", e);
                                return Value::Result {
                                    ok_type: ValueType::List(Box::new(ValueType::String)),
                                    err_type: ValueType::String,
                                    value: Err(Box::new(Value::String(format!(
                                        "Failed to load module: {}",
                                        e
                                    )))),
                                };
                            }
                        };

                        // Instantiate without host functions (just need to call __pack_types)
                        let mut instance = match module.instantiate() {
                            Ok(i) => i,
                            Err(e) => {
                                info!("[GET-EXPORTS] Failed to instantiate: {}", e);
                                return Value::Result {
                                    ok_type: ValueType::List(Box::new(ValueType::String)),
                                    err_type: ValueType::String,
                                    value: Err(Box::new(Value::String(format!(
                                        "Failed to instantiate module: {}",
                                        e
                                    )))),
                                };
                            }
                        };

                        // Get the metadata
                        let metadata = match instance.types() {
                            Ok(m) => m,
                            Err(e) => {
                                info!("[GET-EXPORTS] No metadata found: {}", e);
                                return Value::Result {
                                    ok_type: ValueType::List(Box::new(ValueType::String)),
                                    err_type: ValueType::String,
                                    value: Err(Box::new(Value::String(format!(
                                        "Package has no metadata: {}",
                                        e
                                    )))),
                                };
                            }
                        };

                        // Convert exports to S-expression strings
                        let exports = extract_exports_from_arena(&metadata);
                        let export_strings: Vec<Value> = exports
                            .iter()
                            .map(|sig| {
                                // Format params as ((name type) (name type))
                                let params = sig
                                    .params
                                    .iter()
                                    .map(|p| format!("({} {})", p.name, type_desc_to_wisp(&p.ty)))
                                    .collect::<Vec<_>>()
                                    .join(" ");

                                // Format results (usually just one)
                                let result = sig
                                    .results
                                    .first()
                                    .map(|t| type_desc_to_wisp(t))
                                    .unwrap_or_else(|| "unit".to_string());

                                // Format: (export "interface" "name" "(params)" "result")
                                let export_str = format!(
                                    "(export \"{}\" \"{}\" \"({})\" \"{}\")",
                                    sig.interface, sig.name, params, result
                                );
                                Value::String(export_str)
                            })
                            .collect();

                        info!(
                            "[GET-EXPORTS] Found {} exports",
                            export_strings.len()
                        );

                        Value::Result {
                            ok_type: ValueType::List(Box::new(ValueType::String)),
                            err_type: ValueType::String,
                            value: Ok(Box::new(Value::List {
                                elem_type: ValueType::String,
                                items: export_strings,
                            })),
                        }
                    },
                )?;
            ctx.mark_satisfied("wisp:imports/metadata");
        }

        Ok(())
    }
}

/// Convert a Pack Type to a Wisp type string.
fn type_desc_to_wisp(ty: &TypeDesc) -> String {
    use pack::types::Type;
    match ty {
        Type::Unit => "unit".to_string(),
        Type::Bool => "bool".to_string(),
        Type::U8 => "u8".to_string(),
        Type::U16 => "u16".to_string(),
        Type::U32 => "u32".to_string(),
        Type::U64 => "u64".to_string(),
        Type::S8 => "s8".to_string(),
        Type::S16 => "s16".to_string(),
        Type::S32 => "s32".to_string(),
        Type::S64 => "s64".to_string(),
        Type::F32 => "f32".to_string(),
        Type::F64 => "f64".to_string(),
        Type::Char => "char".to_string(),
        Type::String => "string".to_string(),
        Type::List(inner) => format!("(list {})", type_desc_to_wisp(inner)),
        Type::Option(inner) => format!("(option {})", type_desc_to_wisp(inner)),
        Type::Result { ok, err } => format!(
            "(result {} {})",
            type_desc_to_wisp(ok),
            type_desc_to_wisp(err)
        ),
        Type::Tuple(types) => {
            let inner = types
                .iter()
                .map(type_desc_to_wisp)
                .collect::<Vec<_>>()
                .join(" ");
            format!("(tuple {})", inner)
        }
        Type::Ref(path) => {
            // Named type reference - use the type name
            path.name().unwrap_or("unknown").to_string()
        }
        Type::Value => "value".to_string(),
    }
}

/// A function signature with its interface name.
struct ExportedFunc {
    interface: String,
    name: String,
    params: Vec<pack::types::Param>,
    results: Vec<pack::types::Type>,
}

/// Extract exports from a Pack Arena metadata structure.
///
/// The Arena structure from `decode_metadata` is:
/// ```text
/// Arena("package")
/// └── Arena("exports")
///     ├── Arena("interface1") → functions
///     └── Arena("interface2") → functions
/// ```
fn extract_exports_from_arena(arena: &pack::types::Arena) -> Vec<ExportedFunc> {
    let mut result = Vec::new();

    // Find the "exports" child arena
    for child in &arena.children {
        if child.name == "exports" {
            // Each child of this arena is an interface
            for interface_arena in &child.children {
                let interface_name = &interface_arena.name;
                for func in &interface_arena.functions {
                    result.push(ExportedFunc {
                        interface: interface_name.clone(),
                        name: func.name.clone(),
                        params: func.params.clone(),
                        results: func.results.clone(),
                    });
                }
            }
        }
    }

    result
}

/// Parse input text for imports and extract the expression.
///
/// Imports have the form: `(import interface-name from "file.wasm")`
/// Example: `(import colin:math/ops from "examples/math.wasm")`
///
/// Returns: (imports, expression) where imports is Vec<(interface, path)>
fn parse_imports_and_expression(input: &str) -> (Vec<(String, String)>, String) {
    let mut imports: Vec<(String, String)> = Vec::new();
    let mut expression_parts: Vec<&str> = Vec::new();

    // Split input into lines/expressions
    // We parse each top-level expression
    let mut remaining = input.trim();

    while !remaining.is_empty() {
        // Skip whitespace
        remaining = remaining.trim_start();
        if remaining.is_empty() {
            break;
        }

        // Check if this is an import expression
        if remaining.starts_with("(import ") {
            // Find matching close paren
            if let Some(end) = find_matching_paren(remaining) {
                let import_expr = &remaining[..=end];
                remaining = &remaining[end + 1..];

                // Parse the import expression
                // Format: (import interface from "path")
                if let Some((interface, path)) = parse_import_expr(import_expr) {
                    imports.push((interface, path));
                }
            } else {
                // Malformed, treat as expression
                expression_parts.push(remaining);
                break;
            }
        } else if remaining.starts_with('(') {
            // This is the expression (or another expression)
            if let Some(end) = find_matching_paren(remaining) {
                expression_parts.push(&remaining[..=end]);
                remaining = &remaining[end + 1..];
            } else {
                // Malformed, include the rest
                expression_parts.push(remaining);
                break;
            }
        } else {
            // Non-parenthesized content - could be a bare identifier or number
            // Find next whitespace or paren
            let end = remaining
                .find(|c: char| c.is_whitespace() || c == '(')
                .unwrap_or(remaining.len());
            if end > 0 {
                expression_parts.push(&remaining[..end]);
            }
            remaining = &remaining[end..];
        }
    }

    let expression = expression_parts.join(" ");
    (imports, expression)
}

/// Find the index of the closing paren matching the opening paren at index 0.
fn find_matching_paren(s: &str) -> Option<usize> {
    if !s.starts_with('(') {
        return None;
    }

    let mut depth = 0;
    let mut in_string = false;
    let mut escape_next = false;

    for (i, c) in s.char_indices() {
        if escape_next {
            escape_next = false;
            continue;
        }

        match c {
            '\\' if in_string => {
                escape_next = true;
            }
            '"' => {
                in_string = !in_string;
            }
            '(' if !in_string => {
                depth += 1;
            }
            ')' if !in_string => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => {}
        }
    }

    None
}

/// Parse an import expression.
/// Format: (import interface-name from "path")
/// Returns: Some((interface, path)) or None if malformed
fn parse_import_expr(s: &str) -> Option<(String, String)> {
    // Remove outer parens
    let inner = s.trim().strip_prefix('(')?.strip_suffix(')')?.trim();

    // Remove "import " prefix
    let rest = inner.strip_prefix("import ")?.trim();

    // Split by " from "
    let parts: Vec<&str> = rest.splitn(2, " from ").collect();
    if parts.len() != 2 {
        return None;
    }

    let interface = parts[0].trim().to_string();
    let path_with_quotes = parts[1].trim();

    // Remove quotes from path
    let path = path_with_quotes
        .strip_prefix('"')?
        .strip_suffix('"')?
        .to_string();

    Some((interface, path))
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

    #[test]
    fn test_parse_imports_simple() {
        let input = r#"(import colin:math/ops from "examples/math.wasm")
(i32.add (i32.const 1) (i32.const 2))"#;

        let (imports, expr) = parse_imports_and_expression(input);

        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].0, "colin:math/ops");
        assert_eq!(imports[0].1, "examples/math.wasm");
        assert_eq!(expr, "(i32.add (i32.const 1) (i32.const 2))");
    }

    #[test]
    fn test_parse_imports_multiple() {
        let input = r#"(import colin:math/ops from "math.wasm")
(import colin:util/helpers from "utils.wasm")
(add (square (i32.const 5)))"#;

        let (imports, expr) = parse_imports_and_expression(input);

        assert_eq!(imports.len(), 2);
        assert_eq!(imports[0].0, "colin:math/ops");
        assert_eq!(imports[0].1, "math.wasm");
        assert_eq!(imports[1].0, "colin:util/helpers");
        assert_eq!(imports[1].1, "utils.wasm");
        assert_eq!(expr, "(add (square (i32.const 5)))");
    }

    #[test]
    fn test_parse_no_imports() {
        let input = "(i32.add (i32.const 40) (i32.const 2))";
        let (imports, expr) = parse_imports_and_expression(input);

        assert_eq!(imports.len(), 0);
        assert_eq!(expr, "(i32.add (i32.const 40) (i32.const 2))");
    }

    #[test]
    fn test_find_matching_paren() {
        assert_eq!(find_matching_paren("(a b c)"), Some(6));
        assert_eq!(find_matching_paren("(a (b c) d)"), Some(10));
        assert_eq!(find_matching_paren(r#"(a ")" b)"#), Some(8));
        assert_eq!(find_matching_paren("no paren"), None);
    }

    #[test]
    fn test_parse_import_expr() {
        let expr = r#"(import colin:math/ops from "examples/math.wasm")"#;
        let result = parse_import_expr(expr);
        assert!(result.is_some());
        let (interface, path) = result.unwrap();
        assert_eq!(interface, "colin:math/ops");
        assert_eq!(path, "examples/math.wasm");
    }

    #[test]
    fn test_interface_hashes_returns_all_interfaces() {
        let handler = WispHandler::new();
        let hashes = handler.interface_hashes();

        // Should have 5 interfaces
        assert_eq!(hashes.len(), 5);

        // Check all interface names are present
        let names: Vec<&str> = hashes.iter().map(|(name, _)| name.as_str()).collect();
        assert!(names.contains(&"wisp:assembler/runtime"));
        assert!(names.contains(&"wisp:repl/helpers"));
        assert!(names.contains(&"wisp:compose/packages"));
        assert!(names.contains(&"wisp:filesystem/runtime"));
        assert!(names.contains(&"wisp:imports/metadata"));
    }

    #[test]
    fn test_interface_hashes_are_deterministic() {
        let handler1 = WispHandler::new();
        let handler2 = WispHandler::new();

        let hashes1 = handler1.interface_hashes();
        let hashes2 = handler2.interface_hashes();

        // Same handler should produce same hashes
        for (h1, h2) in hashes1.iter().zip(hashes2.iter()) {
            assert_eq!(h1.0, h2.0, "Interface names should match");
            assert_eq!(h1.1, h2.1, "Interface hashes should match");
        }
    }

    #[test]
    fn test_interface_hashes_differ_between_interfaces() {
        let handler = WispHandler::new();
        let hashes = handler.interface_hashes();

        // Each interface should have a unique hash
        let hash_values: Vec<_> = hashes.iter().map(|(_, h)| h).collect();
        for (i, h1) in hash_values.iter().enumerate() {
            for (j, h2) in hash_values.iter().enumerate() {
                if i != j {
                    assert_ne!(h1, h2, "Different interfaces should have different hashes");
                }
            }
        }
    }

    #[test]
    fn test_imports_derived_from_interfaces() {
        use theater::handler::Handler;

        let handler = WispHandler::new();
        let imports = handler.imports().expect("Should have imports");
        let interfaces = handler.interfaces();

        // imports() should return the same names as interfaces()
        assert_eq!(imports.len(), interfaces.len());
        for iface in interfaces {
            assert!(imports.contains(&iface.name().to_string()));
        }
    }
}
