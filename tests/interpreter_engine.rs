//! Preflight for Theater's engine/host-import boundary. This deliberately does
//! not claim actor lifecycle or chain replay coverage.
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use packr_core::abi::{Value, ValueType};
use packr_core::{HostImports, WasmEngine, call_with_value, host_fn};
use packr_wasmtime::{WasmtimeEngine, WasmtimeInstance};

fn source_result(result: Result<String, String>) -> Value {
    Value::Result {
        ok_type: ValueType::String,
        err_type: ValueType::String,
        value: result
            .map(|s| Box::new(Value::String(s)))
            .map_err(|s| Box::new(Value::String(s))),
    }
}

fn imports(calls: Arc<Mutex<Vec<String>>>) -> HostImports {
    let mut imports = HostImports::new();
    imports.define(
        "wisp-source",
        "resolve-path",
        host_fn(|input| async move {
            let Value::Tuple(args) = input else {
                panic!("resolve-path input: {input:?}")
            };
            let [Value::String(_base), Value::String(path)] = args.as_slice() else {
                panic!("resolve-path args: {args:?}")
            };
            Ok(source_result(if path == "library.wisp" {
                Ok("/bundle/library.wisp".into())
            } else {
                Err("source is not in the actor bundle".into())
            }))
        }),
    );
    imports.define(
        "wisp-source",
        "read-source",
        host_fn(move |input| {
            let calls = calls.clone();
            async move {
                let Value::String(path) = input else {
                    panic!("read-source input: {input:?}")
                };
                calls.lock().unwrap().push(path.clone());
                Ok(source_result(if path == "/bundle/library.wisp" {
                    Ok("(fn increment ((x s32)) s32 (i32.add x 1))".into())
                } else {
                    Err("source is not in the actor bundle".into())
                }))
            }
        }),
    );
    // The evaluator now imports the Theater rpc bridge (describe/exports/
    // implements). This preflight never evaluates them, so they only need to exist.
    for name in ["describe", "exports", "implements", "call"] {
        imports.define(
            "theater:simple/rpc",
            name,
            host_fn(|_input| async move { Ok(Value::String("rpc not exercised".into())) }),
        );
    }
    // self bridge (packr has no wildcard-trap; stub what the evaluator imports).
    imports.define(
        "theater:simple/self",
        "self",
        host_fn(|_input| async move { Ok(Value::String("self not exercised".into())) }),
    );
    imports.define(
        "theater:simple/self",
        "log",
        host_fn(|_input| async move { Ok(Value::Tuple(vec![])) }),
    );
    // store bridge — likewise only needs to exist; this preflight never calls it.
    for name in [
        "new",
        "get",
        "get-by-label",
        "list-labels",
        "exists",
        "calculate-total-size",
        "store",
        "label",
        "store-at-label",
    ] {
        imports.define(
            "theater:simple/store",
            name,
            host_fn(|_input| async move { Ok(Value::String("store not exercised".into())) }),
        );
    }
    // runtime bridge — likewise only needs to exist for this preflight.
    for name in [
        "list-actors",
        "get-actor-status",
        "get-actor-state",
        "get-actor-manifest",
        "stop-actor",
        "kill-actor",
        "subscribe-to-spawns",
        "unsubscribe-from-spawns",
        "shutdown-runtime",
    ] {
        imports.define(
            "theater:simple/runtime",
            name,
            host_fn(|_input| async move { Ok(Value::String("runtime not exercised".into())) }),
        );
    }
    // message-server / assembler / timer — exist-only stubs for this preflight.
    for name in [
        "register",
        "send",
        "request",
        "list-outstanding-requests",
        "respond-to-request",
        "cancel-request",
        "open-channel",
        "send-on-channel",
        "close-channel",
    ] {
        imports.define(
            "theater:simple/message-server-host",
            name,
            host_fn(
                |_input| async move { Ok(Value::String("message-server not exercised".into())) },
            ),
        );
    }
    imports.define(
        "wisp:assembler/runtime",
        "wat-to-wasm",
        host_fn(|_input| async move { Ok(Value::String("assembler not exercised".into())) }),
    );
    for name in ["now", "set-interval", "clear-interval"] {
        imports.define(
            "theater:simple/timer",
            name,
            host_fn(|_input| async move { Ok(Value::String("timer not exercised".into())) }),
        );
    }
    for name in [
        "read-file",
        "exists",
        "list-dir",
        "metadata",
        "write-file",
        "append-file",
        "delete-file",
        "create-dir",
        "remove-dir",
    ] {
        imports.define(
            "theater:simple/filesystem",
            name,
            host_fn(|_input| async move { Ok(Value::String("filesystem not exercised".into())) }),
        );
    }
    for name in [
        "write-stdout",
        "write-stderr",
        "set-raw-mode",
        "get-size",
        "enable-input",
    ] {
        imports.define(
            "theater:simple/terminal",
            name,
            host_fn(|_input| async move { Ok(Value::String("terminal not exercised".into())) }),
        );
    }
    imports.define(
        "theater:simple/http-client",
        "request",
        host_fn(|_input| async move { Ok(Value::String("http-client not exercised".into())) }),
    );
    for name in [
        "connect",
        "listen",
        "accept",
        "activate",
        "set-active",
        "transfer",
        "transfer-async",
        "peer-address",
        "is-tls",
        "send",
        "receive",
        "close",
        "close-listener",
        "upgrade-to-tls-server",
        "upgrade-to-tls-client",
    ] {
        imports.define(
            "theater:simple/tcp",
            name,
            host_fn(|_input| async move { Ok(Value::String("tcp not exercised".into())) }),
        );
    }
    for name in ["run", "stop", "rm", "list"] {
        imports.define(
            "theater:simple/podman",
            name,
            host_fn(|_input| async move { Ok(Value::String("podman not exercised".into())) }),
        );
    }
    for name in ["monitor", "unmonitor", "link", "unlink"] {
        imports.define(
            "theater:simple/lifecycle",
            name,
            host_fn(|_input| async move { Ok(Value::String("lifecycle not exercised".into())) }),
        );
    }
    imports
}

async fn evaluate(instance: &mut WasmtimeInstance, source: &str) -> String {
    // Theater's call_function_with_value wraps a single argument in a tuple.
    let result = call_with_value(
        instance,
        "evaluate",
        &Value::Tuple(vec![Value::String(source.into())]),
    )
    .await
    .unwrap();
    let Value::String(result) = result else {
        panic!("evaluate output: {result:?}")
    };
    result
}

#[test]
fn test_interpreter_capture_based_engine_session_and_imports() {
    tokio::runtime::Builder::new_current_thread()
        .build()
        .unwrap()
        .block_on(engine_session());
}

async fn engine_session() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    // Compiling the evaluator recurses per expression node and exceeds the default
    // 2 MiB test stack, so build it on a large stack.
    let package = std::thread::scope(|s| {
        std::thread::Builder::new()
            .stack_size(1 << 30)
            .spawn_scoped(s, || {
                granite::compiler::compile(
                    &root.join("wisp/interpreter/evaluator.wisp"),
                    &root.join(format!(
                        "target/interpreter-engine/{}/evaluator",
                        std::process::id()
                    )),
                    granite::compiler::EmitOptions::default(),
                )
                .unwrap()
            })
            .expect("spawn big-stack compile thread")
            .join()
            .expect("big-stack compile thread panicked")
    });
    let wasm = std::fs::read(package.wasm).unwrap();
    let metadata = packr_core::metadata::metadata_with_hashes_from_module(&wasm)
        .unwrap()
        .expect("embedded metadata readable by Theater");
    assert!(!metadata.import_hashes.is_empty());
    let engine = WasmtimeEngine::new();
    let module = engine.compile(&wasm).await.unwrap();
    let calls = Arc::new(Mutex::new(Vec::new()));
    let mut first = engine
        .instantiate(&module, imports(calls.clone()))
        .await
        .unwrap();
    assert_eq!(
        evaluate(&mut first, "(define add-two (lambda (x) (+ x 2)))").await,
        "#<closure>"
    );
    assert_eq!(evaluate(&mut first, "(add-two 40)").await, "42");
    assert_eq!(
        evaluate(&mut first, "(include \"library.wisp\") (increment 41)").await,
        "42"
    );
    assert_eq!(*calls.lock().unwrap(), ["/bundle/library.wisp"]);
    assert!(
        evaluate(&mut first, "(define marker 0) (include \"missing.wisp\")")
            .await
            .starts_with("error:")
    );
    assert!(evaluate(&mut first, "marker").await.contains("unbound"));
    assert_eq!(evaluate(&mut first, "(add-two 40)").await, "42");
    assert!(evaluate(&mut first, "(/ 1 0)").await.starts_with("error:"));
    let mut second = engine.instantiate(&module, imports(calls)).await.unwrap();
    assert!(
        evaluate(&mut second, "(add-two 40)")
            .await
            .contains("unbound")
    );
    assert_eq!(evaluate(&mut first, "(increment 41)").await, "42");
}
