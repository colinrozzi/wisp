//! Preflight for Theater's engine/host-import boundary. This deliberately does
//! not claim actor lifecycle or chain replay coverage.
use std::collections::HashMap;
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

// Editable source files the read-file mock serves (path -> text), so a test can
// change a library between reads; consulted before the fixed greet module.
type Files = Arc<Mutex<HashMap<String, String>>>;

fn imports(calls: Arc<Mutex<Vec<String>>>, files: Files) -> HostImports {
    let mut imports = HostImports::new();
    // Clone before the read-source closure moves `calls`; the filesystem read-file
    // mock (what `require` loads through) logs its reads here too.
    let fs_calls = calls.clone();
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
        "spawn",
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
    // read-file is the capability `require` loads module source through. Serve a
    // greet module for "lib/greet.wisp" as ok(list<u8>); log the reads so the test
    // can assert load-once. Everything else is an exist-only stub.
    {
        let calls = fs_calls.clone();
        imports.define(
            "theater:simple/filesystem",
            "read-file",
            host_fn(move |input| {
                let calls = calls.clone();
                let files = files.clone();
                async move {
                    let Value::String(path) = input else {
                        panic!("read-file input: {input:?}")
                    };
                    calls.lock().unwrap().push(path.clone());
                    let bytes = |s: &str| Value::Result {
                        ok_type: ValueType::List(Box::new(ValueType::U8)),
                        err_type: ValueType::String,
                        value: Ok(Box::new(Value::List {
                            elem_type: ValueType::U8,
                            items: s.bytes().map(Value::U8).collect(),
                        })),
                    };
                    let edited = files.lock().unwrap().get(&path).cloned();
                    Ok(if let Some(text) = edited {
                        bytes(&text)
                    } else if path == "lib/greet.wisp" {
                        bytes("(module greet (provide greeting) (define greeting 7))")
                    } else {
                        Value::Result {
                            ok_type: ValueType::List(Box::new(ValueType::U8)),
                            err_type: ValueType::String,
                            value: Err(Box::new(Value::String("no such file".into()))),
                        }
                    })
                }
            }),
        );
    }
    for name in [
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
    // request is the capability `require` fetches a URL library through. Serve a
    // netmod module as the response body (ok http-response, option body = some);
    // log the fetched url so the test can assert it went over the network.
    {
        let calls = fs_calls.clone();
        imports.define(
            "theater:simple/http-client",
            "request",
            host_fn(move |input| {
                let calls = calls.clone();
                async move {
                    let url = match &input {
                        Value::Record { fields, .. } => fields
                            .iter()
                            .find(|(n, _)| n == "url")
                            .and_then(|(_, v)| match v {
                                Value::String(s) => Some(s.clone()),
                                _ => None,
                            })
                            .unwrap_or_default(),
                        _ => String::new(),
                    };
                    calls.lock().unwrap().push(format!("GET {url}"));
                    let src = "(module netmod (provide answer) (define answer 42))";
                    Ok(Value::Result {
                        ok_type: ValueType::Record("http-response".into()),
                        err_type: ValueType::String,
                        value: Ok(Box::new(Value::Record {
                            type_name: "http-response".into(),
                            fields: vec![
                                ("status".into(), Value::U16(200)),
                                (
                                    "headers".into(),
                                    Value::List {
                                        elem_type: ValueType::Record("http-header".into()),
                                        items: vec![],
                                    },
                                ),
                                (
                                    "body".into(),
                                    Value::Option {
                                        inner_type: ValueType::List(Box::new(ValueType::U8)),
                                        value: Some(Box::new(Value::List {
                                            elem_type: ValueType::U8,
                                            items: src.bytes().map(Value::U8).collect(),
                                        })),
                                    },
                                ),
                            ],
                        })),
                    })
                }
            }),
        );
    }
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
    for name in [
        "bind",
        "close",
        "connect",
        "disconnect",
        "local-addr",
        "connected-peer",
        "send-to",
        "send",
        "recv-from",
        "set-active",
        "set-recv-timeout",
    ] {
        imports.define(
            "theater:simple/udp",
            name,
            host_fn(|_input| async move { Ok(Value::String("udp not exercised".into())) }),
        );
    }
    for name in [
        "spawn",
        "run",
        "write-stdin",
        "close-stdin",
        "read",
        "set-active",
        "resize",
        "signal",
        "kill",
        "status",
        "wait",
        "os-pid",
        "list",
        "close",
        "transfer",
    ] {
        imports.define(
            "theater:simple/process",
            name,
            host_fn(|_input| async move { Ok(Value::String("process not exercised".into())) }),
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
    let files = Files::default();
    let mut first = engine
        .instantiate(&module, imports(calls.clone(), files.clone()))
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
    // require loads a module from an EXPLICIT source read through the filesystem
    // capability (not the bundle), and binds only its provides.
    assert_eq!(
        evaluate(&mut first, "(require greet \"lib/greet.wisp\") greeting").await,
        "7"
    );
    assert!(
        calls
            .lock()
            .unwrap()
            .contains(&"lib/greet.wisp".to_string())
    );
    // A second require reuses the loaded instance (no second read) — load-once.
    assert_eq!(evaluate(&mut first, "(require greet) greeting").await, "7");
    assert_eq!(
        calls
            .lock()
            .unwrap()
            .iter()
            .filter(|p| p.as_str() == "lib/greet.wisp")
            .count(),
        1
    );
    // A URL source fetches over the http-client capability instead of the filesystem.
    assert_eq!(
        evaluate(
            &mut first,
            "(require netmod \"https://example.test/netmod.wisp\") answer"
        )
        .await,
        "42"
    );
    assert!(
        calls
            .lock()
            .unwrap()
            .contains(&"GET https://example.test/netmod.wisp".to_string())
    );
    let mut second = engine
        .instantiate(&module, imports(calls.clone(), Files::default()))
        .await
        .unwrap();
    assert!(
        evaluate(&mut second, "(add-two 40)")
            .await
            .contains("unbound")
    );
    assert_eq!(evaluate(&mut first, "(increment 41)").await, "42");
    reload_session(&mut first, &files, &calls).await;
}

// (reload NAME) re-reads a loaded module from its original source and refreshes
// every importer in the session; a failed reload changes nothing.
async fn reload_session(
    first: &mut WasmtimeInstance,
    files: &Files,
    calls: &Arc<Mutex<Vec<String>>>,
) {
    let path = "lib/counter.wisp";
    let set = |text: &str| {
        files
            .lock()
            .unwrap()
            .insert(path.to_string(), text.to_string());
    };
    let reads = || calls.lock().unwrap().iter().filter(|p| *p == path).count();
    set(
        "(module counter (provide value bump) (define value 1) (define bump (lambda (x) (+ x value))))",
    );
    assert_eq!(
        evaluate(first, "(require counter \"lib/counter.wisp\") (bump 1)").await,
        "2"
    );
    // A second importer, besides main: user holds its own copy of bump.
    assert_eq!(
        evaluate(
            first,
            "(module user (require counter) (provide via) (define via (lambda () (bump 2)))) (require user) (via)"
        )
        .await,
        "3"
    );
    // A hand copy is not an import, so reload leaves it on the old code (its body
    // still resolves top-level names in counter's current env).
    evaluate(first, "(define copied bump)").await;
    // require stays load-once: the edit is not seen until reload.
    set(
        "(module counter (provide value bump) (define value 10) (define bump (lambda (x) (* x value))))",
    );
    assert_eq!(evaluate(first, "(require counter) value").await, "1");
    assert_eq!(reads(), 1);
    // reload re-reads the remembered path and rebinds main's AND user's copies.
    assert_eq!(evaluate(first, "(reload counter)").await, "()");
    assert_eq!(reads(), 2);
    assert_eq!(evaluate(first, "value").await, "10");
    assert_eq!(evaluate(first, "(bump 1)").await, "10");
    assert_eq!(evaluate(first, "(via)").await, "20");
    assert_eq!(evaluate(first, "(copied 1)").await, "11");
    // A name the new version no longer provides is unbound everywhere and reported.
    set("(module counter (provide value) (define value 20))");
    assert_eq!(evaluate(first, "(reload counter)").await, "(bump)");
    assert_eq!(evaluate(first, "value").await, "20");
    assert!(
        evaluate(first, "bump")
            .await
            .contains("unbound symbol: bump")
    );
    assert!(
        evaluate(first, "(via)")
            .await
            .contains("unbound symbol: bump")
    );
    // Failed reloads leave the old module and every binding untouched.
    for (text, error) in [
        (
            "(module counter (provide value) (define value (car 5)))",
            "error:",
        ),
        ("42", "error: source did not define module counter"),
        (
            "(module counter (provide value missing) (define value 30))",
            "error: module counter provides undefined name missing",
        ),
    ] {
        set(text);
        let result = evaluate(first, "(reload counter)").await;
        assert!(result.starts_with(error), "{text}: {result}");
        assert_eq!(evaluate(first, "value").await, "20", "{text}");
    }
    files.lock().unwrap().remove(path);
    assert_eq!(
        evaluate(first, "(reload counter)").await,
        "error: cannot read source: lib/counter.wisp"
    );
    assert_eq!(evaluate(first, "value").await, "20");
    // The old module is still registered and reloadable once the source is fixed.
    set("(module counter (provide value) (define value 40))");
    assert_eq!(evaluate(first, "(reload counter)").await, "()");
    assert_eq!(evaluate(first, "value").await, "40");
}
