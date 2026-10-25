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
        "sync-file",
        "sync-dir",
        "rename",
        "write-file-durable",
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
    imports.define(
        "wisp-sessions",
        "list-sessions",
        host_fn(|_input| async move { Ok(Value::String("sessions not exercised".into())) }),
    );
    imports.define(
        "wisp-events",
        "event-recorded",
        host_fn(|_input| async move { Ok(Value::Tuple(vec![])) }),
    );
    for name in ["watch", "unwatch", "head", "tail", "verify"] {
        imports.define(
            "wisp-chain",
            name,
            host_fn(|_input| async move { Ok(Value::String("chain not exercised".into())) }),
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
    reload_declarations_session(&mut first, &files).await;
    // A required file that does not read names its path and the error's place.
    files.lock().unwrap().insert(
        "lib/broken.wisp".to_string(),
        "(module broken (provide x)\n  (define x (+ 1 2)\n  (define y 3))".to_string(),
    );
    assert_eq!(
        evaluate(&mut first, "(require broken \"lib/broken.wisp\")").await,
        "error: cannot load module broken: lib/broken.wisp: line 1, column 1: unclosed parenthesis: the one that opens here is never closed"
    );
    failed_require_session(&mut first, &files, &calls).await;
    read_text_session(&mut first, &files).await;
}

// A require that fails leaves nothing behind: the next require of the same name
// reads the file again, and the error names the form that failed.
async fn failed_require_session(
    first: &mut WasmtimeInstance,
    files: &Files,
    calls: &Arc<Mutex<Vec<String>>>,
) {
    let set = |path: &str, text: &str| {
        files
            .lock()
            .unwrap()
            .insert(path.to_string(), text.to_string());
    };
    let reads = |path: &str| calls.lock().unwrap().iter().filter(|p| *p == path).count();
    // The record, global, trait and instance come before the failing form, so a
    // retry must be able to declare them again.
    let good_body = "(provide web-connection make-page)
  (record page (title string))
  (global $hits s32 mut 0)
  (trait (Shown T) (fn shown ((x T)) : string))
  (instance (Shown s32) (fn shown ((x s32)) string \"s32\"))
  (define make-page (lambda (t) (page t)))";
    set(
        "lib/web.wisp",
        &format!(
            "(module web {good_body}\n  (define broken (nope 1))\n  (define web-connection 1))"
        ),
    );
    let failed =
        "error: cannot load module web: lib/web.wisp: in (define broken ...): unbound symbol: nope";
    assert_eq!(
        evaluate(first, "(require web \"lib/web.wisp\")").await,
        failed
    );
    // Not "provides undefined name": the file is read again and fails the same way.
    assert_eq!(
        evaluate(first, "(require web \"lib/web.wisp\")").await,
        failed
    );
    assert_eq!(reads("lib/web.wisp"), 2);
    assert_eq!(
        evaluate(first, "(require web)").await,
        "error: module is not loaded; give its source like (require name \"path\"): web"
    );
    assert!(evaluate(first, "make-page").await.contains("unbound"));
    // Fixed, the same require loads it.
    set(
        "lib/web.wisp",
        &format!("(module web {good_body}\n  (define web-connection 1))"),
    );
    assert_eq!(
        evaluate(first, "(require web \"lib/web.wisp\") web-connection").await,
        "1"
    );
    assert_eq!(
        evaluate(first, "(make-page \"home\")").await,
        "(page \"home\")"
    );
    // A provided name the module never defines is a failed load too.
    set("lib/half.wisp", "(module half (provide a b) (define a 1))");
    assert_eq!(
        evaluate(first, "(require half \"lib/half.wisp\")").await,
        "error: cannot load module half: module half provides undefined name b"
    );
    set(
        "lib/half.wisp",
        "(module half (provide a b) (define a 1) (define b 2))",
    );
    assert_eq!(
        evaluate(first, "(require half \"lib/half.wisp\") (+ a b)").await,
        "3"
    );
    // A dependency's failure is named inside its requirer's, and neither stays.
    set(
        "lib/dep.wisp",
        "(module dep (provide d) (define d (car 5)))",
    );
    set(
        "lib/top.wisp",
        "(module top (require dep \"lib/dep.wisp\") (provide t) (define t d))",
    );
    assert_eq!(
        evaluate(first, "(require top \"lib/top.wisp\")").await,
        "error: cannot load module top: lib/top.wisp: in (require dep \"lib/dep.wisp\"): \
         cannot load module dep: lib/dep.wisp: in (define d ...): car/cdr expects a list"
    );
    set("lib/dep.wisp", "(module dep (provide d) (define d 5))");
    assert_eq!(
        evaluate(first, "(require top \"lib/top.wisp\") t").await,
        "5"
    );
}

// fs-read gives bytes; fs-read-text gives the same result with a string.
async fn read_text_session(first: &mut WasmtimeInstance, files: &Files) {
    files
        .lock()
        .unwrap()
        .insert("notes.txt".to_string(), "hi\nthere".to_string());
    assert_eq!(
        evaluate(first, "(fs-read-text \"notes.txt\")").await,
        "(ok string string \"hi\\nthere\")"
    );
    assert_eq!(
        evaluate(
            first,
            "(match (fs-read-text \"notes.txt\") ((ok s) (string-len s)) ((err e) e))"
        )
        .await,
        "8"
    );
    assert_eq!(
        evaluate(first, "(fs-read-text \"missing.txt\")").await,
        "(err (list u8) string \"no such file\")"
    );
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
        assert!(
            result.starts_with("error:") && result.contains(error),
            "{text}: {result}"
        );
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

/// Compile the retention probe (the evaluator plus `heap-mark`, its live bytes,
/// and `fire-message`, a trigger firing) with reference counting.
fn compile_rc_probe(debug_heap: bool) -> Vec<u8> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let package = std::thread::scope(|s| {
        std::thread::Builder::new()
            .stack_size(1 << 30)
            .spawn_scoped(s, || {
                granite::compiler::compile(
                    &root.join("tests/fixtures/retention/probe.wisp"),
                    &root.join(format!(
                        "target/interpreter-engine/{}/rc-probe-{debug_heap}",
                        std::process::id()
                    )),
                    granite::compiler::EmitOptions {
                        rc: true,
                        debug_heap,
                        ..Default::default()
                    },
                )
                .unwrap()
            })
            .expect("spawn big-stack compile thread")
            .join()
            .expect("big-stack compile thread panicked")
    });
    std::fs::read(package.wasm).unwrap()
}

// (reload NAME) of a module that declares a record, variant, global, trait and
// instance (docs/proposals/RELOAD-DECLARATIONS.md): its own declarations are
// declared again, an unchanged type keeps its identity, a mut global keeps its
// value, and a failed reload restores every table.
async fn reload_declarations_session(first: &mut WasmtimeInstance, files: &Files) {
    let path = "lib/shapes.wisp";
    let set = |body: &str| {
        files.lock().unwrap().insert(
            path.to_string(),
            format!("(module shapes (provide make x-of bump hits limit size)\n{body})"),
        );
    };
    let rest = "  (define x-of (lambda (p) (pt.x p)))
  (define bump (lambda () (global.set $shape-hits (i32.add (global.get $shape-hits) 1))))
  (define hits (lambda () (global.get $shape-hits)))
  (define limit (lambda () (global.get $limit)))";
    let trait_decl = "  (trait (Sized T) (fn size ((x T)) : s32))
  (instance (Sized s32) (fn size ((x s32)) s32 x))";
    let v1 = format!(
        "  (record pt (x s32) (y s32))
  (variant shape (circle s32) (square s32))
  (global $shape-hits s32 mut 0)
  (global $limit s32 const 5)
{trait_decl}
  (define make (lambda (n) (pt n n)))
{rest}"
    );
    set(&v1);
    assert_eq!(
        evaluate(first, "(require shapes \"lib/shapes.wisp\")").await,
        "()"
    );
    evaluate(first, "(define p (make 2))").await;
    evaluate(first, "(bump)").await;
    assert_eq!(evaluate(first, "(bump)").await, "2");
    assert_eq!(evaluate(first, "(size 7)").await, "7");

    // Only a function changed: the record keeps its id, so p is still a pt; the
    // mut global keeps its value; the const takes its new initializer.
    set(&v1
        .replace("(pt n n)", "(pt (i32.add n 1) n)")
        .replace("const 5", "const 9"));
    assert_eq!(evaluate(first, "(reload shapes)").await, "()");
    assert_eq!(evaluate(first, "(hits)").await, "2");
    assert_eq!(evaluate(first, "(x-of p)").await, "2");
    assert_eq!(evaluate(first, "(x-of (make 2))").await, "3");
    assert_eq!(evaluate(first, "(limit)").await, "9");
    assert_eq!(evaluate(first, "(size 7)").await, "7");

    // The record changed: a new type. p still matches its old shape, but fails
    // the new type's accessor.
    let v3 = v1
        .replace(
            "(record pt (x s32) (y s32))",
            "(record pt (x s32) (y s32) (z s32))",
        )
        .replace("(pt n n)", "(pt n n n)");
    set(&v3);
    assert_eq!(evaluate(first, "(reload shapes)").await, "()");
    assert_eq!(evaluate(first, "(x-of (make 1))").await, "1");
    assert_eq!(
        evaluate(first, "(x-of p)").await,
        "error: expected record pt"
    );
    assert_eq!(evaluate(first, "(match p ((pt a b) (+ a b)))").await, "4");
    assert_eq!(evaluate(first, "(hits)").await, "2");

    // Failed reloads restore everything: a duplicate in one file, a name another
    // module declared, and a changed trait that another module has an instance of.
    evaluate(first, "(module mine (record thing (n s32)))").await;
    evaluate(
        first,
        "(module inst (instance (Sized string) (fn size ((x string)) s32 1)))",
    )
    .await;
    for (text, error) in [
        (
            format!("{v3}\n  (record pt (x s32))"),
            "invalid or already defined type name",
        ),
        (
            format!("{v3}\n  (record thing (n s32))"),
            "invalid or already defined type name",
        ),
        (
            v3.replace("(fn size ((x T)) : s32)", "(fn size ((x T)) : s64)")
                .replace(
                    "(fn size ((x s32)) s32 x)",
                    "(fn size ((x s32)) s64 (i64.extend_i32_s x))",
                ),
            "trait Sized changed, but module inst has an instance of it",
        ),
        (
            v3.replace(trait_decl, "  (define size (lambda (x) x))"),
            "trait Sized was removed, but module inst has an instance of it",
        ),
    ] {
        set(&text);
        let result = evaluate(first, "(reload shapes)").await;
        assert!(
            result.starts_with("error:") && result.contains(error),
            "{text}: {result}"
        );
        assert_eq!(evaluate(first, "(hits)").await, "2", "{text}");
        assert_eq!(evaluate(first, "(x-of (make 1))").await, "1", "{text}");
        assert_eq!(evaluate(first, "(size 7)").await, "7", "{text}");
    }

    // Declarations the new source leaves out are retired: their names are free.
    set(&v3
        .replace("  (variant shape (circle s32) (square s32))\n", "")
        .replace("  (global $limit s32 const 5)\n", "")
        .replace(
            "(define limit (lambda () (global.get $limit)))",
            "(define limit 0)",
        ));
    assert_eq!(evaluate(first, "(reload shapes)").await, "()");
    assert_eq!(evaluate(first, "(variant shape (blob s32))").await, "()");
    assert_eq!(evaluate(first, "(global $limit s32 const 1)").await, "()");
    assert_eq!(evaluate(first, "(hits)").await, "2");
}

async fn live_bytes(instance: &mut WasmtimeInstance) -> i64 {
    match call_with_value(instance, "heap-mark", &Value::Tuple(vec![]))
        .await
        .unwrap()
    {
        Value::S32(n) => n as i64,
        other => panic!("heap-mark: {other:?}"),
    }
}

/// Run `step` `warm` times, then `n` more, and return the live-byte change over
/// the `n` (the warm-up covers first-use tables and grown buffers).
async fn growth<F>(instance: &mut WasmtimeInstance, warm: usize, n: usize, mut step: F) -> i64
where
    F: AsyncFnMut(&mut WasmtimeInstance),
{
    for _ in 0..warm {
        step(instance).await;
    }
    let before = live_bytes(instance).await;
    for _ in 0..n {
        step(instance).await;
    }
    live_bytes(instance).await - before
}

/// MEMORY.md §7 stage 4: with reference counting a session's live heap is flat
/// under a Wisp loop, repeated `define`, repeated `require` and `reload`, and
/// trigger firings. In the debug build the verifier also checks every count after
/// every export call (exact counts, no extra live objects).
#[test]
fn test_rc_session_memory_is_flat() {
    tokio::runtime::Builder::new_current_thread()
        .build()
        .unwrap()
        .block_on(rc_session_memory());
}

async fn rc_session_memory() {
    // GRANITE_RC=0 turns counting off for every compile: nothing would be freed.
    if std::env::var_os("GRANITE_RC").is_some_and(|v| v == "0") {
        return;
    }
    for debug_heap in [true, false] {
        let wasm = compile_rc_probe(debug_heap);
        let engine = WasmtimeEngine::new();
        let module = engine.compile(&wasm).await.unwrap();
        let calls = Arc::new(Mutex::new(Vec::new()));
        let files = Files::default();
        let mut s = engine
            .instantiate(&module, imports(calls, files))
            .await
            .unwrap();
        // The debug build verifies the whole heap after each call: fewer rounds.
        let n = if debug_heap { 50 } else { 1000 };
        // The event buffer grows to twice its limit (256) and is then trimmed back:
        // a 257-firing cycle. Whole cycles start and end at the same size.
        let firings = if debug_heap { 257 } else { 39 * 257 };

        assert_eq!(
            evaluate(
                &mut s,
                "(define count (lambda (n) (if (= n 0) 0 (count (- n 1)))))"
            )
            .await,
            "#<closure>"
        );
        let loops = growth(&mut s, 10, n, async |s| {
            assert_eq!(evaluate(s, "(count 100)").await, "0");
        })
        .await;
        assert_eq!(loops, 0, "a Wisp loop: {loops} bytes over {n} evaluations");

        let defines = growth(&mut s, 10, n, async |s| {
            assert_eq!(evaluate(s, "(define x (list 1 2 3))").await, "(1 2 3)");
        })
        .await;
        assert_eq!(defines, 0, "repeated define: {defines} bytes over {n}");

        let requires = growth(&mut s, 10, n, async |s| {
            assert_eq!(
                evaluate(s, "(require greet \"lib/greet.wisp\") greeting").await,
                "7"
            );
        })
        .await;
        assert_eq!(requires, 0, "repeated require: {requires} bytes over {n}");

        let reloads = growth(&mut s, 10, n, async |s| {
            assert_eq!(evaluate(s, "(reload greet) greeting").await, "7");
        })
        .await;
        assert_eq!(reloads, 0, "repeated reload: {reloads} bytes over {n}");

        assert_eq!(
            evaluate(
                &mut s,
                "(define on-message (lambda (e) (bytes->string (car e))))"
            )
            .await,
            "#<closure>"
        );
        let fired = growth(&mut s, 600, firings, async |s| {
            call_with_value(
                s,
                "fire-message",
                &Value::Tuple(vec![Value::String("ping".into())]),
            )
            .await
            .unwrap();
        })
        .await;
        assert_eq!(fired, 0, "{firings} trigger firings: {fired} bytes");
    }
}
