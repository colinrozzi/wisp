//! Ticket 0012 retention measurements (ignored by default; run with
//! `cargo test --test retention_measure -- --ignored --nocapture`).
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
        if name == "get" || name == "get-by-label" {
            continue;
        }
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
        if name == "get-actor-status" {
            continue;
        }
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
        if name == "send" {
            continue;
        }
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

async fn mark(instance: &mut WasmtimeInstance) -> i64 {
    match call_with_value(instance, "heap-mark", &Value::Tuple(vec![]))
        .await
        .unwrap()
    {
        Value::S32(n) => n as i64,
        other => panic!("heap-mark: {other:?}"),
    }
}

async fn fire(instance: &mut WasmtimeInstance, text: &str) {
    call_with_value(
        instance,
        "fire-message",
        &Value::Tuple(vec![Value::String(text.into())]),
    )
    .await
    .unwrap();
}

fn ok_of(ok_type: ValueType, v: Value) -> Value {
    Value::Result {
        ok_type,
        err_type: ValueType::String,
        value: Ok(Box::new(v)),
    }
}

// Host stubs with realistic success shapes, so the registry's handler takes its
// real path (label -> ref -> bytes -> status -> reply) instead of failing early.
fn realistic(mut imports: HostImports) -> HostImports {
    const ID: &str = "c54e2f7c-5906-41b9-b547-64b2b411db3a";
    imports.define(
        "theater:simple/message-server-host",
        "send",
        host_fn(|_i| async move { Ok(ok_of(ValueType::Tuple(vec![]), Value::Tuple(vec![]))) }),
    );
    imports.define(
        "theater:simple/store",
        "get-by-label",
        host_fn(|_i| async move {
            Ok(ok_of(
                ValueType::Option(Box::new(ValueType::String)),
                Value::Option {
                    inner_type: ValueType::String,
                    value: Some(Box::new(Value::String("ref".into()))),
                },
            ))
        }),
    );
    imports.define(
        "theater:simple/store",
        "get",
        host_fn(|_i| async move {
            Ok(ok_of(
                ValueType::List(Box::new(ValueType::U8)),
                Value::List {
                    elem_type: ValueType::U8,
                    items: ID.bytes().map(Value::U8).collect(),
                },
            ))
        }),
    );
    imports.define(
        "theater:simple/runtime",
        "get-actor-status",
        host_fn(|_i| async move { Ok(ok_of(ValueType::String, Value::String("running".into()))) }),
    );
    imports
}

async fn fresh(
    engine: &WasmtimeEngine,
    module: &<WasmtimeEngine as WasmEngine>::Module,
    files: Files,
) -> WasmtimeInstance {
    let mut inst = engine
        .instantiate(
            module,
            realistic(imports(Arc::new(Mutex::new(Vec::new())), files)),
        )
        .await
        .unwrap();
    assert_eq!(evaluate(&mut inst, "(+ 1 2)").await, "3");
    inst
}

#[test]
#[ignore]
fn retention_measurements() {
    std::thread::Builder::new()
        .stack_size(1 << 30)
        .spawn(|| {
            tokio::runtime::Builder::new_current_thread()
                .build()
                .unwrap()
                .block_on(measure())
        })
        .unwrap()
        .join()
        .unwrap();
}

async fn measure() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let package = granite::compiler::compile(
        &root.join("tests/fixtures/retention/probe.wisp"),
        &root.join(format!("target/retention/{}/probe", std::process::id())),
        granite::compiler::EmitOptions::default(),
    )
    .unwrap();
    let wasm = std::fs::read(package.wasm).unwrap();
    let engine = WasmtimeEngine::new();
    let module = engine.compile(&wasm).await.unwrap();
    let files = Files::default();
    let registry = std::fs::read_to_string(
        std::env::var("REGISTRY_WISP")
            .unwrap_or("/home/colin/work/experiment/lib/registry.wisp".into()),
    )
    .unwrap();
    files
        .lock()
        .unwrap()
        .insert("lib/registry.wisp".into(), registry);

    // 1. Baseline and per-evaluate cost.
    let mut s = fresh(&engine, &module, files.clone()).await;
    let m0 = mark(&mut s).await;
    println!("baseline heap after first eval: {m0} bytes");
    let a = mark(&mut s).await;
    for _ in 0..100 {
        evaluate(&mut s, "(+ 1 2)").await;
    }
    println!("(+ 1 2): {} bytes/eval", (mark(&mut s).await - a) / 100);

    // 2. Distinct defines: cost of the k-th define grows with k (publish copies).
    let mut s = fresh(&engine, &module, files.clone()).await;
    let mut k = 0;
    for checkpoint in [100, 500, 1000, 2000] {
        while k < checkpoint - 10 {
            evaluate(&mut s, &format!("(define v{k} {k})")).await;
            k += 1;
        }
        let a = mark(&mut s).await;
        for _ in 0..10 {
            evaluate(&mut s, &format!("(define v{k} {k})")).await;
            k += 1;
        }
        println!(
            "define #{checkpoint}: {} bytes/define",
            (mark(&mut s).await - a) / 10
        );
    }
    // Redefining one name: every old binding stays.
    let a = mark(&mut s).await;
    for i in 0..100 {
        evaluate(&mut s, &format!("(define v0 {i})")).await;
    }
    println!(
        "redefine with 2000 bindings: {} bytes/define",
        (mark(&mut s).await - a) / 100
    );

    // 3. Inbound messages: buffered (never polled) vs drained each time.
    for size in [64usize, 1024, 4096] {
        let msg = "x".repeat(size);
        let mut s = fresh(&engine, &module, files.clone()).await;
        evaluate(
            &mut s,
            "(define on-message (lambda (e) (bytes->string (car e))))",
        )
        .await;
        let a = mark(&mut s).await;
        for _ in 0..20 {
            fire(&mut s, &msg).await;
        }
        let b = mark(&mut s).await;
        evaluate(&mut s, "(poll-events)").await;
        let c = mark(&mut s).await;
        for _ in 0..20 {
            fire(&mut s, &msg).await;
            evaluate(&mut s, "(length (poll-events))").await;
        }
        let d = mark(&mut s).await;
        println!(
            "on-message {size} B: {} bytes/firing buffered, poll of 20 = {}, {} bytes/firing+drain",
            (b - a) / 20,
            c - b,
            (d - c) / 20
        );
    }

    // 4. The registry server, as registry-start sets it up.
    let mut s = fresh(&engine, &module, files.clone()).await;
    for form in [
        "(require registry \"lib/registry.wisp\")",
        "(define reg \"store-1\")",
        "(define on-message (lambda (e) (registry-handle reg (bytes->string (car e)))))",
    ] {
        let out = evaluate(&mut s, form).await;
        assert!(!out.starts_with("error"), "{form}: {out}");
    }
    let req = "lookup github-events ab090a61-05c3-44c1-98fb-ff8288d734b8";
    let reg_req =
        "register foo c54e2f7c-5906-41b9-b547-64b2b411db3a ab090a61-05c3-44c1-98fb-ff8288d734b8";
    for (label, text) in [("lookup", req), ("register (no change)", reg_req)] {
        let a = mark(&mut s).await;
        for _ in 0..50 {
            fire(&mut s, text).await;
        }
        let b = mark(&mut s).await;
        let shown = evaluate(&mut s, "(poll-events \"text\")").await;
        let c = mark(&mut s).await;
        println!(
            "registry {label}: {} bytes/request, drain of 50 = {}; last firing: {}",
            (b - a) / 50,
            c - b,
            shown.lines().last().unwrap_or("")
        );
    }
    breakdown(&engine, &module, files.clone()).await;
}

// Where a registry request's heap goes: each piece measured alone, 20 times.
async fn breakdown(
    engine: &WasmtimeEngine,
    module: &<WasmtimeEngine as WasmEngine>::Module,
    files: Files,
) {
    let mut s = fresh(engine, module, files.clone()).await;
    for form in [
        "(require registry \"lib/registry.wisp\")",
        "(module m (provide id) (define id (lambda (x) x)))",
        "(require m)",
        "(define on-message (lambda (e) 0))",
        "(define loop (lambda (n) (if (= n 0) 0 (loop (- n 1)))))",
        "(define msg \"lookup github-events ab090a61-05c3-44c1-98fb-ff8288d734b8\")",
    ] {
        let out = evaluate(&mut s, form).await;
        assert!(!out.starts_with("error"), "{form}: {out}");
    }
    let per = |a: i64, b: i64| (b - a) / 20;
    let a = mark(&mut s).await;
    for _ in 0..20 {
        fire(
            &mut s,
            "lookup github-events ab090a61-05c3-44c1-98fb-ff8288d734b8",
        )
        .await;
    }
    let b = mark(&mut s).await;
    println!(
        "  fire 57-byte message, handler returns 0: {} bytes",
        per(a, b)
    );
    evaluate(&mut s, "(poll-events \"text\")").await;
    for (label, form) in [
        ("evaluate \"0\"", "0"),
        ("(loop 100) = ~600 eval steps", "(loop 100)"),
        ("(string->bytes msg)", "(string->bytes msg)"),
        (
            "(bytes->string (string->bytes msg))",
            "(bytes->string (string->bytes msg))",
        ),
        (
            "(msg-send id msg) host call",
            "(msg-send \"ab090a61-05c3-44c1-98fb-ff8288d734b8\" msg)",
        ),
        (
            "(store-get-by-label reg name) host call",
            "(store-get-by-label \"store-1\" \"github-events\")",
        ),
        (
            "(actor-status id) host call",
            "(actor-status \"ab090a61-05c3-44c1-98fb-ff8288d734b8\")",
        ),
        ("(id 1) cross-module call", "(id 1)"),
        (
            "(registry-handle reg msg)",
            "(registry-handle \"store-1\" msg)",
        ),
    ] {
        evaluate(&mut s, form).await;
        let a = mark(&mut s).await;
        for _ in 0..20 {
            evaluate(&mut s, form).await;
        }
        let b = mark(&mut s).await;
        println!("  {label}: {} bytes/eval", per(a, b));
    }
    // reload: old version's footprint, and what a hand-copied closure keeps.
    let mut s = fresh(engine, module, files.clone()).await;
    evaluate(&mut s, "(require registry \"lib/registry.wisp\")").await;
    let a = mark(&mut s).await;
    let out = evaluate(&mut s, "(reload registry)").await;
    let b = mark(&mut s).await;
    println!("  (reload registry) -> {out}: {} bytes", b - a);
}
