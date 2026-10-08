//! `marshal` (interpreter value -> Pack dynamic `any`) written in Wisp over the
//! `any` byte-view primitives. Each export builds a value of one interpreter type
//! and marshals it; the host checks the resulting dynamic value. Note S64/F64/F32
//! nodes are built with NO per-type compiler builtin — pure Wisp over A′.

use std::path::PathBuf;
use std::sync::OnceLock;

use pack::abi::{Value, ValueType};
use wasmtime::{Caller, Config, Engine, Instance, Linker, Memory, Module, Store};
use wisp::compiler::{self, EmitOptions};

fn package() -> &'static PathBuf {
    static PACKAGE: OnceLock<PathBuf> = OnceLock::new();
    PACKAGE.get_or_init(|| {
        let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let out = root.join(format!(
            "target/marshal-tests/{}/marshal",
            std::process::id()
        ));
        // The fixture includes the full evaluator; compiling it recurses per
        // expression node and exceeds the default 2 MiB test stack, so run the
        // compile on a large stack (as the codegen/self_hosted tests do).
        std::thread::scope(|s| {
            std::thread::Builder::new()
                .stack_size(1 << 30)
                .spawn_scoped(s, || {
                    compiler::compile(
                        &root.join("tests/fixtures/marshal.wisp"),
                        &out,
                        EmitOptions::default(),
                    )
                    .unwrap()
                    .wasm
                })
                .expect("spawn big-stack compile thread")
                .join()
                .expect("big-stack compile thread panicked")
        })
    })
}

struct Guest {
    store: Store<()>,
    instance: Instance,
    memory: Memory,
}

/// Register a mock `theater:simple/rpc` verb that returns `value` (CGRF-encoded).
fn mock_rpc(linker: &mut Linker<()>, name: &'static str, value: Value) {
    mock_import(linker, "theater:simple/rpc", name, value);
}

/// Register a mock host import on any interface that returns `value` (CGRF). Used
/// for both the typed `rpc` verbs and the raw-CGRF capabilities (e.g. `store`):
/// at the wire level every import is the same CGRF in / CGRF out, so one mock fits.
fn mock_import(linker: &mut Linker<()>, module: &'static str, name: &'static str, value: Value) {
    let reply = pack::encode(&value).unwrap();
    linker
        .func_wrap(
            module,
            name,
            move |mut caller: Caller<'_, ()>,
                  _: i32,
                  _: i32,
                  out_ptr_ptr: i32,
                  out_len_ptr: i32|
                  -> i32 {
                let alloc = caller
                    .get_export("__pack_alloc")
                    .unwrap()
                    .into_func()
                    .unwrap();
                let ptr = alloc
                    .typed::<i32, i32>(&caller)
                    .unwrap()
                    .call(&mut caller, reply.len() as i32)
                    .unwrap();
                let mem = caller.get_export("memory").unwrap().into_memory().unwrap();
                mem.write(&mut caller, ptr as usize, &reply).unwrap();
                mem.write(&mut caller, out_ptr_ptr as usize, &ptr.to_le_bytes())
                    .unwrap();
                mem.write(
                    &mut caller,
                    out_len_ptr as usize,
                    &(reply.len() as i32).to_le_bytes(),
                )
                .unwrap();
                0
            },
        )
        .unwrap();
}

impl Guest {
    fn new() -> Self {
        let mut config = Config::new();
        config.wasm_tail_call(true);
        let engine = Engine::new(&config).unwrap();
        let module = Module::from_file(&engine, package()).unwrap();
        let mut store = Store::new(&engine, ());
        // The codec now lives in the full interpreter, which imports wisp-source
        // for `(include ...)`. The marshal tests never trigger includes, so these
        // are unreachable stubs that only need to exist to satisfy linking.
        let mut linker = Linker::new(&engine);
        for name in ["resolve-path", "read-source"] {
            linker
                .func_wrap(
                    "wisp-source",
                    name,
                    |_c: Caller<'_, ()>, _: i32, _: i32, _: i32, _: i32| -> i32 { -1 },
                )
                .unwrap();
        }
        // Mock Theater rpc verbs returning canned dynamic values, so the REPL
        // verbs exercise the full host-dispatch loop.
        mock_rpc(
            &mut linker,
            "describe",
            Value::Record {
                type_name: "actor-desc".into(),
                fields: vec![
                    ("name".into(), Value::String("counter".into())),
                    ("alive".into(), Value::Bool(true)),
                ],
            },
        );
        mock_rpc(
            &mut linker,
            "exports",
            Value::List {
                elem_type: ValueType::String,
                items: vec![
                    Value::String("theater:simple/actor".into()),
                    Value::String("theater:simple/counter".into()),
                ],
            },
        );
        mock_rpc(&mut linker, "implements", Value::Bool(true));
        mock_rpc(
            &mut linker,
            "call",
            Value::String("call-not-exercised".into()),
        );
        // store.new via the raw-CGRF convention: the guest calls the raw import with
        // a marshalled empty-tuple blob and unmarshals the returned result<string,
        // string>. Mock it so the `(store-new)` verb exercises the whole raw path.
        mock_import(
            &mut linker,
            "theater:simple/store",
            "new",
            Value::Result {
                ok_type: ValueType::String,
                err_type: ValueType::String,
                value: Ok(Box::new(Value::String("store-abc123".into()))),
            },
        );
        // store.exists -> result<bool, string> and calculate-total-size ->
        // result<u64, string>: exercise the now-first-class bool/u64 compiler types
        // through the raw-CGRF path (declared with real typed signatures; bridged by
        // the codec). Mocks ignore their input.
        mock_import(
            &mut linker,
            "theater:simple/store",
            "exists",
            Value::Result {
                ok_type: ValueType::Bool,
                err_type: ValueType::String,
                value: Ok(Box::new(Value::Bool(false))),
            },
        );
        mock_import(
            &mut linker,
            "theater:simple/store",
            "calculate-total-size",
            Value::Result {
                ok_type: ValueType::U64,
                err_type: ValueType::String,
                value: Ok(Box::new(Value::U64(0))),
            },
        );
        // runtime.list-actors -> result<list<actor-info>, runtime-error>: the named
        // types (record actor-info, variant runtime-error) are declared in Wisp and
        // registered as pack typedefs so the interface hash matches Theater; here we
        // check the record list decodes to inspectable values through the codec.
        mock_import(
            &mut linker,
            "theater:simple/runtime",
            "list-actors",
            Value::Result {
                ok_type: ValueType::List(Box::new(ValueType::Record("actor-info".into()))),
                err_type: ValueType::Record("runtime-error".into()),
                value: Ok(Box::new(Value::List {
                    elem_type: ValueType::Record("actor-info".into()),
                    items: vec![Value::Record {
                        type_name: "actor-info".into(),
                        fields: vec![
                            ("id".into(), Value::String("actor-1".into())),
                            ("name".into(), Value::String("counter".into())),
                            (
                                "parent-id".into(),
                                Value::Option {
                                    inner_type: ValueType::String,
                                    value: None,
                                },
                            ),
                        ],
                    }],
                })),
            },
        );
        // More runtime verbs: get-actor-manifest (result<string>) and
        // get-actor-state (result<option<list<u8>>>). The mock replies immediately,
        // so these exercise the verb dispatch + codec without the self-query
        // reentrancy that deadlocks a live (actor-state (self)).
        mock_import(
            &mut linker,
            "theater:simple/runtime",
            "get-actor-manifest",
            Value::Result {
                ok_type: ValueType::String,
                err_type: ValueType::Record("runtime-error".into()),
                value: Ok(Box::new(Value::String("name = \"demo\"".into()))),
            },
        );
        mock_import(
            &mut linker,
            "theater:simple/runtime",
            "get-actor-state",
            Value::Result {
                ok_type: ValueType::Option(Box::new(ValueType::List(Box::new(ValueType::U8)))),
                err_type: ValueType::Record("runtime-error".into()),
                value: Ok(Box::new(Value::Option {
                    inner_type: ValueType::List(Box::new(ValueType::U8)),
                    value: Some(Box::new(Value::List {
                        elem_type: ValueType::U8,
                        items: vec![Value::U8(1), Value::U8(2), Value::U8(3)],
                    })),
                })),
            },
        );
        // store.store (writer) -> result<string,string>: mock ignores the list<u8>
        // args (the blob is validated directly by m-strbytes), isolating the guest
        // result path for the (store-put ...) verb.
        mock_import(
            &mut linker,
            "theater:simple/store",
            "store",
            Value::Result {
                ok_type: ValueType::String,
                err_type: ValueType::String,
                value: Ok(Box::new(Value::String("deadbeefhash".into()))),
            },
        );
        // filesystem.exists -> result<bool, filesystem-error>. Its bare name
        // collides with store.exists; both must dispatch to their own interface
        // (proof of the interface-qualified raw symbols). err type is a named
        // variant, exercising the structural-hash typedef registration too.
        mock_import(
            &mut linker,
            "theater:simple/filesystem",
            "exists",
            Value::Result {
                ok_type: ValueType::Bool,
                err_type: ValueType::Record("filesystem-error".into()),
                value: Ok(Box::new(Value::Bool(true))),
            },
        );
        // http-client.request -> result<http-response, string>. The response is a
        // record with a u16 status and a nested list<http-header>; the verb decodes
        // it via register-on-arrival. (The mock ignores the request record arg,
        // which m-http-req validates separately.)
        mock_import(
            &mut linker,
            "theater:simple/http-client",
            "request",
            Value::Result {
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
                                items: vec![Value::Record {
                                    type_name: "http-header".into(),
                                    fields: vec![
                                        ("name".into(), Value::String("content-type".into())),
                                        ("value".into(), Value::String("text/html".into())),
                                    ],
                                }],
                            },
                        ),
                        (
                            "body".into(),
                            Value::Option {
                                inner_type: ValueType::List(Box::new(ValueType::U8)),
                                value: None,
                            },
                        ),
                    ],
                })),
            },
        );
        // self.self -> the actor's own id. The exports/implements/call/actor-state
        // guards call (self) to reject self-targeted RPC; mock it with an id
        // distinct from the "actor-1" these tests target, so the guard stays inert.
        mock_import(
            &mut linker,
            "theater:simple/self",
            "self",
            Value::String("repl-self".into()),
        );
        // Any other host import the evaluator declares (store.get/...) traps.
        linker.define_unknown_imports_as_traps(&module).unwrap();
        let instance = linker.instantiate(&mut store, &module).unwrap();
        let memory = instance.get_memory(&mut store, "memory").unwrap();
        Self {
            store,
            instance,
            memory,
        }
    }

    fn alloc(&mut self, size: i32) -> i32 {
        self.instance
            .get_typed_func::<i32, i32>(&mut self.store, "__pack_alloc")
            .unwrap()
            .call(&mut self.store, size)
            .unwrap()
    }

    fn call(&mut self, name: &str, input: Value) -> Value {
        let input = pack::encode(&input).unwrap();
        let ptr = self.alloc(input.len() as i32);
        let slots = self.alloc(8);
        self.memory
            .write(&mut self.store, ptr as usize, &input)
            .unwrap();
        let status = self
            .instance
            .get_typed_func::<(i32, i32, i32, i32), i32>(&mut self.store, name)
            .unwrap()
            .call(&mut self.store, (ptr, input.len() as i32, slots, slots + 4))
            .unwrap();
        assert_eq!(status, 0);
        let mut ptr_len = [0u8; 8];
        self.memory
            .read(&self.store, slots as usize, &mut ptr_len)
            .unwrap();
        let out = u32::from_le_bytes(ptr_len[..4].try_into().unwrap()) as usize;
        let len = u32::from_le_bytes(ptr_len[4..].try_into().unwrap()) as usize;
        let mut bytes = vec![0; len];
        self.memory.read(&self.store, out, &mut bytes).unwrap();
        pack::decode(&bytes).unwrap()
    }
}

#[test]
fn test_marshal_bool() {
    let mut g = Guest::new();
    assert_eq!(g.call("m-bool", Value::S32(1)), Value::Bool(true));
    assert_eq!(g.call("m-bool", Value::S32(0)), Value::Bool(false));
}

#[test]
fn test_marshal_scalars() {
    let mut g = Guest::new();
    assert_eq!(g.call("m-int", Value::S32(42)), Value::S32(42));
    assert_eq!(g.call("m-int", Value::S32(-7)), Value::S32(-7));
    assert_eq!(g.call("m-wide", Value::S64(i64::MIN)), Value::S64(i64::MIN));
    assert_eq!(g.call("m-double", Value::F64(3.5)), Value::F64(3.5));
    assert_eq!(
        g.call("m-text", Value::String("héllo λ".into())),
        Value::String("héllo λ".into())
    );
}

#[test]
fn test_marshal_tuples() {
    let mut g = Guest::new();
    // A sequence marshals to a positional Tuple (children emitted first, then the
    // Tuple node referencing their indices).
    assert_eq!(
        g.call("m-pair", Value::Tuple(vec![Value::S32(7), Value::S32(9)])),
        Value::Tuple(vec![Value::S32(7), Value::S32(9)])
    );
    assert_eq!(
        g.call(
            "m-mixed",
            Value::Tuple(vec![Value::S32(5), Value::String("hi".into())])
        ),
        Value::Tuple(vec![Value::S32(5), Value::String("hi".into())])
    );
    // Nested tuple: exercises recursive node/index assignment.
    assert_eq!(
        g.call(
            "m-nested",
            Value::Tuple(vec![Value::S32(1), Value::S32(2), Value::S32(3)])
        ),
        Value::Tuple(vec![
            Value::S32(1),
            Value::Tuple(vec![Value::S32(2), Value::S32(3)]),
        ])
    );
}

#[test]
fn test_marshal_u64() {
    let mut g = Guest::new();
    // Stored over s64 bits; the host reads them back as an unsigned u64 — including
    // the all-ones pattern (S64(-1) bits == u64::MAX), exercising unsigned handling.
    assert_eq!(g.call("m-u64", Value::S64(11)), Value::U64(11));
    assert_eq!(g.call("m-u64", Value::S64(-1)), Value::U64(u64::MAX));
}

#[test]
fn test_unmarshal_roundtrips() {
    // u-remarshal = marshal . unmarshal. The host hands in a dynamic value, the
    // guest decodes it to an interp value and re-encodes it; identity proves
    // unmarshal (and marshal) preserve the value across every supported kind.
    let mut g = Guest::new();
    let cases = [
        Value::S32(42),
        Value::S32(-7),
        Value::S64(i64::MIN),
        Value::U64(u64::MAX),
        Value::Bool(true),
        Value::Bool(false),
        Value::U8(200),
        Value::String("héllo λ".into()),
        Value::String("".into()),
        Value::Tuple(vec![
            Value::S32(1),
            Value::String("x".into()),
            Value::Bool(true),
        ]),
        Value::Tuple(vec![
            Value::U64(9),
            Value::Tuple(vec![Value::Bool(false), Value::S32(-3)]),
        ]),
    ];
    for v in cases {
        assert_eq!(g.call("u-remarshal", v.clone()), v, "remarshal {v:?}");
    }
}

#[test]
fn test_unmarshal_option_result() {
    let mut g = Guest::new();
    let opt = |v: Option<Value>| Value::Option {
        inner_type: ValueType::S32,
        value: v.map(Box::new),
    };
    // option<s32>: some and none (none carries the inner type via its type-tag).
    assert_eq!(
        g.call("u-remarshal", opt(Some(Value::S32(7)))),
        opt(Some(Value::S32(7)))
    );
    assert_eq!(g.call("u-remarshal", opt(None)), opt(None));
    // result<string, string>: ok and err — the ubiquitous Theater return shape.
    let res = |r: Result<Value, Value>| Value::Result {
        ok_type: ValueType::String,
        err_type: ValueType::String,
        value: r.map(Box::new).map_err(Box::new),
    };
    assert_eq!(
        g.call("u-remarshal", res(Ok(Value::String("done".into())))),
        res(Ok(Value::String("done".into())))
    );
    assert_eq!(
        g.call("u-remarshal", res(Err(Value::String("nope".into())))),
        res(Err(Value::String("nope".into())))
    );
    // result<u64, string> ok, to vary the ok-type tag.
    let res2 = |r: Result<Value, Value>| Value::Result {
        ok_type: ValueType::U64,
        err_type: ValueType::String,
        value: r.map(Box::new).map_err(Box::new),
    };
    assert_eq!(
        g.call("u-remarshal", res2(Ok(Value::U64(42)))),
        res2(Ok(Value::U64(42)))
    );
}

#[test]
fn test_unmarshal_lists() {
    let mut g = Guest::new();
    // list<u8> encodes as an Array node (contiguous bytes) — the common byte buffer.
    let bytes = |items: Vec<u8>| Value::List {
        elem_type: ValueType::U8,
        items: items.into_iter().map(Value::U8).collect(),
    };
    assert_eq!(
        g.call("u-remarshal", bytes(vec![1, 2, 255])),
        bytes(vec![1, 2, 255])
    );
    assert_eq!(g.call("u-remarshal", bytes(vec![])), bytes(vec![]));
    // list<string> encodes as a List node with child String nodes.
    let strs = |items: Vec<&str>| Value::List {
        elem_type: ValueType::String,
        items: items.into_iter().map(|s| Value::String(s.into())).collect(),
    };
    assert_eq!(
        g.call("u-remarshal", strs(vec!["a", "λ", ""])),
        strs(vec!["a", "λ", ""])
    );
    assert_eq!(g.call("u-remarshal", strs(vec![])), strs(vec![]));
}

#[test]
fn test_unmarshal_record_register_on_arrival() {
    let mut g = Guest::new();
    // Initialise the session globals ($types, ...) — the codec registers foreign
    // record types there on arrival.
    g.call("evaluate", Value::String("".into()));

    let rec = |fields: Vec<(&str, Value)>| Value::Record {
        type_name: "item".into(),
        fields: fields
            .into_iter()
            .map(|(n, v)| (n.to_string(), v))
            .collect(),
    };
    // A record whose type the session never declared: unmarshal registers a
    // named-type from the wire (name + field names), builds an aggregate, and
    // marshal re-emits the Record by reading those field names back from $types.
    let v = rec(vec![
        ("id", Value::S32(1)),
        ("title", Value::String("ship the bridge".into())),
        ("done", Value::Bool(false)),
    ]);
    assert_eq!(g.call("u-remarshal", v.clone()), v);

    // Seeing the same type again reuses the registration (name+shape identity);
    // and a nested record round-trips too.
    let nested = rec(vec![
        ("id", Value::S32(2)),
        ("title", Value::String("λ".into())),
        ("done", Value::Bool(true)),
    ]);
    assert_eq!(g.call("u-remarshal", nested.clone()), nested);
    let outer = Value::Record {
        type_name: "wrap".into(),
        fields: vec![
            ("inner".into(), v.clone()),
            ("ok".into(), Value::Bool(true)),
        ],
    };
    assert_eq!(g.call("u-remarshal", outer.clone()), outer);
}

#[test]
fn test_unmarshal_variant_open() {
    let mut g = Guest::new();
    // A variant value reveals only its active case + tag, so it unmarshals into a
    // self-contained `open-variant` (name/case/tag/payload inline) — the default
    // landing spot for any variant whose full type the session doesn't have.
    let var = |case: &str, tag: usize, payload: Vec<Value>| Value::Variant {
        type_name: "runtime-error".into(),
        case_name: case.into(),
        tag,
        payload,
    };
    // A payload-carrying case, preserving a non-zero tag.
    let e = var(
        "permission-denied",
        0,
        vec![Value::String("inspect".into())],
    );
    assert_eq!(g.call("u-remarshal", e.clone()), e);
    // A payloadless case with a higher tag — the tag must survive.
    let unavail = var("runtime-unavailable", 1, vec![]);
    assert_eq!(g.call("u-remarshal", unavail.clone()), unavail);
    // Nested payload (a variant carrying a variant).
    let nested = var(
        "spawn-failed",
        4,
        vec![var("bad-manifest", 0, vec![Value::String("x".into())])],
    );
    assert_eq!(g.call("u-remarshal", nested.clone()), nested);
}

#[test]
fn test_repl_describe_end_to_end() {
    // The full loop: REPL source -> eval -> `describe` builtin -> rpc.describe host
    // -> unmarshal the returned dynamic value (register-on-arrival) -> inspectable
    // Wisp value -> printed. The mock host returns a record; the REPL shows it.
    let mut g = Guest::new();
    let out = match g.call("evaluate", Value::String("(describe \"actor-1\")".into())) {
        Value::String(s) => s,
        other => panic!("expected string, got {other:?}"),
    };
    assert!(
        out.contains("actor-desc") && out.contains("counter") && out.contains("true"),
        "describe result not inspectable as expected: {out}"
    );
    // An arity error surfaces as an ordinary evaluator diagnostic, not a trap.
    let err = match g.call("evaluate", Value::String("(describe)".into())) {
        Value::String(s) => s,
        other => panic!("{other:?}"),
    };
    assert!(err.contains("describe expects"), "{err}");
}

#[test]
fn test_repl_exports_and_implements() {
    let mut g = Guest::new();
    // exports: string arg -> list<string> result, printed as an inspectable list.
    let out = match g.call("evaluate", Value::String("(exports \"actor-1\")".into())) {
        Value::String(s) => s,
        other => panic!("{other:?}"),
    };
    assert!(
        out.contains("theater:simple/actor") && out.contains("theater:simple/counter"),
        "exports: {out}"
    );
    // implements: two string args -> bool result.
    let out = match g.call(
        "evaluate",
        Value::String("(implements \"actor-1\" \"theater:simple/counter\")".into()),
    ) {
        Value::String(s) => s,
        other => panic!("{other:?}"),
    };
    assert_eq!(out, "true", "implements: {out}");
    // Arity checks surface as ordinary diagnostics.
    let out = match g.call("evaluate", Value::String("(implements \"a\")".into())) {
        Value::String(s) => s,
        other => panic!("{other:?}"),
    };
    assert!(out.contains("implements expects"), "{out}");
}

#[test]
fn test_repl_store_new_raw_invoke() {
    // The raw-CGRF convention end to end: REPL source -> eval -> `store-new` verb
    // -> marshal an empty-tuple args blob -> `raw-invoke "new"` -> the raw store
    // import (mocked) -> unmarshal the returned result<string,string> -> inspectable
    // Wisp value. Proves a *typed* capability import bridged purely by the codec,
    // with no per-type decode glue in the compiler.
    let mut g = Guest::new();
    let out = match g.call("evaluate", Value::String("(store-new)".into())) {
        Value::String(s) => s,
        other => panic!("expected string, got {other:?}"),
    };
    // Faithful, un-unwrapped result (left as-is by design): the ok payload shows.
    assert!(
        out.contains("ok") && out.contains("store-abc123"),
        "store-new result not inspectable as expected: {out}"
    );
    // Arity error is an ordinary diagnostic, not a trap.
    let err = match g.call("evaluate", Value::String("(store-new \"x\")".into())) {
        Value::String(s) => s,
        other => panic!("{other:?}"),
    };
    assert!(err.contains("store-new expects"), "{err}");
}

#[test]
fn test_repl_store_bool_and_u64_results() {
    // bool and u64 are first-class compiler types: store.exists and
    // calculate-total-size are declared with their real `result<bool,_>` /
    // `result<u64,_>` signatures (so the interface hash matches Theater), called
    // via raw-invoke, and the codec bridges the scalar results faithfully.
    let mut g = Guest::new();
    let exists = match g.call(
        "evaluate",
        Value::String("(store-exists \"s\" \"ref\")".into()),
    ) {
        Value::String(s) => s,
        other => panic!("{other:?}"),
    };
    assert!(
        exists.contains("bool") && exists.contains("false"),
        "store-exists: {exists}"
    );
    let size = match g.call("evaluate", Value::String("(store-size \"s\")".into())) {
        Value::String(s) => s,
        other => panic!("{other:?}"),
    };
    assert!(
        size.contains("u64") && size.contains("0u64"),
        "store-size: {size}"
    );
}

#[test]
fn test_repl_runtime_list_actors_named_types() {
    // The named-type path: runtime.list-actors is declared with `result<list<
    // actor-info>, runtime-error>`; the compiler registers those Wisp type
    // declarations as pack typedefs so the hash resolves structurally (matching
    // Theater). Here the mock returns one actor-info record; it must decode into
    // an inspectable value showing the record's fields.
    let mut g = Guest::new();
    let out = match g.call("evaluate", Value::String("(list-actors)".into())) {
        Value::String(s) => s,
        other => panic!("{other:?}"),
    };
    assert!(
        out.contains("ok")
            && out.contains("actor-info")
            && out.contains("actor-1")
            && out.contains("counter"),
        "list-actors did not decode as expected: {out}"
    );
}

#[test]
fn test_marshal_string_as_byte_list_args() {
    // The exact shape a writer sends: tuple(string, list<u8>). Decode it host-side
    // and confirm str->bytes produced a real list<u8> (packed Array) and the tuple
    // is well-formed. Regression for the element-template bug (a byte-value template
    // has symbol-name "" so it fell into a List node and pack rejected the blob).
    let mut g = Guest::new();
    let got = g.call("m-strbytes", Value::String("hi".into()));
    assert_eq!(
        got,
        Value::Tuple(vec![
            Value::String("id".into()),
            Value::List {
                elem_type: ValueType::U8,
                items: vec![Value::U8(b'h'), Value::U8(b'i')],
            },
        ]),
        "args blob did not decode to tuple(string, list<u8>): {got:?}"
    );
}

#[test]
fn test_repl_interface_qualified_collision() {
    // store.exists and filesystem.exists share a bare name but live in different
    // interfaces; the raw symbol is interface-qualified, so each REPL verb reaches
    // its own host import. store-exists (mock) -> false, fs-exists (mock) -> true.
    let mut g = Guest::new();
    let store = match g.call(
        "evaluate",
        Value::String("(store-exists \"s\" \"r\")".into()),
    ) {
        Value::String(s) => s,
        other => panic!("{other:?}"),
    };
    assert!(store.contains("bool") && store.contains("false"), "{store}");
    let fs = match g.call("evaluate", Value::String("(fs-exists \"f\")".into())) {
        Value::String(s) => s,
        other => panic!("{other:?}"),
    };
    assert!(fs.contains("bool") && fs.contains("true"), "{fs}");
}

#[test]
fn test_marshal_http_request_record_arg() {
    // The record-valued argument path: build an http-request record (empty headers,
    // none body) and marshal it; the host decodes it to a Record with the right
    // fields. The empty list<http-header> and option<list<u8>> carry their element/
    // inner type tags.
    let mut g = Guest::new();
    let got = g.call(
        "m-http-req",
        Value::Tuple(vec![
            Value::String("GET".into()),
            Value::String("https://example.com".into()),
        ]),
    );
    let Value::Record { type_name, fields } = got else {
        panic!("expected http-request record, got {got:?}")
    };
    assert_eq!(type_name, "http-request");
    let get = |n: &str| fields.iter().find(|(k, _)| k == n).map(|(_, v)| v);
    assert_eq!(get("method"), Some(&Value::String("GET".into())));
    assert_eq!(
        get("url"),
        Some(&Value::String("https://example.com".into()))
    );
    assert!(
        matches!(get("headers"), Some(Value::List { items, .. }) if items.is_empty()),
        "headers: {:?}",
        get("headers")
    );
    assert!(
        matches!(get("body"), Some(Value::Option { value: None, .. })),
        "body: {:?}",
        get("body")
    );
}

#[test]
fn test_repl_http_get_response_record() {
    // (http-get url) -> the verb builds+marshals the request record, calls the
    // mock, and unmarshals the http-response record (u16 status + nested
    // list<http-header>) to an inspectable value.
    let mut g = Guest::new();
    let out = match g.call(
        "evaluate",
        Value::String("(http-get \"https://example.com\")".into()),
    ) {
        Value::String(s) => s,
        other => panic!("{other:?}"),
    };
    assert!(
        out.contains("ok")
            && out.contains("http-response")
            && out.contains("200")
            && out.contains("content-type"),
        "http-get response not decoded as expected: {out}"
    );
}

#[test]
fn test_repl_inbound_event_dispatch() {
    // The live-image inbound path: define on-tick in the session, fire a simulated
    // trigger (what the actor's handle-tick export does), and (poll-events) shows
    // the firing dispatched to the user's definition with its result.
    let mut g = Guest::new();
    assert_eq!(
        g.call(
            "evaluate",
            Value::String("(define on-tick (lambda (n) (string-append \"got \" n)))".into()),
        ),
        Value::String("#<closure>".into())
    );
    // No events buffered yet.
    assert_eq!(
        g.call("evaluate", Value::String("(poll-events)".into())),
        Value::String("()".into())
    );
    g.call("fire-tick", Value::String("beat".into()));
    g.call("fire-tick", Value::String("beat".into()));
    let out = match g.call("evaluate", Value::String("(poll-events)".into())) {
        Value::String(s) => s,
        other => panic!("{other:?}"),
    };
    assert!(
        out.matches("on-tick").count() == 2 && out.contains("got beat"),
        "poll-events did not show dispatched ticks: {out}"
    );
    // Drained.
    assert_eq!(
        g.call("evaluate", Value::String("(poll-events)".into())),
        Value::String("()".into())
    );
}

#[test]
fn test_repl_store_put_writer() {
    // (store-put id content): content string -> list<u8> via str->bytes, marshalled
    // as a tuple, result unmarshalled. Mock replies ok("deadbeefhash").
    let mut g = Guest::new();
    let out = match g.call(
        "evaluate",
        Value::String("(store-put \"s\" \"hi there\")".into()),
    ) {
        Value::String(s) => s,
        other => panic!("{other:?}"),
    };
    assert!(
        out.contains("ok") && out.contains("deadbeefhash"),
        "store-put did not decode as expected: {out}"
    );
}

#[test]
fn test_repl_runtime_more_verbs() {
    // get-actor-manifest -> result<string>, and get-actor-state ->
    // result<option<list<u8>>> (the option-of-byte-buffer ok arm). Both decode to
    // inspectable values, confirming the extended runtime verbs dispatch + the
    // codec handles option<list<u8>> inside a result.
    let mut g = Guest::new();
    let man = match g.call("evaluate", Value::String("(actor-manifest \"a\")".into())) {
        Value::String(s) => s,
        other => panic!("{other:?}"),
    };
    assert!(
        man.contains("ok") && man.contains("demo"),
        "actor-manifest: {man}"
    );
    let state = match g.call("evaluate", Value::String("(actor-state \"a\")".into())) {
        Value::String(s) => s,
        other => panic!("{other:?}"),
    };
    assert!(
        state.contains("ok") && state.contains("some"),
        "actor-state: {state}"
    );
    // Arity errors stay ordinary diagnostics.
    let err = match g.call("evaluate", Value::String("(kill-actor)".into())) {
        Value::String(s) => s,
        other => panic!("{other:?}"),
    };
    assert!(err.contains("kill-actor expects"), "{err}");
}

#[test]
fn test_unmarshal_compound_typed_roundtrips() {
    // These carry MULTI-BYTE type tags (write-type-tag's job): a result whose ok
    // type is list<string>, and a nested option<option<s32>>. Both directions.
    let mut g = Guest::new();
    let res = Value::Result {
        ok_type: ValueType::List(Box::new(ValueType::String)),
        err_type: ValueType::String,
        value: Ok(Box::new(Value::List {
            elem_type: ValueType::String,
            items: vec![Value::String("a".into()), Value::String("b".into())],
        })),
    };
    assert_eq!(g.call("u-remarshal", res.clone()), res);

    let opt = Value::Option {
        inner_type: ValueType::Option(Box::new(ValueType::S32)),
        value: Some(Box::new(Value::Option {
            inner_type: ValueType::S32,
            value: Some(Box::new(Value::S32(5))),
        })),
    };
    assert_eq!(g.call("u-remarshal", opt.clone()), opt);

    // result<string,string> err arm, and option<s32> none — still fine.
    let err = Value::Result {
        ok_type: ValueType::String,
        err_type: ValueType::String,
        value: Err(Box::new(Value::String("nope".into()))),
    };
    assert_eq!(g.call("u-remarshal", err.clone()), err);
}
