//! The import side of the dynamic `any` bridge: a Wisp guest calls a host import
//! typed `-> value` (as `theater:simple/rpc.describe` is) and hands the returned
//! dynamic value straight back out. A mock host supplies a canned value; the
//! guest must decode it into its len-prefixed blob and re-encode it unchanged.

use std::path::PathBuf;
use std::sync::OnceLock;

use pack::abi::{Value, ValueType};
use wasmtime::{Caller, Config, Engine, Linker, Module, Store};
use wisp::compiler::{self, EmitOptions};

fn fixture_wasm() -> &'static PathBuf {
    static WASM: OnceLock<PathBuf> = OnceLock::new();
    WASM.get_or_init(|| {
        let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let out = root.join(format!(
            "target/rpc-any-tests/{}/rpc_any",
            std::process::id()
        ));
        compiler::compile(
            &root.join("tests/fixtures/rpc_any.wisp"),
            &out,
            EmitOptions::default(),
        )
        .unwrap()
        .wasm
    })
}

/// Call `probe` with a mock `rpc.describe` host that returns `canned`, and return
/// what the guest produced for the host to decode.
fn probe_with(canned: Value) -> Value {
    let mut config = Config::new();
    config.wasm_tail_call(true);
    let engine = Engine::new(&config).unwrap();
    let module = Module::from_file(&engine, fixture_wasm()).unwrap();

    let mut linker = Linker::new(&engine);
    let reply = pack::encode(&canned).unwrap();
    linker
        .func_wrap(
            "theater:simple/rpc",
            "describe",
            move |mut caller: Caller<'_, ()>,
                  _in_ptr: i32,
                  _in_len: i32,
                  out_ptr_ptr: i32,
                  out_len_ptr: i32|
                  -> i32 {
                // Allocate the reply in guest memory via its own allocator, write
                // the CGRF bytes, and publish ptr/len into the provided slots.
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

    let mut store = Store::new(&engine, ());
    let instance = linker.instantiate(&mut store, &module).unwrap();
    let memory = instance.get_memory(&mut store, "memory").unwrap();
    let alloc = instance
        .get_typed_func::<i32, i32>(&mut store, "__pack_alloc")
        .unwrap();

    // probe(id): encode the string arg, call, read the CGRF result back.
    let arg = pack::encode(&Value::String("actor-123".into())).unwrap();
    let in_ptr = alloc.call(&mut store, arg.len() as i32).unwrap();
    let slots = alloc.call(&mut store, 8).unwrap();
    memory.write(&mut store, in_ptr as usize, &arg).unwrap();
    let status = instance
        .get_typed_func::<(i32, i32, i32, i32), i32>(&mut store, "probe")
        .unwrap()
        .call(&mut store, (in_ptr, arg.len() as i32, slots, slots + 4))
        .unwrap();
    assert_eq!(status, 0);

    let mut ptr_len = [0u8; 8];
    memory.read(&store, slots as usize, &mut ptr_len).unwrap();
    let ptr = u32::from_le_bytes(ptr_len[..4].try_into().unwrap()) as usize;
    let len = u32::from_le_bytes(ptr_len[4..].try_into().unwrap()) as usize;
    let mut bytes = vec![0; len];
    memory.read(&store, ptr, &mut bytes).unwrap();
    pack::decode(&bytes).unwrap()
}

#[test]
fn test_rpc_any_scalar_result() {
    assert_eq!(probe_with(Value::S32(99)), Value::S32(99));
    assert_eq!(
        probe_with(Value::String("hello from describe".into())),
        Value::String("hello from describe".into())
    );
}

#[test]
fn test_rpc_any_structured_result() {
    // A describe-shaped structured value survives the guest untouched — the guest
    // never inspects it, it just carries the CGRF blob through.
    let item = Value::Record {
        type_name: "todo-item".into(),
        fields: vec![
            ("id".into(), Value::U32(1)),
            ("title".into(), Value::String("ship the bridge".into())),
            ("done".into(), Value::Bool(false)),
        ],
    };
    let wrapped = Value::Result {
        ok_type: ValueType::Record("todo-item".into()),
        err_type: ValueType::String,
        value: Ok(Box::new(item.clone())),
    };
    assert_eq!(probe_with(wrapped.clone()), wrapped);
}
