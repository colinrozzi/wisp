//! A typed import whose host call fails (status -1, as when the host cannot
//! decode a string argument that is not UTF-8) writes no result. The wrapper
//! must not decode one from address 0 (ticket 0050): a `(result T string)`
//! gives `(err "host call failed")`, and other result types trap cleanly.

use std::path::PathBuf;
use std::sync::OnceLock;

use granite::compiler::{self, EmitOptions};
use wasmtime::{Caller, Config, Engine, Linker, Module, Store};

const SOURCE: &str = r#"
(import host fetch ((key string)) (result s32 string))
(import host text ((key string)) string)
(import host count ((key string)) s32)

(export (fn probe-fetch ((k string)) string
  (match (fetch k) ((ok v) "ok") ((err e) e))))
(export (fn probe-text ((k string)) string (text k)))
(export (fn probe-count ((k string)) s32 (count k)))
"#;

fn fixture_wasm() -> &'static PathBuf {
    static WASM: OnceLock<PathBuf> = OnceLock::new();
    WASM.get_or_init(|| {
        let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let dir = root.join(format!("target/import-failed-tests/{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let src = dir.join("import_failed.wisp");
        std::fs::write(&src, SOURCE).unwrap();
        compiler::compile(&src, &dir.join("import_failed"), EmitOptions::default())
            .unwrap()
            .wasm
    })
}

/// Call export `name` with a string argument against a host whose every import
/// fails with status -1 and writes nothing. Returns the export's status and the
/// CGRF it wrote, or the trap.
fn call_failing(name: &str) -> wasmtime::Result<(i32, Vec<u8>)> {
    let mut config = Config::new();
    config.wasm_tail_call(true);
    let engine = Engine::new(&config).unwrap();
    let module = Module::from_file(&engine, fixture_wasm()).unwrap();
    let mut linker = Linker::new(&engine);
    for import in ["fetch", "text", "count"] {
        linker
            .func_wrap(
                "host",
                import,
                |_: Caller<'_, ()>, _: i32, _: i32, _: i32, _: i32| -> i32 { -1 },
            )
            .unwrap();
    }
    let mut store = Store::new(&engine, ());
    let instance = linker.instantiate(&mut store, &module).unwrap();
    let memory = instance.get_memory(&mut store, "memory").unwrap();
    let alloc = instance
        .get_typed_func::<i32, i32>(&mut store, "__pack_alloc")
        .unwrap();
    let arg = pack::encode(&pack::abi::Value::String("k".into())).unwrap();
    let in_ptr = alloc.call(&mut store, arg.len() as i32).unwrap();
    let slots = alloc.call(&mut store, 8).unwrap();
    memory.write(&mut store, in_ptr as usize, &arg).unwrap();
    let status = instance
        .get_typed_func::<(i32, i32, i32, i32), i32>(&mut store, name)
        .unwrap()
        .call(&mut store, (in_ptr, arg.len() as i32, slots, slots + 4))?;
    let mut ptr_len = [0u8; 8];
    memory.read(&store, slots as usize, &mut ptr_len).unwrap();
    let ptr = u32::from_le_bytes(ptr_len[..4].try_into().unwrap()) as usize;
    let len = u32::from_le_bytes(ptr_len[4..].try_into().unwrap()) as usize;
    let mut bytes = vec![0; len];
    memory.read(&store, ptr, &mut bytes).unwrap();
    Ok((status, bytes))
}

#[test]
fn test_failed_result_import_is_an_err() {
    let (status, bytes) = call_failing("probe-fetch").unwrap();
    assert_eq!(status, 0);
    assert_eq!(
        pack::decode(&bytes).unwrap(),
        pack::abi::Value::String("host call failed".into())
    );
}

#[test]
fn test_failed_import_without_an_err_traps() {
    for name in ["probe-text", "probe-count"] {
        let err = call_failing(name).unwrap_err();
        let trap = err.downcast_ref::<wasmtime::Trap>();
        assert_eq!(
            trap,
            Some(&wasmtime::Trap::UnreachableCodeReached),
            "{name}: {err:?}"
        );
    }
}
