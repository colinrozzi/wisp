use std::path::PathBuf;
use std::process::Command;
use std::sync::OnceLock;

use granite::compiler::{self, EmitOptions};
use pack::abi::{Value, ValueType};
use wasmtime::{Config, Engine, Instance, Memory, Module, Store};

fn package() -> &'static PathBuf {
    static PACKAGE: OnceLock<PathBuf> = OnceLock::new();
    PACKAGE.get_or_init(|| {
        let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let out = root.join(format!("target/cgrf-tests/{}/values", std::process::id()));
        compiler::compile(
            &root.join("tests/fixtures/cgrf_v3.wisp"),
            &out,
            EmitOptions::default(),
        )
        .unwrap()
        .wasm
    })
}

struct Guest {
    store: Store<()>,
    instance: Instance,
    memory: Memory,
}

impl Guest {
    fn new() -> Self {
        let mut config = Config::new();
        config.wasm_tail_call(true);
        let engine = Engine::new(&config).unwrap();
        let module = Module::from_file(&engine, package()).unwrap();
        let mut store = Store::new(&engine, ());
        let instance = Instance::new(&mut store, &module, &[]).unwrap();
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

    fn read_output(&self, slots: i32) -> Vec<u8> {
        let mut ptr_len = [0u8; 8];
        self.memory
            .read(&self.store, slots as usize, &mut ptr_len)
            .unwrap();
        let ptr = u32::from_le_bytes(ptr_len[..4].try_into().unwrap()) as usize;
        let len = u32::from_le_bytes(ptr_len[4..].try_into().unwrap()) as usize;
        let mut bytes = vec![0; len];
        self.memory.read(&self.store, ptr, &mut bytes).unwrap();
        assert_v3(&bytes);
        bytes
    }

    fn call(&mut self, name: &str, input: Value) -> Value {
        let input = pack::encode(&input).unwrap();
        assert_v3(&input);
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
        pack::decode(&self.read_output(slots)).unwrap()
    }
}

fn assert_v3(bytes: &[u8]) {
    assert_eq!(&bytes[..4], b"CGRF");
    assert_eq!(u16::from_le_bytes(bytes[4..6].try_into().unwrap()), 3);
}

#[test]
fn test_cgrf_metadata_uses_v3() {
    let mut guest = Guest::new();
    let slots = guest.alloc(8);
    let status = guest
        .instance
        .get_typed_func::<(i32, i32), i32>(&mut guest.store, "__pack_types")
        .unwrap()
        .call(&mut guest.store, (slots, slots + 4))
        .unwrap();
    assert_eq!(status, 0);
    let metadata = pack::decode_metadata_with_hashes(&guest.read_output(slots)).unwrap();
    assert!(
        metadata
            .export_hashes
            .iter()
            .any(|interface| interface.name == "exports")
    );
}

#[test]
fn test_cgrf_scalar_string_and_list_roundtrips() {
    let mut guest = Guest::new();
    assert_eq!(guest.call("answer", Value::Tuple(vec![])), Value::S32(42));
    assert_eq!(
        guest.call("identity64", Value::S64(i64::MAX)),
        Value::S64(i64::MAX)
    );
    assert_eq!(
        guest.call("add", Value::Tuple(vec![Value::S32(17), Value::S32(25)])),
        Value::S32(42)
    );
    for text in ["", "hello, λ 🌍"] {
        let input = Value::String(text.into());
        assert_eq!(guest.call("echo", input.clone()), input);
    }
    for items in [vec![], vec![Value::S32(-1), Value::S32(42)]] {
        let input = Value::List {
            elem_type: ValueType::S32,
            items,
        };
        assert_eq!(guest.call("list-id", input.clone()), input);
    }
}

#[test]
fn test_cgrf_primitive_array_widths() {
    let mut guest = Guest::new();
    for (name, elem_type, items) in [
        (
            "bytes-id",
            ValueType::U8,
            vec![Value::U8(0), Value::U8(255), Value::U8(17)],
        ),
        (
            "list64-id",
            ValueType::S64,
            vec![Value::S64(i64::MIN), Value::S64(i64::MAX)],
        ),
        (
            "floats-id",
            ValueType::F32,
            vec![Value::F32(-1.25), Value::F32(3.5)],
        ),
        (
            "doubles-id",
            ValueType::F64,
            vec![Value::F64(-1.25), Value::F64(3.5)],
        ),
    ] {
        for items in [vec![], items] {
            let input = Value::List {
                elem_type: elem_type.clone(),
                items,
            };
            assert_eq!(guest.call(name, input.clone()), input, "{name}");
        }
    }
}

#[test]
fn test_cgrf_lists_of_strings_and_arrays() {
    let mut guest = Guest::new();
    let tuple = Value::Tuple(vec![
        Value::String("λ".into()),
        Value::List {
            elem_type: ValueType::U8,
            items: vec![Value::U8(255), Value::U8(42)],
        },
    ]);
    assert_eq!(guest.call("tuple-id", tuple.clone()), tuple);
    let strings = Value::List {
        elem_type: ValueType::String,
        items: vec![Value::String("λ".into()), Value::String("".into())],
    };
    assert_eq!(guest.call("strings-id", strings.clone()), strings);
    let nested = Value::List {
        elem_type: ValueType::List(Box::new(ValueType::S32)),
        items: vec![
            Value::List {
                elem_type: ValueType::S32,
                items: vec![Value::S32(42), Value::S32(-1)],
            },
            Value::List {
                elem_type: ValueType::S32,
                items: vec![],
            },
        ],
    };
    assert_eq!(guest.call("nested-id", nested.clone()), nested);
}

#[test]
fn test_cgrf_option_and_result_values() {
    let mut guest = Guest::new();
    for (name, value) in [
        ("some-value", Some(Box::new(Value::S32(42)))),
        ("none-value", None),
    ] {
        assert_eq!(
            guest.call(name, Value::Tuple(vec![])),
            Value::Option {
                inner_type: ValueType::S32,
                value
            }
        );
    }
    for (name, value) in [
        ("ok-value", Ok(Box::new(Value::S32(42)))),
        ("err-value", Err(Box::new(Value::S32(-1)))),
    ] {
        assert_eq!(
            guest.call(name, Value::Tuple(vec![])),
            Value::Result {
                ok_type: ValueType::S32,
                err_type: ValueType::S32,
                value
            }
        );
    }
}

#[test]
fn test_cgrf_pack_runtime_roundtrip() {
    let runtime = pack::Runtime::new();
    let bytes = std::fs::read(package()).unwrap();
    let module = runtime.load_module(&bytes).unwrap();
    let mut instance = module.instantiate().unwrap();
    assert_eq!(
        instance
            .call_with_value("answer", &Value::Tuple(vec![]))
            .unwrap(),
        Value::S32(42)
    );
}

#[test]
fn test_cgrf_cli_and_dependency_bridge() {
    let cli = env!("CARGO_BIN_EXE_granite");
    let output = Command::new(cli)
        .arg("run-module")
        .arg(package())
        .arg("answer")
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8(output.stdout).unwrap().trim(), "42");

    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let imported = compiler::compile(
        &root.join("tests/fixtures/cgrf_v3_import.wisp"),
        &package().parent().unwrap().join("imported"),
        EmitOptions::default(),
    )
    .unwrap();
    let output = Command::new(cli)
        .arg("run-module")
        .arg(&imported.wasm)
        .arg("imported-echo")
        .args(["--input", "hello, λ 🌍", "--dep"])
        .arg(format!("values={}", package().display()))
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        String::from_utf8(output.stdout).unwrap().trim(),
        "hello, λ 🌍"
    );

    let output = Command::new(cli)
        .arg("run-module")
        .arg(&imported.wasm)
        .arg("imported-list")
        .arg("--dep")
        .arg(format!("values={}", package().display()))
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8(output.stdout).unwrap().trim(), "42");

    let output = Command::new(cli)
        .arg("run-module")
        .arg(&imported.wasm)
        .arg("imported-tuple")
        .arg("--dep")
        .arg(format!("values={}", package().display()))
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let result = String::from_utf8(output.stdout).unwrap();
    assert!(result.contains("λ") && result.contains("U8"), "{result}");
}

#[test]
fn test_cgrf_any_scalar_roundtrips() {
    let mut guest = Guest::new();
    // A top-level dynamic `any` must survive the guest boundary unchanged for
    // each scalar node kind: decode copies the input CGRF into a len-prefixed
    // blob, and encode copies it straight back out.
    for value in [
        Value::S32(42),
        Value::S32(-1),
        Value::S64(i64::MIN),
        Value::Bool(true),
        Value::U32(7),
        Value::String("hello, λ".into()),
    ] {
        assert_eq!(
            guest.call("roundtrip", value.clone()),
            value,
            "roundtrip {value:?}"
        );
    }
}

#[test]
fn test_cgrf_any_inspect_and_construct() {
    let mut guest = Guest::new();
    // any-as-s32 reads the scalar out of the incoming CGRF value; any-s32 builds
    // a fresh CGRF S32 node that the boundary encodes back to the host.
    assert_eq!(guest.call("any-inc", Value::S32(41)), Value::S32(42));
    assert_eq!(guest.call("any-inc", Value::S32(-1)), Value::S32(0));
}

#[test]
fn test_cgrf_any_string_inspect_and_construct() {
    let mut guest = Guest::new();
    // any-as-string views the CGRF string payload in place (zero-copy), and
    // any-string builds a fresh CGRF String node the boundary encodes out.
    for (input, want) in [("hi", "hi!"), ("", "!"), ("héllo λ", "héllo λ!")] {
        assert_eq!(
            guest.call("any-shout", Value::String(input.into())),
            Value::String(want.into()),
        );
    }
}

#[test]
fn test_cgrf_any_echo_wisp_codec() {
    let mut guest = Guest::new();
    // any-echo copies the whole CGRF blob in pure Wisp using only the byte-view
    // primitives (any-addr / heap-alloc / any-from-addr). Every value kind must
    // survive — proving the marshal/unmarshal codec needs no per-type support.
    let item = Value::Record {
        type_name: "todo-item".into(),
        fields: vec![
            ("id".into(), Value::U32(1)),
            ("title".into(), Value::String("ship it".into())),
            ("done".into(), Value::Bool(false)),
        ],
    };
    for value in [
        Value::S32(42),
        Value::Bool(true),
        Value::String("héllo λ".into()),
        Value::List {
            elem_type: ValueType::S32,
            items: vec![Value::S32(1), Value::S32(2), Value::S32(3)],
        },
        item,
    ] {
        assert_eq!(
            guest.call("any-echo", value.clone()),
            value,
            "echo {value:?}"
        );
    }
}
