//! Tiny real guest that forwards Pack ABI buffers to Theater's RPC handler.
//! This proves guest -> RPC import -> target mailbox -> evaluator, including
//! discovery, without requiring a second language toolchain for the fixture.
use pack::abi::{Value, ValueType};
use pack::types::{Arena, Function, Param, Type};

pub fn rpc_guest() -> Vec<u8> {
    let call = Function::with_signature(
        "call",
        vec![
            Param::new("actor-id", Type::String),
            Param::new("function", Type::String),
            Param::new("params", Type::Value),
            Param::new("options", Type::Value),
        ],
        vec![Type::Value],
    );
    let describe = Function::with_signature(
        "describe",
        vec![Param::new("actor-id", Type::String)],
        vec![Type::Value],
    );
    let mut package = Arena::new("package");
    let mut imports = Arena::new("imports");
    let mut rpc = Arena::new("theater:simple/rpc");
    rpc.add_function(call.clone());
    rpc.add_function(describe.clone());
    imports.add_child(rpc);
    package.add_child(imports);
    let mut exports = Arena::new("exports");
    let mut relay = Arena::new("test:rpc/relay");
    relay.add_function(call);
    relay.add_function(describe);
    exports.add_child(relay);
    let mut actor = Arena::new("theater:simple/actor");
    actor.add_function(Function::with_signature(
        "init",
        vec![Param::new("config", Type::Value)],
        vec![Type::Result {
            ok: Box::new(Type::Tuple(vec![])),
            err: Box::new(Type::String),
        }],
    ));
    exports.add_child(actor);
    package.add_child(exports);
    let metadata = pack::metadata::encode_metadata_with_hashes(&package).unwrap();
    let success = pack::abi::encode(&Value::Result {
        ok_type: ValueType::Tuple(vec![]),
        err_type: ValueType::String,
        value: Ok(Box::new(Value::Tuple(vec![]))),
    })
    .unwrap();
    let escape = |bytes: &[u8]| -> String { bytes.iter().map(|b| format!("\\{b:02x}")).collect() };
    wat::parse_str(format!(r#"(module
        (import "theater:simple/rpc" "call" (func $call (param i32 i32 i32 i32) (result i32)))
        (import "theater:simple/rpc" "describe" (func $describe (param i32 i32 i32 i32) (result i32)))
        (memory (export "memory") 64)
        (global $heap (mut i32) (i32.const 65536))
        (data (i32.const 40960) "{}")
        (data (i32.const 32768) "{}")
        (func (export "__pack_types") (param i32 i32) (result i32)
            local.get 0 i32.const 40960 i32.store local.get 1 i32.const {} i32.store i32.const 0)
        (func (export "__pack_alloc") (param $size i32) (result i32) (local $ptr i32)
            global.get $heap local.tee $ptr local.get $size i32.add i32.const 7 i32.add
            i32.const -8 i32.and global.set $heap local.get $ptr)
        (func (export "__pack_free") (param i32 i32))
        (func (export "theater:simple/actor.init") (param i32 i32 i32 i32) (result i32)
            local.get 2 i32.const 32768 i32.store local.get 3 i32.const {} i32.store i32.const 0)
        (func (export "test:rpc/relay.call") (param i32 i32 i32 i32) (result i32)
            local.get 0 local.get 1 local.get 2 local.get 3 call $call
            i32.const 0 i32.lt_s if unreachable end i32.const 0)
        (func (export "test:rpc/relay.describe") (param i32 i32 i32 i32) (result i32)
            local.get 0 local.get 1 local.get 2 local.get 3 call $describe
            i32.const 0 i32.lt_s if unreachable end i32.const 0)
    )"#, escape(&metadata), escape(&success), metadata.len(), success.len())).unwrap()
}

pub fn field<'a>(value: &'a Value, key: &str) -> &'a Value {
    match value {
        Value::Record { fields, .. } => &fields.iter().find(|(name, _)| name == key).unwrap().1,
        other => panic!("expected record, got {other:?}"),
    }
}

pub fn items(value: &Value) -> &[Value] {
    match value {
        Value::List { items, .. } => items,
        other => panic!("expected list, got {other:?}"),
    }
}
