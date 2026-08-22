// SPIKE (throwaway): prove the M1 mechanism for the live-image REPL.
// Two separate modules in ONE Store share (a) an imported linear memory and
// (b) an imported *mutable* $__heap_ptr global. Module A bump-allocates and writes;
// module B reads the value back through the pointer. This is exactly what each REPL
// line must do to join one persistent heap.

use wasmtime::{
    Config, Engine, Extern, Global, GlobalType, Instance, Memory, MemoryType, Module,
    Mutability, Store, Val, ValType,
};

const MODULE_A: &str = r#"
(module
  (import "env" "memory" (memory 1))
  (import "env" "__heap_ptr" (global $hp (mut i32)))
  (func (export "alloc") (param $val i32) (result i32)
    (local $ptr i32)
    global.get $hp
    local.set $ptr
    global.get $hp
    i32.const 4
    i32.add
    global.set $hp
    local.get $ptr
    local.get $val
    i32.store
    local.get $ptr))
"#;

const MODULE_B: &str = r#"
(module
  (import "env" "memory" (memory 1))
  (func (export "read") (param $ptr i32) (result i32)
    local.get $ptr
    i32.load))
"#;

#[test]
fn shared_heap_across_two_modules() {
    let engine = Engine::new(&Config::new()).unwrap();
    let mut store = Store::new(&engine, ());

    let memory = Memory::new(&mut store, MemoryType::new(1, None)).unwrap();
    let heap_ptr = Global::new(
        &mut store,
        GlobalType::new(ValType::I32, Mutability::Var),
        Val::I32(1024),
    )
    .unwrap();

    // Module::new accepts WAT text bytes directly (wasmtime `wat` feature is on by default).
    let mod_a = Module::new(&engine, MODULE_A).unwrap();
    let mod_b = Module::new(&engine, MODULE_B).unwrap();

    let a = Instance::new(
        &mut store,
        &mod_a,
        &[Extern::Memory(memory), Extern::Global(heap_ptr)],
    )
    .unwrap();
    let b = Instance::new(&mut store, &mod_b, &[Extern::Memory(memory)]).unwrap();

    let alloc = a.get_typed_func::<i32, i32>(&mut store, "alloc").unwrap();
    let read = b.get_typed_func::<i32, i32>(&mut store, "read").unwrap();

    let p1 = alloc.call(&mut store, 42).unwrap();
    let p2 = alloc.call(&mut store, 99).unwrap();

    assert_eq!(p1, 1024, "first alloc at heap start");
    assert_eq!(p2, 1028, "second alloc bumped the SHARED global");

    assert_eq!(read.call(&mut store, p1).unwrap(), 42);
    assert_eq!(read.call(&mut store, p2).unwrap(), 99);
}
