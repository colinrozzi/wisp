(module
  (memory 1 100)
  (func $store-and-load (param $val i32) (result i32)
    (local i32)
    (local i32)
    (local i32)
    i32.const 0
    local.set 1
    local.get 0
    local.set 2
    local.get 1
    local.get 2
    i32.store
    local.get 2
    local.set 3
    local.get 1
    i32.load
  )
  (func $memory-ops (result i32)
    (local i32)
    (local i32)
    (local i32)
    memory.size
    local.set 0
    i32.const 1
    memory.grow
    local.set 1
    memory.size
    local.set 2
    local.get 2
  )
  (export "store-and-load" (func $store-and-load))
  (export "memory-ops" (func $memory-ops))
)
