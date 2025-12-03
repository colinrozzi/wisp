(module
  (memory 1 100)
  (func $init-heap (result i32)
    (local i32)
    i32.const 16
    local.set 0
    i32.const 0
    local.get 0
    i32.store
    local.get 0
  )
  (func $alloc (param $size i32) (result i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    i32.const 0
    local.set 1
    local.get 1
    i32.load
    local.set 2
    local.get 2
    local.get 0
    i32.add
    local.set 3
    local.get 3
    local.set 4
    local.get 1
    local.get 4
    i32.store
    local.get 4
    local.set 5
    local.get 2
  )
  (func $demo-alloc (result i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    call $init-heap
    local.set 0
    i32.const 4
    call $alloc
    local.set 1
    i32.const 4
    call $alloc
    local.set 2
    i32.const 8
    call $alloc
    local.set 3
    local.get 1
    local.get 2
    i32.add
    local.get 3
    i32.add
  )
  (func $demo-write-read (result i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    call $init-heap
    local.set 0
    i32.const 4
    call $alloc
    local.set 1
    i32.const 42
    local.set 2
    local.get 1
    local.get 2
    i32.store
    local.get 2
    local.set 3
    local.get 1
    i32.load
  )
  (export "demo-alloc" (func $demo-alloc))
  (export "demo-write-read" (func $demo-write-read))
)
