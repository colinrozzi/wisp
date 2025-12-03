(module
  (memory 1 100)
  (func $init-heap (result i32)
    (local i32)
    (local i32)
    (local i32)
    i32.const 16
    local.set 0
    local.get 0
    local.set 1
    i32.const 0
    local.get 1
    i32.store
    local.get 1
    local.set 2
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
  (func $write-i32 (param $addr i32) (param $val i32) (result i32)
    (local i32)
    local.get 1
    local.set 2
    local.get 0
    local.get 2
    i32.store
    local.get 2
  )
  (func $read-i32 (param $addr i32) (result i32)
    local.get 0
    i32.load
  )
  (func $test-allocator (result i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
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
    i32.const 4
    call $alloc
    local.set 3
    local.get 1
    i32.const 100
    call $write-i32
    local.set 4
    local.get 2
    i32.const 200
    call $write-i32
    local.set 5
    local.get 3
    i32.const 300
    call $write-i32
    local.set 6
    local.get 1
    call $read-i32
    local.set 7
    local.get 2
    call $read-i32
    local.set 8
    local.get 3
    call $read-i32
    local.set 9
    local.get 7
    local.get 8
    i32.add
    local.get 9
    i32.add
  )
  (export "init-heap" (func $init-heap))
  (export "alloc" (func $alloc))
  (export "write-i32" (func $write-i32))
  (export "read-i32" (func $read-i32))
  (export "test-allocator" (func $test-allocator))
)
