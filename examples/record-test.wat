(module
  (memory 1 100)
  (global $__heap_ptr (mut i32) (i32.const 0))
  (func $make-point (param $x i32) (param $y i32) (result i32)
    (local i32)
    global.get $__heap_ptr
    local.set 2
    global.get $__heap_ptr
    i32.const 8
    i32.add
    global.set $__heap_ptr
    local.get 2
    local.get 0
    i32.store
    local.get 2
    i32.const 4
    i32.add
    local.get 1
    i32.store
    local.get 2
  )
  (func $get-x (param $p i32) (result i32)
    local.get 0
    i32.load
  )
  (func $get-y (param $p i32) (result i32)
    local.get 0
    i32.const 4
    i32.add
    i32.load
  )
  (func $add-x (param $a i32) (param $b i32) (result i32)
    local.get 0
    i32.load
    local.get 1
    i32.load
    i32.add
  )
  (func $test (result i32)
    (local i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 8
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 10
    i32.store
    local.get 0
    i32.const 4
    i32.add
    i32.const 20
    i32.store
    local.get 0
    local.set 1
    local.get 1
    i32.load
    local.get 1
    i32.const 4
    i32.add
    i32.load
    i32.add
  )
  (func $test2 (result i32)
    (local i32)
    i32.const 5
    i32.const 7
    call $make-point
    local.set 0
    local.get 0
    call $get-y
  )
  (export "test" (func $test))
  (export "test2" (func $test2))
)
