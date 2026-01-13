(module
  (memory 1 100)
  (global $__heap_ptr (mut i32) (i32.const 0))
  (func $area (param $s i32) (result i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    local.get 0
    local.set 1
    local.get 1
    i32.load
    i32.const 0
    i32.eq
    (if (result i32)
      (then
        local.get 1
        i32.const 4
        i32.add
        i32.load
        local.set 2
        local.get 2
        local.get 2
        i32.mul
        i32.const 3
        i32.mul
      )
      (else
    local.get 1
    i32.load
    i32.const 1
    i32.eq
    (if (result i32)
      (then
        local.get 1
        i32.const 4
        i32.add
        i32.load
        local.set 3
        local.get 1
        i32.const 8
        i32.add
        i32.load
        local.set 4
        local.get 3
        local.get 4
        i32.mul
      )
      (else
    local.get 1
    i32.load
    i32.const 2
    i32.eq
    (if (result i32)
      (then
        i32.const 0
      )
      (else
        unreachable
      )
    )
      )
    )
      )
    )
  )
  (func $test-circle (result i32)
    (local i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 12
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 0
    i32.store
    local.get 0
    i32.const 4
    i32.add
    i32.const 5
    i32.store
    local.get 0
    local.set 1
    local.get 1
    call $area
  )
  (func $test-rect (result i32)
    (local i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 12
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 1
    i32.store
    local.get 0
    i32.const 4
    i32.add
    i32.const 4
    i32.store
    local.get 0
    i32.const 8
    i32.add
    i32.const 7
    i32.store
    local.get 0
    local.set 1
    local.get 1
    call $area
  )
  (func $test-point (result i32)
    (local i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 12
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 2
    i32.store
    local.get 0
    local.set 1
    local.get 1
    call $area
  )
  (export "test-circle" (func $test-circle))
  (export "test-rect" (func $test-rect))
  (export "test-point" (func $test-point))
)
