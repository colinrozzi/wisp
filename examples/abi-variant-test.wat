(module
  (memory 1 100)
  (global $__heap_ptr (mut i32) (i32.const 0))
  (func $shape-area__internal (param $s i32) (result i32)
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
        unreachable
      )
    )
      )
    )
  )
  (func $shape-area (param $s_0 i32) (param $s_1 i32) (param $s_2 i32) (result i32)
    (local i32)
    global.get $__heap_ptr
    local.set 3
    global.get $__heap_ptr
    i32.const 4
    i32.add
    global.set $__heap_ptr
    local.get 3
    local.get $s_0
    i32.store
    local.get 3
    i32.const 4
    i32.add
    local.get $s_1
    i32.store
    local.get 3
    i32.const 8
    i32.add
    local.get $s_2
    i32.store
    local.get 3
    call $shape-area__internal
  )
  (func $test-circle (result i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
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
    local.set 2
    local.get 2
    i32.load
    i32.const 0
    i32.eq
    (if (result i32)
      (then
        local.get 2
        i32.const 4
        i32.add
        i32.load
        local.set 3
        local.get 3
        local.get 3
        i32.mul
        i32.const 3
        i32.mul
      )
      (else
    local.get 2
    i32.load
    i32.const 1
    i32.eq
    (if (result i32)
      (then
        local.get 2
        i32.const 4
        i32.add
        i32.load
        local.set 4
        local.get 2
        i32.const 8
        i32.add
        i32.load
        local.set 5
        local.get 4
        local.get 5
        i32.mul
      )
      (else
        unreachable
      )
    )
      )
    )
  )
  (func $test-rectangle (result i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
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
    i32.const 6
    i32.store
    local.get 0
    local.set 1
    local.get 1
    local.set 2
    local.get 2
    i32.load
    i32.const 0
    i32.eq
    (if (result i32)
      (then
        local.get 2
        i32.const 4
        i32.add
        i32.load
        local.set 3
        local.get 3
        local.get 3
        i32.mul
        i32.const 3
        i32.mul
      )
      (else
    local.get 2
    i32.load
    i32.const 1
    i32.eq
    (if (result i32)
      (then
        local.get 2
        i32.const 4
        i32.add
        i32.load
        local.set 4
        local.get 2
        i32.const 8
        i32.add
        i32.load
        local.set 5
        local.get 4
        local.get 5
        i32.mul
      )
      (else
        unreachable
      )
    )
      )
    )
  )
  (func $cabi_realloc (param $old_ptr i32) (param $old_size i32) (param $align i32) (param $new_size i32) (result i32)
    (local $ptr i32)
    global.get $__heap_ptr
    local.get $align
    i32.add
    i32.const 1
    i32.sub
    local.get $align
    i32.const 1
    i32.sub
    i32.const -1
    i32.xor
    i32.and
    local.set $ptr
    local.get $ptr
    local.get $new_size
    i32.add
    global.set $__heap_ptr
    local.get $ptr
  )
  (export "shape-area" (func $shape-area))
  (export "test-circle" (func $test-circle))
  (export "test-rectangle" (func $test-rectangle))
  (export "memory" (memory 0))
  (export "cabi_realloc" (func $cabi_realloc))
)
