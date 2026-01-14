(module
  (memory 1 100)
  (global $__heap_ptr (mut i32) (i32.const 0))
  (func $make-point__internal (param $x i32) (param $y i32) (result i32)
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
  (func $make-point (param $x_0 i32) (param $y_0 i32) (result i32)
    local.get $x_0
    local.get $y_0
    call $make-point__internal
  )
  (func $point-sum__internal (param $p i32) (result i32)
    local.get 0
    i32.load
    local.get 0
    i32.const 4
    i32.add
    i32.load
    i32.add
  )
  (func $point-sum (param $p_0 i32) (param $p_1 i32) (result i32)
    (local i32)
    global.get $__heap_ptr
    local.set 2
    global.get $__heap_ptr
    i32.const 4
    i32.add
    global.set $__heap_ptr
    local.get 2
    local.get $p_0
    i32.store
    local.get 2
    i32.const 4
    i32.add
    local.get $p_1
    i32.store
    local.get 2
    call $point-sum__internal
  )
  (func $test-internal (result i32)
    (local i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 8
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 15
    i32.store
    local.get 0
    i32.const 4
    i32.add
    i32.const 25
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
  (export "make-point" (func $make-point))
  (export "point-sum" (func $point-sum))
  (export "test-internal" (func $test-internal))
  (export "memory" (memory 0))
  (export "cabi_realloc" (func $cabi_realloc))
)
