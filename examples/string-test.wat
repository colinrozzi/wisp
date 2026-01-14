(module
  (memory 1 100)
  (global $__heap_ptr (mut i32) (i32.const 0))
  (func $test-string-len (result i32)
    (local i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 9
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 5
    i32.store
    local.get 0
    i32.const 4
    i32.add
    i32.const 104
    i32.store8
    local.get 0
    i32.const 5
    i32.add
    i32.const 101
    i32.store8
    local.get 0
    i32.const 6
    i32.add
    i32.const 108
    i32.store8
    local.get 0
    i32.const 7
    i32.add
    i32.const 108
    i32.store8
    local.get 0
    i32.const 8
    i32.add
    i32.const 111
    i32.store8
    local.get 0
    local.set 1
    local.get 1
    i32.load
  )
  (func $test-empty-string (result i32)
    (local i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 4
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 0
    i32.store
    local.get 0
    local.set 1
    local.get 1
    i32.load
  )
  (func $test-long-string (result i32)
    (local i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 16
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 12
    i32.store
    local.get 0
    i32.const 4
    i32.add
    i32.const 104
    i32.store8
    local.get 0
    i32.const 5
    i32.add
    i32.const 101
    i32.store8
    local.get 0
    i32.const 6
    i32.add
    i32.const 108
    i32.store8
    local.get 0
    i32.const 7
    i32.add
    i32.const 108
    i32.store8
    local.get 0
    i32.const 8
    i32.add
    i32.const 111
    i32.store8
    local.get 0
    i32.const 9
    i32.add
    i32.const 32
    i32.store8
    local.get 0
    i32.const 10
    i32.add
    i32.const 119
    i32.store8
    local.get 0
    i32.const 11
    i32.add
    i32.const 111
    i32.store8
    local.get 0
    i32.const 12
    i32.add
    i32.const 114
    i32.store8
    local.get 0
    i32.const 13
    i32.add
    i32.const 108
    i32.store8
    local.get 0
    i32.const 14
    i32.add
    i32.const 100
    i32.store8
    local.get 0
    i32.const 15
    i32.add
    i32.const 33
    i32.store8
    local.get 0
    local.set 1
    local.get 1
    i32.load
  )
  (func $test-escape-string (result i32)
    (local i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 7
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 3
    i32.store
    local.get 0
    i32.const 4
    i32.add
    i32.const 97
    i32.store8
    local.get 0
    i32.const 5
    i32.add
    i32.const 10
    i32.store8
    local.get 0
    i32.const 6
    i32.add
    i32.const 98
    i32.store8
    local.get 0
    local.set 1
    local.get 1
    i32.load
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
  (export "test-string-len" (func $test-string-len))
  (export "test-empty-string" (func $test-empty-string))
  (export "test-long-string" (func $test-long-string))
  (export "test-escape-string" (func $test-escape-string))
  (export "memory" (memory 0))
  (export "cabi_realloc" (func $cabi_realloc))
)
