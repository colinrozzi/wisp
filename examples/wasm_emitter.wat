(module
  (memory 1 100)
  (global $__heap_ptr (mut i32) (i32.const 0))
  (func $push-bytes (param $bytes i32) (param $b1 i32) (result i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    local.get 0
    local.set 2
    local.get 2
    i32.load
    local.set 3
    local.get 3
    i32.const 1
    i32.add
    i32.const 4
    i32.mul
    local.set 5
    global.get $__heap_ptr
    local.set 4
    global.get $__heap_ptr
    local.get 5
    i32.add
    global.set $__heap_ptr
    local.get 4
    local.get 3
    i32.const 4
    i32.mul
    i32.add
    local.get 1
    i32.store
    local.get 2
    local.get 3
    i32.const 1
    i32.add
    i32.store
    local.get 2
    i32.const 8
    i32.add
    local.get 4
    i32.store
    local.get 2
  )
  (func $push-bytes2 (param $bytes i32) (param $b1 i32) (param $b2 i32) (result i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    local.get 0
    local.set 3
    local.get 3
    i32.load
    local.set 4
    local.get 4
    i32.const 1
    i32.add
    i32.const 4
    i32.mul
    local.set 6
    global.get $__heap_ptr
    local.set 5
    global.get $__heap_ptr
    local.get 6
    i32.add
    global.set $__heap_ptr
    local.get 5
    local.get 4
    i32.const 4
    i32.mul
    i32.add
    local.get 1
    i32.store
    local.get 3
    local.get 4
    i32.const 1
    i32.add
    i32.store
    local.get 3
    i32.const 8
    i32.add
    local.get 5
    i32.store
    local.get 3
    local.set 7
    local.get 7
    i32.load
    local.set 8
    local.get 8
    i32.const 1
    i32.add
    i32.const 4
    i32.mul
    local.set 10
    global.get $__heap_ptr
    local.set 9
    global.get $__heap_ptr
    local.get 10
    i32.add
    global.set $__heap_ptr
    local.get 9
    local.get 8
    i32.const 4
    i32.mul
    i32.add
    local.get 2
    i32.store
    local.get 7
    local.get 8
    i32.const 1
    i32.add
    i32.store
    local.get 7
    i32.const 8
    i32.add
    local.get 9
    i32.store
    local.get 7
  )
  (func $push-bytes4 (param $bytes i32) (param $b1 i32) (param $b2 i32) (param $b3 i32) (param $b4 i32) (result i32)
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
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    local.get 0
    local.set 5
    local.get 5
    i32.load
    local.set 6
    local.get 6
    i32.const 1
    i32.add
    i32.const 4
    i32.mul
    local.set 8
    global.get $__heap_ptr
    local.set 7
    global.get $__heap_ptr
    local.get 8
    i32.add
    global.set $__heap_ptr
    local.get 7
    local.get 6
    i32.const 4
    i32.mul
    i32.add
    local.get 1
    i32.store
    local.get 5
    local.get 6
    i32.const 1
    i32.add
    i32.store
    local.get 5
    i32.const 8
    i32.add
    local.get 7
    i32.store
    local.get 5
    local.set 9
    local.get 9
    i32.load
    local.set 10
    local.get 10
    i32.const 1
    i32.add
    i32.const 4
    i32.mul
    local.set 12
    global.get $__heap_ptr
    local.set 11
    global.get $__heap_ptr
    local.get 12
    i32.add
    global.set $__heap_ptr
    local.get 11
    local.get 10
    i32.const 4
    i32.mul
    i32.add
    local.get 2
    i32.store
    local.get 9
    local.get 10
    i32.const 1
    i32.add
    i32.store
    local.get 9
    i32.const 8
    i32.add
    local.get 11
    i32.store
    local.get 9
    local.set 13
    local.get 13
    i32.load
    local.set 14
    local.get 14
    i32.const 1
    i32.add
    i32.const 4
    i32.mul
    local.set 16
    global.get $__heap_ptr
    local.set 15
    global.get $__heap_ptr
    local.get 16
    i32.add
    global.set $__heap_ptr
    local.get 15
    local.get 14
    i32.const 4
    i32.mul
    i32.add
    local.get 3
    i32.store
    local.get 13
    local.get 14
    i32.const 1
    i32.add
    i32.store
    local.get 13
    i32.const 8
    i32.add
    local.get 15
    i32.store
    local.get 13
    local.set 17
    local.get 17
    i32.load
    local.set 18
    local.get 18
    i32.const 1
    i32.add
    i32.const 4
    i32.mul
    local.set 20
    global.get $__heap_ptr
    local.set 19
    global.get $__heap_ptr
    local.get 20
    i32.add
    global.set $__heap_ptr
    local.get 19
    local.get 18
    i32.const 4
    i32.mul
    i32.add
    local.get 4
    i32.store
    local.get 17
    local.get 18
    i32.const 1
    i32.add
    i32.store
    local.get 17
    i32.const 8
    i32.add
    local.get 19
    i32.store
    local.get 17
  )
  (func $emit-const-i32 (param $value i32) (result i32)
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
    (local i32)
    global.get $__heap_ptr
    local.set 1
    global.get $__heap_ptr
    i32.const 12
    i32.add
    global.set $__heap_ptr
    local.get 1
    i32.const 0
    i32.store
    local.get 1
    i32.const 4
    i32.add
    i32.const 0
    i32.store
    local.get 1
    i32.const 8
    i32.add
    i32.const 0
    i32.store
    local.get 1
    local.set 2
    local.get 2
    i32.const 0
    i32.const 97
    i32.const 115
    i32.const 109
    call $push-bytes4
    local.set 3
    local.get 3
    i32.const 1
    i32.const 0
    i32.const 0
    i32.const 0
    call $push-bytes4
    local.set 4
    local.get 4
    i32.const 1
    call $push-bytes
    local.set 5
    local.get 5
    i32.const 5
    call $push-bytes
    local.set 6
    local.get 6
    i32.const 1
    call $push-bytes
    local.set 7
    local.get 7
    i32.const 96
    call $push-bytes
    local.set 8
    local.get 8
    i32.const 0
    call $push-bytes
    local.set 9
    local.get 9
    i32.const 1
    i32.const 127
    call $push-bytes2
    local.set 10
    local.get 10
    i32.const 3
    i32.const 2
    i32.const 1
    i32.const 0
    call $push-bytes4
    local.set 11
    local.get 11
    i32.const 7
    i32.const 8
    call $push-bytes2
    local.set 12
    local.get 12
    i32.const 1
    call $push-bytes
    local.set 13
    local.get 13
    i32.const 4
    call $push-bytes
    local.set 14
    local.get 14
    i32.const 101
    i32.const 118
    i32.const 97
    i32.const 108
    call $push-bytes4
    local.set 15
    local.get 15
    i32.const 0
    i32.const 0
    call $push-bytes2
    local.set 16
    local.get 16
    i32.const 10
    i32.const 6
    call $push-bytes2
    local.set 17
    local.get 17
    i32.const 1
    call $push-bytes
    local.set 18
    local.get 18
    i32.const 4
    call $push-bytes
    local.set 19
    local.get 19
    i32.const 0
    call $push-bytes
    local.set 20
    local.get 20
    i32.const 65
    local.get 0
    call $push-bytes2
    local.set 21
    local.get 21
    i32.const 11
    call $push-bytes
  )
  (func $emit-wasm__internal (result i32)
    i32.const 42
    call $emit-const-i32
  )
  (func $emit-wasm (result i32)
    call $emit-wasm__internal
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
  (export "emit-wasm" (func $emit-wasm))
  (export "memory" (memory 0))
  (export "cabi_realloc" (func $cabi_realloc))
)
