(module
  (memory 1 100)
  (global $__heap_ptr (mut i32) (i32.const 0))
  (func $make-some-value (result i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 8
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 1
    i32.store
    local.get 0
    i32.const 4
    i32.add
    i32.const 42
    i32.store
    local.get 0
  )
  (func $make-none-value (result i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 8
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 0
    i32.store
    local.get 0
  )
  (func $make-ok-value (result i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 8
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 0
    i32.store
    local.get 0
    i32.const 4
    i32.add
    i32.const 100
    i32.store
    local.get 0
  )
  (func $make-err-value (result i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 8
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 1
    i32.store
    local.get 0
    i32.const 4
    i32.add
    i32.const 999
    i32.store
    local.get 0
  )
  (func $make-list (result i32)
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
    i32.const 0
    i32.store
    local.get 0
    i32.const 8
    i32.add
    i32.const 0
    i32.store
    local.get 0
    local.set 1
    local.get 1
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
    i32.const 10
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
    local.set 6
    local.get 6
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
    i32.const 20
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
    local.set 11
    local.get 11
  )
  (func $unwrap-or (param $opt i32) (param $default i32) (result i32)
    (local i32)
    (local i32)
    local.get 0
    local.set 2
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
        local.set 3
        local.get 3
      )
      (else
    local.get 2
    i32.load
    i32.const 0
    i32.eq
    (if (result i32)
      (then
        local.get 1
      )
      (else
        unreachable
      )
    )
      )
    )
  )
  (func $result-to-s32 (param $res i32) (result i32)
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
        local.get 3
        i32.const -1
        i32.mul
      )
      (else
        unreachable
      )
    )
      )
    )
  )
  (func $test-some (result i32)
    (local i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 8
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 1
    i32.store
    local.get 0
    i32.const 4
    i32.add
    i32.const 100
    i32.store
    local.get 0
    local.set 1
    i32.const 1
  )
  (func $test-none (result i32)
    (local i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 8
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 0
    i32.store
    local.get 0
    local.set 1
    i32.const 0
  )
  (func $test-ok (result i32)
    (local i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 8
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 0
    i32.store
    local.get 0
    i32.const 4
    i32.add
    i32.const 42
    i32.store
    local.get 0
    local.set 1
    i32.const 1
  )
  (func $test-err (result i32)
    (local i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 8
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 1
    i32.store
    local.get 0
    i32.const 4
    i32.add
    i32.const 500
    i32.store
    local.get 0
    local.set 1
    i32.const 2
  )
  (func $test-list-len (result i32)
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
    i32.const 0
    i32.store
    local.get 0
    i32.const 8
    i32.add
    i32.const 0
    i32.store
    local.get 0
    local.set 1
    local.get 1
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
    i32.const 10
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
    local.set 6
    local.get 6
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
    i32.const 20
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
    local.set 11
    local.get 11
    i32.load
  )
  (func $test-list-get (result i32)
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
    i32.const 0
    i32.store
    local.get 0
    i32.const 8
    i32.add
    i32.const 0
    i32.store
    local.get 0
    local.set 1
    local.get 1
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
    i32.const 10
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
    local.set 6
    local.get 6
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
    i32.const 20
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
    local.set 11
    local.get 11
    local.set 12
    i32.const 1
    local.set 13
    local.get 12
    i32.const 8
    i32.add
    i32.load
    local.get 13
    i32.const 4
    i32.mul
    i32.add
    i32.load
  )
  (func $test-unwrap-some (result i32)
    (local i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 8
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 1
    i32.store
    local.get 0
    i32.const 4
    i32.add
    i32.const 42
    i32.store
    local.get 0
    local.set 1
    local.get 1
    i32.const 0
    call $unwrap-or
  )
  (func $test-unwrap-none (result i32)
    (local i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 8
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 0
    i32.store
    local.get 0
    local.set 1
    local.get 1
    i32.const 99
    call $unwrap-or
  )
  (func $test-result-ok (result i32)
    (local i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 8
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 0
    i32.store
    local.get 0
    i32.const 4
    i32.add
    i32.const 100
    i32.store
    local.get 0
    local.set 1
    local.get 1
    call $result-to-s32
  )
  (func $test-result-err (result i32)
    (local i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 8
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 1
    i32.store
    local.get 0
    i32.const 4
    i32.add
    i32.const 50
    i32.store
    local.get 0
    local.set 1
    local.get 1
    call $result-to-s32
  )
  (export "test-some" (func $test-some))
  (export "test-none" (func $test-none))
  (export "test-ok" (func $test-ok))
  (export "test-err" (func $test-err))
  (export "test-list-len" (func $test-list-len))
  (export "test-list-get" (func $test-list-get))
  (export "test-unwrap-some" (func $test-unwrap-some))
  (export "test-unwrap-none" (func $test-unwrap-none))
  (export "test-result-ok" (func $test-result-ok))
  (export "test-result-err" (func $test-result-err))
)
