(module
  (memory (export "memory") 1 100)
  (global $__heap_ptr (mut i32) (i32.const 49152))
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
  (func $test-string-ref-first (result i32)
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
    i32.const 4
    i32.add
    i32.const 0
    i32.add
    i32.load8_u
  )
  (func $test-string-ref-middle (result i32)
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
    i32.const 4
    i32.add
    i32.const 2
    i32.add
    i32.load8_u
  )
  (func $test-string-ref-last (result i32)
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
    i32.const 4
    i32.add
    i32.const 4
    i32.add
    i32.load8_u
  )
  (func $test-substring-start (result i32)
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
    local.set 2
    i32.const 0
    local.set 3
    i32.const 3
    local.set 4
    local.get 4
    local.get 3
    i32.sub
    local.set 5
    global.get $__heap_ptr
    local.set 6
    global.get $__heap_ptr
    i32.const 4
    local.get 5
    i32.add
    i32.add
    global.set $__heap_ptr
    local.get 6
    local.get 5
    i32.store
    local.get 6
    i32.const 4
    i32.add
    local.get 2
    i32.const 4
    i32.add
    local.get 3
    i32.add
    local.get 5
    memory.copy
    local.get 6
    local.set 7
    local.get 7
    i32.load
  )
  (func $test-substring-middle (result i32)
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
    i32.const 15
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 11
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
    local.set 1
    local.get 1
    local.set 2
    i32.const 6
    local.set 3
    i32.const 11
    local.set 4
    local.get 4
    local.get 3
    i32.sub
    local.set 5
    global.get $__heap_ptr
    local.set 6
    global.get $__heap_ptr
    i32.const 4
    local.get 5
    i32.add
    i32.add
    global.set $__heap_ptr
    local.get 6
    local.get 5
    i32.store
    local.get 6
    i32.const 4
    i32.add
    local.get 2
    i32.const 4
    i32.add
    local.get 3
    i32.add
    local.get 5
    memory.copy
    local.get 6
    local.set 7
    local.get 7
    i32.load
  )
  (func $test-substring-first-char (result i32)
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
    local.set 2
    i32.const 0
    local.set 3
    i32.const 1
    local.set 4
    local.get 4
    local.get 3
    i32.sub
    local.set 5
    global.get $__heap_ptr
    local.set 6
    global.get $__heap_ptr
    i32.const 4
    local.get 5
    i32.add
    i32.add
    global.set $__heap_ptr
    local.get 6
    local.get 5
    i32.store
    local.get 6
    i32.const 4
    i32.add
    local.get 2
    i32.const 4
    i32.add
    local.get 3
    i32.add
    local.get 5
    memory.copy
    local.get 6
    local.set 7
    local.get 7
    i32.const 4
    i32.add
    i32.const 0
    i32.add
    i32.load8_u
  )
  (func $test-string-append-len (result i32)
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
    global.get $__heap_ptr
    local.set 2
    global.get $__heap_ptr
    i32.const 10
    i32.add
    global.set $__heap_ptr
    local.get 2
    i32.const 6
    i32.store
    local.get 2
    i32.const 4
    i32.add
    i32.const 32
    i32.store8
    local.get 2
    i32.const 5
    i32.add
    i32.const 119
    i32.store8
    local.get 2
    i32.const 6
    i32.add
    i32.const 111
    i32.store8
    local.get 2
    i32.const 7
    i32.add
    i32.const 114
    i32.store8
    local.get 2
    i32.const 8
    i32.add
    i32.const 108
    i32.store8
    local.get 2
    i32.const 9
    i32.add
    i32.const 100
    i32.store8
    local.get 2
    local.set 3
    local.get 1
    local.set 4
    local.get 3
    local.set 5
    local.get 4
    i32.load
    local.set 6
    local.get 5
    i32.load
    local.set 7
    local.get 6
    local.get 7
    i32.add
    local.set 8
    global.get $__heap_ptr
    local.set 9
    global.get $__heap_ptr
    i32.const 4
    local.get 8
    i32.add
    i32.add
    global.set $__heap_ptr
    local.get 9
    local.get 8
    i32.store
    local.get 9
    i32.const 4
    i32.add
    local.get 4
    i32.const 4
    i32.add
    local.get 6
    memory.copy
    local.get 9
    i32.const 4
    i32.add
    local.get 6
    i32.add
    local.get 5
    i32.const 4
    i32.add
    local.get 7
    memory.copy
    local.get 9
    local.set 10
    local.get 10
    i32.load
  )
  (func $test-string-append-first-char (result i32)
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
    global.get $__heap_ptr
    local.set 2
    global.get $__heap_ptr
    i32.const 10
    i32.add
    global.set $__heap_ptr
    local.get 2
    i32.const 6
    i32.store
    local.get 2
    i32.const 4
    i32.add
    i32.const 32
    i32.store8
    local.get 2
    i32.const 5
    i32.add
    i32.const 119
    i32.store8
    local.get 2
    i32.const 6
    i32.add
    i32.const 111
    i32.store8
    local.get 2
    i32.const 7
    i32.add
    i32.const 114
    i32.store8
    local.get 2
    i32.const 8
    i32.add
    i32.const 108
    i32.store8
    local.get 2
    i32.const 9
    i32.add
    i32.const 100
    i32.store8
    local.get 2
    local.set 3
    local.get 1
    local.set 4
    local.get 3
    local.set 5
    local.get 4
    i32.load
    local.set 6
    local.get 5
    i32.load
    local.set 7
    local.get 6
    local.get 7
    i32.add
    local.set 8
    global.get $__heap_ptr
    local.set 9
    global.get $__heap_ptr
    i32.const 4
    local.get 8
    i32.add
    i32.add
    global.set $__heap_ptr
    local.get 9
    local.get 8
    i32.store
    local.get 9
    i32.const 4
    i32.add
    local.get 4
    i32.const 4
    i32.add
    local.get 6
    memory.copy
    local.get 9
    i32.const 4
    i32.add
    local.get 6
    i32.add
    local.get 5
    i32.const 4
    i32.add
    local.get 7
    memory.copy
    local.get 9
    local.set 10
    local.get 10
    i32.const 4
    i32.add
    i32.const 0
    i32.add
    i32.load8_u
  )
  (func $test-string-append-boundary (result i32)
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
    global.get $__heap_ptr
    local.set 2
    global.get $__heap_ptr
    i32.const 10
    i32.add
    global.set $__heap_ptr
    local.get 2
    i32.const 6
    i32.store
    local.get 2
    i32.const 4
    i32.add
    i32.const 32
    i32.store8
    local.get 2
    i32.const 5
    i32.add
    i32.const 119
    i32.store8
    local.get 2
    i32.const 6
    i32.add
    i32.const 111
    i32.store8
    local.get 2
    i32.const 7
    i32.add
    i32.const 114
    i32.store8
    local.get 2
    i32.const 8
    i32.add
    i32.const 108
    i32.store8
    local.get 2
    i32.const 9
    i32.add
    i32.const 100
    i32.store8
    local.get 2
    local.set 3
    local.get 1
    local.set 4
    local.get 3
    local.set 5
    local.get 4
    i32.load
    local.set 6
    local.get 5
    i32.load
    local.set 7
    local.get 6
    local.get 7
    i32.add
    local.set 8
    global.get $__heap_ptr
    local.set 9
    global.get $__heap_ptr
    i32.const 4
    local.get 8
    i32.add
    i32.add
    global.set $__heap_ptr
    local.get 9
    local.get 8
    i32.store
    local.get 9
    i32.const 4
    i32.add
    local.get 4
    i32.const 4
    i32.add
    local.get 6
    memory.copy
    local.get 9
    i32.const 4
    i32.add
    local.get 6
    i32.add
    local.get 5
    i32.const 4
    i32.add
    local.get 7
    memory.copy
    local.get 9
    local.set 10
    local.get 10
    i32.const 4
    i32.add
    i32.const 5
    i32.add
    i32.load8_u
  )
  (func $test-string-append-last-char (result i32)
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
    global.get $__heap_ptr
    local.set 2
    global.get $__heap_ptr
    i32.const 10
    i32.add
    global.set $__heap_ptr
    local.get 2
    i32.const 6
    i32.store
    local.get 2
    i32.const 4
    i32.add
    i32.const 32
    i32.store8
    local.get 2
    i32.const 5
    i32.add
    i32.const 119
    i32.store8
    local.get 2
    i32.const 6
    i32.add
    i32.const 111
    i32.store8
    local.get 2
    i32.const 7
    i32.add
    i32.const 114
    i32.store8
    local.get 2
    i32.const 8
    i32.add
    i32.const 108
    i32.store8
    local.get 2
    i32.const 9
    i32.add
    i32.const 100
    i32.store8
    local.get 2
    local.set 3
    local.get 1
    local.set 4
    local.get 3
    local.set 5
    local.get 4
    i32.load
    local.set 6
    local.get 5
    i32.load
    local.set 7
    local.get 6
    local.get 7
    i32.add
    local.set 8
    global.get $__heap_ptr
    local.set 9
    global.get $__heap_ptr
    i32.const 4
    local.get 8
    i32.add
    i32.add
    global.set $__heap_ptr
    local.get 9
    local.get 8
    i32.store
    local.get 9
    i32.const 4
    i32.add
    local.get 4
    i32.const 4
    i32.add
    local.get 6
    memory.copy
    local.get 9
    i32.const 4
    i32.add
    local.get 6
    i32.add
    local.get 5
    i32.const 4
    i32.add
    local.get 7
    memory.copy
    local.get 9
    local.set 10
    local.get 10
    i32.const 4
    i32.add
    i32.const 10
    i32.add
    i32.load8_u
  )
  (func $test-string-eq-same (result i32)
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
    global.get $__heap_ptr
    local.set 2
    global.get $__heap_ptr
    i32.const 9
    i32.add
    global.set $__heap_ptr
    local.get 2
    i32.const 5
    i32.store
    local.get 2
    i32.const 4
    i32.add
    i32.const 104
    i32.store8
    local.get 2
    i32.const 5
    i32.add
    i32.const 101
    i32.store8
    local.get 2
    i32.const 6
    i32.add
    i32.const 108
    i32.store8
    local.get 2
    i32.const 7
    i32.add
    i32.const 108
    i32.store8
    local.get 2
    i32.const 8
    i32.add
    i32.const 111
    i32.store8
    local.get 2
    local.set 3
    local.get 1
    local.set 4
    local.get 3
    local.set 5
    local.get 4
    i32.load
    local.set 6
    local.get 5
    i32.load
    local.set 7
    block (result i32) ;; string-eq outer
      local.get 6
      local.get 7
      i32.ne
      if (result i32)
        i32.const 0
      else
        i32.const 0
        local.set 8
        block (result i32) ;; comparison result
          loop ;; compare loop
            local.get 8
            local.get 6
            i32.ge_u
            if
              i32.const 1
              br 2 ;; exit with 1
            end
            local.get 4
            i32.const 4
            i32.add
            local.get 8
            i32.add
            i32.load8_u
            local.get 5
            i32.const 4
            i32.add
            local.get 8
            i32.add
            i32.load8_u
            i32.ne
            if
              i32.const 0
              br 3 ;; exit with 0
            end
            local.get 8
            i32.const 1
            i32.add
            local.set 8
            br 0 ;; continue loop
          end ;; loop
          i32.const 1 ;; fallback (empty strings)
        end ;; comparison result block
      end ;; if
    end ;; string-eq outer
  )
  (func $test-string-eq-different (result i32)
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
    global.get $__heap_ptr
    local.set 2
    global.get $__heap_ptr
    i32.const 9
    i32.add
    global.set $__heap_ptr
    local.get 2
    i32.const 5
    i32.store
    local.get 2
    i32.const 4
    i32.add
    i32.const 119
    i32.store8
    local.get 2
    i32.const 5
    i32.add
    i32.const 111
    i32.store8
    local.get 2
    i32.const 6
    i32.add
    i32.const 114
    i32.store8
    local.get 2
    i32.const 7
    i32.add
    i32.const 108
    i32.store8
    local.get 2
    i32.const 8
    i32.add
    i32.const 100
    i32.store8
    local.get 2
    local.set 3
    local.get 1
    local.set 4
    local.get 3
    local.set 5
    local.get 4
    i32.load
    local.set 6
    local.get 5
    i32.load
    local.set 7
    block (result i32) ;; string-eq outer
      local.get 6
      local.get 7
      i32.ne
      if (result i32)
        i32.const 0
      else
        i32.const 0
        local.set 8
        block (result i32) ;; comparison result
          loop ;; compare loop
            local.get 8
            local.get 6
            i32.ge_u
            if
              i32.const 1
              br 2 ;; exit with 1
            end
            local.get 4
            i32.const 4
            i32.add
            local.get 8
            i32.add
            i32.load8_u
            local.get 5
            i32.const 4
            i32.add
            local.get 8
            i32.add
            i32.load8_u
            i32.ne
            if
              i32.const 0
              br 3 ;; exit with 0
            end
            local.get 8
            i32.const 1
            i32.add
            local.set 8
            br 0 ;; continue loop
          end ;; loop
          i32.const 1 ;; fallback (empty strings)
        end ;; comparison result block
      end ;; if
    end ;; string-eq outer
  )
  (func $test-string-eq-different-len (result i32)
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
    global.get $__heap_ptr
    local.set 2
    global.get $__heap_ptr
    i32.const 6
    i32.add
    global.set $__heap_ptr
    local.get 2
    i32.const 2
    i32.store
    local.get 2
    i32.const 4
    i32.add
    i32.const 104
    i32.store8
    local.get 2
    i32.const 5
    i32.add
    i32.const 105
    i32.store8
    local.get 2
    local.set 3
    local.get 1
    local.set 4
    local.get 3
    local.set 5
    local.get 4
    i32.load
    local.set 6
    local.get 5
    i32.load
    local.set 7
    block (result i32) ;; string-eq outer
      local.get 6
      local.get 7
      i32.ne
      if (result i32)
        i32.const 0
      else
        i32.const 0
        local.set 8
        block (result i32) ;; comparison result
          loop ;; compare loop
            local.get 8
            local.get 6
            i32.ge_u
            if
              i32.const 1
              br 2 ;; exit with 1
            end
            local.get 4
            i32.const 4
            i32.add
            local.get 8
            i32.add
            i32.load8_u
            local.get 5
            i32.const 4
            i32.add
            local.get 8
            i32.add
            i32.load8_u
            i32.ne
            if
              i32.const 0
              br 3 ;; exit with 0
            end
            local.get 8
            i32.const 1
            i32.add
            local.set 8
            br 0 ;; continue loop
          end ;; loop
          i32.const 1 ;; fallback (empty strings)
        end ;; comparison result block
      end ;; if
    end ;; string-eq outer
  )
  (func $test-string-eq-empty (result i32)
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
    i32.const 4
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 0
    i32.store
    local.get 0
    local.set 1
    global.get $__heap_ptr
    local.set 2
    global.get $__heap_ptr
    i32.const 4
    i32.add
    global.set $__heap_ptr
    local.get 2
    i32.const 0
    i32.store
    local.get 2
    local.set 3
    local.get 1
    local.set 4
    local.get 3
    local.set 5
    local.get 4
    i32.load
    local.set 6
    local.get 5
    i32.load
    local.set 7
    block (result i32) ;; string-eq outer
      local.get 6
      local.get 7
      i32.ne
      if (result i32)
        i32.const 0
      else
        i32.const 0
        local.set 8
        block (result i32) ;; comparison result
          loop ;; compare loop
            local.get 8
            local.get 6
            i32.ge_u
            if
              i32.const 1
              br 2 ;; exit with 1
            end
            local.get 4
            i32.const 4
            i32.add
            local.get 8
            i32.add
            i32.load8_u
            local.get 5
            i32.const 4
            i32.add
            local.get 8
            i32.add
            i32.load8_u
            i32.ne
            if
              i32.const 0
              br 3 ;; exit with 0
            end
            local.get 8
            i32.const 1
            i32.add
            local.set 8
            br 0 ;; continue loop
          end ;; loop
          i32.const 1 ;; fallback (empty strings)
        end ;; comparison result block
      end ;; if
    end ;; string-eq outer
  )
  (func $test-string-eq-one-empty (result i32)
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
    i32.const 4
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 0
    i32.store
    local.get 0
    local.set 1
    global.get $__heap_ptr
    local.set 2
    global.get $__heap_ptr
    i32.const 5
    i32.add
    global.set $__heap_ptr
    local.get 2
    i32.const 1
    i32.store
    local.get 2
    i32.const 4
    i32.add
    i32.const 97
    i32.store8
    local.get 2
    local.set 3
    local.get 1
    local.set 4
    local.get 3
    local.set 5
    local.get 4
    i32.load
    local.set 6
    local.get 5
    i32.load
    local.set 7
    block (result i32) ;; string-eq outer
      local.get 6
      local.get 7
      i32.ne
      if (result i32)
        i32.const 0
      else
        i32.const 0
        local.set 8
        block (result i32) ;; comparison result
          loop ;; compare loop
            local.get 8
            local.get 6
            i32.ge_u
            if
              i32.const 1
              br 2 ;; exit with 1
            end
            local.get 4
            i32.const 4
            i32.add
            local.get 8
            i32.add
            i32.load8_u
            local.get 5
            i32.const 4
            i32.add
            local.get 8
            i32.add
            i32.load8_u
            i32.ne
            if
              i32.const 0
              br 3 ;; exit with 0
            end
            local.get 8
            i32.const 1
            i32.add
            local.set 8
            br 0 ;; continue loop
          end ;; loop
          i32.const 1 ;; fallback (empty strings)
        end ;; comparison result block
      end ;; if
    end ;; string-eq outer
  )
  (func $test-string-len__export (export "test-string-len") (param $in_ptr i32) (param $in_len i32) (param $out_ptr i32) (param $out_cap i32) (result i32)
    (local $value i32)
    call $test-string-len
    local.set $value
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
  )
  (func $test-empty-string__export (export "test-empty-string") (param $in_ptr i32) (param $in_len i32) (param $out_ptr i32) (param $out_cap i32) (result i32)
    (local $value i32)
    call $test-empty-string
    local.set $value
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
  )
  (func $test-long-string__export (export "test-long-string") (param $in_ptr i32) (param $in_len i32) (param $out_ptr i32) (param $out_cap i32) (result i32)
    (local $value i32)
    call $test-long-string
    local.set $value
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
  )
  (func $test-escape-string__export (export "test-escape-string") (param $in_ptr i32) (param $in_len i32) (param $out_ptr i32) (param $out_cap i32) (result i32)
    (local $value i32)
    call $test-escape-string
    local.set $value
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
  )
  (func $test-string-ref-first__export (export "test-string-ref-first") (param $in_ptr i32) (param $in_len i32) (param $out_ptr i32) (param $out_cap i32) (result i32)
    (local $value i32)
    call $test-string-ref-first
    local.set $value
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
  )
  (func $test-string-ref-middle__export (export "test-string-ref-middle") (param $in_ptr i32) (param $in_len i32) (param $out_ptr i32) (param $out_cap i32) (result i32)
    (local $value i32)
    call $test-string-ref-middle
    local.set $value
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
  )
  (func $test-string-ref-last__export (export "test-string-ref-last") (param $in_ptr i32) (param $in_len i32) (param $out_ptr i32) (param $out_cap i32) (result i32)
    (local $value i32)
    call $test-string-ref-last
    local.set $value
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
  )
  (func $test-substring-start__export (export "test-substring-start") (param $in_ptr i32) (param $in_len i32) (param $out_ptr i32) (param $out_cap i32) (result i32)
    (local $value i32)
    call $test-substring-start
    local.set $value
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
  )
  (func $test-substring-middle__export (export "test-substring-middle") (param $in_ptr i32) (param $in_len i32) (param $out_ptr i32) (param $out_cap i32) (result i32)
    (local $value i32)
    call $test-substring-middle
    local.set $value
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
  )
  (func $test-substring-first-char__export (export "test-substring-first-char") (param $in_ptr i32) (param $in_len i32) (param $out_ptr i32) (param $out_cap i32) (result i32)
    (local $value i32)
    call $test-substring-first-char
    local.set $value
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
  )
  (func $test-string-append-len__export (export "test-string-append-len") (param $in_ptr i32) (param $in_len i32) (param $out_ptr i32) (param $out_cap i32) (result i32)
    (local $value i32)
    call $test-string-append-len
    local.set $value
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
  )
  (func $test-string-append-first-char__export (export "test-string-append-first-char") (param $in_ptr i32) (param $in_len i32) (param $out_ptr i32) (param $out_cap i32) (result i32)
    (local $value i32)
    call $test-string-append-first-char
    local.set $value
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
  )
  (func $test-string-append-boundary__export (export "test-string-append-boundary") (param $in_ptr i32) (param $in_len i32) (param $out_ptr i32) (param $out_cap i32) (result i32)
    (local $value i32)
    call $test-string-append-boundary
    local.set $value
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
  )
  (func $test-string-append-last-char__export (export "test-string-append-last-char") (param $in_ptr i32) (param $in_len i32) (param $out_ptr i32) (param $out_cap i32) (result i32)
    (local $value i32)
    call $test-string-append-last-char
    local.set $value
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
  )
  (func $test-string-eq-same__export (export "test-string-eq-same") (param $in_ptr i32) (param $in_len i32) (param $out_ptr i32) (param $out_cap i32) (result i32)
    (local $value i32)
    call $test-string-eq-same
    local.set $value
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
  )
  (func $test-string-eq-different__export (export "test-string-eq-different") (param $in_ptr i32) (param $in_len i32) (param $out_ptr i32) (param $out_cap i32) (result i32)
    (local $value i32)
    call $test-string-eq-different
    local.set $value
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
  )
  (func $test-string-eq-different-len__export (export "test-string-eq-different-len") (param $in_ptr i32) (param $in_len i32) (param $out_ptr i32) (param $out_cap i32) (result i32)
    (local $value i32)
    call $test-string-eq-different-len
    local.set $value
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
  )
  (func $test-string-eq-empty__export (export "test-string-eq-empty") (param $in_ptr i32) (param $in_len i32) (param $out_ptr i32) (param $out_cap i32) (result i32)
    (local $value i32)
    call $test-string-eq-empty
    local.set $value
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
  )
  (func $test-string-eq-one-empty__export (export "test-string-eq-one-empty") (param $in_ptr i32) (param $in_len i32) (param $out_ptr i32) (param $out_cap i32) (result i32)
    (local $value i32)
    call $test-string-eq-one-empty
    local.set $value
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
  )
)
