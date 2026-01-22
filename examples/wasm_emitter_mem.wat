(module
  (memory 1 100)
  (func $write-byte (param $offset i32) (param $byte i32) (result i32)
    (local i32)
    (local i32)
    local.get 1
    local.set 2
    local.get 0
    local.get 2
    i32.store8
    local.get 2
    local.set 3
    local.get 0
    i32.const 1
    i32.add
  )
  (func $emit-header (param $out i32) (result i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    local.get 0
    i32.const 0
    call $write-byte
    local.set 1
    local.get 1
    i32.const 97
    call $write-byte
    local.set 2
    local.get 2
    i32.const 115
    call $write-byte
    local.set 3
    local.get 3
    i32.const 109
    call $write-byte
    local.set 4
    local.get 4
    i32.const 1
    call $write-byte
    local.set 5
    local.get 5
    i32.const 0
    call $write-byte
    local.set 6
    local.get 6
    i32.const 0
    call $write-byte
    local.set 7
    local.get 7
    i32.const 0
    call $write-byte
  )
  (func $emit-type-section (param $out i32) (result i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    local.get 0
    i32.const 1
    call $write-byte
    local.set 1
    local.get 1
    i32.const 5
    call $write-byte
    local.set 2
    local.get 2
    i32.const 1
    call $write-byte
    local.set 3
    local.get 3
    i32.const 96
    call $write-byte
    local.set 4
    local.get 4
    i32.const 0
    call $write-byte
    local.set 5
    local.get 5
    i32.const 1
    call $write-byte
    local.set 6
    local.get 6
    i32.const 127
    call $write-byte
  )
  (func $emit-func-section (param $out i32) (result i32)
    (local i32)
    (local i32)
    (local i32)
    local.get 0
    i32.const 3
    call $write-byte
    local.set 1
    local.get 1
    i32.const 2
    call $write-byte
    local.set 2
    local.get 2
    i32.const 1
    call $write-byte
    local.set 3
    local.get 3
    i32.const 0
    call $write-byte
  )
  (func $emit-export-section (param $out i32) (result i32)
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
    i32.const 7
    call $write-byte
    local.set 1
    local.get 1
    i32.const 8
    call $write-byte
    local.set 2
    local.get 2
    i32.const 1
    call $write-byte
    local.set 3
    local.get 3
    i32.const 4
    call $write-byte
    local.set 4
    local.get 4
    i32.const 101
    call $write-byte
    local.set 5
    local.get 5
    i32.const 118
    call $write-byte
    local.set 6
    local.get 6
    i32.const 97
    call $write-byte
    local.set 7
    local.get 7
    i32.const 108
    call $write-byte
    local.set 8
    local.get 8
    i32.const 0
    call $write-byte
    local.set 9
    local.get 9
    i32.const 0
    call $write-byte
  )
  (func $emit-code-section (param $out i32) (result i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    local.get 0
    i32.const 10
    call $write-byte
    local.set 1
    local.get 1
    i32.const 6
    call $write-byte
    local.set 2
    local.get 2
    i32.const 1
    call $write-byte
    local.set 3
    local.get 3
    i32.const 4
    call $write-byte
    local.set 4
    local.get 4
    i32.const 0
    call $write-byte
    local.set 5
    local.get 5
    i32.const 65
    call $write-byte
    local.set 6
    local.get 6
    i32.const 42
    call $write-byte
    local.set 7
    local.get 7
    i32.const 11
    call $write-byte
  )
  (func $emit-module (param $out i32) (result i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    local.get 0
    local.set 1
    local.get 0
    call $emit-header
    local.set 2
    local.get 2
    call $emit-type-section
    local.set 3
    local.get 3
    call $emit-func-section
    local.set 4
    local.get 4
    call $emit-export-section
    local.set 5
    local.get 5
    call $emit-code-section
    local.set 6
    local.get 6
    local.get 1
    i32.sub
  )
  (func $emit-wasm (result i32)
    i32.const 1024
    call $emit-module
  )
  (func $emit-and-get-byte (param $index i32) (result i32)
    (local i32)
    i32.const 1024
    call $emit-module
    local.set 1
    i32.const 1024
    local.get 0
    i32.add
    i32.load8_u
  )
  (func $emit-and-get-magic (result i32)
    (local i32)
    i32.const 1024
    call $emit-module
    local.set 0
    i32.const 1024
    i32.load
  )
  (export "emit-wasm" (func $emit-wasm))
  (export "emit-and-get-byte" (func $emit-and-get-byte))
  (export "emit-and-get-magic" (func $emit-and-get-magic))
  (export "memory" (memory 0))
)
