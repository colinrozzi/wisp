(module
  (memory 1 100)
  (func $byte-count (result i32)
    i32.const 37
  )
  (func $wasm-magic (result i32)
    i32.const 0
    i32.const 97
    i32.const 256
    i32.mul
    i32.add
    i32.const 115
    i32.const 65536
    i32.mul
    i32.const 109
    i32.const 16777216
    i32.mul
    i32.add
    i32.add
  )
  (func $wasm-version (result i32)
    i32.const 1
  )
  (export "byte-count" (func $byte-count))
  (export "wasm-magic" (func $wasm-magic))
  (export "wasm-version" (func $wasm-version))
  (export "memory" (memory 0))
)
