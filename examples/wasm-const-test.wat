(module
  (memory 1 100)
  (func $test-const (param $x i32) (result i32)
    local.get 0
    i32.const 42
    i32.add
  )
  (func $test-convert (param $x i32) (result i64)
    local.get 0
    i64.extend_i32_s
  )
  (export "test-const" (func $test-const))
  (export "test-convert" (func $test-convert))
)
