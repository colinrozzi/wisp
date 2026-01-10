(module
  (memory 1 100)
  (func $test-no-capture (result i32)
    (local i32)
    (local i32)
    i32.const 42
    local.set 0
    i32.const 0
    local.set 1
    local.get 0
  )
  (func $test-self-ref (result i32)
    (local i32)
    i32.const 100
    local.set 0
    local.get 0
  )
  (func $test-nested (result i32)
    (local i32)
    (local i32)
    (local i32)
    i32.const 100
    local.set 0
    i32.const 1
    local.set 1
    i32.const 2
    local.set 2
    local.get 2
    local.get 0
    i32.add
  )
  (export "test-no-capture" (func $test-no-capture))
  (export "test-self-ref" (func $test-self-ref))
  (export "test-nested" (func $test-nested))
)
