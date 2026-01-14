(module
  (memory 1 100)
  (func $identity (param $s i32) (result i32)
    i32.const 42
  )
  (export "identity" (func $identity))
  (export "memory" (memory 0))
)
