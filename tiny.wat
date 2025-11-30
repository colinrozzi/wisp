(module
  (func $main (param $x i32) (result i32)
    local.get 0
    i32.const 2
    i32.mul
    i32.const 3
    i32.mul
  )
  (export "run" (func $main))
)
