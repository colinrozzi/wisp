(module
  (func $main (param $x i32) (result i32)
    local.get 0
    i32.const 2
    i32.mul
    i32.const 1
    i32.add
  )
  (export "run" (func $main))
)
