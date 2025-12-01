(module
  (func $double (param $x i32) (result i32)
    local.get 0
    i32.const 2
    i32.mul
  )
  (export "double" (func $double))
)
