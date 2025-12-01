(module
  (import "math" "double" (func $double (param $x i32) (result i32)))
  (func $run (param $x i32) (result i32)
    local.get 0
    call $double
  )
  (export "run" (func $run))
)
