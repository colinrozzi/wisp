(module
  (func $add64 (param $x i64) (param $y i64) (result i64)
    local.get 0
    local.get 1
    i64.add
  )
  (func $mul-f64 (param $x f64) (result f64)
    local.get 0
    f64.const 2.5
    f64.mul
  )
  (export "add64" (func $add64))
  (export "mul-f64" (func $mul-f64))
)
