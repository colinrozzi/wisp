(module
  (func $add-and-scale (param $x f64) (param $y f64) (result f64)
    local.get 0
    local.get 1
    f64.add
    f64.const 1.5
    f64.mul
  )
  (func $dot-f32 (param $ax f32) (param $ay f32) (param $bx f32) (param $by f32) (result f32)
    local.get 0
    local.get 2
    f32.mul
    local.get 1
    local.get 3
    f32.mul
    f32.add
  )
  (func $widen-then-trunc (param $x i32) (result i32)
    i32.const 2
    i64.extend_i32_s
    local.get 0
    i64.extend_i32_s
    i64.mul
    i32.wrap_i64
  )
  (export "add-and-scale" (func $add-and-scale))
  (export "dot-f32" (func $dot-f32))
  (export "widen-then-trunc" (func $widen-then-trunc))
)
