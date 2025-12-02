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
  (export "add-and-scale" (func $add-and-scale))
  (export "dot-f32" (func $dot-f32))
)
