(module
  (func $main (param $x i32) (param $y i32) (result i32)
    (local i32)
    local.get 0
    i32.const 2
    i32.mul
    local.set 2
    local.get 2
    local.get 1
    i32.gt_s
    (if (result i32)
      (then
        local.get 2
      )
      (else
        local.get 1
      )
    )
  )
  (export "run" (func $main))
)
