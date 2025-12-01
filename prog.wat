(module
  (func $double (param $x i32) (result i32)
    local.get 0
    i32.const 2
    i32.mul
  )
  (func $bigger (param $a i32) (param $b i32) (result i32)
    local.get 0
    local.get 1
    i32.gt_s
    (if (result i32)
      (then
        local.get 0
      )
      (else
        local.get 1
      )
    )
  )
  (func $smaller (param $a i32) (param $b i32) (result i32)
    local.get 0
    local.get 1
    i32.lt_s
    (if (result i32)
      (then
        local.get 0
      )
      (else
        local.get 1
      )
    )
  )
  (func $factorial (param $n i32) (result i32)
    local.get 0
    i32.const 0
    i32.eq
    (if (result i32)
      (then
        i32.const 1
      )
      (else
        local.get 0
        local.get 0
        i32.const 1
        i32.sub
        call $factorial
        i32.mul
      )
    )
  )
  (func $main (param $x i32) (result i32)
    local.get 0
    call $factorial
  )
  (func $run (param $x i32) (result i32)
    local.get 0
    call $main
  )
  (export "double" (func $double))
  (export "smaller" (func $smaller))
  (export "factorial" (func $factorial))
  (export "run" (func $run))
)
