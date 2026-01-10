(module
  (memory 1 100)
  (func $double (param $x i32) (result i32)
    local.get 0
    i32.const 2
    i32.mul
  )
  (func $add-five (param $x i32) (result i32)
    local.get 0
    i32.const 5
    i32.add
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
  (func $test-when (param $x i32) (result i32)
    local.get 0
    i32.const 0
    i32.gt_s
    (if (result i32)
      (then
        local.get 0
        local.get 0
        i32.mul
      )
      (else
        i32.const 0
      )
    )
  )
  (export "double" (func $double))
  (export "add-five" (func $add-five))
  (export "factorial" (func $factorial))
  (export "test-when" (func $test-when))
)
