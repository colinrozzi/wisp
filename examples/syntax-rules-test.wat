(module
  (memory 1 100)
  (func $test-simple (param $x i32) (param $y i32) (result i32)
    local.get 0
    local.get 1
    i32.add
  )
  (func $test-inc (param $x i32) (result i32)
    local.get 0
    i32.const 1
    i32.add
  )
  (func $test-inc-n (param $x i32) (param $n i32) (result i32)
    local.get 0
    local.get 1
    i32.add
  )
  (func $test-begin (param $x i32) (result i32)
    (local i32)
    (local i32)
    i32.const 1
    local.set 1
    i32.const 2
    local.set 2
    local.get 0
    i32.const 10
    i32.add
  )
  (func $test-else (result i32)
    i32.const 42
  )
  (func $test-cond (param $x i32) (result i32)
    local.get 0
    i32.const 0
    i32.gt_s
    (if (result i32)
      (then
        i32.const 100
      )
      (else
        i32.const 0
      )
    )
  )
  (export "test-simple" (func $test-simple))
  (export "test-inc" (func $test-inc))
  (export "test-inc-n" (func $test-inc-n))
  (export "test-begin" (func $test-begin))
  (export "test-else" (func $test-else))
  (export "test-cond" (func $test-cond))
)
