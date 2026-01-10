(module
  (memory 1 100)
  (func $test-simple (param $x i32) (param $y i32) (result i32)
    local.get 0
    local.get 1
    i32.add
  )
  (func $test-guard-id (param $n i32) (result i32)
    local.get 0
    local.get 0
    i32.add
  )
  (func $test-guard-nonid (result i32)
    i32.const 21
  )
  (func $test-quasisyntax (param $n i32) (result i32)
    local.get 0
    i32.const 1
    i32.add
  )
  (func $test-ct-let (param $n i32) (result i32)
    local.get 0
    local.get 0
    i32.add
    local.get 0
    i32.add
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
  (export "test-simple" (func $test-simple))
  (export "test-guard-id" (func $test-guard-id))
  (export "test-guard-nonid" (func $test-guard-nonid))
  (export "test-quasisyntax" (func $test-quasisyntax))
  (export "test-ct-let" (func $test-ct-let))
  (export "test-when" (func $test-when))
)
