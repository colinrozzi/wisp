(module
  (memory 1 100)
  (func $test-add (param $a i32) (param $b i32) (result i32)
    local.get 0
    local.get 1
    i32.add
  )
  (func $test-div (param $a i32) (param $b i32) (result i32)
    local.get 0
    local.get 1
    i32.div_s
  )
  (func $test-compare (param $a i32) (param $b i32) (result i32)
    local.get 0
    local.get 1
    i32.eq
  )
  (export "test-add" (func $test-add))
  (export "test-div" (func $test-div))
  (export "test-compare" (func $test-compare))
)
