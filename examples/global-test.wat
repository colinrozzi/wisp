(module
  (memory 1 100)
  (global $counter (mut i32) (i32.const 0))
  (global $multiplier i32 (i32.const 10))
  (func $get-counter (result i32)
    global.get $counter
  )
  (func $increment (result i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    global.get $counter
    local.set 0
    local.get 0
    i32.const 1
    i32.add
    local.set 1
    local.get 1
    local.set 2
    local.get 2
    global.set $counter
    local.get 2
    local.set 3
    local.get 1
  )
  (func $scaled-counter (result i32)
    global.get $counter
    global.get $multiplier
    i32.mul
  )
  (func $reset (result i32)
    (local i32)
    (local i32)
    i32.const 0
    local.set 0
    local.get 0
    global.set $counter
    local.get 0
    local.set 1
    i32.const 0
  )
  (export "get-counter" (func $get-counter))
  (export "increment" (func $increment))
  (export "scaled-counter" (func $scaled-counter))
  (export "reset" (func $reset))
)
