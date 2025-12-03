(module
  (memory 1 100)
  (func $store-bytes (param $b1 i32) (param $b2 i32) (param $b3 i32) (param $b4 i32) (result i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    i32.const 0
    local.set 4
    local.get 0
    local.set 5
    local.get 4
    local.get 5
    i32.store8
    local.get 5
    local.set 6
    local.get 1
    local.set 7
    i32.const 1
    local.get 7
    i32.store8
    local.get 7
    local.set 8
    local.get 2
    local.set 9
    i32.const 2
    local.get 9
    i32.store8
    local.get 9
    local.set 10
    local.get 3
    local.set 11
    i32.const 3
    local.get 11
    i32.store8
    local.get 11
    local.set 12
    local.get 4
    i32.load
  )
  (func $pack-unpack (param $val i32) (result i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    i32.const 100
    local.set 1
    local.get 0
    local.set 2
    local.get 1
    local.get 2
    i32.store
    local.get 2
    local.set 3
    local.get 1
    i32.load8_u
    local.set 4
    local.get 1
    i32.const 1
    i32.add
    i32.load8_u
    local.set 5
    local.get 1
    i32.const 2
    i32.add
    i32.load8_u
    local.set 6
    local.get 1
    i32.const 3
    i32.add
    i32.load8_u
    local.set 7
    local.get 4
    local.get 5
    i32.add
    local.get 6
    local.get 7
    i32.add
    i32.add
  )
  (export "store-bytes" (func $store-bytes))
  (export "pack-unpack" (func $pack-unpack))
)
