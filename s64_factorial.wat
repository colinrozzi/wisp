(module
  (func $factorial64 (param $n i64) (result i64)
    local.get 0
    i64.const 0
    i64.eq
    (if (result i64)
      (then
        i64.const 1
      )
      (else
        local.get 0
        local.get 0
        i64.const 1
        i64.sub
        call $factorial64
        i64.mul
      )
    )
  )
  (export "factorial64" (func $factorial64))
)
