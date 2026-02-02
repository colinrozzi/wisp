(module
  (memory (export "memory") 16000 16000)
  (global $__heap_ptr (mut i32) (i32.const 49152))
  (func $__alloc (param $size i32) (result i32)
    (local $ptr i32)
    (local $end i32)
    (local $pages_needed i32)
    ;; Get current heap pointer
    global.get $__heap_ptr
    local.set $ptr
    ;; Calculate end of allocation
    local.get $ptr
    local.get $size
    i32.add
    local.set $end
    ;; Check if we need to grow memory
    ;; memory.size returns pages, multiply by 64KB to get bytes
    memory.size
    i32.const 65536
    i32.mul
    local.get $end
    i32.lt_u
    (if
      (then
        ;; Calculate pages needed: (end - current_size + 65535) / 65536
        local.get $end
        memory.size
        i32.const 65536
        i32.mul
        i32.sub
        i32.const 65535
        i32.add
        i32.const 65536
        i32.div_u
        local.set $pages_needed
        ;; Grow memory
        local.get $pages_needed
        memory.grow
        ;; Check if grow failed (returns -1)
        i32.const -1
        i32.eq
        (if
          (then
            ;; Out of memory - trap
            unreachable
          )
        )
      )
    )
    ;; Bump heap pointer
    local.get $end
    global.set $__heap_ptr
    ;; Return old pointer
    local.get $ptr
  )
  (func (export "__pack_alloc") (param $size i32) (result i32)
    local.get $size
    call $__alloc
  )
  (func (export "__pack_free") (param $ptr i32) (param $len i32)
    ;; Simple bump allocator doesn't actually free, but we need the export
    ;; for Pack's ABI. A future optimization could track free lists.
    nop
  )
  (func $not (param $x i32) (result i32)
    local.get 0
    i32.const 0
    i32.eq
    (if (result i32)
      (then
        i32.const 1
      )
      (else
        i32.const 0
      )
    )
  )
  (func $and (param $a i32) (param $b i32) (result i32)
    local.get 0
    i32.const 0
    i32.eq
    (if (result i32)
      (then
        i32.const 0
      )
      (else
        local.get 1
        i32.const 0
        i32.eq
        (if (result i32)
          (then
            i32.const 0
          )
          (else
            i32.const 1
          )
        )
      )
    )
  )
  (func $or (param $a i32) (param $b i32) (result i32)
    local.get 0
    i32.const 0
    i32.eq
    (if (result i32)
      (then
        local.get 1
        i32.const 0
        i32.eq
        (if (result i32)
          (then
            i32.const 0
          )
          (else
            i32.const 1
          )
        )
      )
      (else
        i32.const 1
      )
    )
  )
  (func $xor (param $a i32) (param $b i32) (result i32)
    local.get 0
    i32.const 0
    i32.eq
    (if (result i32)
      (then
        local.get 1
        i32.const 0
        i32.eq
        (if (result i32)
          (then
            i32.const 0
          )
          (else
            i32.const 1
          )
        )
      )
      (else
        local.get 1
        i32.const 0
        i32.eq
        (if (result i32)
          (then
            i32.const 1
          )
          (else
            i32.const 0
          )
        )
      )
    )
  )
  (func $bool (param $x i32) (result i32)
    local.get 0
    i32.const 0
    i32.eq
    (if (result i32)
      (then
        i32.const 0
      )
      (else
        i32.const 1
      )
    )
  )
  (func $not__export (export "not") (param $in_ptr i32) (param $in_len i32) (param $out_ptr_ptr i32) (param $out_len_ptr i32) (result i32)
    (local $out_ptr i32)
    (local $bytes_written i32)
    (local $value i32)
    (local $param_x i32)
    ;; Decode input parameters from CGRF
    ;; Decode s32 from CGRF
    local.get $in_ptr
    i32.const 24
    i32.add
    i32.load
    local.set $param_x
    local.get $param_x
    call $not
    local.set $value
    i32.const 32
    call $__alloc
    local.set $out_ptr
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
    local.set $bytes_written
    ;; Write output pointer to out_ptr_ptr slot
    local.get $out_ptr_ptr
    local.get $out_ptr
    i32.store
    ;; Write output length to out_len_ptr slot
    local.get $out_len_ptr
    local.get $bytes_written
    i32.store
    ;; Return 0 for success
    i32.const 0
  )
  (func $and__export (export "and") (param $in_ptr i32) (param $in_len i32) (param $out_ptr_ptr i32) (param $out_len_ptr i32) (result i32)
    (local $out_ptr i32)
    (local $bytes_written i32)
    (local $value i32)
    (local $param_a i32)
    (local $param_b i32)
    ;; Decode input parameters from CGRF
    ;; Multiple params - expecting tuple root
    ;; Decode tuple param 0 (a)
    local.get $in_ptr
    i32.const 24
    i32.add
    i32.load
    local.set $param_a
    ;; Decode tuple param 1 (b)
    local.get $in_ptr
    i32.const 36
    i32.add
    i32.load
    local.set $param_b
    local.get $param_a
    local.get $param_b
    call $and
    local.set $value
    i32.const 32
    call $__alloc
    local.set $out_ptr
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
    local.set $bytes_written
    ;; Write output pointer to out_ptr_ptr slot
    local.get $out_ptr_ptr
    local.get $out_ptr
    i32.store
    ;; Write output length to out_len_ptr slot
    local.get $out_len_ptr
    local.get $bytes_written
    i32.store
    ;; Return 0 for success
    i32.const 0
  )
  (func $or__export (export "or") (param $in_ptr i32) (param $in_len i32) (param $out_ptr_ptr i32) (param $out_len_ptr i32) (result i32)
    (local $out_ptr i32)
    (local $bytes_written i32)
    (local $value i32)
    (local $param_a i32)
    (local $param_b i32)
    ;; Decode input parameters from CGRF
    ;; Multiple params - expecting tuple root
    ;; Decode tuple param 0 (a)
    local.get $in_ptr
    i32.const 24
    i32.add
    i32.load
    local.set $param_a
    ;; Decode tuple param 1 (b)
    local.get $in_ptr
    i32.const 36
    i32.add
    i32.load
    local.set $param_b
    local.get $param_a
    local.get $param_b
    call $or
    local.set $value
    i32.const 32
    call $__alloc
    local.set $out_ptr
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
    local.set $bytes_written
    ;; Write output pointer to out_ptr_ptr slot
    local.get $out_ptr_ptr
    local.get $out_ptr
    i32.store
    ;; Write output length to out_len_ptr slot
    local.get $out_len_ptr
    local.get $bytes_written
    i32.store
    ;; Return 0 for success
    i32.const 0
  )
  (func $xor__export (export "xor") (param $in_ptr i32) (param $in_len i32) (param $out_ptr_ptr i32) (param $out_len_ptr i32) (result i32)
    (local $out_ptr i32)
    (local $bytes_written i32)
    (local $value i32)
    (local $param_a i32)
    (local $param_b i32)
    ;; Decode input parameters from CGRF
    ;; Multiple params - expecting tuple root
    ;; Decode tuple param 0 (a)
    local.get $in_ptr
    i32.const 24
    i32.add
    i32.load
    local.set $param_a
    ;; Decode tuple param 1 (b)
    local.get $in_ptr
    i32.const 36
    i32.add
    i32.load
    local.set $param_b
    local.get $param_a
    local.get $param_b
    call $xor
    local.set $value
    i32.const 32
    call $__alloc
    local.set $out_ptr
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
    local.set $bytes_written
    ;; Write output pointer to out_ptr_ptr slot
    local.get $out_ptr_ptr
    local.get $out_ptr
    i32.store
    ;; Write output length to out_len_ptr slot
    local.get $out_len_ptr
    local.get $bytes_written
    i32.store
    ;; Return 0 for success
    i32.const 0
  )
  (func $bool__export (export "bool") (param $in_ptr i32) (param $in_len i32) (param $out_ptr_ptr i32) (param $out_len_ptr i32) (result i32)
    (local $out_ptr i32)
    (local $bytes_written i32)
    (local $value i32)
    (local $param_x i32)
    ;; Decode input parameters from CGRF
    ;; Decode s32 from CGRF
    local.get $in_ptr
    i32.const 24
    i32.add
    i32.load
    local.set $param_x
    local.get $param_x
    call $bool
    local.set $value
    i32.const 32
    call $__alloc
    local.set $out_ptr
    local.get $out_ptr
    i32.const 1179797315
    i32.store
    local.get $out_ptr
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $out_ptr
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    local.get $value
    i32.store
    i32.const 28
    local.set $bytes_written
    ;; Write output pointer to out_ptr_ptr slot
    local.get $out_ptr_ptr
    local.get $out_ptr
    i32.store
    ;; Write output length to out_len_ptr slot
    local.get $out_len_ptr
    local.get $bytes_written
    i32.store
    ;; Return 0 for success
    i32.const 0
  )
)
