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
  (func $make-point (param $x i32) (param $y i32) (result i32)
    (local i32)
    global.get $__heap_ptr
    local.set 2
    global.get $__heap_ptr
    i32.const 8
    i32.add
    global.set $__heap_ptr
    local.get 2
    local.get 0
    i32.store
    local.get 2
    i32.const 4
    i32.add
    local.get 1
    i32.store
    local.get 2
  )
  (func $point-sum (param $p i32) (result i32)
    local.get 0
    i32.load
    local.get 0
    i32.const 4
    i32.add
    i32.load
    i32.add
  )
  (func $test-internal (result i32)
    (local i32)
    (local i32)
    global.get $__heap_ptr
    local.set 0
    global.get $__heap_ptr
    i32.const 8
    i32.add
    global.set $__heap_ptr
    local.get 0
    i32.const 15
    i32.store
    local.get 0
    i32.const 4
    i32.add
    i32.const 25
    i32.store
    local.get 0
    local.set 1
    local.get 1
    i32.load
    local.get 1
    i32.const 4
    i32.add
    i32.load
    i32.add
  )
  (func $make-point__export (export "make-point") (param $in_ptr i32) (param $in_len i32) (param $out_ptr_ptr i32) (param $out_len_ptr i32) (result i32)
    (local $out_ptr i32)
    (local $bytes_written i32)
    (local $value i32)
    (local $param_x i32)
    (local $param_y i32)
    ;; Decode input parameters from CGRF
    ;; Multiple params - expecting tuple root
    ;; Decode tuple param 0 (x)
    local.get $in_ptr
    i32.const 24
    i32.add
    i32.load
    local.set $param_x
    ;; Decode tuple param 1 (y)
    local.get $in_ptr
    i32.const 36
    i32.add
    i32.load
    local.set $param_y
    local.get $param_x
    local.get $param_y
    call $make-point
    local.set $value
    i32.const 4096
    call $__alloc
    local.set $out_ptr
    ;; Encode record 'point' with 2 fields (CGRF v2)
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
    i32.const 3
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $out_ptr
    i32.const 16
    i32.add
    i32.const 9
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
    i32.const 31
    i32.store
    local.get $out_ptr
    i32.const 24
    i32.add
    i32.const 5
    i32.store
    local.get $out_ptr
    i32.const 28
    i32.add
    i32.const 112
    i32.store8
    local.get $out_ptr
    i32.const 29
    i32.add
    i32.const 111
    i32.store8
    local.get $out_ptr
    i32.const 30
    i32.add
    i32.const 105
    i32.store8
    local.get $out_ptr
    i32.const 31
    i32.add
    i32.const 110
    i32.store8
    local.get $out_ptr
    i32.const 32
    i32.add
    i32.const 116
    i32.store8
    local.get $out_ptr
    i32.const 33
    i32.add
    i32.const 2
    i32.store
    local.get $out_ptr
    i32.const 37
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 41
    i32.add
    i32.const 120
    i32.store8
    local.get $out_ptr
    i32.const 42
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 46
    i32.add
    i32.const 121
    i32.store8
    local.get $out_ptr
    i32.const 47
    i32.add
    i32.const 1
    i32.store
    local.get $out_ptr
    i32.const 51
    i32.add
    i32.const 2
    i32.store
    local.get $out_ptr
    i32.const 55
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 56
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 57
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 59
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 63
    i32.add
    local.get $value
    i32.load
    i32.store
    local.get $out_ptr
    i32.const 67
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    i32.const 68
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    i32.const 69
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    i32.const 71
    i32.add
    i32.const 4
    i32.store
    local.get $out_ptr
    i32.const 75
    i32.add
    local.get $value
    i32.const 4
    i32.add
    i32.load
    i32.store
    i32.const 79
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
  (func $point-sum__export (export "point-sum") (param $in_ptr i32) (param $in_len i32) (param $out_ptr_ptr i32) (param $out_len_ptr i32) (result i32)
    (local $out_ptr i32)
    (local $bytes_written i32)
    (local $value i32)
    (local $param_p i32)
    (local $rec_ptr i32)
    (local $field_val i32)
    (local $child_idx i32)
    (local $child_offset i32)
    (local $scan_i i32)
    (local $payload_len i32)
    ;; Decode input parameters from CGRF
    ;; Decode record 'point' (CGRF v2)
    global.get $__heap_ptr
    local.set $rec_ptr
    global.get $__heap_ptr
    i32.const 8
    i32.add
    global.set $__heap_ptr
    ;; Field 0 'x' at wisp offset 0
    local.get $in_ptr
    i32.const 24
    i32.add
    i32.load
    local.set $field_val
    local.get $rec_ptr
    local.get $field_val
    i32.store
    ;; Field 1 'y' at wisp offset 4
    local.get $in_ptr
    i32.const 36
    i32.add
    i32.load
    local.set $field_val
    local.get $rec_ptr
    i32.const 4
    i32.add
    local.get $field_val
    i32.store
    local.get $rec_ptr
    local.set $param_p
    local.get $param_p
    call $point-sum
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
  (func $test-internal__export (export "test-internal") (param $in_ptr i32) (param $in_len i32) (param $out_ptr_ptr i32) (param $out_len_ptr i32) (result i32)
    (local $out_ptr i32)
    (local $bytes_written i32)
    (local $value i32)
    call $test-internal
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
