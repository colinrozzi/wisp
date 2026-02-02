(module
  (import "math" "abs" (func $__raw_abs (param i32 i32 i32 i32) (result i32)))
  (import "math" "min" (func $__raw_min (param i32 i32 i32 i32) (result i32)))
  (import "math" "max" (func $__raw_max (param i32 i32 i32 i32) (result i32)))
  (import "math" "pow" (func $__raw_pow (param i32 i32 i32 i32) (result i32)))
  (import "logic" "not" (func $__raw_not (param i32 i32 i32 i32) (result i32)))
  (import "logic" "and" (func $__raw_and (param i32 i32 i32 i32) (result i32)))
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
  (func $abs (param $x i32) (result i32)
    (local $in_buf i32)
    (local $in_len i32)
    (local $out_ptr i32)
    (local $out_len i32)
    (local $status i32)
    i32.const 32768
    local.set $in_buf
    ;; Encode single s32 argument to CGRF
    local.get $in_buf
    i32.const 1179797315
    i32.store
    local.get $in_buf
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $in_buf
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $in_buf
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $in_buf
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $in_buf
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $in_buf
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $in_buf
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $in_buf
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $in_buf
    i32.const 24
    i32.add
    local.get $x
    i32.store
    i32.const 28
    local.set $in_len
    ;; Call raw import with ptr/len slots
    local.get $in_buf
    local.get $in_len
    i32.const 36864
    i32.const 36868
    call $__raw_abs
    local.set $status
    i32.const 36864
    i32.load
    local.set $out_ptr
    i32.const 36868
    i32.load
    local.set $out_len
    ;; Decode result from CGRF
    local.get $out_ptr
    i32.const 24
    i32.add
    i32.load
  )
  (func $min (param $a i32) (param $b i32) (result i32)
    (local $in_buf i32)
    (local $in_len i32)
    (local $out_ptr i32)
    (local $out_len i32)
    (local $status i32)
    i32.const 32768
    local.set $in_buf
    ;; Encode 2 s32 arguments as CGRF tuple (children first)
    local.get $in_buf
    i32.const 1179797315
    i32.store
    local.get $in_buf
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $in_buf
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $in_buf
    i32.const 8
    i32.add
    i32.const 3
    i32.store
    local.get $in_buf
    i32.const 12
    i32.add
    i32.const 2
    i32.store
    local.get $in_buf
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $in_buf
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $in_buf
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $in_buf
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $in_buf
    i32.const 24
    i32.add
    local.get $a
    i32.store
    local.get $in_buf
    i32.const 28
    i32.add
    i32.const 2
    i32.store8
    local.get $in_buf
    i32.const 29
    i32.add
    i32.const 0
    i32.store8
    local.get $in_buf
    i32.const 30
    i32.add
    i32.const 0
    i32.store16
    local.get $in_buf
    i32.const 32
    i32.add
    i32.const 4
    i32.store
    local.get $in_buf
    i32.const 36
    i32.add
    local.get $b
    i32.store
    local.get $in_buf
    i32.const 40
    i32.add
    i32.const 11
    i32.store8
    local.get $in_buf
    i32.const 41
    i32.add
    i32.const 0
    i32.store8
    local.get $in_buf
    i32.const 42
    i32.add
    i32.const 0
    i32.store16
    local.get $in_buf
    i32.const 44
    i32.add
    i32.const 8
    i32.store
    local.get $in_buf
    i32.const 48
    i32.add
    i32.const 0
    i32.store
    local.get $in_buf
    i32.const 52
    i32.add
    i32.const 1
    i32.store
    i32.const 56
    local.set $in_len
    ;; Call raw import with ptr/len slots
    local.get $in_buf
    local.get $in_len
    i32.const 36864
    i32.const 36868
    call $__raw_min
    local.set $status
    i32.const 36864
    i32.load
    local.set $out_ptr
    i32.const 36868
    i32.load
    local.set $out_len
    ;; Decode result from CGRF
    local.get $out_ptr
    i32.const 24
    i32.add
    i32.load
  )
  (func $max (param $a i32) (param $b i32) (result i32)
    (local $in_buf i32)
    (local $in_len i32)
    (local $out_ptr i32)
    (local $out_len i32)
    (local $status i32)
    i32.const 32768
    local.set $in_buf
    ;; Encode 2 s32 arguments as CGRF tuple (children first)
    local.get $in_buf
    i32.const 1179797315
    i32.store
    local.get $in_buf
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $in_buf
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $in_buf
    i32.const 8
    i32.add
    i32.const 3
    i32.store
    local.get $in_buf
    i32.const 12
    i32.add
    i32.const 2
    i32.store
    local.get $in_buf
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $in_buf
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $in_buf
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $in_buf
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $in_buf
    i32.const 24
    i32.add
    local.get $a
    i32.store
    local.get $in_buf
    i32.const 28
    i32.add
    i32.const 2
    i32.store8
    local.get $in_buf
    i32.const 29
    i32.add
    i32.const 0
    i32.store8
    local.get $in_buf
    i32.const 30
    i32.add
    i32.const 0
    i32.store16
    local.get $in_buf
    i32.const 32
    i32.add
    i32.const 4
    i32.store
    local.get $in_buf
    i32.const 36
    i32.add
    local.get $b
    i32.store
    local.get $in_buf
    i32.const 40
    i32.add
    i32.const 11
    i32.store8
    local.get $in_buf
    i32.const 41
    i32.add
    i32.const 0
    i32.store8
    local.get $in_buf
    i32.const 42
    i32.add
    i32.const 0
    i32.store16
    local.get $in_buf
    i32.const 44
    i32.add
    i32.const 8
    i32.store
    local.get $in_buf
    i32.const 48
    i32.add
    i32.const 0
    i32.store
    local.get $in_buf
    i32.const 52
    i32.add
    i32.const 1
    i32.store
    i32.const 56
    local.set $in_len
    ;; Call raw import with ptr/len slots
    local.get $in_buf
    local.get $in_len
    i32.const 36864
    i32.const 36868
    call $__raw_max
    local.set $status
    i32.const 36864
    i32.load
    local.set $out_ptr
    i32.const 36868
    i32.load
    local.set $out_len
    ;; Decode result from CGRF
    local.get $out_ptr
    i32.const 24
    i32.add
    i32.load
  )
  (func $pow (param $base i32) (param $exp i32) (result i32)
    (local $in_buf i32)
    (local $in_len i32)
    (local $out_ptr i32)
    (local $out_len i32)
    (local $status i32)
    i32.const 32768
    local.set $in_buf
    ;; Encode 2 s32 arguments as CGRF tuple (children first)
    local.get $in_buf
    i32.const 1179797315
    i32.store
    local.get $in_buf
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $in_buf
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $in_buf
    i32.const 8
    i32.add
    i32.const 3
    i32.store
    local.get $in_buf
    i32.const 12
    i32.add
    i32.const 2
    i32.store
    local.get $in_buf
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $in_buf
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $in_buf
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $in_buf
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $in_buf
    i32.const 24
    i32.add
    local.get $base
    i32.store
    local.get $in_buf
    i32.const 28
    i32.add
    i32.const 2
    i32.store8
    local.get $in_buf
    i32.const 29
    i32.add
    i32.const 0
    i32.store8
    local.get $in_buf
    i32.const 30
    i32.add
    i32.const 0
    i32.store16
    local.get $in_buf
    i32.const 32
    i32.add
    i32.const 4
    i32.store
    local.get $in_buf
    i32.const 36
    i32.add
    local.get $exp
    i32.store
    local.get $in_buf
    i32.const 40
    i32.add
    i32.const 11
    i32.store8
    local.get $in_buf
    i32.const 41
    i32.add
    i32.const 0
    i32.store8
    local.get $in_buf
    i32.const 42
    i32.add
    i32.const 0
    i32.store16
    local.get $in_buf
    i32.const 44
    i32.add
    i32.const 8
    i32.store
    local.get $in_buf
    i32.const 48
    i32.add
    i32.const 0
    i32.store
    local.get $in_buf
    i32.const 52
    i32.add
    i32.const 1
    i32.store
    i32.const 56
    local.set $in_len
    ;; Call raw import with ptr/len slots
    local.get $in_buf
    local.get $in_len
    i32.const 36864
    i32.const 36868
    call $__raw_pow
    local.set $status
    i32.const 36864
    i32.load
    local.set $out_ptr
    i32.const 36868
    i32.load
    local.set $out_len
    ;; Decode result from CGRF
    local.get $out_ptr
    i32.const 24
    i32.add
    i32.load
  )
  (func $not (param $x i32) (result i32)
    (local $in_buf i32)
    (local $in_len i32)
    (local $out_ptr i32)
    (local $out_len i32)
    (local $status i32)
    i32.const 32768
    local.set $in_buf
    ;; Encode single s32 argument to CGRF
    local.get $in_buf
    i32.const 1179797315
    i32.store
    local.get $in_buf
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $in_buf
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $in_buf
    i32.const 8
    i32.add
    i32.const 1
    i32.store
    local.get $in_buf
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get $in_buf
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $in_buf
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $in_buf
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $in_buf
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $in_buf
    i32.const 24
    i32.add
    local.get $x
    i32.store
    i32.const 28
    local.set $in_len
    ;; Call raw import with ptr/len slots
    local.get $in_buf
    local.get $in_len
    i32.const 36864
    i32.const 36868
    call $__raw_not
    local.set $status
    i32.const 36864
    i32.load
    local.set $out_ptr
    i32.const 36868
    i32.load
    local.set $out_len
    ;; Decode result from CGRF
    local.get $out_ptr
    i32.const 24
    i32.add
    i32.load
  )
  (func $and (param $a i32) (param $b i32) (result i32)
    (local $in_buf i32)
    (local $in_len i32)
    (local $out_ptr i32)
    (local $out_len i32)
    (local $status i32)
    i32.const 32768
    local.set $in_buf
    ;; Encode 2 s32 arguments as CGRF tuple (children first)
    local.get $in_buf
    i32.const 1179797315
    i32.store
    local.get $in_buf
    i32.const 4
    i32.add
    i32.const 2
    i32.store16
    local.get $in_buf
    i32.const 6
    i32.add
    i32.const 0
    i32.store16
    local.get $in_buf
    i32.const 8
    i32.add
    i32.const 3
    i32.store
    local.get $in_buf
    i32.const 12
    i32.add
    i32.const 2
    i32.store
    local.get $in_buf
    i32.const 16
    i32.add
    i32.const 2
    i32.store8
    local.get $in_buf
    i32.const 17
    i32.add
    i32.const 0
    i32.store8
    local.get $in_buf
    i32.const 18
    i32.add
    i32.const 0
    i32.store16
    local.get $in_buf
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get $in_buf
    i32.const 24
    i32.add
    local.get $a
    i32.store
    local.get $in_buf
    i32.const 28
    i32.add
    i32.const 2
    i32.store8
    local.get $in_buf
    i32.const 29
    i32.add
    i32.const 0
    i32.store8
    local.get $in_buf
    i32.const 30
    i32.add
    i32.const 0
    i32.store16
    local.get $in_buf
    i32.const 32
    i32.add
    i32.const 4
    i32.store
    local.get $in_buf
    i32.const 36
    i32.add
    local.get $b
    i32.store
    local.get $in_buf
    i32.const 40
    i32.add
    i32.const 11
    i32.store8
    local.get $in_buf
    i32.const 41
    i32.add
    i32.const 0
    i32.store8
    local.get $in_buf
    i32.const 42
    i32.add
    i32.const 0
    i32.store16
    local.get $in_buf
    i32.const 44
    i32.add
    i32.const 8
    i32.store
    local.get $in_buf
    i32.const 48
    i32.add
    i32.const 0
    i32.store
    local.get $in_buf
    i32.const 52
    i32.add
    i32.const 1
    i32.store
    i32.const 56
    local.set $in_len
    ;; Call raw import with ptr/len slots
    local.get $in_buf
    local.get $in_len
    i32.const 36864
    i32.const 36868
    call $__raw_and
    local.set $status
    i32.const 36864
    i32.load
    local.set $out_ptr
    i32.const 36868
    i32.load
    local.set $out_len
    ;; Decode result from CGRF
    local.get $out_ptr
    i32.const 24
    i32.add
    i32.load
  )
  (func $test-math (result i32)
    i32.const -5
    call $abs
    i32.const 3
    i32.const 7
    call $min
    i32.add
    i32.const 3
    i32.const 7
    call $max
    i32.const 2
    i32.const 3
    call $pow
    i32.add
    i32.add
  )
  (func $test-logic (result i32)
    i32.const 0
    call $not
    i32.const 1
    i32.const 1
    call $and
    i32.add
    i32.const 1
    i32.const 0
    call $and
    call $not
    i32.add
  )
  (func $test-math__export (export "test-math") (param $in_ptr i32) (param $in_len i32) (param $out_ptr_ptr i32) (param $out_len_ptr i32) (result i32)
    (local $out_ptr i32)
    (local $bytes_written i32)
    (local $value i32)
    call $test-math
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
  (func $test-logic__export (export "test-logic") (param $in_ptr i32) (param $in_len i32) (param $out_ptr_ptr i32) (param $out_len_ptr i32) (result i32)
    (local $out_ptr i32)
    (local $bytes_written i32)
    (local $value i32)
    call $test-logic
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
