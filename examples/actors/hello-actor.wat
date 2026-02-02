(module
  (memory (export "memory") 16000 16000)
  (data (i32.const 4096) "\43\47\52\46\02\00\00\00\03\00\00\00\02\00\00\00\0a\00\00\00\03\00\00\00\07\0c\00\0b\00\00\00\08\00\00\00\01\00\00\00\00\00\00\00\14\00\00\00\12\00\00\00\0b\01\00\00\00\0a\07\0c\06\00\00\00\00\01\01\00\00\00")
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
  (func $init (param $in-ptr i32) (param $in-len i32) (param $out-ptr-ptr i32) (param $out-len-ptr i32) (result i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    i32.const 4096
    local.set 4
    local.get 2
    local.get 4
    i32.store
    local.get 4
    local.set 5
    i32.const 69
    local.set 6
    local.get 3
    local.get 6
    i32.store
    local.get 6
    local.set 7
    i32.const 0
  )
  (export "theater:simple/actor.init" (func $init))
)
