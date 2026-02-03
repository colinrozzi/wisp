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
  (func $init__export (export "theater:simple/actor.init") (param $in_ptr i32) (param $in_len i32) (param $out_ptr_ptr i32) (param $out_len_ptr i32) (result i32)
    (local $out_ptr i32)
    (local $bytes_written i32)
    (local $value i32)
    (local $param_in-ptr i32)
    (local $param_in-len i32)
    (local $param_out-ptr-ptr i32)
    (local $param_out-len-ptr i32)
    (local $buf_cursor i32)
    (local $node_idx i32)
    (local $enc_root_idx i32)
    (local $enc_header_start i32)
    (local $enc_tmp i32)
    (local $enc_tmp_i64 i64)
    (local $enc_tmp_f32 f32)
    (local $enc_tmp_f64 f64)
    (local $enc_save_child i32)
    (local $enc_save_root i32)
    (local $enc_tuple_header i32)
    (local $enc_tuple_ci_pos i32)
    (local $enc_list_header i32)
    (local $enc_list_ci_pos i32)
    (local $enc_list_i i32)
    (local $enc_list_len i32)
    (local $enc_list_data i32)
    (local $enc_list_root_idx i32)
    (local $dec_node_offset i32)
    (local $dec_result i32)
    (local $dec_child_idx i32)
    (local $dec_scan_offset i32)
    (local $dec_scan_i i32)
    (local $dec_payload_len i32)
    (local $dec_tmp i32)
    (local $dec_opt_ptr i32)
    (local $dec_opt_node_offset i32)
    (local $dec_list_ptr i32)
    (local $dec_list_data i32)
    (local $dec_list_len i32)
    (local $dec_list_i i32)
    (local $dec_list_node_offset i32)
    ;; Decode input parameters from CGRF
    ;; Multiple params - expecting tuple root
    ;; Decode tuple param 0 (in-ptr)
    local.get $in_ptr
    i32.const 24
    i32.add
    i32.load
    local.set $param_in-ptr
    ;; Decode tuple param 1 (in-len)
    local.get $in_ptr
    i32.const 36
    i32.add
    i32.load
    local.set $param_in-len
    ;; Decode tuple param 2 (out-ptr-ptr)
    local.get $in_ptr
    i32.const 48
    i32.add
    i32.load
    local.set $param_out-ptr-ptr
    ;; Decode tuple param 3 (out-len-ptr)
    local.get $in_ptr
    i32.const 60
    i32.add
    i32.load
    local.set $param_out-len-ptr
    local.get $param_in-ptr
    local.get $param_in-len
    local.get $param_out-ptr-ptr
    local.get $param_out-len-ptr
    call $init
    local.set $value
    i32.const 32
    call $__alloc
    local.set $out_ptr
    ;; Encode result value to CGRF (recursive encoder)
    i32.const 16
    local.set $buf_cursor
    i32.const 0
    local.set $node_idx
    ;; encode S32
    local.get $node_idx
    local.set $enc_root_idx
    local.get $buf_cursor
    local.set $enc_header_start
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 2
    i32.store8
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 1
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 2
    i32.add
    i32.const 0
    i32.store16
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 4
    i32.add
    i32.const 0
    i32.store
    local.get $buf_cursor
    i32.const 8
    i32.add
    local.set $buf_cursor
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    local.get $value
    i32.store
    local.get $out_ptr
    local.get $enc_header_start
    i32.add
    i32.const 4
    i32.add
    local.get $buf_cursor
    local.get $enc_header_start
    i32.sub
    i32.const 8
    i32.sub
    i32.store
    local.get $buf_cursor
    i32.const 4
    i32.add
    local.set $buf_cursor
    local.get $node_idx
    i32.const 1
    i32.add
    local.set $node_idx
    ;; Write CGRF header
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
    local.get $node_idx
    i32.store
    local.get $out_ptr
    i32.const 12
    i32.add
    local.get $enc_root_idx
    i32.store
    local.get $buf_cursor
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
