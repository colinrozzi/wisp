(module
  (import "theater:simple/runtime" "log" (func $__raw_log (param i32 i32 i32 i32) (result i32)))
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
  (func $log (param $msg i32) (result i32)
    (local $in_buf i32)
    (local $in_len i32)
    (local $out_ptr i32)
    (local $out_len i32)
    (local $status i32)
    i32.const 32768
    local.set $in_buf
    ;; Write CGRF header for string argument
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
    i32.const 6
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
    local.get $msg
    i32.load
    i32.const 4
    i32.add
    i32.store
    local.get $in_buf
    i32.const 24
    i32.add
    local.get $msg
    i32.load
    i32.store
    ;; Copy string data to CGRF buffer
    local.get $in_buf
    i32.const 28
    i32.add
    local.get $msg
    i32.const 4
    i32.add
    local.get $msg
    i32.load
    memory.copy
    i32.const 28
    local.get $msg
    i32.load
    i32.add
    local.set $in_len
    ;; Call raw import with ptr/len slots
    local.get $in_buf
    local.get $in_len
    i32.const 36864
    i32.const 36868
    call $__raw_log
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
  (func $init (param $state i32) (result i32)
    (local i32)
    (local i32)
    (local i32)
    (local i32)
    global.get $__heap_ptr
    local.set 1
    global.get $__heap_ptr
    i32.const 28
    i32.add
    global.set $__heap_ptr
    local.get 1
    i32.const 24
    i32.store
    local.get 1
    i32.const 4
    i32.add
    i32.const 84
    i32.store8
    local.get 1
    i32.const 5
    i32.add
    i32.const 121
    i32.store8
    local.get 1
    i32.const 6
    i32.add
    i32.const 112
    i32.store8
    local.get 1
    i32.const 7
    i32.add
    i32.const 101
    i32.store8
    local.get 1
    i32.const 8
    i32.add
    i32.const 100
    i32.store8
    local.get 1
    i32.const 9
    i32.add
    i32.const 32
    i32.store8
    local.get 1
    i32.const 10
    i32.add
    i32.const 97
    i32.store8
    local.get 1
    i32.const 11
    i32.add
    i32.const 99
    i32.store8
    local.get 1
    i32.const 12
    i32.add
    i32.const 116
    i32.store8
    local.get 1
    i32.const 13
    i32.add
    i32.const 111
    i32.store8
    local.get 1
    i32.const 14
    i32.add
    i32.const 114
    i32.store8
    local.get 1
    i32.const 15
    i32.add
    i32.const 32
    i32.store8
    local.get 1
    i32.const 16
    i32.add
    i32.const 105
    i32.store8
    local.get 1
    i32.const 17
    i32.add
    i32.const 110
    i32.store8
    local.get 1
    i32.const 18
    i32.add
    i32.const 105
    i32.store8
    local.get 1
    i32.const 19
    i32.add
    i32.const 116
    i32.store8
    local.get 1
    i32.const 20
    i32.add
    i32.const 105
    i32.store8
    local.get 1
    i32.const 21
    i32.add
    i32.const 97
    i32.store8
    local.get 1
    i32.const 22
    i32.add
    i32.const 108
    i32.store8
    local.get 1
    i32.const 23
    i32.add
    i32.const 105
    i32.store8
    local.get 1
    i32.const 24
    i32.add
    i32.const 122
    i32.store8
    local.get 1
    i32.const 25
    i32.add
    i32.const 101
    i32.store8
    local.get 1
    i32.const 26
    i32.add
    i32.const 100
    i32.store8
    local.get 1
    i32.const 27
    i32.add
    i32.const 33
    i32.store8
    local.get 1
    call $log
    drop
    global.get $__heap_ptr
    local.set 2
    global.get $__heap_ptr
    i32.const 8
    i32.add
    global.set $__heap_ptr
    local.get 2
    i32.const 0
    i32.store
    local.get 2
    i32.const 4
    i32.add
    local.get 0
    local.set 3
    global.get $__heap_ptr
    local.set 4
    global.get $__heap_ptr
    i32.const 4
    i32.add
    global.set $__heap_ptr
    local.get 4
    local.get 3
    i32.store
    local.get 4
    i32.store
    local.get 2
  )
  (func $init__export (export "theater:simple/actor.init") (param $in_ptr i32) (param $in_len i32) (param $out_ptr_ptr i32) (param $out_len_ptr i32) (result i32)
    (local $out_ptr i32)
    (local $bytes_written i32)
    (local $value i32)
    (local $param_state i32)
    (local $rec_ptr i32)
    (local $field_val i32)
    (local $child_idx i32)
    (local $child_offset i32)
    (local $scan_i i32)
    (local $payload_len i32)
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
    local.get $in_ptr
    i32.const 12
    i32.add
    i32.load
    local.set $dec_child_idx
    ;; Find node at index $dec_child_idx
    i32.const 16
    local.set $dec_scan_offset
    i32.const 0
    local.set $dec_scan_i
    (block $dec_found
      (loop $dec_scan
        local.get $dec_scan_i
        local.get $dec_child_idx
        i32.ge_u
        br_if $dec_found
        local.get $in_ptr
        local.get $dec_scan_offset
        i32.add
        i32.const 4
        i32.add
        i32.load
        local.set $dec_payload_len
        local.get $dec_scan_offset
        i32.const 8
        i32.add
        local.get $dec_payload_len
        i32.add
        local.set $dec_scan_offset
        local.get $dec_scan_i
        i32.const 1
        i32.add
        local.set $dec_scan_i
        br $dec_scan
      )
    )
    local.get $dec_scan_offset
    local.set $dec_node_offset
    ;; decode option
    global.get $__heap_ptr
    local.set $dec_opt_ptr
    global.get $__heap_ptr
    i32.const 8
    i32.add
    global.set $__heap_ptr
    local.get $dec_node_offset
    local.set $dec_opt_node_offset
    local.get $in_ptr
    local.get $dec_node_offset
    i32.add
    i32.const 10
    i32.add
    i32.load8_u
    (if
      (then
        local.get $dec_opt_ptr
        i32.const 1
        i32.store
        local.get $in_ptr
        local.get $dec_opt_node_offset
        i32.add
        i32.const 11
        i32.add
        i32.load
        local.set $dec_child_idx
    ;; Find node at index $dec_child_idx
    i32.const 16
    local.set $dec_scan_offset
    i32.const 0
    local.set $dec_scan_i
    (block $dec_found
      (loop $dec_scan
        local.get $dec_scan_i
        local.get $dec_child_idx
        i32.ge_u
        br_if $dec_found
        local.get $in_ptr
        local.get $dec_scan_offset
        i32.add
        i32.const 4
        i32.add
        i32.load
        local.set $dec_payload_len
        local.get $dec_scan_offset
        i32.const 8
        i32.add
        local.get $dec_payload_len
        i32.add
        local.set $dec_scan_offset
        local.get $dec_scan_i
        i32.const 1
        i32.add
        local.set $dec_scan_i
        br $dec_scan
      )
    )
    local.get $dec_scan_offset
    local.set $dec_node_offset
    ;; decode list
    local.get $dec_node_offset
    local.set $dec_list_node_offset
    local.get $in_ptr
    local.get $dec_node_offset
    i32.add
    i32.const 9
    i32.add
    i32.load
    local.set $dec_list_len
    global.get $__heap_ptr
    local.set $dec_list_ptr
    global.get $__heap_ptr
    i32.const 12
    i32.add
    global.set $__heap_ptr
    local.get $dec_list_ptr
    local.get $dec_list_len
    i32.store
    local.get $dec_list_ptr
    i32.const 4
    i32.add
    local.get $dec_list_len
    i32.store
    global.get $__heap_ptr
    local.set $dec_list_data
    global.get $__heap_ptr
    i32.const 4
    local.get $dec_list_len
    i32.mul
    i32.add
    global.set $__heap_ptr
    local.get $dec_list_ptr
    i32.const 8
    i32.add
    local.get $dec_list_data
    i32.store
    i32.const 0
    local.set $dec_list_i
    block $dec_list_break
      loop $dec_list_loop
        local.get $dec_list_i
        local.get $dec_list_len
        i32.ge_u
        br_if $dec_list_break
        local.get $in_ptr
        local.get $dec_list_node_offset
        i32.add
        i32.const 13
        i32.add
        local.get $dec_list_i
        i32.const 4
        i32.mul
        i32.add
        i32.load
        local.set $dec_child_idx
    ;; Find node at index $dec_child_idx
    i32.const 16
    local.set $dec_scan_offset
    i32.const 0
    local.set $dec_scan_i
    (block $dec_found
      (loop $dec_scan
        local.get $dec_scan_i
        local.get $dec_child_idx
        i32.ge_u
        br_if $dec_found
        local.get $in_ptr
        local.get $dec_scan_offset
        i32.add
        i32.const 4
        i32.add
        i32.load
        local.set $dec_payload_len
        local.get $dec_scan_offset
        i32.const 8
        i32.add
        local.get $dec_payload_len
        i32.add
        local.set $dec_scan_offset
        local.get $dec_scan_i
        i32.const 1
        i32.add
        local.set $dec_scan_i
        br $dec_scan
      )
    )
    local.get $dec_scan_offset
    local.set $dec_node_offset
    ;; decode u8
    local.get $in_ptr
    local.get $dec_node_offset
    i32.add
    i32.const 8
    i32.add
    i32.load8_u
    local.set $dec_result
        local.get $dec_list_data
        local.get $dec_list_i
        i32.const 4
        i32.mul
        i32.add
        local.get $dec_result
        i32.store
        local.get $dec_list_i
        i32.const 1
        i32.add
        local.set $dec_list_i
        br $dec_list_loop
      end
    end
    local.get $dec_list_ptr
    local.set $dec_result
        local.get $dec_opt_ptr
        i32.const 4
        i32.add
        local.get $dec_result
        i32.store
      )
      (else
        local.get $dec_opt_ptr
        i32.const 0
        i32.store
      )
    )
    local.get $dec_opt_ptr
    local.set $dec_result
    local.get $dec_result
    local.set $param_state
    local.get $param_state
    call $init
    local.set $value
    i32.const 4096
    call $__alloc
    local.set $out_ptr
    ;; Encode result value to CGRF (recursive encoder)
    i32.const 16
    local.set $buf_cursor
    i32.const 0
    local.set $node_idx
    ;; encode result
    local.get $value
    i32.load
    (if
      (then
        ;; Err branch: encode err value
    local.get $value
    i32.const 4
    i32.add
    i32.load
    local.set $enc_tmp
    ;; encode string
    local.get $node_idx
    local.set $enc_root_idx
    local.get $buf_cursor
    local.set $enc_header_start
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 6
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
    local.get $enc_tmp
    i32.load
    local.set $enc_tmp
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    local.get $enc_tmp
    i32.store
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 4
    i32.add
    local.get $enc_tmp
    i32.const 4
    i32.add
    local.get $enc_tmp
    memory.copy
    local.get $buf_cursor
    local.get $enc_tmp
    i32.add
    i32.const 4
    i32.add
    local.set $buf_cursor
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
    local.get $node_idx
    i32.const 1
    i32.add
    local.set $node_idx
      )
      (else
        ;; Ok branch: encode ok value
    local.get $value
    i32.const 4
    i32.add
    i32.load
    local.set $enc_tmp
    ;; encode tuple
    local.get $node_idx
    local.set $enc_root_idx
    local.get $enc_root_idx
    local.set $enc_save_root
    local.get $node_idx
    i32.const 1
    i32.add
    local.set $node_idx
    local.get $buf_cursor
    local.set $enc_header_start
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 11
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
    local.get $enc_header_start
    local.set $enc_tuple_header
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 1
    i32.store
    local.get $buf_cursor
    i32.const 4
    i32.add
    local.set $enc_tuple_ci_pos
    local.get $buf_cursor
    i32.const 8
    i32.add
    local.set $buf_cursor
    ;; encode tuple element 0
    local.get $enc_tmp
    i32.load
    local.set $enc_tmp
    ;; encode option
    local.get $enc_tmp
    i32.load
    (if
      (then
        ;; Some: encode child first
    local.get $enc_tmp
    i32.const 4
    i32.add
    i32.load
    local.set $enc_tmp
    ;; encode list
    local.get $enc_tmp
    i32.load
    local.set $enc_list_len
    local.get $enc_tmp
    i32.const 8
    i32.add
    i32.load
    local.set $enc_list_data
    local.get $node_idx
    local.set $enc_root_idx
    local.get $enc_root_idx
    local.set $enc_list_root_idx
    local.get $node_idx
    i32.const 1
    i32.add
    local.set $node_idx
    local.get $buf_cursor
    local.set $enc_header_start
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 7
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
    local.get $enc_header_start
    local.set $enc_list_header
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 12
    i32.store8
    local.get $buf_cursor
    i32.const 1
    i32.add
    local.set $buf_cursor
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    local.get $enc_list_len
    i32.store
    local.get $buf_cursor
    i32.const 4
    i32.add
    local.set $enc_list_ci_pos
    local.get $buf_cursor
    i32.const 4
    i32.add
    local.get $enc_list_len
    i32.const 4
    i32.mul
    i32.add
    local.set $buf_cursor
    i32.const 0
    local.set $enc_list_i
    block $list_break
      loop $list_loop
        local.get $enc_list_i
        local.get $enc_list_len
        i32.ge_u
        br_if $list_break
        local.get $enc_list_data
        local.get $enc_list_i
        i32.const 4
        i32.mul
        i32.add
        i32.load
        local.set $enc_tmp
    ;; encode U8
    local.get $node_idx
    local.set $enc_root_idx
    local.get $buf_cursor
    local.set $enc_header_start
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 12
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
    local.get $enc_tmp
    i32.store8
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
    i32.const 1
    i32.add
    local.set $buf_cursor
    local.get $node_idx
    i32.const 1
    i32.add
    local.set $node_idx
        local.get $out_ptr
        local.get $enc_list_ci_pos
        local.get $enc_list_i
        i32.const 4
        i32.mul
        i32.add
        i32.add
        local.get $enc_root_idx
        i32.store
        local.get $enc_list_i
        i32.const 1
        i32.add
        local.set $enc_list_i
        br $list_loop
      end
    end
    local.get $out_ptr
    local.get $enc_list_header
    i32.add
    i32.const 4
    i32.add
    local.get $buf_cursor
    local.get $enc_list_header
    i32.sub
    i32.const 8
    i32.sub
    i32.store
    local.get $enc_list_root_idx
    local.set $enc_root_idx
        local.get $enc_root_idx
        local.set $enc_save_child
        local.get $node_idx
        local.set $enc_root_idx
    local.get $buf_cursor
    local.set $enc_header_start
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 10
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
    i32.const 7
    i32.store8
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 1
    i32.add
    i32.const 12
    i32.store8
    local.get $buf_cursor
    i32.const 2
    i32.add
    local.set $buf_cursor
        local.get $out_ptr
        local.get $buf_cursor
        i32.add
        i32.const 1
        i32.store8
        local.get $buf_cursor
        i32.const 1
        i32.add
        local.set $buf_cursor
        local.get $out_ptr
        local.get $buf_cursor
        i32.add
        local.get $enc_save_child
        i32.store
        local.get $buf_cursor
        i32.const 4
        i32.add
        local.set $buf_cursor
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
        local.get $node_idx
        i32.const 1
        i32.add
        local.set $node_idx
      )
      (else
        ;; None: write option node only
        local.get $node_idx
        local.set $enc_root_idx
    local.get $buf_cursor
    local.set $enc_header_start
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 10
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
    i32.const 7
    i32.store8
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 1
    i32.add
    i32.const 12
    i32.store8
    local.get $buf_cursor
    i32.const 2
    i32.add
    local.set $buf_cursor
        local.get $out_ptr
        local.get $buf_cursor
        i32.add
        i32.const 0
        i32.store8
        local.get $buf_cursor
        i32.const 1
        i32.add
        local.set $buf_cursor
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
        local.get $node_idx
        i32.const 1
        i32.add
        local.set $node_idx
      )
    )
    local.get $out_ptr
    local.get $enc_tuple_ci_pos
    i32.add
    local.get $enc_root_idx
    i32.store
    local.get $out_ptr
    local.get $enc_tuple_header
    i32.add
    i32.const 4
    i32.add
    local.get $buf_cursor
    local.get $enc_tuple_header
    i32.sub
    i32.const 8
    i32.sub
    i32.store
    local.get $enc_save_root
    local.set $enc_root_idx
      )
    )
    local.get $enc_root_idx
    local.set $enc_save_child
    local.get $node_idx
    local.set $enc_root_idx
    local.get $buf_cursor
    local.set $enc_header_start
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 20
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
    i32.const 11
    i32.store8
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 1
    i32.add
    i32.const 1
    i32.store8
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 2
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 3
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 4
    i32.add
    i32.const 0
    i32.store8
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 5
    i32.add
    i32.const 10
    i32.store8
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 6
    i32.add
    i32.const 7
    i32.store8
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 7
    i32.add
    i32.const 12
    i32.store8
    local.get $buf_cursor
    i32.const 8
    i32.add
    local.set $buf_cursor
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 6
    i32.store8
    local.get $buf_cursor
    i32.const 1
    i32.add
    local.set $buf_cursor
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    local.get $value
    i32.load
    i32.store
    local.get $buf_cursor
    i32.const 4
    i32.add
    local.set $buf_cursor
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    i32.const 1
    i32.store8
    local.get $buf_cursor
    i32.const 1
    i32.add
    local.set $buf_cursor
    local.get $out_ptr
    local.get $buf_cursor
    i32.add
    local.get $enc_save_child
    i32.store
    local.get $buf_cursor
    i32.const 4
    i32.add
    local.set $buf_cursor
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
