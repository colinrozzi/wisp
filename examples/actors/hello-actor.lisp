; hello-actor.lisp — Minimal Theater actor in Wisp
;
; This is the first Theater actor written in Wisp. It exports the
; theater:simple/actor.init function with the Pack/Graph ABI signature.
;
; The init function receives CGRF-encoded Tuple(Option<List<U8>>, Tuple([]))
; and returns CGRF-encoded Result<Tuple<Option<List<U8>>>, String> = Ok((None,))
;
; The response is a hardcoded CGRF v2 graph buffer (69 bytes).

; CGRF v2 buffer at address 0x1000 (4096), 69 bytes
; Layout:
;   Header (16 bytes): magic "CGRF", version=2, flags=0, node_count=3, root=2
;   Node 0 (11 bytes): Option<List<U8>> = None
;   Node 1 (16 bytes): Tuple([Node 0])
;   Node 2 (26 bytes): Result<...> = Ok(Node 1)  [root]
(data 4096 "\x43\x47\x52\x46\x02\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x0a\x00\x00\x00\x03\x00\x00\x00\x07\x0c\x00\x0b\x00\x00\x00\x08\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x12\x00\x00\x00\x0b\x01\x00\x00\x00\x0a\x07\x0c\x06\x00\x00\x00\x00\x01\x01\x00\x00\x00")

; Export init with the Theater actor interface name
(export "theater:simple/actor.init"
  (fn init ((in-ptr s32) (in-len s32) (out-ptr-ptr s32) (out-len-ptr s32)) s32
    ; Point output to the pre-initialized CGRF buffer
    (let (_ (i32.store out-ptr-ptr (i32.const 4096)))
    (let (_ (i32.store out-len-ptr (i32.const 69)))
    ; Return 0 = success
    (i32.const 0)))))
