; marshal.lisp — convert an interpreter `value` to a Pack dynamic `any` (CGRF).
;
; One recursive, tag-directed, TOTAL map: each interpreter type has exactly one
; Pack type. No coercion, no target type, no consulting rpc.describe — the value's
; own type decides its encoding. A type mismatch against a callee's parameter is
; a type error at the call site, not papered over here.
;
; Built entirely in Wisp over the `any` byte-view primitives (heap-alloc,
; any-from-addr, string-addr) + raw stores — no per-type compiler support.
;
; CGRF is a flat node graph: a 16-byte header then N self-describing nodes, each
; [kind:1][flags:1][reserved:2][payload_len:4][payload]. Compound nodes reference
; children by node index. We emit children first (depth-first), then the
; container, tracking the write cursor and next index in two globals.
;
; `any` blob layout: [len:u32][cgrf]. cgrf header @ blob+4, first node @ blob+20.

(global $enc-cur s32 mut 0)   ; next node write address (absolute)
(global $enc-idx s32 mut 0)   ; next node index
(global $tag-end s32 mut 0)   ; address just past the value-type tag read-type-tag parsed

; Recursive byte copy (CGRF has no aligned guarantees; memory.copy isn't exposed).
(fn blit ((dst s32) (src s32) (n s32)) s32
  (if (i32.eq n (i32.const 0)) (i32.const 0)
    (begin
      (i32.store8 dst (i32.load8_u src))
      (blit (i32.add dst (i32.const 1)) (i32.add src (i32.const 1)) (i32.sub n (i32.const 1))))))

; Write a run of s32 child indices from a list, starting at addr.
(fn write-indices ((addr s32) (idxs (list s32)) (i s32)) s32
  (if (i32.ge_s i (list-len idxs)) (i32.const 0)
    (begin
      (i32.store (i32.add addr (i32.mul i (i32.const 4))) (list-get idxs i))
      (write-indices addr idxs (i32.add i (i32.const 1))))))

; Reserve the next node index and advance the cursor past a node whose header +
; payload occupy `total` bytes. Returns the node's index.
(fn take-node ((total s32)) s32
  (let (idx (global.get $enc-idx))
    (begin
      (global.set $enc-cur (i32.add (global.get $enc-cur) total))
      (global.set $enc-idx (i32.add idx (i32.const 1)))
      idx)))

; Emit a scalar node (kind, payload-len) whose payload the caller has NOT written
; yet; returns (cursor-before). The caller writes the payload at cur+8.
(fn emit-scalar-header ((kind s32) (payload-len s32)) s32
  (let (cur (global.get $enc-cur))
    (begin
      (i32.store cur kind)                                   ; kind, flags=0, reserved=0
      (i32.store (i32.add cur (i32.const 4)) payload-len)    ; payload_len
      cur)))

; CGRF type-tag byte for a SCALAR type descriptor (a symbol); 0 = not a scalar
; (compound types are handled by write-type-tag). Mirrors type-of-tag.
(fn tag-of-type ((ty value)) s32
  (let (name (symbol-name ty))
    (if (string=? name "bool") (i32.const 1)
      (if (string=? name "s32") (i32.const 2)
        (if (string=? name "s64") (i32.const 3)
          (if (string=? name "f32") (i32.const 4)
            (if (string=? name "f64") (i32.const 5)
              (if (string=? name "string") (i32.const 6)
                (if (string=? name "u8") (i32.const 12)
                  (if (string=? name "u16") (i32.const 13)
                    (if (string=? name "u32") (i32.const 14)
                      (if (string=? name "u64") (i32.const 15)
                        (if (string=? name "s8") (i32.const 16)
                          (if (string=? name "s16") (i32.const 17)
                            (if (string=? name "char") (i32.const 18)
                              (if (string=? name "flags") (i32.const 19) (i32.const 0)))))))))))))))))

; Inverse: a SCALAR value-type tag byte -> its type descriptor symbol (every
; pack-abi scalar TYPE_*; compound tags are handled by read-type-tag).
(fn type-of-tag ((tag s32)) value
  (if (i32.eq tag (i32.const 1)) (symbol "bool")
    (if (i32.eq tag (i32.const 2)) (symbol "s32")
      (if (i32.eq tag (i32.const 3)) (symbol "s64")
        (if (i32.eq tag (i32.const 4)) (symbol "f32")
          (if (i32.eq tag (i32.const 5)) (symbol "f64")
            (if (i32.eq tag (i32.const 6)) (symbol "string")
              (if (i32.eq tag (i32.const 12)) (symbol "u8")
                (if (i32.eq tag (i32.const 13)) (symbol "u16")
                  (if (i32.eq tag (i32.const 14)) (symbol "u32")
                    (if (i32.eq tag (i32.const 15)) (symbol "u64")
                      (if (i32.eq tag (i32.const 16)) (symbol "s8")
                        (if (i32.eq tag (i32.const 17)) (symbol "s16")
                          (if (i32.eq tag (i32.const 18)) (symbol "char")
                            (if (i32.eq tag (i32.const 19)) (symbol "flags")
                              (symbol "unknown"))))))))))))))))

; Type descriptors matching the interpreter's own (collections.lisp).
(fn mk-option-type ((inner value)) value
  (sequence (list-push (list-push (list-new value) (symbol "option")) inner)))
(fn mk-result-type ((ok value) (err value)) value
  (sequence (list-push (list-push (list-push (list-new value) (symbol "result")) ok) err)))

; Write a CGRF value-type tag for descriptor `ty` at addr (mirrors pack-abi
; encode_value_type); return the address just past it. Inverse of read-type-tag.
; Variable-length — a nested ok/err/element type recurses.
(fn write-tuple-elems ((addr s32) (parts (list value)) (i s32)) s32
  (if (i32.ge_s i (list-len parts)) addr
    (write-tuple-elems (write-type-tag addr (list-get parts i)) parts (i32.add i (i32.const 1)))))
(fn write-tag1 ((addr s32) (byte s32)) s32   ; write one tag byte, return addr+1
  (begin (i32.store8 addr byte) (i32.add addr (i32.const 1))))
(fn write-type-tag ((addr s32) (ty value)) s32
  (if (symbol? ty)
    (let (scalar (tag-of-type ty))
      (if (i32.ne scalar (i32.const 0))
        (write-tag1 addr scalar)                         ; scalar: one byte
        ; a named record/variant type -> RECORD(9) + namelen:u32 + name bytes
        (let (sp (string-addr (symbol-name ty)))
          (let (nlen (i32.load sp))
            (begin
              (i32.store8 addr (i32.const 9))
              (i32.store (i32.add addr (i32.const 1)) nlen)
              (blit (i32.add addr (i32.const 5)) (i32.add sp (i32.const 4)) nlen)
              (i32.add addr (i32.add (i32.const 5) nlen)))))))
    (let (parts (items-of ty))
      (let (head (symbol-name (list-get parts 0)))
        (if (string=? head "list")   (write-type-tag (write-tag1 addr (i32.const 7)) (list-get parts 1))
          (if (string=? head "option") (write-type-tag (write-tag1 addr (i32.const 10)) (list-get parts 1))
            (if (string=? head "set")  (write-type-tag (write-tag1 addr (i32.const 23)) (list-get parts 1))
              (if (string=? head "result")
                (write-type-tag (write-type-tag (write-tag1 addr (i32.const 20)) (list-get parts 1)) (list-get parts 2))
                (if (string=? head "map")
                  (write-type-tag (write-type-tag (write-tag1 addr (i32.const 22)) (list-get parts 1)) (list-get parts 2))
                  (if (string=? head "tuple")
                    (begin
                      (i32.store8 addr (i32.const 11))
                      (i32.store (i32.add addr (i32.const 1)) (i32.sub (list-len parts) (i32.const 1)))
                      (write-tuple-elems (i32.add addr (i32.const 5)) parts 1))
                    (write-tag1 addr (i32.const 0))))))))))))   ; unknown -> placeholder scalar

; Emit a Tuple node (0x0B) given already-emitted child indices; return its index.
(fn enc-tuple-node ((child-idxs (list s32))) s32
  (let (count (list-len child-idxs))
    (let (cur (global.get $enc-cur))
      (begin
        (i32.store cur (i32.const 11))
        (i32.store (i32.add cur (i32.const 4)) (i32.add (i32.const 4) (i32.mul count (i32.const 4))))
        (i32.store (i32.add cur (i32.const 8)) count)
        (write-indices (i32.add cur (i32.const 12)) child-idxs 0)
        (take-node (i32.add (i32.const 12) (i32.mul count (i32.const 4))))))))

; Emit an Option node (0x0A). payload: [inner-tag:u8][presence:u8][child:u32?].
; Emit an Option node (0x0A). payload: [inner-type-tag][presence:u8][child:u32?].
; The inner type tag is variable-length (write-type-tag), so sizes are computed.
(fn enc-option ((inner value) (fields (list value))) s32
  (if (i32.eq (list-len fields) 0)
    (let (cur (global.get $enc-cur))
      (let (e (write-type-tag (i32.add cur (i32.const 8)) inner))   ; e = past the type tag
        (begin
          (i32.store8 e (i32.const 0))                              ; presence=0
          (i32.store cur (i32.const 10))
          (i32.store (i32.add cur (i32.const 4)) (i32.sub (i32.add e (i32.const 1)) (i32.add cur (i32.const 8))))
          (take-node (i32.sub (i32.add e (i32.const 1)) cur)))))
    (let (child (enc-value (list-get fields 0)))
      (let (cur (global.get $enc-cur))
        (let (e (write-type-tag (i32.add cur (i32.const 8)) inner))
          (begin
            (i32.store8 e (i32.const 1))                            ; presence=1
            (i32.store (i32.add e (i32.const 1)) child)             ; child index
            (i32.store cur (i32.const 10))
            (i32.store (i32.add cur (i32.const 4)) (i32.sub (i32.add e (i32.const 5)) (i32.add cur (i32.const 8))))
            (take-node (i32.sub (i32.add e (i32.const 5)) cur))))))))

; Emit a Result node (0x14). payload: [ok-tag][err-tag][tag:u32][has:u8][child:u32].
(fn enc-result ((ok value) (err value) (is-err s32) (fields (list value))) s32
  (let (child (enc-value (list-get fields 0)))
    (let (cur (global.get $enc-cur))
      (let (e (write-type-tag (write-type-tag (i32.add cur (i32.const 8)) ok) err))  ; e = past both tags
        (begin
          (i32.store e is-err)                                      ; tag:u32
          (i32.store8 (i32.add e (i32.const 4)) (i32.const 1))      ; has_payload
          (i32.store (i32.add e (i32.const 5)) child)               ; child:u32
          (i32.store cur (i32.const 20))
          (i32.store (i32.add cur (i32.const 4)) (i32.sub (i32.add e (i32.const 9)) (i32.add cur (i32.const 8))))
          (take-node (i32.sub (i32.add e (i32.const 9)) cur)))))))

; list<u8> -> Array node (0x15): [elem-tag:u8][count:u32][contiguous bytes].
(fn byte-of ((v value)) s32 (value-case v ((byte-value b) b) (else 0)))
(fn write-bytes ((addr s32) (items (list value)) (i s32)) s32
  (if (i32.ge_s i (list-len items)) (i32.const 0)
    (begin
      (i32.store8 (i32.add addr i) (byte-of (list-get items i)))
      (write-bytes addr items (i32.add i (i32.const 1))))))
(fn enc-array-u8 ((items (list value))) s32
  (let (count (list-len items))
    (let (cur (global.get $enc-cur))
      (begin
        (i32.store cur (i32.const 21))                                 ; kind=Array 0x15
        (i32.store (i32.add cur (i32.const 4)) (i32.add (i32.const 5) count)) ; payload_len
        (i32.store8 (i32.add cur (i32.const 8)) (i32.const 12))        ; elem tag = u8
        (i32.store (i32.add cur (i32.const 9)) count)
        (write-bytes (i32.add cur (i32.const 13)) items 0)
        (take-node (i32.add (i32.const 13) count))))))

; Non-fixed-element list (string/compound) -> List node (0x07):
; [elem-type-tag (variable)][count:u32][child-indices]. Children emitted first.
(fn enc-list-node ((elem value) (items (list value))) s32
  (let (child-idxs (enc-children items 0 (list-new s32)))
    (let (cur (global.get $enc-cur))
      (let (e (write-type-tag (i32.add cur (i32.const 8)) elem))
        (let (count (list-len child-idxs))
          (begin
            (i32.store e count)                                     ; count:u32
            (write-indices (i32.add e (i32.const 4)) child-idxs 0)  ; child indices
            (i32.store cur (i32.const 7))                           ; kind LIST
            (i32.store (i32.add cur (i32.const 4)) (i32.sub (i32.add e (i32.add (i32.const 4) (i32.mul count (i32.const 4)))) (i32.add cur (i32.const 8))))
            (take-node (i32.sub (i32.add e (i32.add (i32.const 4) (i32.mul count (i32.const 4)))) cur))))))))

; Encode `v` into the buffer at the global cursor; return its node index.
(fn enc-value ((v value)) s32
  (value-case v
    ((boolean b)
      (let (cur (emit-scalar-header (i32.const 1) (i32.const 1)))   ; CGRF Bool, 1-byte payload
        (begin (i32.store8 (i32.add cur (i32.const 8)) b) (take-node (i32.const 9)))))
    ((byte-value b)
      (let (cur (emit-scalar-header (i32.const 12) (i32.const 1)))   ; CGRF U8 = 0x0C
        (begin (i32.store8 (i32.add cur (i32.const 8)) b) (take-node (i32.const 9)))))
    ((integer n)
      (let (cur (emit-scalar-header (i32.const 2) (i32.const 4)))
        (begin (i32.store (i32.add cur (i32.const 8)) n) (take-node (i32.const 12)))))
    ((wide-integer n)
      (let (cur (emit-scalar-header (i32.const 3) (i32.const 8)))
        (begin (i64.store (i32.add cur (i32.const 8)) n) (take-node (i32.const 16)))))
    ((u64-value n)
      (let (cur (emit-scalar-header (i32.const 15) (i32.const 8)))   ; CGRF U64 = 0x0F
        (begin (i64.store (i32.add cur (i32.const 8)) n) (take-node (i32.const 16)))))
    ((single f)
      (let (cur (emit-scalar-header (i32.const 4) (i32.const 4)))
        (begin (f32.store (i32.add cur (i32.const 8)) f) (take-node (i32.const 12)))))
    ((double d)
      (let (cur (emit-scalar-header (i32.const 5) (i32.const 8)))
        (begin (f64.store (i32.add cur (i32.const 8)) d) (take-node (i32.const 16)))))
    ((text s)
      (let (sp (string-addr s))
        (let (slen (i32.load sp))
          (let (cur (emit-scalar-header (i32.const 6) (i32.add (i32.const 4) slen)))
            (begin
              (i32.store (i32.add cur (i32.const 8)) slen)                 ; string length
              (blit (i32.add cur (i32.const 12)) (i32.add sp (i32.const 4)) slen)
              (take-node (i32.add (i32.const 12) slen)))))))
    ; A heterogeneous s-expression marshals to a positional Tuple.
    ((sequence items)
      (enc-tuple-node (enc-children items 0 (list-new s32))))
    ; Homogeneous list: fixed-width primitives (u8) -> Array; string -> List node.
    ; Other element types (s32/s64/.../nested) are the next increment.
    ; u8 lists use the packed Array node; every other element type (string,
    ; nested) uses a List node whose element type-tag write-type-tag now encodes.
    ((typed-list element items)
      (if (i32.eq (tag-of-type element) (i32.const 12)) (enc-array-u8 items)
        (enc-list-node element items)))
    ; Typed compound values: tuple -> Tuple, some/none -> Option, ok/err -> Result.
    ; `ty` is the type descriptor; its inner/ok/err types (possibly compound) are
    ; written by write-type-tag.
    ((compound ty case fields)
      (if (string=? case "tuple") (enc-tuple-node (enc-children fields 0 (list-new s32)))
        (if (string=? case "some") (enc-option (list-get (items-of ty) 1) fields)
          (if (string=? case "none") (enc-option (list-get (items-of ty) 1) fields)
            (if (string=? case "ok")
              (enc-result (list-get (items-of ty) 1) (list-get (items-of ty) 2) (i32.const 0) fields)
              (if (string=? case "err")
                (enc-result (list-get (items-of ty) 1) (list-get (items-of ty) 2) (i32.const 1) fields)
                (enc-tuple-node (list-new s32))))))))      ; records/variants: later
    ; A nominal record/variant aggregate. Records emit a Record node, reading
    ; field names from the $types schema. (Variants are the next increment.)
    ((aggregate name id case-name fields)
      (let (nt (list-get (global.get $types) id))
        (if (named-type.variant? nt)
          (enc-open-variant name case-name (variant-tag (named-type.schema nt) case-name 0) fields)
          (enc-record name (schema-field-names (named-type.schema nt) 0 (list-new value)) fields))))
    ; A foreign variant carries its own identity (name, case, tag) inline.
    ((open-variant tn cn tag fields) (enc-open-variant tn cn tag fields))
    (else
      ; Unmarshalable (closures, etc.): emit an empty tuple as a placeholder.
      (let (cur (global.get $enc-cur))
        (begin
          (i32.store cur (i32.const 11))
          (i32.store (i32.add cur (i32.const 4)) (i32.const 4))
          (i32.store (i32.add cur (i32.const 8)) (i32.const 0))
          (take-node (i32.const 12)))))))

; Encode each item in order (children must precede their container), collecting
; node indices.
(fn enc-children ((items (list value)) (i s32) (acc (list s32))) (list s32)
  (if (i32.ge_s i (list-len items)) acc
    (enc-children items (i32.add i (i32.const 1)) (list-push acc (enc-value (list-get items i))))))

(fn marshal ((v value)) any
  ; TODO: size pass instead of a fixed over-allocation (bump heap never frees).
  (let (buf (heap-alloc (i32.const 16388)))
    (begin
      (global.set $enc-cur (i32.add buf (i32.const 20)))   ; past len prefix(4) + header(16)
      (global.set $enc-idx (i32.const 0))
      (let (root (enc-value v))
        (let (cgrf-len (i32.sub (global.get $enc-cur) (i32.add buf (i32.const 4))))
          (begin
            (i32.store (i32.add buf (i32.const 4)) (i32.const 1179797315)) ; magic "CGRF"
            (i32.store (i32.add buf (i32.const 8)) (i32.const 3))          ; version=3, flags=0
            (i32.store (i32.add buf (i32.const 12)) (global.get $enc-idx)) ; node_count
            (i32.store (i32.add buf (i32.const 16)) root)                  ; root_index
            (i32.store buf cgrf-len)                                       ; len prefix
            (any-from-addr buf)))))))

; ---------------------------------------------------------------------------
; unmarshal — Pack dynamic `any` (CGRF) back into an interpreter `value`.
; The mirror of marshal: walk the node graph from the root, dispatch on the
; node-kind byte, build the interp value. Nodes are laid out sequentially in
; index order from cgrf+16, so node[i] is found by skipping i nodes.

; Offset (relative to cgrf base) of node `index`, scanning from `off`.
(fn find-node ((cgrf s32) (index s32) (off s32)) s32
  (if (i32.eq index 0) off
    (find-node cgrf (i32.sub index 1)
      (i32.add off (i32.add (i32.const 8)
        (i32.load (i32.add (i32.add cgrf off) (i32.const 4))))))))

(fn unmarshal-node ((cgrf s32) (off s32)) value
  (let (base (i32.add cgrf off))
    (let (kind (i32.load8_u base))
      (let (payload (i32.add base (i32.const 8)))
        (if (i32.eq kind (i32.const 1)) (boolean (i32.load8_u payload))
          (if (i32.eq kind (i32.const 2)) (integer (i32.load payload))
            (if (i32.eq kind (i32.const 3)) (wide-integer (i64.load payload))
              (if (i32.eq kind (i32.const 4)) (single (f32.load payload))
                (if (i32.eq kind (i32.const 5)) (double (f64.load payload))
                  (if (i32.eq kind (i32.const 12)) (byte-value (i32.load8_u payload))
                    (if (i32.eq kind (i32.const 15)) (u64-value (i64.load payload))
                      ; A CGRF string payload is [len:u32][bytes] — a Wisp string.
                      (if (i32.eq kind (i32.const 6)) (text (string-from-addr payload))
                        ; Tuple payload: [count:u32][child-indices:u32*].
                        (if (i32.eq kind (i32.const 11))
                          (sequence (unmarshal-children cgrf (i32.add payload (i32.const 4))
                                      (i32.load payload) 0 (list-new value)))
                          ; Option payload: [inner-tag:u8][presence:u8][child:u32?].
                          (if (i32.eq kind (i32.const 10)) (unmarshal-option cgrf payload)
                            ; Result payload: [ok-tag:u8][err-tag:u8][tag:u32][has:u8][child:u32].
                            (if (i32.eq kind (i32.const 20)) (unmarshal-result cgrf payload)
                              (if (i32.eq kind (i32.const 7)) (unmarshal-list cgrf payload)
                                (if (i32.eq kind (i32.const 21)) (unmarshal-array payload)
                                  ; Record payload: [name][field_count][field-names][child-indices].
                                  (if (i32.eq kind (i32.const 9)) (unmarshal-record cgrf payload)
                                    ; Variant payload: [name][case][tag:u32][count:u32][child-indices].
                                    (if (i32.eq kind (i32.const 8)) (unmarshal-variant cgrf payload)
                                      (failure "unmarshal: unsupported CGRF node kind"))))))))))))))))))))

; Decode a CGRF value-type tag at addr (mirrors pack-abi decode_value_type).
; Returns the type descriptor and sets $tag-end to the address just past the tag.
; Variable-length — this is what lets a nested ok/err/element type skip correctly.
(fn read-tuple-tag ((addr s32) (count s32) (i s32) (acc (list value))) value
  (if (i32.ge_s i count) (sequence acc)
    (let (elem (read-type-tag addr))
      (read-tuple-tag (global.get $tag-end) count (i32.add i (i32.const 1)) (list-push acc elem)))))
(fn read-type-tag ((addr s32)) value
  (let (tag (i32.load8_u addr))
    (if (i32.eq tag (i32.const 7))                       ; LIST (0x07): tag + elem
      (unary-type "list" (read-type-tag (i32.add addr (i32.const 1))))
      (if (i32.eq tag (i32.const 10))                    ; OPTION (0x0A): tag + inner
        (unary-type "option" (read-type-tag (i32.add addr (i32.const 1))))
        (if (i32.eq tag (i32.const 23))                  ; SET (0x17): tag + elem
          (unary-type "set" (read-type-tag (i32.add addr (i32.const 1))))
          (if (i32.eq tag (i32.const 20))                ; RESULT (0x14): tag + ok + err
            (let (ok (read-type-tag (i32.add addr (i32.const 1))))
              (mk-result-type ok (read-type-tag (global.get $tag-end))))
            (if (i32.eq tag (i32.const 22))              ; MAP (0x16): tag + key + value
              (let (k (read-type-tag (i32.add addr (i32.const 1))))
                (unary-type "map" (read-type-tag (global.get $tag-end))))
              (if (i32.or (i32.eq tag (i32.const 9)) (i32.eq tag (i32.const 8))) ; RECORD/VARIANT: tag + namelen + name
                (begin
                  (global.set $tag-end (i32.add addr (i32.add (i32.const 5) (i32.load (i32.add addr (i32.const 1))))))
                  (symbol (string-from-addr (i32.add addr (i32.const 1)))))
                (if (i32.eq tag (i32.const 11))          ; TUPLE (0x0B): tag + count + elems
                  (begin
                    (global.set $tag-end (i32.add addr (i32.const 5)))
                    (read-tuple-tag (i32.add addr (i32.const 5)) (i32.load (i32.add addr (i32.const 1)))
                      0 (list-push (list-new value) (symbol "tuple"))))
                  (begin                                 ; scalar: a single tag byte
                    (global.set $tag-end (i32.add addr (i32.const 1)))
                    (type-of-tag tag)))))))))))

(fn unmarshal-option ((cgrf s32) (payload s32)) value
  (let (inner (read-type-tag payload))
    (let (p (global.get $tag-end))
      (if (i32.load8_u p)
        (compound (mk-option-type inner) "some"
          (list-push (list-new value)
            (unmarshal-node cgrf (find-node cgrf (i32.load (i32.add p (i32.const 1))) (i32.const 16)))))
        (compound (mk-option-type inner) "none" (list-new value))))))

; List node (0x07): [elem-type-tag][count:u32][child-indices:u32*] -> typed-list.
(fn unmarshal-list ((cgrf s32) (payload s32)) value
  (let (elem (read-type-tag payload))
    (let (p (global.get $tag-end))
      (typed-list elem
        (unmarshal-children cgrf (i32.add p (i32.const 4)) (i32.load p) 0 (list-new value))))))

; Array node (0x15): [elem-type-tag][count:u32][contiguous data]. Only u8 (width 1)
; is handled for now; other widths are the next increment.
(fn read-bytes ((addr s32) (count s32) (i s32) (acc (list value))) (list value)
  (if (i32.ge_s i count) acc
    (read-bytes addr count (i32.add i (i32.const 1))
      (list-push acc (byte-value (i32.load8_u (i32.add addr i)))))))
(fn unmarshal-array ((payload s32)) value
  (let (elem (read-type-tag payload))
    (let (p (global.get $tag-end))
      (typed-list elem (read-bytes (i32.add p (i32.const 4)) (i32.load p) 0 (list-new value))))))

; Result node (0x14): [ok-type-tag][err-type-tag][tag:u32][has:u8][child:u32].
; The type tags are variable-length (recursive), so read them to find the fields.
(fn unmarshal-result ((cgrf s32) (payload s32)) value
  (let (ok-ty (read-type-tag payload))
    (let (err-ty (read-type-tag (global.get $tag-end)))
      (let (p (global.get $tag-end))
        (let (is-err (i32.load p))
          (if (i32.load8_u (i32.add p (i32.const 4)))
            (compound (mk-result-type ok-ty err-ty) (if is-err "err" "ok")
              (list-push (list-new value)
                (unmarshal-node cgrf (find-node cgrf (i32.load (i32.add p (i32.const 5))) (i32.const 16)))))
            (compound (mk-result-type ok-ty err-ty) (if is-err "err" "ok") (list-new value))))))))

(fn unmarshal-children ((cgrf s32) (indices s32) (count s32) (i s32) (acc (list value))) (list value)
  (if (i32.ge_s i count) acc
    (unmarshal-children cgrf indices count (i32.add i (i32.const 1))
      (list-push acc
        (unmarshal-node cgrf
          (find-node cgrf (i32.load (i32.add indices (i32.mul i (i32.const 4)))) (i32.const 16)))))))

(fn unmarshal ((x any)) value
  (let (cgrf (i32.add (any-addr x) (i32.const 4)))
    (unmarshal-node cgrf
      (find-node cgrf (i32.load (i32.add cgrf (i32.const 12))) (i32.const 16)))))

; ---------------------------------------------------------------------------
; record register-on-arrival (nominal). The codec now lives in the interpreter,
; so it can reach the $types registry. A foreign record arrives self-describing
; (type name + field names inline); we register a named-type (keyed by name +
; field-name shape, reusing a match) and build an aggregate against it.

; Write a Pack string [len:u32][utf8] at addr; return the address just past it.
(fn write-pstring ((addr s32) (s string)) s32
  (let (sp (string-addr s))
    (let (slen (i32.load sp))
      (begin
        (i32.store addr slen)
        (blit (i32.add addr (i32.const 4)) (i32.add sp (i32.const 4)) slen)
        (i32.add addr (i32.add (i32.const 4) slen))))))
(fn write-pstrings ((addr s32) (names (list value)) (i s32)) s32
  (if (i32.ge_s i (list-len names)) addr
    (write-pstrings (write-pstring addr (symbol-name (list-get names i))) names (i32.add i (i32.const 1)))))
(fn write-idx-run ((addr s32) (idxs (list s32)) (i s32)) s32
  (if (i32.ge_s i (list-len idxs)) addr
    (begin
      (i32.store addr (list-get idxs i))
      (write-idx-run (i32.add addr (i32.const 4)) idxs (i32.add i (i32.const 1))))))

; Emit a Record node (0x09): [name][field_count][field-names][child-indices].
(fn enc-record ((name string) (fnames (list value)) (fields (list value))) s32
  (let (child-idxs (enc-children fields 0 (list-new s32)))
    (let (idx (global.get $enc-idx))
      (let (cur (global.get $enc-cur))
        (let (p (i32.add cur (i32.const 8)))
          (let (p1 (write-pstring p name))
            (begin
              (i32.store cur (i32.const 9))                      ; kind=Record 0x09
              (i32.store p1 (list-len fnames))                   ; field_count
              (let (p4 (write-idx-run (write-pstrings (i32.add p1 (i32.const 4)) fnames 0) child-idxs 0))
                (begin
                  (i32.store (i32.add cur (i32.const 4)) (i32.sub p4 p))  ; payload_len
                  (global.set $enc-cur p4)
                  (global.set $enc-idx (i32.add idx (i32.const 1)))
                  idx)))))))))

; The field-name symbols of a named-type's schema (each field is (name type)).
(fn schema-field-names ((schema (list value)) (i s32) (acc (list value))) (list value)
  (if (i32.ge_s i (list-len schema)) acc
    (schema-field-names schema (i32.add i (i32.const 1))
      (list-push acc (list-get (items-of (list-get schema i)) 0)))))

; --- unmarshal side ---
; Collect field-name symbols by walking the variable-length name run.
(fn read-field-names ((addr s32) (count s32) (i s32) (acc (list value))) (list value)
  (if (i32.ge_s i count) acc
    (read-field-names (i32.add addr (i32.add (i32.const 4) (i32.load addr))) count (i32.add i (i32.const 1))
      (list-push acc (symbol (string-from-addr addr))))))
; Address just past `count` names (where the child-index array begins).
(fn skip-field-names ((addr s32) (count s32) (i s32)) s32
  (if (i32.ge_s i count) addr
    (skip-field-names (i32.add addr (i32.add (i32.const 4) (i32.load addr))) count (i32.add i (i32.const 1)))))

; Build schema field descriptors (name type) from names + field values.
(fn build-schema ((names (list value)) (values (list value)) (i s32) (acc (list value))) (list value)
  (if (i32.ge_s i (list-len names)) acc
    (build-schema names values (i32.add i (i32.const 1))
      (list-push acc (sequence (list-push (list-push (list-new value) (list-get names i))
                                          (value-type (list-get values i))))))))

; Do a named-type's schema field names equal `names`?
(fn names-match ((schema (list value)) (names (list value)) (i s32)) s32
  (if (i32.ne (list-len schema) (list-len names)) 0
    (if (i32.ge_s i (list-len names)) 1
      (if (string=? (symbol-name (list-get (items-of (list-get schema i)) 0)) (symbol-name (list-get names i)))
        (names-match schema names (i32.add i (i32.const 1))) 0))))
; Find an existing record type with this name + field-name shape; id or -1.
(fn find-record ((name string) (names (list value)) (index s32)) s32
  (if (i32.lt_s index 0) (i32.const -1)
    (let (nt (list-get (global.get $types) index))
      (if (i32.and (i32.eq (named-type.variant? nt) 0)
            (i32.and (string=? (named-type.name nt) name) (names-match (named-type.schema nt) names 0)))
        (named-type.id nt)
        (find-record name names (i32.sub index 1))))))
; Reuse or register (name+shape identity); return the type id.
(fn register-record ((name string) (names (list value)) (values (list value))) s32
  (let (existing (find-record name names (i32.sub (list-len (global.get $types)) (i32.const 1))))
    (if (i32.ge_s existing 0) existing
      (let (id (list-len (global.get $types)))
        (begin
          (global.set $types (list-push (global.get $types)
            (named-type name id 0 (build-schema names values 0 (list-new value)))))
          id)))))

; Record node (0x09): [name][field_count][field-names][child-indices].
(fn unmarshal-record ((cgrf s32) (payload s32)) value
  (let (name (string-from-addr payload))
    (let (p1 (i32.add payload (i32.add (i32.const 4) (i32.load payload))))
      (let (fcount (i32.load p1))
        (let (names-start (i32.add p1 (i32.const 4)))
          (let (names (read-field-names names-start fcount 0 (list-new value)))
            (let (values (unmarshal-children cgrf (skip-field-names names-start fcount 0) fcount 0 (list-new value)))
              (aggregate name (register-record name names values) name values))))))))

; ---------------------------------------------------------------------------
; Variants. A *sum* value reveals only its active case, so a single value can
; never reconstruct the whole nominal type; the faithful representation carries
; what the wire gives — (type-name, case-name, tag, payload) — inline, as the
; `open-variant` value. One encoder serves both: a foreign open-variant (tag
; inline) and a defined `aggregate` variant (tag = case index in its schema).

; Variant node (0x08): [name][case][tag:u32][count:u32][child-indices].
(fn enc-open-variant ((tn string) (cn string) (tag s32) (fields (list value))) s32
  (let (child-idxs (enc-children fields 0 (list-new s32)))
    (let (idx (global.get $enc-idx))
      (let (cur (global.get $enc-cur))
        (let (p (i32.add cur (i32.const 8)))
          (let (p2 (write-pstring (write-pstring p tn) cn))
            (begin
              (i32.store cur (i32.const 8))                                  ; kind=Variant 0x08
              (i32.store p2 tag)
              (i32.store (i32.add p2 (i32.const 4)) (list-len child-idxs))   ; count
              (let (p3 (write-idx-run (i32.add p2 (i32.const 8)) child-idxs 0))
                (begin
                  (i32.store (i32.add cur (i32.const 4)) (i32.sub p3 p))     ; payload_len
                  (global.set $enc-cur p3)
                  (global.set $enc-idx (i32.add idx (i32.const 1)))
                  idx)))))))))

; Tag of a case within a defined variant's schema (its declared case index).
(fn variant-tag ((cases (list value)) (cn string) (i s32)) s32
  (if (i32.ge_s i (list-len cases)) (i32.const 0)
    (if (string=? (symbol-name (list-get (items-of (list-get cases i)) 0)) cn) i
      (variant-tag cases cn (i32.add i (i32.const 1))))))

(fn unmarshal-variant ((cgrf s32) (payload s32)) value
  (let (tn (string-from-addr payload))
    (let (p1 (i32.add payload (i32.add (i32.const 4) (i32.load payload))))
      (let (cn (string-from-addr p1))
        (let (p2 (i32.add p1 (i32.add (i32.const 4) (i32.load p1))))
          (open-variant tn cn (i32.load p2)
            (unmarshal-children cgrf (i32.add p2 (i32.const 8)) (i32.load (i32.add p2 (i32.const 4))) 0 (list-new value))))))))
