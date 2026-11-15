; Vectors and hash maps (ticket 0121): mutable containers with reference
; semantics, as in Scheme. A container owns its lists outright and changes them
; in place with list-set!, list-push! and list-truncate!, which write whatever
; the list's count, so every holder of the container sees a change. A list
; never leaves a container uncopied (vector->list, map-keys, map->list, the
; copies, marshal reads only), and the constructors copy what they are given.
;
; A map keeps its entries in insertion order (keys, vals, hashes, by entry
; index) and finds them through `slots`, an open-addressing table of entry
; indices (-1 empty) whose size is a power of two. A removed entry keeps its
; place with hash -1, so its slot never matches; `table-rehash` compacts the
; entries and rebuilds the slots when entries reach 3/4 of the slots, or when
; removed entries outnumber live ones. `counts` holds (live removed).

(fn container-builtin? ((name string)) s32
  (if (string-prefix? name "vector")
    (i32.or (string=? name "vector")
      (i32.or (i32.or (string=? name "vector-ref") (string=? name "vector-set!"))
        (i32.or (i32.or (string=? name "vector-push!") (string=? name "vector-pop!"))
          (i32.or (i32.or (string=? name "vector-length") (string=? name "vector->list"))
            (string=? name "vector-copy")))))
    (if (string-prefix? name "map")
      (i32.or (i32.or (string=? name "map-get") (string=? name "map-has?"))
        (i32.or (i32.or (string=? name "map-set!") (string=? name "map-delete!"))
          (i32.or (i32.or (string=? name "map-count") (string=? name "map-keys"))
            (i32.or (i32.or (string=? name "map-values") (string=? name "map->list"))
              (string=? name "map-copy")))))
      (i32.or (i32.or (string=? name "make-vector") (string=? name "list->vector"))
        (i32.or (string=? name "hash-map") (string=? name "equal?"))))))

(fn copy-values ((items (list value)) (i s32) (out (list value))) (list value)
  (if (i32.ge_s i (list-len items)) out
    (copy-values items (i32.add i 1) (list-push out (list-get items i)))))

; ---------------------------------------------------------------------------
; Hashing and equality

; FNV-1a steps over s32 words, then murmur3's finalizer, kept non-negative so
; -1 can mean "not a key".
(fn mix-hash ((h s32) (x s32)) s32 (i32.mul (i32.xor h x) 16777619))
(fn finish-hash ((h s32)) s32
  (let (a (i32.mul (i32.xor h (i32.shr_u h 16)) -2048144789))
    (let (b (i32.mul (i32.xor a (i32.shr_u a 13)) -1028477387))
      (i32.and (i32.xor b (i32.shr_u b 16)) 2147483647))))
(fn hash-start ((tag s32)) s32 (mix-hash -2128831035 tag))
(fn hash-bytes ((s string) (i s32) (h s32)) s32
  (if (i32.ge_s i (string-len s)) (mix-hash h (string-len s))
    (hash-bytes s (i32.add i 1) (mix-hash h (string-ref s i)))))
(fn hash-wide ((tag s32) (n s64)) s32
  (finish-hash (mix-hash (mix-hash (hash-start tag) (i32.wrap_i64 n))
    (i32.wrap_i64 (i64.shr_u n 32s64)))))
(fn hash-items ((items (list value)) (i s32) (h s32) (depth s32)) s32
  (if (i32.ge_s i (list-len items)) (finish-hash (mix-hash h (list-len items)))
    (let (item (key-hash-at (list-get items i) depth))
      (if (i32.lt_s item 0) -1
        (hash-items items (i32.add i 1) (mix-hash h item) depth)))))

; The hash of a map key, or -1 when V cannot be one: only integers, strings,
; symbols, bytes, booleans, and lists, tuples, options, records and variants
; of these are keys. Floats, functions, vectors and maps are not.
(fn key-hash ((v value)) s32 (key-hash-at v 0))
(fn key-hash-at ((v value) (depth s32)) s32
  (if (i32.ge_s depth 64) -1
    (let (d (i32.add depth 1))
      (value-case v
        ((integer n) (finish-hash (mix-hash (hash-start 1) n)))
        ((byte-value n) (finish-hash (mix-hash (hash-start 2) n)))
        ((wide-integer n) (hash-wide 3 n))
        ((u64-value n) (hash-wide 4 n))
        ((u32-value n) (finish-hash (mix-hash (hash-start 5) n)))
        ((u16-value n) (finish-hash (mix-hash (hash-start 6) n)))
        ((text s) (finish-hash (hash-bytes s 0 (hash-start 7))))
        ((symbol s) (finish-hash (hash-bytes s 0 (hash-start 8))))
        ((identifier s key) (finish-hash (hash-bytes s 0 (hash-start 8))))
        ((boolean b) (finish-hash (mix-hash (hash-start 9) b)))
        ((sequence items) (hash-items items 0 (hash-start 10) d))
        ((typed-list element items) (hash-items items 0 (hash-start 11) d))
        ((compound ty case-name fields) (hash-items fields 0 (hash-bytes case-name 0 (hash-start 12)) d))
        ((aggregate name id case-name fields)
          (hash-items fields 0 (hash-bytes case-name 0 (mix-hash (hash-start 13) id)) d))
        ((open-variant tn cn tag fields)
          (hash-items fields 0 (hash-bytes cn 0 (hash-bytes tn 0 (hash-start 14))) d))
        (else -1)))))

; (equal? a b): 1 when A and B have the same structure, else 0. Numbers must
; have the same type. Vectors compare item by item; maps as sets of entries,
; in any order. Functions are never equal. 2 means the walk went deeper than
; 4096 levels (a container that holds itself).
(fn same-name? ((a value) (b value)) s32
  (value-case b
    ((symbol s) (string=? s (symbol-name a)))
    ((identifier s key) (string=? s (symbol-name a)))
    (else 0)))
(fn items-equal ((a (list value)) (b (list value)) (i s32) (depth s32)) s32
  (if (i32.ne (list-len a) (list-len b)) 0
    (if (i32.ge_s i (list-len a)) 1
      (let (r (values-equal (list-get a i) (list-get b i) depth))
        (if (i32.ne r 1) r (items-equal a b (i32.add i 1) depth))))))
(fn values-equal ((a value) (b value) (depth s32)) s32
  (if (i32.ge_s depth 4096) 2
    (let (d (i32.add depth 1))
      (value-case a
        ((integer n) (value-case b ((integer m) (i32.eq n m)) (else 0)))
        ((byte-value n) (value-case b ((byte-value m) (i32.eq n m)) (else 0)))
        ((boolean n) (value-case b ((boolean m) (i32.eq n m)) (else 0)))
        ((u32-value n) (value-case b ((u32-value m) (i32.eq n m)) (else 0)))
        ((u16-value n) (value-case b ((u16-value m) (i32.eq n m)) (else 0)))
        ((wide-integer n) (value-case b ((wide-integer m) (i64.eq n m)) (else 0)))
        ((u64-value n) (value-case b ((u64-value m) (i64.eq n m)) (else 0)))
        ((single n) (value-case b ((single m) (f32.eq n m)) (else 0)))
        ((double n) (value-case b ((double m) (f64.eq n m)) (else 0)))
        ((text s) (value-case b ((text t) (string=? s t)) (else 0)))
        ((symbol s) (same-name? a b))
        ((identifier s key) (same-name? a b))
        ((sequence items) (value-case b ((sequence other) (items-equal items other 0 d)) (else 0)))
        ((typed-list element items)
          (value-case b
            ((typed-list other-element other)
              (if (same-type? element other-element) (items-equal items other 0 d) 0))
            (else 0)))
        ((compound ty case-name fields)
          (value-case b
            ((compound other-ty other-case other)
              (if (string=? case-name other-case) (items-equal fields other 0 d) 0))
            (else 0)))
        ((aggregate name id case-name fields)
          (value-case b
            ((aggregate other-name other-id other-case other)
              (if (i32.and (i32.eq id other-id) (string=? case-name other-case))
                (items-equal fields other 0 d) 0))
            (else 0)))
        ((open-variant tn cn tag fields)
          (value-case b
            ((open-variant other-tn other-cn other-tag other)
              (if (i32.and (string=? tn other-tn) (string=? cn other-cn))
                (items-equal fields other 0 d) 0))
            (else 0)))
        ((vector-value items) (value-case b ((vector-value other) (items-equal items other 0 d)) (else 0)))
        ((map-value table) (value-case b ((map-value other) (tables-equal table other d)) (else 0)))
        (else 0)))))

; ---------------------------------------------------------------------------
; Hash tables

(fn table-live ((t hash-table)) s32 (list-get (hash-table.counts t) 0))
(fn table-removed ((t hash-table)) s32 (list-get (hash-table.counts t) 1))

(fn empty-slots ((slots (list s32)) (n s32)) (list s32)
  (if (i32.ge_s (list-len slots) n) slots (empty-slots (list-push slots -1) n)))
(fn new-table () hash-table
  (hash-table (list-new value) (list-new value) (list-new s32)
    (empty-slots (list-new s32) 8) (list-push (list-push (list-new s32) 0) 0)))

; The entry index of key K (hash H), or -1.
(fn table-find ((t hash-table) (k value) (h s32)) s32
  (let (slots (hash-table.slots t))
    (table-probe t k h slots (i32.sub (list-len slots) 1) (i32.and h (i32.sub (list-len slots) 1)))))
(fn table-probe ((t hash-table) (k value) (h s32) (slots (list s32)) (mask s32) (i s32)) s32
  (let (s (list-get slots i))
    (if (i32.lt_s s 0) -1
      (if (if (i32.eq (list-get (hash-table.hashes t) s) h)
            (i32.eq (values-equal (list-get (hash-table.keys t) s) k 0) 1) 0)
        s
        (table-probe t k h slots mask (i32.and (i32.add i 1) mask))))))

; Point the first empty slot on H's probe sequence at entry E.
(fn slot-place ((slots (list s32)) (mask s32) (i s32) (e s32)) s32
  (if (i32.lt_s (list-get slots i) 0) (list-set! slots i e)
    (slot-place slots mask (i32.and (i32.add i 1) mask) e)))

; Move the live entries to the front, in order, and cut the rest.
(fn table-compact ((t hash-table) (i s32) (j s32)) s32
  (if (i32.ge_s i (list-len (hash-table.keys t)))
    (begin
      (list-truncate! (hash-table.keys t) j)
      (list-truncate! (hash-table.vals t) j)
      (list-truncate! (hash-table.hashes t) j)
      (list-set! (hash-table.counts t) 1 0))
    (let (h (list-get (hash-table.hashes t) i))
      (if (i32.lt_s h 0) (table-compact t (i32.add i 1) j)
        (begin
          (if (i32.ne i j)
            (begin
              (list-set! (hash-table.keys t) j (list-get (hash-table.keys t) i))
              (list-set! (hash-table.vals t) j (list-get (hash-table.vals t) i))
              (list-set! (hash-table.hashes t) j h))
            0)
          (table-compact t (i32.add i 1) (i32.add j 1)))))))

(fn slots-size ((n s32) (cap s32)) s32
  (if (i32.lt_s cap (i32.mul (i32.add n 1) 2)) (slots-size n (i32.mul cap 2)) cap))
(fn clear-slots ((slots (list s32)) (i s32)) s32
  (if (i32.ge_s i (list-len slots)) 0
    (begin (list-set! slots i -1) (clear-slots slots (i32.add i 1)))))
(fn grow-slots ((slots (list s32)) (n s32)) s32
  (if (i32.ge_s (list-len slots) n) 0
    (begin (list-push! slots -1) (grow-slots slots n))))
(fn place-entries ((t hash-table) (slots (list s32)) (mask s32) (e s32)) s32
  (if (i32.ge_s e (list-len (hash-table.hashes t))) 0
    (begin
      (slot-place slots mask (i32.and (list-get (hash-table.hashes t) e) mask) e)
      (place-entries t slots mask (i32.add e 1)))))

; Compact the entries and rebuild the slots at twice the live count or more,
; so the next rehash is at least as many inserts away as there are entries.
(fn table-rehash ((t hash-table)) s32
  (let (slots (hash-table.slots t))
    (begin
      (table-compact t 0 0)
      (let (size (slots-size (table-live t) 8))
        (begin
          (list-truncate! slots size)
          (grow-slots slots size)
          (clear-slots slots 0)
          (place-entries t slots (i32.sub size 1) 0))))))

(fn table-set ((t hash-table) (k value) (v value)) value
  (let (h (key-hash k))
    (if (i32.lt_s h 0) (failure "map key must be an integer, string, symbol, byte, boolean, or a list, tuple or record of these")
      (let (e (table-find t k h))
        (if (i32.ge_s e 0) (begin (list-set! (hash-table.vals t) e v) v)
          (begin
            (if (i32.gt_s (i32.mul (i32.add (list-len (hash-table.keys t)) 1) 4)
                  (i32.mul (list-len (hash-table.slots t)) 3))
              (table-rehash t) 0)
            (list-push! (hash-table.keys t) k)
            (list-push! (hash-table.vals t) v)
            (let (e (i32.sub (list-push! (hash-table.hashes t) h) 1))
              (let (slots (hash-table.slots t))
                (begin
                  (slot-place slots (i32.sub (list-len slots) 1) (i32.and h (i32.sub (list-len slots) 1)) e)
                  (list-set! (hash-table.counts t) 0 (i32.add (table-live t) 1))
                  v)))))))))

; The entry index of K, or -1 (also for a value that cannot be a key).
(fn table-lookup ((t hash-table) (k value)) s32
  (let (h (key-hash k))
    (if (i32.lt_s h 0) -1 (table-find t k h))))

(fn table-delete ((t hash-table) (k value)) s32
  (let (e (table-lookup t k))
    (if (i32.lt_s e 0) 0
      (begin
        (list-set! (hash-table.hashes t) e -1)
        (list-set! (hash-table.keys t) e (nil))
        (list-set! (hash-table.vals t) e (nil))
        (list-set! (hash-table.counts t) 0 (i32.sub (table-live t) 1))
        (list-set! (hash-table.counts t) 1 (i32.add (table-removed t) 1))
        (if (i32.and (i32.gt_s (table-removed t) 16) (i32.gt_s (table-removed t) (table-live t)))
          (table-rehash t) 0)
        1))))

; The live entries as a fresh list: their keys (0), values (1), or (k v) pairs (2).
(fn table-items ((t hash-table) (what s32) (i s32) (out (list value))) (list value)
  (if (i32.ge_s i (list-len (hash-table.keys t))) out
    (table-items t what (i32.add i 1)
      (if (i32.lt_s (list-get (hash-table.hashes t) i) 0) out
        (list-push out
          (if (i32.eq what 0) (list-get (hash-table.keys t) i)
            (if (i32.eq what 1) (list-get (hash-table.vals t) i)
              (sequence (list-push (list-push (list-new value) (list-get (hash-table.keys t) i))
                (list-get (hash-table.vals t) i))))))))))

(fn table-copy ((t hash-table) (i s32) (out hash-table)) hash-table
  (if (i32.ge_s i (list-len (hash-table.keys t))) out
    (begin
      (if (i32.lt_s (list-get (hash-table.hashes t) i) 0) 0
        (begin (table-set out (list-get (hash-table.keys t) i) (list-get (hash-table.vals t) i)) 0))
      (table-copy t (i32.add i 1) out))))

(fn tables-equal ((a hash-table) (b hash-table) (depth s32)) s32
  (if (i32.ne (table-live a) (table-live b)) 0 (entries-in a b 0 depth)))
; Whether every live entry of A is in B with an equal value.
(fn entries-in ((a hash-table) (b hash-table) (i s32) (depth s32)) s32
  (if (i32.ge_s i (list-len (hash-table.keys a))) 1
    (if (i32.lt_s (list-get (hash-table.hashes a) i) 0) (entries-in a b (i32.add i 1) depth)
      (let (e (table-find b (list-get (hash-table.keys a) i) (list-get (hash-table.hashes a) i)))
        (if (i32.lt_s e 0) 0
          (let (r (values-equal (list-get (hash-table.vals a) i) (list-get (hash-table.vals b) e) depth))
            (if (i32.ne r 1) r (entries-in a b (i32.add i 1) depth))))))))

; ---------------------------------------------------------------------------
; The verbs

(fn some-value ((v value)) value
  (compound (unary-type "option" (inferred-type v)) "some" (list-push (list-new value) v)))
(fn none-value () value
  (compound (unary-type "option" (sequence (list-push (list-new value) (symbol "tuple")))) "none" (list-new value)))

(fn make-vector-items ((n s32) (fill value) (out (list value))) (list value)
  (if (i32.ge_s (list-len out) n) out (make-vector-items n fill (list-push out fill))))
(fn hash-map-pairs ((args (list value)) (i s32) (t hash-table)) value
  (if (i32.ge_s i (list-len args)) (map-value t)
    (let (r (table-set t (list-get args i) (list-get args (i32.add i 1))))
      (if (failed? r) r (hash-map-pairs args (i32.add i 2) t)))))

(fn vector-index ((items (list value)) (at value) (extra s32)) s32
  (value-case at
    ((integer i) (if (i32.and (i32.ge_s i 0) (i32.lt_s i (i32.add (list-len items) extra))) i -1))
    (else -1)))

(fn apply-vector ((name string) (items (list value)) (args (list value))) value
  (if (string=? name "vector-length")
    (if (i32.eq (list-len args) 1) (integer (list-len items)) (failure "vector-length expects (vector-length v)"))
  (if (string=? name "vector->list")
    (if (i32.eq (list-len args) 1) (sequence (copy-values items 0 (list-new value))) (failure "vector->list expects (vector->list v)"))
  (if (string=? name "vector-copy")
    (if (i32.eq (list-len args) 1) (vector-value (copy-values items 0 (list-new value))) (failure "vector-copy expects (vector-copy v)"))
  (if (string=? name "vector-push!")
    (if (i32.eq (list-len args) 2) (begin (list-push! items (list-get args 1)) (list-get args 1))
      (failure "vector-push! expects (vector-push! v x)"))
  (if (string=? name "vector-pop!")
    (if (i32.ne (list-len args) 1) (failure "vector-pop! expects (vector-pop! v)")
      (if (i32.eq (list-len items) 0) (failure "vector-pop! of an empty vector")
        (let (last (list-get items (i32.sub (list-len items) 1)))
          (begin (list-truncate! items (i32.sub (list-len items) 1)) last))))
  (if (string=? name "vector-ref")
    (if (i32.ne (list-len args) 2) (failure "vector-ref expects (vector-ref v i)")
      (let (i (vector-index items (list-get args 1) 0))
        (if (i32.lt_s i 0) (failure "vector index out of bounds or not an s32") (list-get items i))))
    ; vector-set!
    (if (i32.ne (list-len args) 3) (failure "vector-set! expects (vector-set! v i x)")
      (let (i (vector-index items (list-get args 1) 0))
        (if (i32.lt_s i 0) (failure "vector index out of bounds or not an s32")
          (begin (list-set! items i (list-get args 2)) (list-get args 2))))))))))))

(fn apply-map ((name string) (t hash-table) (args (list value))) value
  (let (n (list-len args))
    (if (string=? name "map-count")
      (if (i32.eq n 1) (integer (table-live t)) (failure "map-count expects (map-count m)"))
    (if (string=? name "map-keys")
      (if (i32.eq n 1) (sequence (table-items t 0 0 (list-new value))) (failure "map-keys expects (map-keys m)"))
    (if (string=? name "map-values")
      (if (i32.eq n 1) (sequence (table-items t 1 0 (list-new value))) (failure "map-values expects (map-values m)"))
    (if (string=? name "map->list")
      (if (i32.eq n 1) (sequence (table-items t 2 0 (list-new value))) (failure "map->list expects (map->list m)"))
    (if (string=? name "map-copy")
      (if (i32.eq n 1) (map-value (table-copy t 0 (new-table))) (failure "map-copy expects (map-copy m)"))
    (if (string=? name "map-set!")
      (if (i32.eq n 3) (table-set t (list-get args 1) (list-get args 2)) (failure "map-set! expects (map-set! m k v)"))
    (if (i32.ne n 2) (failure (string-append name " expects a map and a key"))
      (if (string=? name "map-delete!") (integer (table-delete t (list-get args 1)))
        (let (e (table-lookup t (list-get args 1)))
          (if (string=? name "map-has?") (integer (i32.ge_s e 0))
            ; map-get
            (if (i32.lt_s e 0) (none-value) (some-value (list-get (hash-table.vals t) e)))))))))))))))

(fn apply-container ((name string) (args (list value))) value
  (if (string=? name "vector") (vector-value (copy-values args 0 (list-new value)))
  (if (string=? name "hash-map")
    (if (i32.and (list-len args) 1) (failure "hash-map expects keys and values in pairs")
      (hash-map-pairs args 0 (new-table)))
  (if (string=? name "equal?")
    (if (i32.ne (list-len args) 2) (failure "equal? expects two values")
      (let (r (values-equal (list-get args 0) (list-get args 1) 0))
        (if (i32.eq r 2) (failure "equal?: nesting deeper than 4096 (a container that holds itself?)") (integer r))))
  (if (string=? name "make-vector")
    (if (i32.ne (list-len args) 2) (failure "make-vector expects (make-vector n fill)")
      (value-case (list-get args 0)
        ((integer n) (if (i32.lt_s n 0) (failure "make-vector expects a size of 0 or more")
          (vector-value (make-vector-items n (list-get args 1) (list-new value)))))
        (else (failure "make-vector expects an s32 size"))))
  (if (string=? name "list->vector")
    (if (i32.ne (list-len args) 1) (failure "list->vector expects (list->vector xs)")
      (value-case (list-get args 0)
        ((sequence items) (vector-value (copy-values items 0 (list-new value))))
        ((typed-list element items) (vector-value (copy-values items 0 (list-new value))))
        (else (failure "list->vector expects a list"))))
  (if (i32.eq (list-len args) 0) (failure (string-append name " expects a container first"))
    (if (string-prefix? name "vector")
      (value-case (list-get args 0)
        ((vector-value items) (apply-vector name items args))
        (else (failure (string-append name " expects a vector"))))
      (value-case (list-get args 0)
        ((map-value t) (apply-map name t args))
        (else (failure (string-append name " expects a map"))))))))))))

; The (k v) pairs of a map for the printer and marshal.
(fn map-pairs ((t hash-table)) value (sequence (table-items t 2 0 (list-new value))))
