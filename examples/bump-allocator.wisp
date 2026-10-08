(export init-heap)
(export alloc)
(export write-i32)
(export read-i32)
(export test-allocator)

(fn init-heap () s32
  (let (heap-start (i32.const 16))
    (let (stored (i32.store (i32.const 0) heap-start))
      heap-start)))

(fn alloc ((size s32)) s32
  (let (ptr-addr (i32.const 0))
    (let (current-ptr (i32.load ptr-addr))
      (let (new-ptr (i32.add current-ptr size))
        (let (stored (i32.store ptr-addr new-ptr))
          current-ptr)))))

(fn write-i32 ((addr s32) (val s32)) s32
  (i32.store addr val))

(fn read-i32 ((addr s32)) s32
  (i32.load addr))

(fn test-allocator () s32
  (let (heap (init-heap))
    (let (ptr1 (alloc (i32.const 4)))
      (let (ptr2 (alloc (i32.const 4)))
        (let (ptr3 (alloc (i32.const 4)))
          (let (w1 (write-i32 ptr1 (i32.const 100)))
            (let (w2 (write-i32 ptr2 (i32.const 200)))
              (let (w3 (write-i32 ptr3 (i32.const 300)))
                (let (v1 (read-i32 ptr1))
                  (let (v2 (read-i32 ptr2))
                    (let (v3 (read-i32 ptr3))
                      (i32.add (i32.add v1 v2) v3))))))))))))
