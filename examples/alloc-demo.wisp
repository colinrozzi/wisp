(export demo-alloc)
(export demo-write-read)

(fn init-heap () s32
  (i32.store (i32.const 0) (i32.const 16)))

(fn alloc ((size s32)) s32
  (let (ptr-addr (i32.const 0))
    (let (current-ptr (i32.load ptr-addr))
      (let (new-ptr (i32.add current-ptr size))
        (let (stored (i32.store ptr-addr new-ptr))
          current-ptr)))))

(fn demo-alloc () s32
  (let (dummy (init-heap))
    (let (p1 (alloc (i32.const 4)))
      (let (p2 (alloc (i32.const 4)))
        (let (p3 (alloc (i32.const 8)))
          (i32.add (i32.add p1 p2) p3))))))

(fn demo-write-read () s32
  (let (dummy (init-heap))
    (let (addr (alloc (i32.const 4)))
      (let (stored (i32.store addr (i32.const 42)))
        (i32.load addr)))))
