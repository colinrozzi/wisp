; std/memory.lisp — Bump allocator and memory utilities

(export
  (fn init-heap () s32
    (let (heap-start (i32.const 65536))
      (let (_ (i32.store (i32.const 0) heap-start))
        heap-start))))

(export
  (fn alloc ((size s32)) s32
    (let (current-ptr (i32.load (i32.const 0)))
      (let (new-ptr (i32.add current-ptr size))
        (let (_ (i32.store (i32.const 0) new-ptr))
          current-ptr)))))

(export
  (fn free ((ptr s32) (size s32)) s32
    ; Bump allocator - no-op free
    (i32.const 0)))

(export
  (fn write-i32 ((addr s32) (val s32)) s32
    (i32.store addr val)))

(export
  (fn read-i32 ((addr s32)) s32
    (i32.load addr)))

(export
  (fn write-i8 ((addr s32) (val s32)) s32
    (i32.store8 addr val)))

(export
  (fn read-i8 ((addr s32)) s32
    (i32.load8_u addr)))
