(export store-and-load)
(export memory-ops)

(fn store-and-load ((val s32)) s32
  (let (addr (i32.const 0))
    (let (stored (i32.store addr val))
      (i32.load addr))))

(fn memory-ops () s32
  (let (initial-size (memory.size))
    (let (new-size (memory.grow (i32.const 1)))
      (let (final-size (memory.size))
        final-size))))
