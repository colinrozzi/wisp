(export store-bytes)
(export pack-unpack)

(fn store-bytes ((b1 s32) (b2 s32) (b3 s32) (b4 s32)) s32
  (let (addr (i32.const 0))
    (let (dummy1 (i32.store8 addr b1))
      (let (dummy2 (i32.store8 (i32.const 1) b2))
        (let (dummy3 (i32.store8 (i32.const 2) b3))
          (let (dummy4 (i32.store8 (i32.const 3) b4))
            (i32.load addr)))))))

(fn pack-unpack ((val s32)) s32
  (let (addr (i32.const 100))
    (let (dummy (i32.store addr val))
      (let (b0 (i32.load8_u addr))
        (let (b1 (i32.load8_u (i32.add addr (i32.const 1))))
          (let (b2 (i32.load8_u (i32.add addr (i32.const 2))))
            (let (b3 (i32.load8_u (i32.add addr (i32.const 3))))
              (i32.add (i32.add b0 b1) (i32.add b2 b3)))))))))
