; Use small explicit Wasm constants: the self-hosted reader still stores s32.
(export (fn shift-mask () s32
  (i32.wrap_i64 (i64.shl (i64.const 1) (i64.const 65)))))
(export (fn rotate-left () s32
  (i32.wrap_i64 (i64.rotl (i64.const -1) (i64.const 1)))))
(export (fn rotate-right () s32
  (i32.wrap_i64 (i64.rotr (i64.const 2) (i64.const 1)))))
(export (fn signed-shift () s32
  (i32.wrap_i64 (i64.shr_s (i64.const -2) (i64.const 63)))))
(export (fn unsigned-shift () s32
  (i32.wrap_i64 (i64.shr_u (i64.const -2) (i64.const 63)))))
(export (fn signed-compare () s32 (i64.lt_s (i64.const -1) (i64.const 0))))
(export (fn unsigned-compare () s32 (i64.gt_u (i64.const -1) (i64.const 0))))
(export (fn bitwise () s32
  (i32.wrap_i64 (i64.xor (i64.or (i64.const 16) (i64.const 3)) (i64.and (i64.const 7) (i64.const 3))))))
