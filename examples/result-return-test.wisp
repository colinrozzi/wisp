; Test returning result types from exported functions

(fn get-ok () (result s32 s32)
  (ok s32 s32 42))

(fn get-err () (result s32 s32)
  (err s32 s32 -1))

(export get-ok)
(export get-err)
