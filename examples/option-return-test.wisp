; Test returning option types from exported functions

(fn get-some () (option s32)
  (some s32 42))

(fn get-none () (option s32)
  (none s32))

(export get-some)
(export get-none)
