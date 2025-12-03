(global $counter s32 mut 0)
(global $multiplier s32 const 10)

(export (fn get-counter () s32
  (global.get $counter)))

(export (fn increment () s32
  (let (current (global.get $counter))
    (let (next (i32.add current (i32.const 1)))
      (let (_ (global.set $counter next))
        next)))))

(export (fn scaled-counter () s32
  (i32.mul (global.get $counter) (global.get $multiplier))))

(export (fn reset () s32
  (let (_ (global.set $counter (i32.const 0)))
    (i32.const 0))))
