; Test list parameter decoding

; Get the length of a list
(fn get-list-len ((nums (list s32))) s32
  (list-len nums))

; Get the first element of a list (returns -1 if empty)
(fn list-first ((nums (list s32))) s32
  (if (i32.gt_s (list-len nums) 0)
      (list-get nums 0)
      -1))

; Get the second element of a list (returns -1 if not enough elements)
(fn list-second ((nums (list s32))) s32
  (if (i32.gt_s (list-len nums) 1)
      (list-get nums 1)
      -1))

; Sum first two elements (or partial if list is shorter)
(fn sum-first-two ((nums (list s32))) s32
  (let (len (list-len nums))
    (if (i32.ge_s len 2)
        (i32.add (list-get nums 0) (list-get nums 1))
        (if (i32.eq len 1)
            (list-get nums 0)
            0))))

(export get-list-len)
(export list-first)
(export list-second)
(export sum-first-two)
