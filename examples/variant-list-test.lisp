; Test variants containing lists

; A variant that can hold a list of numbers
(variant value
  (single s32)
  (multiple (list s32)))

; Check if a value is a list
(fn is-list ((v value)) s32
  (match v
    ((single n) (i32.const 0))
    ((multiple l) (i32.const 1))))

; Get length of list (0 if single)
(fn get-list-len ((v value)) s32
  (match v
    ((single n) (i32.const 0))
    ((multiple l) (list-len l))))

; Test 1: Single value is not a list
(export (fn test-single-not-list () s32
  (let (v (single (i32.const 42)))
    (is-list v))))  ; Expected: 0

; Test 2: Multiple is a list
(export (fn test-multiple-is-list () s32
  (let (l (list-new s32))
    (let (v (multiple l))
      (is-list v)))))  ; Expected: 1

; Test 3: Get length of empty list
(export (fn test-empty-list-len () s32
  (let (l (list-new s32))
    (let (v (multiple l))
      (get-list-len v)))))  ; Expected: 0

; Test 4: Get length of list with items
(export (fn test-list-with-items () s32
  (let (l (list-new s32))
    (let (l2 (list-push l (i32.const 1)))
      (let (l3 (list-push l2 (i32.const 2)))
        (let (l4 (list-push l3 (i32.const 3)))
          (let (v (multiple l4))
            (get-list-len v))))))))  ; Expected: 3
