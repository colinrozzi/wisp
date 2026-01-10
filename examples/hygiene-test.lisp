; Test case for macro hygiene
; The macro introduces a variable 'tmp', but it shouldn't capture the user's 'tmp'

(defmacro with-temp (body)
  `(let (tmp (i32.const 0))
     ,body))

; Test 1: User's 'tmp' should NOT be captured by macro's 'tmp'
; The user's tmp is 42, and the macro introduces tmp=0
; The body references user's tmp, should get 42, not 0
(export (fn test-no-capture () s32
  (let (tmp (i32.const 42))
    (with-temp tmp))))  ; Should return 42

; Test 2: Macro can still reference its own 'tmp'
(defmacro make-temp ()
  `(let (tmp (i32.const 100))
     tmp))

(export (fn test-self-ref () s32
  (make-temp)))  ; Should return 100

; Test 3: Nested macros with same variable name
(defmacro outer (x)
  `(let (tmp (i32.const 1))
     (inner ,x)))

(defmacro inner (y)
  `(let (tmp (i32.const 2))
     (i32.add tmp ,y)))

(export (fn test-nested () s32
  (let (tmp (i32.const 100))
    (outer tmp))))  ; Should return 2 + 100 = 102
