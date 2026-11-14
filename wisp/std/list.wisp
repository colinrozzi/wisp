; std/list — list helpers. Load with (require list "std/list.wisp").
;
; Every walk goes by index (list-get) and builds with list-push, in a tail call:
; car/cdr/cons copy the whole list, so walking with them is quadratic, and the
; evaluator caps non-tail nesting at 4096, so a naive recursive map fails on a very
; long list. They take (list ...) and (list-new T) lists alike and return untyped lists.
; length, list-len and list-get are builtins; nth is here for the familiar name.
(module list
  (provide map filter fold reverse append nth member sort)

  (define map-from (lambda (f xs i n out)
    (if (>= i n) out
      (map-from f xs (+ i 1) n (list-push out (f (list-get xs i)))))))
  ; (map f xs): a new list of (f x) for each x, in order.
  (define map (lambda (f xs) (map-from f xs 0 (length xs) (list))))

  (define filter-from (lambda (keep? xs i n out)
    (if (>= i n) out
      (filter-from keep? xs (+ i 1) n
        (if (keep? (list-get xs i)) (list-push out (list-get xs i)) out)))))
  ; (filter keep? xs): the x for which (keep? x) is true, in order.
  (define filter (lambda (keep? xs) (filter-from keep? xs 0 (length xs) (list))))

  (define fold-from (lambda (f acc xs i n)
    (if (>= i n) acc
      (fold-from f (f acc (list-get xs i)) xs (+ i 1) n))))
  ; (fold f init xs): left fold, (f (f init x0) x1) ...; f takes (acc x).
  (define fold (lambda (f init xs) (fold-from f init xs 0 (length xs))))

  (define reverse-from (lambda (xs i out)
    (if (< i 0) out
      (reverse-from xs (- i 1) (list-push out (list-get xs i))))))
  ; (reverse xs): the same elements, last first.
  (define reverse (lambda (xs) (reverse-from xs (- (length xs) 1) (list))))

  (define push-all (lambda (out xs i n)
    (if (>= i n) out
      (push-all (list-push out (list-get xs i)) xs (+ i 1) n))))
  ; (append a b): a's elements, then b's. Two lists; there are no varargs.
  (define append (lambda (a b)
    (push-all (push-all (list) a 0 (length a)) b 0 (length b))))

  ; (nth xs i): element i, counting from 0; an error if out of bounds.
  (define nth (lambda (xs i) (list-get xs i)))

  (define member-from (lambda (same? x xs i n)
    (if (>= i n) 0
      (if (same? x (list-get xs i)) 1
        (member-from same? x xs (+ i 1) n)))))
  ; (member x xs same?): 1 if (same? x y) for some y in xs, else 0. The equality is
  ; passed in because Wisp has no generic one: = takes numbers, string=? strings.
  (define member (lambda (x xs same?) (member-from same? x xs 0 (length xs))))

  ; Merge two sorted lists into `out`, taking from a on a tie so the sort is stable.
  ; The lengths are arguments because this loop is what the step budget pays for.
  (define merge (lambda (less? a na b nb i j out)
    (if (>= i na) (push-all out b j nb)
      (if (>= j nb) (push-all out a i na)
        (if (less? (list-get b j) (list-get a i))
          (merge less? a na b nb i (+ j 1) (list-push out (list-get b j)))
          (merge less? a na b nb (+ i 1) j (list-push out (list-get a i))))))))

  ; Merge sort of xs[lo, hi). Not a tail call, but it nests only log2(n) deep.
  (define sort-range (lambda (less? xs lo hi)
    (if (>= lo hi) (list)
      (if (= (+ lo 1) hi) (list (list-get xs lo))
        (let (mid (+ lo (/ (- hi lo) 2)))
          (merge less?
            (sort-range less? xs lo mid) (- mid lo)
            (sort-range less? xs mid hi) (- hi mid)
            0 0 (list)))))))
  ; (sort less? xs): a new list in order of (less? a b); stable.
  ; Limit: one top-level evaluation has 1,000,000 steps. With (lambda (a b) (< a b))
  ; on shuffled numbers, 1800 elements fit and 2000 do not (measured 2026-11-01);
  ; a dearer less? fits fewer. For more, raise the session's budget first:
  ; (step-limit 10000000) covers 10000. The other functions here handle 10000 at
  ; the default.
  (define sort (lambda (less? xs) (sort-range less? xs 0 (length xs)))))
