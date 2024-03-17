(define nested-data '((1 2 3) (4 5 6)))

(match nested-data
  [((m . _) (4 5 6))
    (begin (display m) (newline))]
  [_
    (display "Failed to match\n")])

(match nested-data
  [((_ (? (lambda (m) (= m 0))) _) . _)
    (display "m is 0\n")]
  [_
    (display "m is not 0\n")])

(match nested-data
  [((m n o) ...)
    (begin
      (display m) (newline)
      (display n) (newline)
      (display o) (newline))])

(define nested-data2 '(((1 2) (3 4)) ((5 6) (7 8) (9 10))))

(match nested-data2
  [(((m n) ...) ...)
    (begin
      (display m) (newline)
      (display n) (newline))])
