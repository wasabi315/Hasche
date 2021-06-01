(define fail
  (lambda () (print "no solution")))
(define in-range
  (lambda (a b)
    (call/cc
      (lambda (cont)
        (enumerate a b cont)))))
(define enumerate
  (lambda (a b cont)
    (if (> a b)
      (fail)
      (let ((save fail))
        (set! fail
          (lambda ()
            (set! fail save)
            (enumerate (+ a 1) b cont)))
        (cont a)))))
(display (let ((x (in-range 2 9))
      (y (in-range 2 9))
      (z (in-range 2 9)))
  (if (= (* x x)
          (+ (* y y) (* z z)))
      (list x y z)
      (fail))))
