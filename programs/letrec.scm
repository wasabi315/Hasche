(display
  (letrec
    ((e? (lambda (n) (if (= n 0) #t (o? (- n 1)))))
    (o?  (lambda (n) (if (= n 0) #f (e? (- n 1))))))
    (e? 11))) (newline)
(display (let ((x 1) (y 1)) (+ x y))) (newline)
(display (let* ((x 1) (y x)) (+ x y))) (newline)
(display (letrec ((x 1) (y 1)) (+ x y))) (newline)
