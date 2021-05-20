(define-macro (s-cons a b) `(cons ,a (delay ,b)))
(define (s-car s) (car s))
(define (s-cdr s) (force (cdr s)))
(define s-nil '())
(define s-null? null?)

(define (s-map f s)
  (if (s-null? s)
      s-nil
      (s-cons (f (s-car s)) (s-map f (s-cdr s)))))

(define (s-filter pred s)
  (cond ((s-null? s) s-nil)
        ((pred (s-car s)) (s-cons (s-car s) (s-filter pred (s-cdr s))))
        (else (s-filter pred (s-cdr s)))))

(define (s-zip-with f s1 s2)
  (if (or (s-null? s1) (s-null? s2))
      '()
      (s-cons (f (s-car s1) (s-car s2)) (s-zip-with f (s-cdr s1) (s-cdr s2)))))

(define fib*
  (s-cons 1
    (s-cons 1
      (s-zip-with + fib* (s-cdr fib*)))))
