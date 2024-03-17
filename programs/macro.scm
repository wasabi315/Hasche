(define-macro (incr v)
  `(let ([x 1]) (set! ,v (+ ,v 1))))

(define x 0)

; expands to (let ([x 1]) (set! x (+ x 1)))
; x is shadowed!
(incr x)

(display x) (newline) ; displays 0

(define-macro (incr2 v)
  `(let ([,(gensym) 1]) (set! ,v (+ ,v 1))))

(incr2 x) ; x is not shadowed this time

(display x) (newline) ; displays 1
