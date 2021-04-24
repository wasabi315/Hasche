(display (+ 1 (call/cc (lambda (k) (+ 2 (k 3) 4))) 5))
(newline)
