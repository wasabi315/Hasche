; Basic functions
(define (not x) (if x #f #t))
(define (neq? x y) (not (eq? x y)))

; Lists, Pairs
(define (list . l) l)

(define (list? x)
    (if (null? x)
        #t
        (if (pair? x)
            (list? (cdr x))
            #f)))

(define (length xs)
    (if (null? xs)
        0
        (if (pair? xs)
            (+ 1 (length (cdr xs)))
            1)))

(define (append l m)
    (if (null? l)
        m
        (cons (car l) (append (cdr l) m))))

(define (last l)
    (if (null? (cdr l))
        (car l)
        (last (cdr l))))

(define (memq x l)
    (if (null? l)
        #f
        (if (eq? x (car l))
            l
            (memq x (cdr l)))))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
