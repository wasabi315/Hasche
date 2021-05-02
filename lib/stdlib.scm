; Basic functions
(define (not x) (if x #f #t))
(define (neq? x y) (not (eq? x y)))

(define call-with-current-continuation call/cc)

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

(define (for-each f l)
  (if (null? l)
      ((lambda ()))
      (if (pair? l)
          ((lambda () (f (car l)) (for-each f (cdr l))))
          ((lambda () (f l) ((lambda ())))))))

(define (map f l)
  (if (null? l)
      ()
      (if (pair? l)
          (cons (f (car l)) (map f (cdr l)))
          (f l))))

; Input / Output
(define (newline) (display "\n"))

(define (load path)
  (define p (open-input-file path))
  (define es (read p))
  (close-input-port p)
  (for-each eval es))

; Basic Macros
(define-macro (let binds . body)
  (define vars (map car binds))
  (define inits (map cadr binds))
  `((lambda ,vars ,@body) ,@inits))
(define-macro (let* binds . body)
  (if (null? binds)
      `(begin ,@body)
      (begin
        (define var (car (car binds)))
        (define init (cadr (car binds)))
        `(let ((,var ,init)) (let* ,(cdr binds) ,@body)))))

(define-macro (cond . row)
  (if (null? row)
      `(begin)
      (if (eq? (caar row) 'else)
          `(begin ,@(cdar row))
          `(if ,(caar row)
               (begin ,@(cdar row))
               (cond ,@(cdr row))))))

(define-macro (begin . body) `((lambda () ,@body)))
(define-macro (when test . body) `(if ,test (begin ,@body)))
(define-macro (unless test . body) `(if (not ,test) (begin ,@body)))

(define-macro (and . l)
  (if (null? l)
      `#t
      `(if ,(car l)
           (and ,@(cdr l))
           #f)))
(define-macro (or . l)
  (if (null? l)
      `#f
      `(if ,(car l)
           #t
           (or ,@(cdr l)))))

; Lazy
(define-macro (delay x) `(lambda () ,x))
(define (force x) (x))
