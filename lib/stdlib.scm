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
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define (for-each f l)
  (if (null? l)
      ((lambda ()))
      (if (pair? l)
          ((lambda () (f (car l)) (for-each f (cdr l))))
          ((lambda () (f l) ((lambda ())))))))

(define (map f l . more)
  (if (null? more)
      (begin
        (define (map1 l)
          (if (null? l)
              '()
              (cons (f (car l)) (map1 (cdr l)))))
        (map1 l))
      (begin
        (define (map-more l more)
          (if (null? l)
              '()
              (cons (apply f (car l) (map car more))
                    (map-more (cdr l) (map cdr more)))))
        (map-more l more))))

; Input / Output
(define (newline) (display "\n"))

(define (load path)
  (define p (open-input-file path))
  (define es (read p))
  (close-input-port p)
  (for-each eval es))

; Basic Macros
(define-macro (let . clauses)
  (match clauses
    (((? symbol? name) ((var init) ...) body ...)
      `(letrec ((,name (lambda ,var ,@body))) (,name ,@init)))
    ((((var init) ...) body ...)
      `((lambda ,var ,@body) ,@init))
    (_
      (error "invalid let syntax"))))

(define-macro (let* . clauses)
  (match clauses
    ((() body ...)
      `(begin ,@body))
    ((((var init) bind ...) body ...)
      `(let ((,var ,init)) (let* ,bind ,@body)))))

(define-macro (letrec . clauses)
  (match clauses
    ((((var init) ...) body ...)
      `(let
        ,(map (lambda (v) `(,v ())) var)
        ,@(map (lambda (v i) `(set! ,v ,i)) var init)
        ,@body))))

(define-macro (cond . clauses)
  (match clauses
    (()
      (error "cond with no arms"))
    ((('else body ...))
      `(begin ,@body))
    (((pred body ...))
      `(if ,pred (begin ,@body)))
    ((('else _ ...) _ ...)
      (error "else is not allowed here"))
    (((pred body ...) rest ...)
      `(if ,pred (begin ,@body) (cond ,@rest)))))

(define-macro (begin . body) `((lambda () ,@body)))
(define-macro (when test . body) `(if ,test (begin ,@body)))
(define-macro (unless test . body) `(if (not ,test) (begin ,@body)))

(define-macro (do . clauses)
  (match clauses
    ((((var init step) ...) (test expr ...) command ...)
      (let ((sym (gensym)))
           `(letrec
              ((,sym
                (lambda ,var
                  (if ,test
                      (begin ,@expr)
                      (begin ,@command (,sym ,@step))))))
              (,sym ,@init))))))

(define-macro (and . l)
  (match l
    (() #t)
    ((test) test)
    ((test1 test2 ...)
      `(if ,test1 (and ,@test2) #f))))

(define-macro (or . l)
  (match l
    (() #f)
    ((test) test)
    ((test1 test2 ...)
      (let ((sym (gensym)))
           `(let ((,sym ,test1)) (if ,sym ,sym (or ,@test2)))))))

; Lazy
(define (make-promise done? proc)
  (list (cons done? proc)))

(define-macro (delay-force expr)
  `(make-promise #f (lambda () ,expr)))

(define-macro (delay expr)
  `(delay-force (make-promise #t ,expr)))

(define (force p)
  (if (promise-done? p)
      (promise-value p)
      (let ((p* ((promise-value p))))
        (unless (promise-done? p)
          (promise-update! p* p))
        (force p))))

(define (promise-done? x) (caar x))
(define (promise-value x) (cdar x))
(define (promise-update! new old)
  (set-car! (car old) (promise-done? new))
  (set-cdr! (car old) (promise-value new))
  (set-car! new (car old)))
