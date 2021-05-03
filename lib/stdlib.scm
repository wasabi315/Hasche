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
(define-macro (let . args)
  (define (let-expander binds body)
    (define vars (map car binds))
    (define inits (map cadr binds))
    `((lambda ,vars ,@body) ,@inits))

  (define (named-let-expander name binds body)
    (define vars (map car binds))
    (define inits (map cadr binds))
    (define proc `(lambda ,vars ,@body))
    `(letrec ((,name ,proc)) (,name ,@inits)))

  (if (symbol? (car args))
      (named-let-expander (car args) (cadr args) (cddr args))
      (let-expander (car args) (cdr args))))

(define-macro (let* binds . body)
  (if (null? binds)
      `(begin ,@body)
      (begin
        (define var (car (car binds)))
        (define init (cadr (car binds)))
        (define rest-binds (cdr binds))
        `(let ((,var ,init)) (let* ,rest-binds ,@body)))))

(define-macro (letrec binds . body)
  (define inits
    (map (lambda (bind) `(,(car bind) ())) binds))
  (define sets
    (map (lambda (bind) `(set! ,(car bind) ,(cadr bind))) binds))
  `(let ,inits ,@sets ,@body))

(define-macro (cond . rows)
  (if (null? rows)
      `(begin)
      (begin
        (define pred (caar rows))
        (define clauses (cdar rows))
        (define rest-rows (cdr rows))
        (if (eq? pred 'else)
            `(begin ,@clauses)
            `(if ,pred
                 (begin ,@clauses)
                 (cond ,@rest-rows))))))

(define-macro (begin . body) `((lambda () ,@body)))
(define-macro (when test . body) `(if ,test (begin ,@body)))
(define-macro (unless test . body) `(if (not ,test) (begin ,@body)))

(define-macro (do binds test&exprs . body)
  (define vars (map car binds))
  (define inits (map cadr binds))
  (define steps (map caddr binds))
  (define test (car test&exprs))
  (define exprs (cdr test&exprs))
  (define sym (gensym))
  `(letrec
    ((,sym
      (lambda ,vars
        (if ,test
            (begin ,@exprs)
            (begin ,@body (,sym ,@steps))))))
    (,sym ,@inits)))

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
