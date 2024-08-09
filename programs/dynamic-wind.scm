(define cont1 #f)
(define cont2 #f)
(dynamic-wind
  (lambda () (display "before0\n"))
  (lambda ()
    (dynamic-wind
      (lambda () (display "before1\n"))
      (lambda () ;; thunk1
        (display "enter thunk1\n")
        (call/cc (lambda (cc) (set! cont1 cc)))
        ;; label A
        (if cont2 (cont2)) ;; goto B in case the 2nd time
        (display "leave thunk1\n"))
      (lambda () (display "after1\n")))
    (dynamic-wind
      (lambda () (display "before2\n"))
      (lambda () ;; thunk2
        (display "enter thunk2\n")
        (call/cc (lambda (cc)
                   (set! cont2 cc)
                   (cont1))) ;; goto A
        ;; label B
        (display "leave thunk2\n"))
      (lambda () (display "after2\n"))))
  (lambda () (display "after0\n")))

; before0
; before1
; enter thunk1
; leave thunk1
; after1
; before2
; enter thunk2
; after2
; before1
; after1
; before2
; leave thunk2
; after2
; after0
