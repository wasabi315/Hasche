;; From https://en.wikipedia.org/wiki/Call-with-current-continuation

;; [LISTOF X] -> ( -> X u 'you-fell-off-the-end)
(define (generate-one-element-at-a-time lst)
  ;; Both internal functions are closures over lst

  ;; Internal variable/Function which passes the current element in a list
  ;; to its return argument (which is a continuation), or passes an end-of-list marker
  ;; if no more elements are left. On each step the function name is
  ;; rebound to a continuation which points back into the function body,
  ;; while return is rebound to whatever continuation the caller specifies.
  (define (control-state return)
    (for-each
     (lambda (element)
               (set! return (call-with-current-continuation
                              (lambda (resume-here)
                                ;; Grab the current continuation
                               (set! control-state resume-here)
                               (return element))))) ;; (return element) evaluates to next return
     lst)
    (return 'you-fell-off-the-end))

  ;; (-> X u 'you-fell-off-the-end)
  ;; This is the actual generator, producing one item from a-list at a time.
  (define (generator)
    (call-with-current-continuation control-state))

  ;; Return the generator
  generator)

(define generate-digit
  (generate-one-element-at-a-time (list 0 1 2 3 4)))

(display (generate-digit)) ;; 0
(newline)
(display (generate-digit)) ;; 1
(newline)
(display (generate-digit)) ;; 2
(newline)
(display (generate-digit)) ;; 3
(newline)
(display (generate-digit)) ;; 4
(newline)
(display (generate-digit)) ;; you-fell-off-the-end
(newline)
(display (generate-digit)) ;; you-fell-off-the-end
(newline)
