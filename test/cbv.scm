(define toplevel 1)

(define mutator
  (lambda (a)
    (set! a 2)
    (display toplevel)
    (set! toplevel 3)
    (display a)))

(define _ (mutator toplevel))
 
