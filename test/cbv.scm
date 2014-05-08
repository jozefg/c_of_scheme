(define toplevel 1)

(define mutator
  (lambda (a)
    (set! a 2)
    (display a)
    (display toplevel)))
(define _ (mutator toplevel))
