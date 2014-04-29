(define apply
  (lambda (f a)
    (f a)))

(define inc
  (lambda (i)
    (+ 1 i)))

(define _ (display (apply inc 2)))
