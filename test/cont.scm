(define test
  (lambda ()
    (call/cc
     (lambda (c)
       (+ 1 (c 1))))))
(define _ (display (test)))
