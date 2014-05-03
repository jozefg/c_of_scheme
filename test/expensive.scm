(define sum
  (lambda (n s)
    (if (eq? n 0)
        (display s)
        (sum (- n 1)
             (+ s 1)))))

(define _ (sum 20000 0))
