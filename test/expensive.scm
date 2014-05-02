(define sum
  (lambda (n)
    (if (eq? n 0)
        1
        (+ 1 (sum (- n 1))))))

(define _ (display (sum 2000)))
