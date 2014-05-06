(define or
  (lambda (a b)
    (if a 1 b)))

(define fib
  (lambda (n)
    (if (or (eq? n 0)
            (eq? n 1))
            n
            (+ (fib (- n 1))
               (fib (- n 2))))))
(define _
  (display (fib 20)))
