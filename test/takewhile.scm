(define end 'end)

(define length
  (lambda (xs)
    (if (eq? xs end)
        0
        (+ 1 (length (cdr xs))))))

(define cons-later
  (lambda (c x)
    (lambda (r)
      (cons x (c r)))))

(define worker
  (lambda (c pred xs)
    (if (pred (car xs))
        (worker (cons-later c (car xs))
                pred
                (cdr xs))
        (c end))))


(define take-while
  (lambda (pred xs)
    (worker (lambda (x) x) pred xs)))

(define zero?
  (lambda (n)
    (eq? n 0)))

(define _
  (display (length (take-while zero?
                               (cons 0 (cons 0 (cons 0 (cons 0 (cons 1 end)))))))))
