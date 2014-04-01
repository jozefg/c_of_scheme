(define map
  (lambda (f l)
    (if (eq? l 'empty)
        'empty
        (cons (f (car l))
              (map f (cdr l))))))
            
(define inc
  (lambda (i)
    (+ 1 i)))

(define l
  (cons 1 (cons 2 (cons 3 'empty))))

(define _ (display (map inc l)))
