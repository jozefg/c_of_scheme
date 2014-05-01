(define end 'end)

(define map
  (lambda (f l)
    (if (eq? l end)
        end
        (cons (f (car l))
              (map f (cdr l))))))
(define inc
  (lambda (n)
    (+ 1 n)))

(define _ (display
           (car
            (map inc (cons 1 end)))))
