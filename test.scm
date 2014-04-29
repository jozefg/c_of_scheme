(define end 'end)

(define map
  (lambda (f l)
    (if (eq? l end)
        end
        (cons (f l)
              (map f (cdr l))))))

(define _ (display (eq? 'end 'end)))
