(define closure
  (lambda (a)
    (lambda ()
      (set! a (+ a 1))
      a)))

(define _
  ((lambda (mut)
     (display (mut))
     (display (mut))
     (display (mut)))
   (closure 1)))
     
    
