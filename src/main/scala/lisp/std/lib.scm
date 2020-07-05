(define (curry func arg1)  (lambda (arg) (func arg1 arg)))
(define (compose f g)      (lambda (arg) (f (g arg))))