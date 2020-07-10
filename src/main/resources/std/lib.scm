(define id (lambda (obj) obj))
(define compose (lambda (f g) (lambda (arg) (f (g arg)))))