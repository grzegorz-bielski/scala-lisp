(define compose (lambda (f g) (lambda (arg) (f (g arg)))))