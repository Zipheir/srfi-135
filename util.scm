;; Utility
(define (complain name . args)
  (apply error
         (string-append (symbol->string name) ": illegal arguments")
         args))

(define (pair-or-null? x)
  (or (null? x) (pair? x)))

(define (natural-fixnum? x)
  (and (fixnum? x) (>= x 0)))
