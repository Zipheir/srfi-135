;; Utility

(: complain (symbol #!rest -> noreturn))
(define (complain name . args)
  (apply error
         (string-append (symbol->string name) ": illegal arguments")
         args))

(: pair-or-null? (* -> boolean))
(define (pair-or-null? x)
  (or (null? x) (pair? x)))

(: natural-fixnum? (* -> boolean))
(define (natural-fixnum? x)
  (and (fixnum? x) (>= x 0)))
