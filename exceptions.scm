(define (type-exception loc msg . args)
  (abort
   (make-composite-condition
    (make-property-condition 'exn
     'location loc
     'message msg
     'arguments args)
    (make-property-condition 'type)
    (make-property-condition 'assertion))))

(define-syntax assert-type
  (syntax-rules ()
    ((assert-type loc expr)
     (unless expr
       (type-exception loc "type check failed" (list 'expr))))))

(define (arity-exception loc argl)
  (abort
   (make-composite-condition
    (make-property-condition 'exn
     'location loc
     'message "invalid number of arguments"
     'arguments argl)
    (make-property-condition 'arity)
    (make-property-condition 'assertion))))

(define (bounds-exception loc msg . args)
  (abort
   (make-composite-condition
    (make-property-condition 'exn
     'location loc
     'message msg
     'arguments args)
    (make-property-condition 'bounds)
    (make-property-condition 'assertion))))
