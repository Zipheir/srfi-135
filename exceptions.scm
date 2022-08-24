  (define-syntax assert-type
    (syntax-rules ()
      ((assert-type loc expr)
       (unless expr
         (abort
          (make-composite-condition
           (make-property-condition 'exn
            'location loc
            'message "type check failed"
            'arguments (list 'expr))
           (make-property-condition 'type)
           (make-property-condition 'assertion)))))))

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

  (define (%check-index loc t i)
    (unless (<= 0 i (%textual-length-no-checks t))
      (bounds-exception loc "index out of bounds" i t)))

  (define (%check-range loc t start end)
    (unless (<= 0 start end (%textual-length-no-checks t))
      (bounds-exception loc
                        "invalid range"
                        start
                        end
                        t)))

;;; Same things, but for bytevectors.

(define (%check-bv-index loc bv i)
  (unless (<= 0 i (bytevector-length bv))
    (bounds-exception loc "index out of bounds" i bv)))

(define (%check-bv-range loc bv start end)
  (unless (<= 0 start end (bytevector-length bv))
    (bounds-exception loc
                      "invalid range"
                      start
                      end
                      bv)))
