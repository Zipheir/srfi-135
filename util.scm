;; Utility
(define (complain name . args)
  (apply error
         (string-append (symbol->string name) ": illegal arguments")
         args))

(: list->bytevector ((list-of fixnum) -> bytevector))
(define (list->bytevector bytes)
  (let* ((n (length bytes))
         (bv (make-bytevector n)))
    (do ((i 0 (+ i 1))
         (bytes bytes (cdr bytes)))
        ((= i n))
      (bytevector-u8-set! bv i (car bytes)))
    bv))

(define (pair-or-null? x)
  (or (null? x) (pair? x)))

(define (exact-natural? x)
  (and (exact-integer? x) (>= x 0)))

(define (%textual-or-char? x)
  (or (string? x) (text? x) (char? x)))
