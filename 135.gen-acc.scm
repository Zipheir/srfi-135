;;;; SRFI 158 generators and accumulator for texts.

;; text-ref is O(1), so this always converts strings to texts rather
;; than dispatching to string->generator.
(: textual->generator (textual #!optional fixnum fixnum -> procedure))
(define (textual->generator t . args)
  (assert-type 'textual->generator (textual? t))
  (let* ((txt (textual->text t))
         (len (text-length txt)))
    (let-optionals args ((start 0) (end len))
      (assert-type 'textual->generator (fixnum? start))
      (assert-type 'textual->generator (fixnum? end))
      (%check-range 'textual->generator t start end)
      (lambda ()
        (if (= start end)
            #!eof
            (let ((c (text-ref txt start)))
              (set! start (+ start 1))
              c))))))

(: generator->text (procedure #!optional fixnum -> text))
(define generator->text
  (case-lambda
    ((g)
     (assert-type 'generator->text (procedure? g))
     (text-unfold eof-object?
                  values
                  (lambda (_) (g))
                  (g)))
    ((g k)
     (assert-type 'generator->text (procedure? g))
     (assert-type 'generator->text (natural-fixnum? k))
     (text-unfold (lambda (p)
                    (or (eof-object? (car p)) (zero? (cdr p))))
                  car
                  (lambda (p) (cons (g) (- (cdr p) 1)))
                  (cons (g) k)))))

(: text-accumulator (-> procedure))
(define (text-accumulator)
  (let ((input '()))
    (lambda (x)
      (cond ((eof-object? x) (reverse-list->text input))
            ((char? x) (set! input (cons x input)))
            (else (error 'text-accumulator "illegal value" x))))))
