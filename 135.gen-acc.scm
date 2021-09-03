;;;; SRFI 158 generators and accumulator for texts.

;; text-ref is O(1), so this always converts strings to texts rather
;; than dispatching to string->generator.
(: textual->generator (textual #!optional integer integer -> procedure))
(define (textual->generator t . args)
  (let* ((txt (%textual->text t 'textual->generator t))
         (len (%text-length txt)))
    (let-optionals args ((start 0) (end len))
      (assert (exact-integer? start)
        'textual->generator "illegal argument" start)
      (assert (exact-integer? end)
        'textual->generator "illegal argument" end)
      (assert (<= 0 start end len)
        'textual->generator "start/end out of range" start end t)
      (lambda ()
        (if (= start end)
            #!eof
            (let ((c (%text-ref txt start)))
              (set! start (+ start 1))
              c))))))

(: generator->text (procedure #!optional integer -> text))
(define generator->text
  (case-lambda
    ((g)
     (assert (procedure? g) 'generator->text "illegal argument" g)
     (text-unfold eof-object?
                  values
                  (lambda (_) (g))
                  (g)))
    ((g k)
     (assert (procedure? g) 'generator->text "illegal argument" g)
     (assert (exact-natural? k) 'generator->text "illegal argument" k)
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
