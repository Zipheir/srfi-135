;;;; Analogues of the familiar I/O procedures for texts.

;;;; Input

;; FIXME: This correctly handles only UNIX-style lines.  Per R7RS
;; read-line, it should also handle CRLF and CRCR...LF endings.
(: text-read-line (#!optional input-port -> (or eof text)))
(define (text-read-line . args)
  (let-optionals args ((port (current-input-port)))
    (assert-type 'text-read-line (input-port? port))
    (if (eof-object? (peek-char port))
        #!eof
        (text-unfold (lambda (x)
                       (or (eof-object? x) (char=? x #\newline)))
                     values
                     (lambda (_) (read-char port))
                     (read-char port)))))

(: read-text (fixnum #!optional input-port -> (or eof text)))
(define (read-text k . args)
  (assert-type 'read-text (natural-fixnum? k))
  (let-optionals args ((port (current-input-port)))
    (assert-type 'read-text (input-port? port))
    (if (eof-object? (peek-char port))  ; workaround utf8's read-string
        #!eof
        (string->text-1 (read-string k port)))))

;; Analogous to read-lines from (chicken io).
(: text-read-lines (#!optional input-port fixnum -> (list-of text)))
(define text-read-lines
  (case-lambda
    (() (text-read-lines (current-input-port)))
    ((port)
     (assert-type 'text-read-lines (input-port? port))
     (unfold eof-object?
             values
             (lambda (_) (text-read-line port))
             (text-read-line port)))
    ((port max)
     (assert-type 'text-read-lines (input-port? port))
     (assert-type 'text-read-lines (natural-fixnum? max))
     (unfold (lambda (p)
               (or (eof-object? (car p)) (zero? (cdr p))))
             car
             (lambda (p)
               (cons (text-read-line port) (- (cdr p) 1)))
             (cons (text-read-line port) max)))))

;;;; Output

(: write-textual
   (textual #!optional output-port fixnum fixnum -> undefined))
(define write-textual
  (case-lambda
    ((t) (write-textual t (current-output-port)))
    ((t port)
     (assert-type 'write-textual (output-port? port))
     (cond ((string? t) (write-string t (string-length t) port))
           ((text? t) (write-text t port))
           (else (type-exception 'write-textual "illegal argument" t))))
    ((t port start) (write-textual t port start (textual-length t)))
    ((t port start end)
     (assert-type 'write-textual (output-port? port))
     (assert-type 'write-textual (fixnum? start))
     (assert-type 'write-textual (fixnum? end))
     (%check-range 'write-textual t start end)
     (cond ((string? t)
            (write-string (substring/shared t start end) #f port))
           ((text? t) (write-text (subtext t start end) port))
           (else
            (type-exception 'write-textual "illegal argument" t))))))

;;;; Text(ual) ports.

;;; FIXME: A direct implementation is possible, but it's not just a
;;; matter of using text-ref; the latter produces Unicode codepoints
;;; and is thus incompatible with utf8's read-string and with other
;;; procedures which expect UTF-8.  TODO!

(: open-input-textual (textual -> input-port))
(define (open-input-textual t)
  (cond ((string? t) (open-input-string t))
        ((text? t) (open-input-string (textual->string t)))
        (else
         (type-exception 'open-input-textual "illegal argument" t))))

;;; FIXME: These are just wrappers around string ports.  (chicken port)
;;; doesn't provide the primitives to do a direct implementation, but
;;; I'm not sure that would be any more efficient, anyway.

(: open-output-text (-> output-port))
(define (open-output-text) (open-output-string))

(: get-output-text (output-port -> text))
(define (get-output-text port)
  (assert-type 'get-output-text (output-port? port))
  (string->text-1 (get-output-string port)))
