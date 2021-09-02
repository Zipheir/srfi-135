;;;; Analogues of the familiar I/O procedures for texts.

;;;; Input

;; FIXME: This correctly handles only UNIX-style lines.  Per R7RS
;; read-line, it should also handle CRLF and CRCR...LF endings.
(: text-read-line (#!optional input-port -> (or eof text)))
(define (text-read-line . args)
  (let-optionals args ((port (current-input-port)))
    (assert (input-port? port) 'text-read-line "illegal argument" port)
    (if (eof-object? (peek-char port))
        #!eof
        (text-unfold (lambda (x)
                       (or (eof-object? x) (char=? x #\newline)))
                     values
                     (lambda (_) (read-char port))
                     (read-char port)))))

(: read-text (integer #!optional input-port -> (or eof text)))
(define (read-text k . args)
  (assert (exact-natural? k) 'text-read-line "illegal argument" k)
  (let-optionals args ((port (current-input-port)))
    (assert (input-port? port) 'text-read-line "illegal argument" port)
    (let ((x (read-string k port)))
      (if (eof-object? x)
          #!eof
          (%string->text x)))))

;; Analogous to read-lines from (chicken io).
(: text-read-lines (#!optional input-port integer -> (list-of text)))
(define text-read-lines
  (case-lambda
    (() (text-read-lines (current-input-port)))
    ((port)
     (assert (input-port? port) 'text-read-line "illegal argument" port)
     (unfold eof-object?
             values
             (lambda (_) (text-read-line port))
             (text-read-line port)))
    ((port max)
     (assert (input-port? port) 'text-read-line "illegal argument" port)
     (assert (exact-natural? max)
       'text-read-line "illegal argument" max)
     (unfold (lambda (p)
               (or (eof-object? (car p)) (zero? (cdr p))))
             car
             (lambda (p)
               (cons (text-read-line port) (- (cdr p) 1)))
             (cons (text-read-line port) max)))))

;;;; Output

(: write-textual
   (textual #!optional output-port integer integer -> undefined))
(define write-textual
  (case-lambda
    ((t) (write-textual t (current-output-port)))
    ((t port)
     (assert (output-port? port) 'write-textual "illegal argument" port)
     (cond ((string? t) (write-string t (string-length t) port))
           ((text? t)
            (textual-for-each (lambda (c) (write-char c port)) t))
           (else (error 'write-textual "illegal argument" t))))
    ((t port start) (write-textual t port start (textual-length t)))
    ((t port start end)
     (assert (textual? t) 'write-textual "illegal argument" t)
     (assert (output-port? port) 'write-textual "illegal argument" port)
     (assert (exact-integer? start)
       'write-textual "illegal argument" start)
     (assert (exact-integer? end) 'write-textual "illegal argument" end)
     (assert (<= 0 start end (textual-length t))
       'write-textual "start/end out of range" start end t)
     (if (string? t)
         (write-string (substring t start end) port)
         (textual-for-each (lambda (c) (write-char c port))
                           (subtext t start end))))))

;;;; Text(ual) ports.
#|
(define (%open-input-text t))

(: open-input-textual (textual -> input-port))
(define (open-input-textual t)
  (cond ((string? t) (open-input-string t))
        ((text? t) (%open-input-text t))
        (else (error 'open-input-textual "illegal argument" t))))
|#
