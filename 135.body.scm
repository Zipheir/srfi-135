;;; Copyright (C) William D Clinger (2016). All Rights Reserved.
;;; Copyright (C) Wolfgang Corcoran-Mathe (2022)
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Some macros to make textual arguments and optional arguments
;;; less painful.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax %textual->text
  (syntax-rules ()
    ((_ x name arg ...)  ; args are ignored
     (begin
      (assert-type name (textual? x))
      (if (string? x)
          (string->text x)
          x)))))

;;; Several procedures take a first argument that can be either
;;; a text or a string.  They can be written as though the first
;;; argument is always a text:
;;;
;;; (define-textual (f textual args ...) ...)

(define-syntax define-textual
  (syntax-rules ()
    ((_ (f textual arg . args) expr1 expr2 ...)
     (define (f textual arg . args)
       (let ((textual (%textual->text textual 'f textual arg)))
         expr1 expr2 ...)))))

;;; Several procedures take optional start and end arguments
;;; that follow a textual argument.  They can be written as
;;; though the textual argument is always a text, the start
;;; and end arguments are always provided, and the start and
;;; end arguments are always legal:
;;;
;;; (define-textual-start-end (f args ... textual start end)
;;;   ...)

(define-syntax define-textual-start-end
  (syntax-rules ()
    ((_ (f args ... textual start end) expr1 expr2 ...)
     (define f
       ;; Don't change this to letrec or an internal definition,
       ;; because recursive calls should call the version that checks.
       (let ((f
              (lambda (args ... textual start end) expr1 expr2 ...)))
         (case-lambda
          ((args ... textual)
           (let ((text (%textual->text textual 'f args ... textual)))
             (f args ... text 0 (%text-length text))))
          ((args ... textual start)
           (let* ((text (%textual->text textual 'f args ... textual start))
                  (n (%text-length text)))
             (assert-type 'f (exact-integer? start))
             (%check-index 'f text start)
             (f args ... text start n)))
          ((args ... textual start end)
           (let* ((text (%textual->text textual 'f args ... textual start end))
                  (n (%text-length text)))
             (assert-type 'f (exact-integer? start))
             (assert-type 'f (exact-integer? end))
             (%check-range 'f text start end)
             (f args ... text start end)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Predicates
;;;
;;; text? is defined by the kernel

(: textual? (* --> boolean))
(define (textual? x)
  (or (text? x)
      (string? x)))

(: textual-null? (textual --> boolean))
(define (textual-null? txt)
  (assert-type 'textual-null? (textual? txt))
  (= 0 (textual-length txt)))

(: textual-every ((char -> *) textual integer integer -> *))
(define-textual-start-end (textual-every pred textual start end)
  (assert-type 'textual-every (procedure? pred))
  (if (= start end)
      #t
      (let ((end-1 (- end 1)))
        (let loop ((i start))
          (if (= i end-1)
              (pred (%text-ref textual i))
              (and (pred (%text-ref textual i))
                   (loop (+ i 1))))))))

(: textual-any ((char -> *) textual integer integer -> *))
(define-textual-start-end (textual-any pred textual start end)
  (assert-type 'textual-any (procedure? pred))
  (let loop ((i start))
    (if (= i end)
        #f
        (or (pred (%text-ref textual i))
            (loop (+ i 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Constructors
;;;
;;; text-tabulate is defined by the kernel

(: make-text (integer char -> text))
(define (make-text n c)
  (assert-type 'make-text (exact-natural? n))
  (assert-type 'make-text (char? c))
  (text-tabulate (lambda (i) c) n))

(: text (#!rest char -> text))
(define (text . chars)
  (assert-type 'text (every char? chars))
  (string->text (list->string chars)))

;;; These next two procedures take care to accumulate texts of
;;; the kernel's preferred size, N.

(: text-unfold
   (procedure procedure procedure * #!optional (or char string text) procedure
     -> text))
(define text-unfold
  (case-lambda
   ((stop? mapper succ seed)
    (text-unfold stop? mapper succ seed (text) (lambda (x) (text))))
   ((stop? mapper succ seed base)
    (text-unfold stop? mapper succ seed base (lambda (x) (text))))
   ((stop? mapper succ seed base make-final)
  (assert-type 'text-unfold (procedure? stop?))
  (assert-type 'text-unfold (procedure? mapper))
  (assert-type 'text-unfold (procedure? succ))
    (assert-type 'text-unfold (procedure? make-final))
    (let* ((txt (%textual->text (if (char? base) (text base) base)
                                'text-unfold
                                stop? mapper succ seed base make-final))
           (k (%text-length txt)))
      (let loop ((k k)
                 (texts (list txt))
                 (chars '())
                 (seed seed))
        (cond ((>= k N)
               (let* ((k/N   (quotient k N))
                      (k     (- k (* k/N N)))
                      (texts (cons (reverse-list->text (list-tail chars k))
                                   texts))
                      (chars (take chars k)))
                 (loop k texts chars seed)))
              ((stop? seed)
               (let* ((texts (if (null? chars)
                                 texts
                                 (cons (reverse-list->text chars) texts)))
                      (final (make-final seed)))
                 (assert-type 'text-unfold (%textual-or-char? final))
                 (textual-concatenate-reverse texts final)))
              (else
               (let ((x (mapper seed)))
                 (assert-type 'text-unfold (%textual-or-char? x))
                 (cond ((char? x)
                        (loop (+ k 1)
                              texts
                              (cons x chars)
                              (succ seed)))
                       ((string? x)
                        (loop (+ k (string-length x))
                              texts
                              (append (reverse (string->list x)) chars)
                              (succ seed)))
                       ((text? x)
                        (loop (+ k (%text-length x))
                              texts
                              (append (reverse (textual->list x)) chars)
                              (succ seed))))))))))))

(: text-unfold-right
   (procedure procedure procedure * #!optional (or char string text) procedure
     -> text))
(define text-unfold-right
  (case-lambda
   ((stop? mapper succ seed)
    (text-unfold-right stop? mapper succ seed (text) (lambda (x) (text))))
   ((stop? mapper succ seed base)
    (text-unfold-right stop? mapper succ seed base (lambda (x) (text))))
   ((stop? mapper succ seed base make-final)
  (assert-type 'text-unfold-right (procedure? stop?))
  (assert-type 'text-unfold-right (procedure? mapper))
  (assert-type 'text-unfold-right (procedure? succ))
    (assert-type 'text-unfold-right (procedure? make-final))
    (let* ((txt (%textual->text (if (char? base) (text base) base)
                                'text-unfold-right
                                stop? mapper succ seed base make-final))
           (k (%text-length txt)))
      (let loop ((k k)
                 (texts (list txt))
                 (chars '())
                 (seed seed))
        (cond ((>= k N)
               (let* ((k/N   (quotient k N))
                      (k     (- k (* k/N N)))
                      (texts (cons (list->text (list-tail chars k)) texts))
                      (chars (take chars k)))
                 (loop k texts chars seed)))
              ((stop? seed)
               (let* ((texts (if (null? chars)
                                 texts
                                 (cons (list->text chars) texts)))
                      (final (make-final seed)))
                 (assert-type 'text-unfold (%textual-or-char? final))
                 (textual-concatenate (cons final texts))))
              (else
               (let ((x (mapper seed)))
                 (assert-type 'text-unfold (%textual-or-char? x))
                 (cond ((char? x)
                        (loop (+ k 1)
                              texts
                              (cons x chars)
                              (succ seed)))
                       ((string? x)
                        (loop (+ k (string-length x))
                              texts
                              (append (string->list x) chars)
                              (succ seed)))
                       ((text? x)
                        (loop (+ k (%text-length x))
                              texts
                              (append (textual->list x) chars)
                              (succ seed))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Conversion
;;;
;;; FIXME: a lot of these could be made more efficient, especially
;;; when a string is passed instead of a text.

(: textual->text (textual -> text))
(define (textual->text x)
  (assert-type 'textual->text (textual? x))
  (cond ((string? x)
         (string->text x))
        ((text? x)
         x)))

(: textual->string (textual #!optional integer integer -> string))
(define textual->string
  (case-lambda
   ((txt)
    (assert-type 'textual->string (textual? txt))
    (if (string? txt)
        txt
        (textual->string txt 0 (textual-length txt))))
   ((txt start)
    (assert-type 'textual-string (exact-integer? start))
    (%check-index 'textual-string txt start)
    (if (string? txt)
        (substring txt start (string-length txt))
        (textual->string txt start (textual-length txt))))
   ((txt start end)
  (assert-type 'textual-string (textual? txt))
    (assert-type 'textual-string (exact-integer? start))
    (assert-type 'textual-string (exact-integer? end))
    (%check-range 'textual-string txt start end)
    (let* ((txt (%textual->text txt 'textual->string txt start end))
           (n (- end start))
           (s (make-string n)))
      (do ((i start (+ i 1)))
          ((= i end)
           s)
        (string-set! s (- i start) (%text-ref txt i)))))))

;;; FIXME: Improve these two.

(: textual->vector (textual #!optional integer integer -> (vector-of char)))
(define-textual-start-end (textual->vector txt start end)
  (list->vector (string->list (textual->string (subtext txt start end)))))

(: textual->list (textual #!optional integer integer -> (list-of char)))
(define-textual-start-end (textual->list txt start end)
  (string->list (textual->string (subtext txt start end))))

(: string->text (string #!optional integer integer -> text))
(define string->text
  (case-lambda
   ((s)
  (assert-type 'string->text (string? s))
    (%string->text s))
   ((s start)
    (string->text s start (string-length s)))
   ((s start end)
  (assert-type 'string->text (string? s))
  (assert-type 'string->text (exact-integer? start))
  (assert-type 'string->text (exact-integer? end))
    (%check-range 'string->text s start end)
    (%string->text (substring s start end)))))

(: vector->text ((vector-of char) #!optional integer integer -> text))
(define (vector->text v . args)
  (let ((lenv (vector-length v)))
    (let-optionals args ((start 0) (end lenv))
  (assert-type 'vector->text (vector? v))
  (assert-type 'vector->text (exact-integer? start))
  (assert-type 'vector->text (exact-integer? end))
      (unless (<= 0 start end lenv)
        (bounds-exception 'vector->text "invalid range" start end vec))
      (text-tabulate (lambda (i) (vector-ref v (+ i start)))
                     (- end start)))))

(: list->text ((list-of char) #!optional integer integer -> text))
(define (list->text chars . start/end)
  (assert-type 'list->text (pair-or-null? chars))
  (apply string->text (list->string chars) start/end))

(: reverse-list->text ((list-of char) -> text))
(define (reverse-list->text chars)
  (assert-type 'list->text (pair-or-null? chars))
  (string->text (list->string (reverse chars))))

(: textual->utf8 (textual #!optional integer integer -> bytevector))
(define (textual->utf8 txt . args)
  (assert-type 'textual->utf8 (textual? txt))
  (let ((len (textual-length txt)))
    (let-optionals args ((start 0) (end len))
  (assert-type 'textual->utf8 (exact-integer? start))
  (assert-type 'textual->utf8 (exact-integer? end))
      (%check-range 'textual->utf8 txt start end)
      (if (string? txt)
          (string->utf8 txt start end)
          (string->utf8 (textual->string (subtext txt start end)))))))

(: textual->utf16 (textual #!optional integer integer -> bytevector))
(define-textual-start-end (textual->utf16 txt start end)
  (%textual->utf16 txt start end #f))

(: textual->utf16be (textual #!optional integer integer -> bytevector))
(define-textual-start-end (textual->utf16be txt start end)
  (%textual->utf16 txt start end 'big))

(: textual->utf8le (textual #!optional integer integer -> bytevector))
(define-textual-start-end (textual->utf16le txt start end)
  (%textual->utf16 txt start end 'little))

;;; FIXME: should this check for illegal code points?

(: %textual->utf8le (textual integer integer symbol -> bytevector))
(define (%textual->utf16 txt start end endianness)
  (let* ((n (textual-fold (lambda (c n)
                            (cond ((< (char->integer c) #x10000)
                                   (+ n 2))
                                  (else
                                   (+ n 4))))
                          0
                          txt start end))
         (n (if endianness n (+ n 2)))
         (result (make-bytevector n 0))
         (hibits (case endianness
                  ((big) 0)
                  ((little) 1)
                  (else 0)))
         (lobits (- 1 hibits)))
    (if (not endianness)
        (begin (bytevector-u8-set! result 0 #xfe)
               (bytevector-u8-set! result 1 #xff)))
    (let loop ((i start)
               (j (if endianness 0 2)))
      (if (= i end)
          result
          (let* ((c (text-ref txt i))
                 (cp (char->integer c)))
            (cond ((< cp #x10000)
                   (let* ((high (quotient cp 256))
                          (low  (- cp (* 256 high))))
                     (bytevector-u8-set! result (+ j hibits) high)
                     (bytevector-u8-set! result (+ j lobits) low))
                   (loop (+ i 1) (+ j 2)))
                  (else
                   (let* ((k (- cp #x10000))
                          (high-surrogate (+ #xd800 (quotient k 1024)))
                          (low-surrogate  (+ #xdc00 (remainder k 1024)))
                          (high0 (quotient high-surrogate 256))
                          (low0  (- high-surrogate (* 256 high0)))
                          (high1 (quotient low-surrogate 256))
                          (low1  (- low-surrogate  (* 256 high1))))
                     (bytevector-u8-set! result (+ j hibits) high0)
                     (bytevector-u8-set! result (+ j lobits) low0)
                     (bytevector-u8-set! result (+ j 2 hibits) high1)
                     (bytevector-u8-set! result (+ j 2 lobits) low1))
                   (loop (+ i 1) (+ j 4)))))))))

(: utf8->text (bytevector #!optional integer integer -> text))
(define utf8->text
  (case-lambda
   ((bv)
  (assert-type 'utf8->text (bytevector? bv))
    (string->text (utf8->string bv)))
   ((bv start)
  (assert-type 'utf8->text (bytevector? bv))
    (assert-type 'utf8->text (exact-integer? start))
    (%check-bv-index 'utf8->text bv start)
    (string->text (utf8->string bv start)))
   ((bv start end)
  (assert-type 'utf8->text (bytevector? bv))
    (assert-type 'utf8->text (exact-integer? start))
    (assert-type 'utf8->text (exact-integer? end))
    (%check-bv-range 'utf8->text bv start end)
    (string->text (utf8->string bv start end)))))

(: utf16->text (bytevector #!optional integer integer -> text))
(define (utf16->text bv . args)
  (assert-type 'utf16->text (bytevector? bv))
  (let ((len (bytevector-length bv)))
    (let-optionals args ((start 0) (end len))
  (assert-type 'utf16->text (exact-integer? start))
  (assert-type 'utf16->text (exact-integer? end))
      (%check-bv-range 'utf16->text bv start end)
      (%utf16->text bv start end #f))))

(: utf16be->text (bytevector #!optional integer integer -> text))
(define (utf16be->text bv . args)
  (assert-type 'utf16be->text (bytevector? bv))
  (let ((len (bytevector-length bv)))
    (let-optionals args ((start 0) (end len))
  (assert-type 'utf16be->text (exact-integer? start))
  (assert-type 'utf16be->text (exact-integer? end))
      (%check-bv-range 'utf16be->text bv start end)
      (%utf16->text bv start end 'big))))

(: utf16le->text (bytevector #!optional integer integer -> text))
(define (utf16le->text bv . args)
  (assert-type 'utf16le->text (bytevector? bv))
  (let ((len (bytevector-length bv)))
    (let-optionals args ((start 0) (end len))
  (assert-type 'utf16le->text (exact-integer? start))
  (assert-type 'utf16le->text (exact-integer? end))
      (%check-bv-range 'utf16le->text bv start end)
      (%utf16->text bv start end 'little))))

(: %utf16le->text (bytevector integer integer symbol -> text))
(define (%utf16->text bv start end endianness)
  (let* ((bom (and (not endianness)
                   (< start end)
                   (let ((byte0 (bytevector-u8-ref bv start))
                         (byte1 (bytevector-u8-ref bv (+ start 1))))
                     (cond ((and (= byte0 #xfe) (= byte1 #xff))
                            'big)
                           ((and (= byte1 #xfe) (= byte0 #xff))
                            'little)
                           (else #f)))))
         (start (if bom (+ start 2) start))
         (endianness (or endianness bom 'big))
         (hibits (if (eq? endianness 'big) 0 1))
         (lobits (- 1 hibits)))
    (text-unfold
     (lambda (i) (>= i end))
     (lambda (i)
       (let* ((high (bytevector-u8-ref bv (+ i hibits)))
              (low  (bytevector-u8-ref bv (+ i lobits)))
              (cp   (if (= high 0) low (+ (* 256 high) low))))
         (cond ((< cp #xd800)
                (integer->char cp))
               ((and (< cp #xdc00)
                     (< (+ i 2) end))
                (let* ((i (+ i 2))
                       (high (bytevector-u8-ref bv (+ i hibits)))
                       (low  (bytevector-u8-ref bv (+ i lobits)))
                       (cp2  (if (= high 0) low (+ (* 256 high) low))))
                  (cond ((<= #xdc00 cp2 #xdfff)
                         (integer->char
                          (+ #x10000
                             (* 1024 (- cp #xd800))
                             (- cp2 #xdc00))))
                        (else
                         (%illegal-utf16 bv (- i 2) cp cp2)))))
               ((< cp #x10000)
                (integer->char cp))
               (else
                (%illegal-utf16 bv i cp)))))
     (lambda (i)
       (let ((cp (+ (* 256 (bytevector-u8-ref bv (+ i hibits)))
                    (bytevector-u8-ref bv (+ i lobits)))))
         (if (or (< cp #xd800)
                 (<= #xe000 cp #xffff))
             (+ i 2)
             (+ i 4))))
     start)))

;; FIXME: This should raise a condition with a distinct kind.
(define (%illegal-utf16 bv i cp . rest)
  (if (null? rest)
      (error "illegal UTF-16: " bv i cp)
      (error "illegal UTF-16: " bv i cp (car rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Selection
;;;
;;; text-length, text-ref, and subtext are defined by the kernel

(: textual-length (textual --> integer))
(define (textual-length txt)
  (assert-type 'textual-length (textual? txt))
  (if (string? txt)
      (string-length txt)
      (%text-length txt)))

(: textual-ref (textual integer -> char))
(define (textual-ref txt i)
  (assert-type 'textual-ref (textual? txt))
  (assert-type 'textual-ref (exact-integer? i))
  (if (string? txt)
      (string-ref txt i)
      (%text-ref txt i)))

(: subtextual (textual integer integer -> text))
(define-textual (subtextual txt start end)
  (subtext txt start end))

;;; FIXME: could be faster, but this procedure shouldn't be used much

(: textual-copy (textual #!optional integer integer -> text))
(define-textual-start-end (textual-copy text start end)
  (string->text (textual->string text start end)))

(: textual-take (textual integer -> text))
(define-textual (textual-take txt nchars)
  (subtextual txt 0 nchars))

(: textual-drop (textual integer -> text))
(define-textual (textual-drop txt nchars)
  (subtextual txt nchars (%text-length txt)))

(: textual-take-right (textual integer -> text))
(define-textual (textual-take-right txt nchars)
  (let ((n (%text-length txt)))
    (subtextual txt (- n nchars) n)))

(: textual-drop-right (textual integer -> text))
(define-textual (textual-drop-right txt nchars)
  (let ((n (%text-length txt)))
    (subtextual txt 0 (- n nchars))))

(: textual-pad
   (textual integer #!optional char integer integer -> text))
(define (textual-pad t len . args)
  (let* ((txt (%textual->text t 'textual-pad t))
         (old-len (%text-length txt)))
    (let-optionals args ((c #\space) (start 0) (end old-len))
  (assert-type 'text-pad (exact-natural? len))
  (assert-type 'text-pad (char? c))
  (assert-type 'text-pad (exact-integer? start))
  (assert-type 'text-pad (exact-integer? end))
      (assert (<= 0 start end old-len)
        'text-pad "start/end out of range" start end txt)
      (%text-pad txt len c start end))))

(: %text-pad (text integer char integer integer -> text))
(define (%text-pad txt len c start end)
  (let* ((n (%text-length txt))
         (k (- end start)))
    (cond ((= n k len)
           txt)
          ((= k len)
           (if (= n k)
               txt
               (subtext txt start end)))
          ((< k len)
           (textual-append (make-text (- len k) c)
                           (if (= n k)
                               txt
                               (subtext txt start end))))
          (else
           (subtext txt (- end len) end)))))

(: textual-pad-right
   (textual integer #!optional char integer integer -> text))
(define (textual-pad-right t len . args)
  (let* ((txt (%textual->text t 'textual-pad-right t len))
         (old-len (%text-length txt)))
    (let-optionals args ((c #\space) (start 0) (end old-len))
  (assert-type 'text-pad-right (exact-natural? len))
  (assert-type 'text-pad-right (char? c))
      (assert (exact-integer? start)
        'text-pad-right "illegal argument" start)
  (assert-type 'text-pad-right (exact-integer? end))
      (assert (<= 0 start end old-len)
        'text-pad-right "start/end out of range" start end txt)
      (%text-pad-right txt len c start end))))

(: %text-pad-right (text integer char integer integer -> text))
(define (%text-pad-right txt len c start end)
  (let* ((n (%text-length txt))
         (k (- end start)))
    (cond ((= n k len)
           txt)
          ((= k len)
           (if (= n k)
               txt
               (subtext txt start end)))
          ((< k len)
           (textual-append (if (= n k)
                               txt
                               (subtext txt start end))
                           (make-text (- len k) c)))
          (else
           (subtext txt start (+ start len))))))

(: textual-trim
   (textual #!optional (char -> boolean) integer integer -> text))
(define textual-trim
  (case-lambda
   ((txt)
    (textual-trim txt char-whitespace? 0))
   ((txt pred)
    (textual-trim txt pred 0))
   ((txt pred start)
    (let ((txt (%textual->text txt 'textual-trim txt pred start)))
      (%text-trim txt pred start (%text-length txt))))
   ((txt pred start end)
    (let ((txt (%textual->text txt 'textual-trim txt pred start end)))
      (%text-trim txt pred start end)))))

(: %text-trim (text (char -> boolean) integer integer -> text))
(define (%text-trim txt pred start end)
  (assert-type 'text-trim (procedure? pred))
  (assert-type 'text-trim (exact-integer? start))
  (assert-type 'text-trim (exact-integer? end))
  (assert (<= 0 start end (%text-length txt))
    'text-trim "start/end out of range" start end txt)
  (let loop ((i start))
    (cond ((= i end)
           (text))
          ((pred (%text-ref txt i))
           (loop (+ i 1)))
          (else
           (subtext txt i end)))))

(: textual-trim-right
   (textual #!optional (char -> boolean) integer integer -> text))
(define textual-trim-right
  (case-lambda
   ((txt)
    (textual-trim-right txt char-whitespace? 0))
   ((txt pred)
    (textual-trim-right txt pred 0))
   ((txt pred start)
    (let ((txt (%textual->text txt 'textual-trim-right txt pred start)))
      (%text-trim-right txt pred start (%text-length txt))))
   ((txt pred start end)
    (let ((txt (%textual->text txt 'textual-trim-right txt pred start end)))
      (%text-trim-right txt pred start end)))))

(: %text-trim-right (text (char -> boolean) integer integer -> text))
(define (%text-trim-right txt pred start end)
  (assert-type 'text-trim-right (procedure? pred))
  (assert-type 'text-trim-right (exact-integer? start))
  (assert-type 'text-trim-right (exact-integer? end))
  (assert (<= 0 start end (%text-length txt))
    'text-trim-right "start/end out of range" start end txt)
  (let loop ((i (- end 1)))
    (cond ((< i start)
           (text))
          ((pred (%text-ref txt i))
           (loop (- i 1)))
          (else
           (subtext txt start (+ i 1))))))

(: textual-trim-both
   (textual #!optional (char -> boolean) integer integer -> text))
(define textual-trim-both
  (case-lambda
   ((txt)
    (textual-trim-both txt char-whitespace? 0))
   ((txt pred)
    (textual-trim-both txt pred 0))
   ((txt pred start)
    (let ((txt (%textual->text txt 'textual-trim-both txt pred start)))
      (%text-trim-both txt pred start (%text-length txt))))
   ((txt pred start end)
    (let ((txt (%textual->text txt 'textual-trim-both txt pred start end)))
      (%text-trim-both txt pred start end)))))

;;; This is efficient because subtext is fast.

(: %text-trim-both (text (char -> boolean) integer integer -> text))
(define (%text-trim-both txt pred start end)
  (assert-type 'text-trim-right (procedure? pred))
  (assert-type 'text-trim-right (exact-integer? start))
  (assert-type 'text-trim-right (exact-integer? end))
  (assert (<= 0 start end (%text-length txt))
    'text-trim-right "start/end out of range" start end txt)
  (textual-trim (textual-trim-right txt pred start end) pred))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Replacement

(: textual-replace
   (text text integer integer #!optional integer integer -> text))
(define textual-replace
  (case-lambda
   ((txt1 txt2 start1 end1 start2 end2)
    (textual-append (subtextual txt1 0 start1)
                    (subtextual txt2 start2 end2)
                    (subtextual txt1 end1 (textual-length txt1))))
   ((txt1 txt2 start1 end1 start2)
    (textual-append (subtextual txt1 0 start1)
                    (subtextual txt2 start2 (textual-length txt2))
                    (subtextual txt1 end1 (textual-length txt1))))
   ((txt1 txt2 start1 end1)
    (textual-append (subtextual txt1 0 start1)
                    txt2
                    (subtextual txt1 end1 (textual-length txt1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Comparison

(define (make-nary-comparison name binop0)
  (let ((binop (lambda (a b)
                 (let ((a (%textual->text a name a b))
                       (b (%textual->text b name a b)))
                   (binop0 a b)))))
    (letrec ((loop (lambda (first rest)
                     (cond ((null? rest)
                            #t)
                           ((binop first (car rest))
                            (loop (car rest) (cdr rest)))
                           (else
                            #f)))))
      (lambda (a b . rest)
        (if (null? rest)
            (binop a b)
            (and (binop a b)
                 (loop b rest)))))))

(: textual=? (textual textual #!rest textual -> boolean))
(define textual=?
  (make-nary-comparison 'textual=?
                        (lambda (a b)
                          (%text-compare a b =))))

(: textual<? (textual textual #!rest textual -> boolean))
(define textual<?
  (make-nary-comparison 'textual<?
                        (lambda (a b)
                          (%text-compare a b <))))

(: textual<=? (textual textual #!rest textual -> boolean))
(define textual<=?
  (make-nary-comparison 'textual<=?
                        (lambda (a b)
                          (%text-compare a b <=))))

(: textual>? (textual textual #!rest textual -> boolean))
(define textual>?
  (make-nary-comparison 'textual>?
                        (lambda (a b)
                          (%text-compare a b >))))

(: textual>=? (textual textual #!rest textual -> boolean))
(define textual>=?
  (make-nary-comparison 'textual>=?
                        (lambda (a b)
                          (%text-compare a b >=))))

(: textual-ci=? (textual textual #!rest textual -> boolean))
(define textual-ci=?
  (make-nary-comparison 'textual-ci=?
                        (lambda (a b)
                          (%text-compare-ci a b = string-ci=?))))

(: textual-ci<? (textual textual #!rest textual -> boolean))
(define textual-ci<?
  (make-nary-comparison 'textual-ci<?
                        (lambda (a b)
                          (%text-compare-ci a b < string-ci<?))))

(: textual-ci<=? (textual textual #!rest textual -> boolean))
(define textual-ci<=?
  (make-nary-comparison 'textual-ci<=?
                        (lambda (a b)
                          (%text-compare-ci a b <= string-ci<=?))))

(: textual-ci>? (textual textual #!rest textual -> boolean))
(define textual-ci>?
  (make-nary-comparison 'textual-ci>?
                        (lambda (a b)
                          (%text-compare-ci a b > string-ci>?))))

(: textual-ci>=? (textual textual #!rest textual -> boolean))
(define textual-ci>=?
  (make-nary-comparison 'textual-ci>=?
                        (lambda (a b)
                          (%text-compare-ci a b >= string-ci>=?))))

;;; Compares texts a and b.
;;; Determines whether a is less than b (-1), equal (0), or
;;; greater than b (+1), computes the boolean result by
;;; calling make-boolean on that numerical value and 0.

(: %text-compare (text text (integer integer -> boolean) -> boolean))
(define (%text-compare a b make-boolean)
  (let* ((na (%text-length a))
         (nb (%text-length b))
         (n (if (<= na nb) na nb)))
    (define (loop i)
      (if (= i n)
          (cond ((< na nb) (make-boolean -1 0))
                ((> na nb) (make-boolean +1 0))
                (else (make-boolean 0 0)))
          (let ((ca (%text-ref a i))
                (cb (%text-ref b i)))
            (cond ((char<? ca cb) (make-boolean -1 0))
                  ((char>? ca cb) (make-boolean +1 0))
                  (else (loop (+ i 1)))))))
    (loop 0)))

;;; Compares texts a and b, folding case.
;;; If either text contains non-ASCII characters, both are converted
;;; to strings and compared using string-pred.

(: %text-compare-ci
   (text text (integer integer -> boolean) (string string -> boolean)
     -> boolean))
(define (%text-compare-ci a b make-boolean string-pred)
  (let* ((na (%text-length a))
         (nb (%text-length b))
         (n (if (<= na nb) na nb)))
    (define (loop i)
      (if (= i n)
          (cond ((< na nb) (make-boolean -1 0))
                ((> na nb) (make-boolean +1 0))
                (else (make-boolean 0 0)))
          (let ((ca (%text-ref a i))
                (cb (%text-ref b i)))
            (if (or (char>? ca #\delete)
                    (char>? cb #\delete))
                (string-pred (textual->string a)
                             (textual->string b))
                (let ((ca (char-foldcase ca))
                      (cb (char-foldcase cb)))
                  (cond ((char<? ca cb) (make-boolean -1 0))
                        ((char>? ca cb) (make-boolean +1 0))
                        (else (loop (+ i 1)))))))))
    (loop 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Prefixes & suffixes

(define (%make-text-prefix/suffix-proc proc name)
  (lambda (t1 t2 . args)
    (let ((txt1 (%textual->text t1 name t1))
          (len1 (textual-length t1))
          (txt2 (%textual->text t2 name t2))
          (len2 (textual-length t2)))
      (let-optionals args ((start1 0)
                           (end1 len1)
                           (start2 0)
                           (end2 len2))
        (assert (exact-integer? start1) name "illegal argument" start1)
        (assert (exact-integer? end1) name "illegal argument" end1)
        (assert (<= 0 start1 end1 len1)
          name "start1/end1 out of range" start1 end1 txt1)
        (assert (exact-integer? start2) name "illegal argument" start2)
        (assert (exact-integer? end2) name "illegal argument" end2)
        (assert (<= 0 start2 end2 len2)
          name "start2/end2 out of range" start2 end2 txt2)
        (proc txt1 txt2 start1 end1 start2 end2)))))

(: textual-prefix-length
   (textual textual #!optional integer integer integer integer
     -> integer))
(define textual-prefix-length
  (%make-text-prefix/suffix-proc
   (lambda (txt1 txt2 start1 end1 start2 end2)
     (%text-prefix-length txt1 txt2 start1 end1 start2 end2))
   'textual-prefix-length))

(: textual-suffix-length
   (textual textual #!optional integer integer integer integer
     -> integer))
(define textual-suffix-length
  (%make-text-prefix/suffix-proc
   (lambda (txt1 txt2 start1 end1 start2 end2)
     (%text-suffix-length txt1 txt2 start1 end1 start2 end2))
   'textual-suffix-length))

(: textual-prefix?
   (textual textual #!optional integer integer integer integer
     -> boolean))
(define textual-prefix?
  (%make-text-prefix/suffix-proc
   (lambda (txt1 txt2 start1 end1 start2 end2)
     (%text-prefix? txt1 txt2 start1 end1 start2 end2))
   'textual-prefix?))

(: textual-suffix?
   (textual textual #!optional integer integer integer integer
     -> boolean))
(define textual-suffix?
  (%make-text-prefix/suffix-proc
   (lambda (txt1 txt2 start1 end1 start2 end2)
     (%text-suffix? txt1 txt2 start1 end1 start2 end2))
   'textual-suffix?))

;;; All error checking has already been done.

(: %text-prefix-length
   (text text integer integer integer integer -> integer))
(define (%text-prefix-length txt1 txt2 start1 end1 start2 end2)
  (let* ((k1   (- end1 start1))
         (k2   (- end2 start2))
         (k    (min k1 k2))
         (end1 (+ start1 k)))
    (let loop ((i start1)
               (j start2))
      (cond ((= i end1) k)
            ((char=? (%text-ref txt1 i) (%text-ref txt2 j))
             (loop (+ i 1) (+ j 1)))
            (else (- i start1))))))

(: %text-suffix-length
   (text text integer integer integer integer -> integer))
(define (%text-suffix-length txt1 txt2 start1 end1 start2 end2)
  (let* ((k1     (- end1 start1))
         (k2     (- end2 start2))
         (k      (min k1 k2))
         (start1 (- end1 k)))
    (let loop ((i (- end1 1))
               (j (- end2 1)))
      (cond ((< i start1) k)
            ((char=? (%text-ref txt1 i) (%text-ref txt2 j))
             (loop (- i 1) (- j 1)))
            (else (- end1 i 1))))))

(: %text-prefix?
   (text text integer integer integer integer -> boolean))
(define (%text-prefix? txt1 txt2 start1 end1 start2 end2)
  (let ((k1 (- end1 start1))
        (k2 (- end2 start2)))
    (and (<= k1 k2)
         (= k1 (%text-prefix-length txt1 txt2 start1 end1 start2 end2)))))

(: %text-suffix?
   (text text integer integer integer integer -> boolean))
(define (%text-suffix? txt1 txt2 start1 end1 start2 end2)
  (let ((k1 (- end1 start1))
        (k2 (- end2 start2)))
    (and (<= k1 k2)
         (= k1 (%text-suffix-length txt1 txt2 start1 end1 start2 end2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Searching

(: textual-index
   (textual (char -> boolean) #!optional integer integer
     -> (or integer false)))
(define (textual-index t pred . args)
  (let* ((txt (%textual->text t 'textual-index t))
         (len (%text-length txt)))
    (let-optionals args ((start 0) (end len))
  (assert-type 'textual-index (procedure? pred))
      (assert (exact-integer? start)
        'textual-index "illegal argument" start)
  (assert-type 'textual-index (exact-integer? end))
      (assert (<= 0 start end len)
        'textual-index "start/end out of range" start end txt)
      (let loop ((i start))
         (cond ((= i end) #f)
               ((pred (%text-ref txt i)) i)
               (else (loop (+ i 1))))))))

(: textual-index-right
   (textual (char -> boolean) #!optional integer integer
     -> (or integer false)))
(define (textual-index-right t pred . args)
  (let* ((txt (%textual->text t 'textual-index-right t))
         (len (%text-length txt)))
    (let-optionals args ((start 0) (end len))
      (assert (procedure? pred)
        'textual-index-right "illegal argument" pred)
      (assert (exact-integer? start)
        'textual-index-right "illegal argument" start)
      (assert (exact-integer? end)
        'textual-index-right "illegal argument" end)
      (assert (<= 0 start end len)
        'textual-index-right "start/end out of range" start end txt)
      (let loop ((i (- end 1)))
        (cond ((< i start) #f)
              ((pred (%text-ref txt i)) i)
              (else (loop (- i 1))))))))

(: textual-skip
   (textual (char -> boolean) #!optional integer integer
     -> (or integer false)))
(define (textual-skip txt pred . rest)
  (apply textual-index txt (lambda (x) (not (pred x))) rest))

(: textual-skip-right
   (textual (char -> boolean) #!optional integer integer
     -> (or integer false)))
(define (textual-skip-right txt pred . rest)
  (apply textual-index-right txt (lambda (x) (not (pred x))) rest))

(: textual-contains
   (textual textual #!optional integer integer integer integer
     -> (or integer false)))
(define (textual-contains t1 t2 . args)
  (let ((txt1 (%textual->text t1 'textual-contains t1 t2))
        (txt2 (%textual->text t2 'textual-contains t1 t2)))
    (let-optionals args ((start1 0)
                         (end1 (%text-length txt1))
                         (start2 0)
                         (end2 (%text-length txt2)))
      (assert (exact-integer? start1)
        'textual-contains "illegal argument" start1)
      (assert (exact-integer? end1)
        'textual-contains "illegal argument" end1)
      (assert (<= 0 start1 end1 (%text-length txt1))
        'textual-contains "start1/end1 out of range" start1 end1 txt1)
      (assert (exact-integer? start2)
        'textual-contains "illegal argument" start2)
      (assert (exact-integer? end2)
        'textual-contains "illegal argument" end2)
      (assert (<= 0 start2 end2 (%text-length txt2))
        'textual-contains "start2/end2 out of range" start2 end2 txt2)
      (%textual-contains txt1 txt2 start1 end1 start2 end2))))

;;; No checking needed here.
;;;
;;; Naive search works well when
;;;     txt1 is very short
;;;     txt2 is very short
;;;     txt2 is almost as long as txt1
;;; Boyer-Moore-Horspool search works well when
;;;     txt2 is very short
;;;     txt1 is considerably longer than txt2, txt2 is not too short,
;;;         and the rightmost character of txt2 is distinct from
;;;         (in its low 8 bits) from several characters that precede it
;;; Rabin-Karp works reasonably well all the time, so is used when
;;;     neither naive search nor Boyer-Moore-Horspool do well

(define %threshold:short1 10)   ; is txt1 shorter than this?
(define %threshold:short2 3)    ; is txt2 shorter than this?
(define %threshold:longer 1)    ; is txt1 at least this much longer?
(define %threshold:rightmost 2) ; are rightmost characters the same?

(: %textual-contains
   (textual textual integer integer integer integer
     -> (or integer false)))
(define (%textual-contains txt1 txt2 start1 end1 start2 end2)
  (let ((n1 (- end1 start1))
        (n2 (- end2 start2)))
    (cond ((< n1 %threshold:short1)
           (%textual-contains:naive txt1 txt2 start1 end1 start2 end2))
          ((< (- n1 n2) %threshold:longer)
           (%textual-contains:naive txt1 txt2 start1 end1 start2 end2))
          ((< n2 %threshold:short2)
           (%textual-contains:boyer-moore txt1 txt2 start1 end1 start2 end2))
          ((and (> n2 %threshold:rightmost)
                (let ((j (remainder (char->integer (text-ref txt2 (- end2 1)))
                                    128)))
                  (let loop ((i (- end2 %threshold:rightmost)))
                    (cond ((= i (- end2 1))
                           #t)
                          ((= j
                              (remainder (char->integer (text-ref txt2 i))
                                         128))
                           #f)
                          (else
                           (loop (+ i 1)))))))
           (%textual-contains:boyer-moore txt1 txt2 start1 end1 start2 end2))
          (else
           (%textual-contains:rabin-karp txt1 txt2 start1 end1 start2 end2)))))

(: %textual-contains:naive
   (textual textual integer integer integer integer
     -> (or integer false)))
(define (%textual-contains:naive txt1 txt2 start1 end1 start2 end2)
  (let* ((n1 (- end1 start1))
         (n2 (- end2 start2))
         (lim1 (- end1 n2)))
    (let loop ((i start1))
      (cond ((> i lim1)
             #f)
            ((textual-prefix? txt2 txt1 start2 end2 i end1)
             i)
            (else
             (loop (+ i 1)))))))

(: %textual-contains:rabin-karp
   (textual textual integer integer integer integer
     -> (or integer false)))
(define (%textual-contains:rabin-karp txt1 txt2 start1 end1 start2 end2)
  (define (hash txt start end)
    (do ((i start (+ i 1))
         (h 0 (+ h (char->integer (text-ref txt i)))))
        ((= i end)
         h)))
  (let* ((n1 (- end1 start1))
         (n2 (- end2 start2))
         (lim1 (- end1 n2))
         (h1 (hash txt1 start1 (min (+ start1 n2) end1)))
         (h2 (hash txt2 start2 end2)))
    (let loop ((i start1)
               (h1 h1))
      (cond ((> i lim1)
             #f)
            ((and (= h1 h2)
                  (textual-prefix? txt2 txt1 start2 end2 i end1))
             i)
            ((= i lim1)
             #f)
            (else
             (loop (+ i 1)
                   (+ (- h1 (char->integer (text-ref txt1 i)))
                      (char->integer (text-ref txt1 (+ i n2))))))))))

;;; This is actually the Boyer-Moore-Horspool algorithm,
;;; but the name is already pretty long.

(: %textual-contains:boyer-moore
   (textual textual integer integer integer integer
     -> (or integer false)))
(define (%textual-contains:boyer-moore txt1 txt2 start1 end1 start2 end2)
  (if (= start2 end2)
      start1
      (let* ((n1 (- end1 start1))
             (n2 (- end2 start2))
             (lim1 (- end1 n2))
             (lastchar (text-ref txt2 (- end2 1)))
             (lastj (remainder (char->integer lastchar) 128))
             (table (make-vector 128 n2)))
        (do ((i 0 (+ i 1)))
            ((>= i (- n2 1)))
          (let* ((c  (text-ref txt2 (+ i start2)))
                 (cp (char->integer c))
                 (j  (remainder cp 128)))
            (vector-set! table j (- n2 i 1))))
        (let loop ((i start1))
          (if (>= i lim1)
              (if (and (= i lim1)
                       (textual-prefix? txt2 txt1 start2 end2 i end1))
                  i
                  #f)
              (let* ((c  (text-ref txt1 (+ i n2 -1)))
                     (cp (char->integer c))
                     (j  (remainder cp 128)))
                (cond ((not (char=? c lastchar))
                       (loop (+ i (vector-ref table j))))
                      ((textual-prefix? txt2 txt1 start2 end2 i end1)
                       i)
                      (else
                       (loop (+ i (vector-ref table lastj)))))))))))

;;; FIXME: no Rabin-Karp algorithm for now

(: textual-contains-right
   (textual textual #!optional integer integer integer integer
     -> (or integer false)))
(define (textual-contains-right t1 t2 . rest0)
  (let* ((txt1 (%textual->text t1 'textual-contains-right t1 t2))
         (txt2 (%textual->text t2 'textual-contains-right t1 t2))
         (rest rest0)
         (start1 (if (null? rest) 0 (car rest)))
         (rest (if (null? rest) rest (cdr rest)))
         (end1 (if (null? rest) (%text-length txt1) (car rest)))
         (rest (if (null? rest) rest (cdr rest)))
         (start2 (if (null? rest) 0 (car rest)))
         (rest (if (null? rest) rest (cdr rest)))
         (end2 (if (null? rest) (%text-length txt2) (car rest)))
         (rest (if (null? rest) rest (cdr rest))))
    (if (and (null? rest)
             (exact-integer? start1)
             (exact-integer? end1)
             (exact-integer? start2)
             (exact-integer? end2)
             (<= 0 start1 end1 (%text-length txt1))
             (<= 0 start2 end2 (%text-length txt2)))
        (%textual-contains-right txt1 txt2 start1 end1 start2 end2)
        (apply complain 'textual-contains-right t1 t2 rest0))))

(: %textual-contains-right
   (textual textual integer integer integer integer
     -> (or integer false)))
(define (%textual-contains-right txt1 txt2 start1 end1 start2 end2)
  (let ((n1 (- end1 start1))
        (n2 (- end2 start2)))
    (cond ((< n1 %threshold:short1)
           (%textual-contains-right:naive
            txt1 txt2 start1 end1 start2 end2))
          ((< (- n1 n2) %threshold:longer)
           (%textual-contains-right:naive
            txt1 txt2 start1 end1 start2 end2))
          ((< n2 %threshold:short2)
           (%textual-contains-right:boyer-moore
            txt1 txt2 start1 end1 start2 end2))
          (else
           (%textual-contains-right:boyer-moore
            txt1 txt2 start1 end1 start2 end2)))))

(: %textual-contains-right:naive
   (textual textual integer integer integer integer
     -> (or integer false)))
(define (%textual-contains-right:naive txt1 txt2 start1 end1 start2 end2)
  (let* ((n1 (- end1 start1))
         (n2 (- end2 start2))
         (lim1 (- end1 n2)))
    (let loop ((i lim1))
      (cond ((< i start1)
             #f)
            ((textual-prefix? txt2 txt1 start2 end2 i end1)
             i)
            (else
             (loop (- i 1)))))))

;;; This is actually the Boyer-Moore-Horspool algorithm,
;;; but the name is already pretty long.

(: %textual-contains-right:boyer-moore
   (textual textual integer integer integer integer
     -> (or integer false)))
(define (%textual-contains-right:boyer-moore txt1 txt2 start1 end1 start2 end2)
  (if (= start2 end2)
      end1
      (let* ((n1 (- end1 start1))
             (n2 (- end2 start2))
             (firstchar (text-ref txt2 0))
             (firstj (remainder (char->integer firstchar) 128))
             (table (make-vector 128 n2)))
        (do ((i (- n2 1) (- i 1)))
            ((<= i 0))
          (let* ((c  (text-ref txt2 (+ i start2)))
                 (cp (char->integer c))
                 (j  (remainder cp 128)))
            (vector-set! table j i)))
        (let loop ((i (- end1 n2)))
          (if (<= i start1)
              (if (and (= i start1)
                       (textual-prefix? txt2 txt1 start2 end2 i end1))
                  i
                  #f)
              (let* ((c  (text-ref txt1 i))
                     (cp (char->integer c))
                     (j  (remainder cp 128)))
                (cond ((not (char=? c firstchar))
                       (loop (- i (vector-ref table j))))
                      ((textual-prefix? txt2 txt1 start2 end2 i end1)
                       i)
                      (else
                       (loop (- i (vector-ref table firstj)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Case conversion

;;; Two special cases:
;;;     the given text can be returned as is
;;;     the given text is entirely ASCII
;;;
;;; For all other cases, calls the corresponding procedures for strings.

(: textual-upcase (textual -> text))
(define (textual-upcase txt)
  (cond ((string? txt)
         (string->text (string-upcase txt)))
        ((text? txt)
         (%text-upcase txt))
        (else
         (complain 'textual-upcase txt))))

(: textual-downcase (textual -> text))
(define (textual-downcase txt)
  (cond ((string? txt)
         (string->text (string-downcase txt)))
        ((text? txt)
         (%text-downcase txt string-downcase))
        (else
         (complain 'textual-downcase txt))))

(: textual-foldcase (textual -> text))
(define (textual-foldcase txt)
  (cond ((string? txt)
         (string->text (string-foldcase txt)))
        ((text? txt)
         (%text-downcase txt string-foldcase))
        (else
         (complain 'textual-foldcase txt))))

(: textual-titlecase (textual -> text))
(define (textual-titlecase txt)
  (cond ((string? txt)
         (string->text (string-titlecase txt)))
        ((text? txt)
         (string->text
          (string-titlecase (textual->string txt))))
        (else
         (complain 'textual-titlecase txt))))

(: %text-upcase (text -> text))
(define (%text-upcase txt)
  (let* ((n (%text-length txt)))

    ;; So far, no conversion has been necessary.

    (define (fastest i)
      (if (= i n)
          txt
          (let ((c (%text-ref txt i)))
            (cond ((char>? c #\delete)
                   (textual-upcase (textual->string txt)))
                  ((char<=? #\a c #\z)
                   (fast i (list (subtext txt 0 i)) '()))
                  (else
                   (fastest (+ i 1)))))))

    ;; Conversions are necessary but it's been all-ASCII so far.
    ;; The upcased text for characters with index < i is
    ;;     (text-concatenate (reverse (cons (list->text (reverse chars))
    ;;                                      texts)))

    (define (fast i texts chars)
      (cond ((= i n)
             (if (null? chars)
                 (textual-concatenate-reverse texts)
                 (textual-concatenate-reverse texts
                                              (reverse-list->text chars))))
            ((and (= 0 (remainder i N))
                  (not (null? chars)))
             (fast i (cons (reverse-list->text chars) texts) '()))
            (else
             (let ((c (%text-ref txt i)))
               (cond ((char>? c #\delete)
                      (textual-append (textual-concatenate-reverse texts)
                                      (reverse-list->text chars)
                                      (string->text
                                       (string-upcase (subtext txt i n)))))
                     ((char<=? #\a c #\z)
                      (fast (+ i 1) texts (cons (char-upcase c) chars)))
                     (else
                      (fast (+ i 1) texts (cons c chars))))))))

    (fastest 0)))

;;; The string-caser is either string-downcase or string-foldcase.
;;; For ASCII, down-casing and fold-casing are the same.

(: %text-downcase (text (string -> string) -> text))
(define (%text-downcase txt string-caser)
  (let* ((n (%text-length txt)))

    ;; So far, no conversion has been necessary.

    (define (fastest i)
      (if (= i n)
          txt
          (let ((c (%text-ref txt i)))
            (cond ((char>? c #\delete)
                   (textual-downcase (textual->string txt)))
                  ((char<=? #\A c #\Z)
                   (fast i (list (subtext txt 0 i)) '()))
                  (else
                   (fastest (+ i 1)))))))

    ;; Conversions are necessary but it's been all-ASCII so far.
    ;; The downcased text for characters with index < i is
    ;;     (textual-concatenate (reverse (cons (list->text (reverse chars))
    ;;                                         texts)))

    (define (fast i texts chars)
      (cond ((= i n)
             (if (null? chars)
                 (textual-concatenate-reverse texts)
                 (textual-concatenate-reverse texts
                                              (reverse-list->text chars))))
            ((and (= 0 (remainder i N))
                  (not (null? chars)))
             (fast i (cons (reverse-list->text chars) texts) '()))
            (else
             (let ((c (%text-ref txt i)))
               (cond ((char>? c #\delete)
                      (textual-append (textual-concatenate-reverse texts)
                                      (reverse-list->text chars)
                                      (string->text
                                       (string-caser (subtext txt i n)))))
                     ((char<=? #\A c #\Z)
                      (fast (+ i 1) texts (cons (char-downcase c) chars)))
                     (else
                      (fast (+ i 1) texts (cons c chars))))))))

    (fastest 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Concatenation
;;;
;;; textual-concatenate is defined by the kernel

(: textual-append (#!rest textual -> text))
(define (textual-append . texts)
  (textual-concatenate texts))

(: textual-concatenate-reverse
   ((list-of textual) #!optional textual integer -> text))
(define textual-concatenate-reverse
  (case-lambda
   ((texts)
    (textual-concatenate (reverse texts)))
   ((texts final-textual)
    (textual-concatenate-reverse (cons final-textual texts)))
   ((texts final-textual end)
    (textual-concatenate-reverse texts
                                 (subtext
                                  (%textual->text final-textual
                                                  'textual-concatenate-reverse
                                                  texts final-textual end)
                                  0 end)))))

(: textual-join ((list-of textual) #!optional textual symbol -> text))
(define textual-join
  (case-lambda
   ((textuals)
    (textual-join textuals " " 'infix))
   ((textuals delimiter)
    (textual-join textuals delimiter 'infix))
   ((textuals delimiter grammar)
  (assert-type 'textual-join (pair-or-null? textuals))
  (assert-type 'textual-join (textual? delimiter))
    (assert (memq grammar '(infix strict-infix prefix suffix))
      'textual-join "invalid grammar argument" grammar)
    (let* ((texts (map (lambda (t) (%textual->text t 'textual-join textuals))
                       textuals))
           (delimiter (%textual->text delimiter
                                      'textual-join textuals delimiter)))
      (if (null? texts)
          (case grammar
            ((strict-infix)
             (complain 'textual-join textuals delimiter grammar))
            (else (text)))
          (let loop ((rtxts (reverse texts))
                     (texts (if (eq? grammar 'suffix)
                                (list delimiter)
                                '())))
            (cond ((null? rtxts)
                   (let ((texts (if (eq? grammar 'prefix)
                                    texts
                                    (cdr texts))))
                     (textual-concatenate texts)))
                  (else
                   (loop (cdr rtxts)
                         (cons delimiter (cons (car rtxts) texts)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Fold & map & friends

(: textual-fold (procedure * textual #!optional integer integer -> *))
(define-textual-start-end (textual-fold kons knil txt start end)
  (assert-type 'textual-fold (procedure? kons))
  (let loop ((knil knil)
             (i start))
    (if (< i end)
        (loop (kons (%text-ref txt i) knil)
              (+ i 1))
        knil)))

(: textual-fold-right
   (procedure * textual #!optional integer integer -> *))
(define-textual-start-end (textual-fold-right kons knil txt start end)
  (assert-type 'textual-fold (procedure? kons))
  (let loop ((knil knil)
             (i (- end 1)))
    (if (>= i start)
        (loop (kons (%text-ref txt i) knil)
              (- i 1))
        knil)))

(: textual-map
   ((#!rest char -> (or char string text)) textual #!rest textual
     -> text))
(define textual-map
  (case-lambda
   ((proc txt)
  (assert-type 'textual-map (procedure? proc))
    (%textual-map1 proc txt))
   ((proc txt1 txt2 . rest)
  (assert-type 'textual-map (procedure? proc))
    (%textual-mapn proc (cons txt1 (cons txt2 rest))))))

(: %textual-map1 ((char -> (or char string text)) textual -> text))
(define (%textual-map1 proc txt)
  (let ((txt (%textual->text txt 'textual-map proc txt)))
    (let ((n (%text-length txt)))
      (let loop ((i 0)
                 (pieces '())
                 (chars '())
                 (k 0))
        (cond ((= i n)
               (textual-concatenate
                (reverse (%text-map-pieces pieces chars))))
              ((>= k N)
               (loop i
                     (%text-map-pieces pieces chars)
                     '()
                     (remainder k N)))
              (else
               (let ((x (proc (%text-ref txt i))))
                 (loop (+ i 1)
                       pieces
                       (cons x chars)
                       (+ k (cond ((char? x) 1)
                                  ((string? x) (string-length x))
                                  ((text? x) (%text-length x))
                                  (else
                                   (%textual-map-bad-result proc x))))))))))))

(: %textual-mapn
   ((#!rest char -> (or char string text)) (list-of textual) -> text))
(define (%textual-mapn proc textuals)
  (let* ((texts (map (lambda (txt)
                       (%textual->text txt 'textual-map textuals))
                     textuals))
         (n (apply min (map %text-length texts))))
    (let loop ((i 0)
               (pieces '())
               (chars '())
               (k 0))
      (cond ((= i n)
             (textual-concatenate
              (reverse (%text-map-pieces pieces chars))))
            ((>= k N)
             (loop i
                   (%text-map-pieces pieces chars)
                   '()
                   (remainder k N)))
            (else
             (let ((x (apply proc (%fetch-all texts i))))
               (loop (+ i 1)
                     pieces
                     (cons x chars)
                     (+ k (cond ((char? x) 1)
                                ((string? x) (string-length x))
                                ((text? x) (%text-length x))
                                (else
                                 (%textual-map-bad-result proc x)))))))))))

;; FIXME: Misleading error message.
(define (%textual-map-bad-result proc x)
  (error "textual-map: proc returned non-character" x))

;;; Given a list of texts and a list of mixed characters/strings/texts,
;;; in reverse order, converts the second argument into a text and
;;; returns that text consed onto the first argument.

(: %text-map-pieces
   ((list-of text) (list-of (or char string text)) -> (list-of text)))
(define (%text-map-pieces texts stuff)
  (let loop ((revstuff stuff)
             (stuff '())
             (n 0))
    (if (null? revstuff)
        (let ((s (make-string n)))    ; probably short
          (let inner-loop ((stuff stuff)
                           (i 0))
            (if (null? stuff)
                (cons (string->text s) texts)
                (let ((x (car stuff)))
                  (cond ((char? x)
                         (string-set! s i x)
                         (inner-loop (cdr stuff) (+ i 1)))
                        ((string? x)
                         (string-copy! s i x)
                         (inner-loop (cdr stuff) (+ i (string-length x))))
                        (else
                         (string-copy! s i (textual->string x))
                         (inner-loop (cdr stuff) (+ i (text-length x)))))))))
        (let* ((x (car revstuff))
               (revstuff (cdr revstuff))
               (stuff (cons x stuff)))
          (loop revstuff
                stuff
                (+ n (cond ((char? x) 1)
                           ((string? x) (string-length x))
                           (else (text-length x)))))))))

(: textual-for-each
   ((#!rest char -> *) textual #!rest textual -> undefined))
(define textual-for-each
  (case-lambda
   ((proc txt)
  (assert-type 'textual-for-each (procedure? proc))
    (%textual-for-each1 proc txt))
   ((proc txt1 txt2 . rest)
  (assert-type 'textual-for-each (procedure? proc))
    (%textual-for-eachn proc (cons txt1 (cons txt2 rest))))))

(: textual-for-each1 ((char -> *) textual -> undefined))
(define (%textual-for-each1 proc txt)
  (let* ((txt (%textual->text txt 'textual-for-each proc txt))
         (n (%text-length txt)))
    (let loop ((i 0))
      (if (< i n)
          (begin (proc (%text-ref txt i))
                 (loop (+ i 1)))))))

(: textual-for-eachn ((#!rest char -> *) (list-of textual) -> undefined))
(define (%textual-for-eachn proc textuals)
  (let* ((texts (map (lambda (txt)
                       (%textual->text txt 'textual-map textuals))
                     textuals))
         (n (apply min (map %text-length texts))))
    (let loop ((i 0))
      (if (< i n)
          (begin (apply proc (%fetch-all texts i))
                 (loop (+ i 1)))))))

;; FIXME: Rewrite.
(: %fetch-all ((list-of text) integer -> (list-of char)))
(define (%fetch-all texts i)
  (if (null? texts)
      '()
      (cons (%text-ref (car texts) i)
            (%fetch-all (cdr texts) i))))

;;; FIXME: there's no reason to convert a string to a text here

(: textual-map-index
   ((integer -> (or char string text)) textual #!optional integer integer
     -> text))
(define-textual-start-end (textual-map-index proc txt start end)
  (assert-type 'textual-map-index (procedure? proc))
  (let ((n end))
    (let loop ((i start)
               (pieces '())
               (chars '())
               (k 0))
      (cond ((= i n)
             (textual-concatenate
              (reverse (%text-map-pieces pieces chars))))
            ((>= k N)
             (loop i
                   (%text-map-pieces pieces chars)
                   '()
                   (remainder k N)))
            (else
             (let ((x (proc i)))
               (loop (+ i 1)
                     pieces
                     (cons x chars)
                     (+ k (cond ((char? x) 1)
                                ((string? x) (string-length x))
                                ((text? x) (%text-length x))
                                (else
                                 ;; FIXME: Copy-paste disease (fix name).
                                 (%textual-map-bad-result proc x)))))))))))

;;; FIXME: there's no reason to convert a string to a text here

;; FIXME: Remove unneeded binding?
(: textual-for-each-index
   ((integer -> *) textual #!optional integer integer -> undefined))
(define-textual-start-end (textual-for-each-index proc txt start end)
  (assert-type 'textual-for-each-index (procedure? proc))
  (let ((n end))
    (let loop ((i start))
      (if (< i n)
          (begin (proc i)
                 (loop (+ i 1)))))))

;; FIXME: Better optionals handling.
(: textual-count
   (textual (char -> boolean) #!optional integer integer -> integer))
(define-textual (textual-count txt pred . rest)
  (let ((start (if (null? rest) 0 (car rest)))
        (end (if (or (null? rest) (null? (cdr rest)))
                 (%text-length txt)
                 (car (cdr rest)))))
  (assert-type 'textual-count (procedure? pred))
  (assert-type 'textual-count (exact-integer? start))
  (assert-type 'textual-count (exact-integer? end))
    (assert (<= 0 start end (%text-length txt))
      'textual-count "start/end out of range" start end txt)
    (textual-fold (lambda (c n)
                    (if (pred c)
                        (+ n 1)
                        n))
                  0 txt start end)))

(: textual-filter
   ((char -> boolean) textual #!optional integer integer -> text))
(define-textual-start-end (textual-filter pred txt start end)
  (assert-type 'textual-filter (procedure? pred))
  (textual-map (lambda (c) (if (pred c) c "")) (subtext txt start end)))

;;; FIXME: checks arguments twice

(: textual-filter
   ((char -> boolean) textual #!optional integer integer -> text))
(define-textual-start-end (textual-remove pred txt start end)
  (textual-filter (lambda (c) (not (pred c))) txt start end))

;;; FIXME: not linear-time unless string-set! is O(1)
;;; (but this is a pretty useless procedure anyway)

(: textual-reverse (textual #!optional integer integer -> text))
(define-textual-start-end (textual-reverse txt start end)
  (let* ((n (- end start))
         (s (make-string n)))
    (do ((i start (+ i 1)))
        ((= i end)
         (string->text s))
      (string-set! s (- n (- i start) 1) (%text-ref txt i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Replication & splitting

(: textual-replicate
   (textual integer integer #!optional integer integer -> text))
(define (textual-replicate t from to . args)
  (assert-type 'textual-replicate (exact-integer? from))
  (assert-type 'textual-replicate (exact-integer? to))
  (let* ((txt (%textual->text t 'textual-replicate t))
         (len (%text-length txt)))
    (let-optionals args ((start 0) (end len))
      (assert (exact-integer? start)
        'textual-replicate "illegal argument" start)
      (assert (exact-integer? end)
        'textual-replicate "illegal argument" end)
      (assert (<= 0 start end len)
        'textual-replicate "start/end out of range" start end t)
      (%text-replicate (subtext txt start end) from to))))

(define (%text-replicate txt from to)
  (let* ((n (- to from))
         (len (%text-length txt)))
    (cond ((= n 0)
           "")
          ((or (< n 0)
               (= len 0))
           (complain 'textual-replicate txt from to))
          (else
           (let* ((from (euclidean-remainder from len))
                  (to (+ from n)))
             (do ((replicates '() (cons txt replicates))
                  (replicates-length 0 (+ replicates-length len)))
                 ((>= replicates-length to)
                  (subtext (textual-concatenate replicates)
                           from to))))))))

(: textual-split
   (textual textual #!optional symbol (or integer false) integer integer
     -> (list-of text)))
(define textual-split
  (case-lambda
   ((s delimiter grammar limit start end)
    (textual-split (subtextual s start end) delimiter grammar limit))
   ((s delimiter grammar limit start)
    (textual-split (subtextual s start (textual-length s))
                   delimiter grammar limit))
   ((s delimiter)
    (textual-split s delimiter 'infix #f))
   ((s delimiter grammar)
    (textual-split s delimiter grammar #f))
   ((s0 delimiter grammar limit)
    (define (bad-arguments)
      (complain 'textual-split s0 delimiter grammar limit))
    (let* ((s (%textual->text s0 'textual-split s0 delimiter grammar limit))
           (delimiter
            (%textual->text delimiter
                            'textual-split s0 delimiter grammar limit))
           (limit (or limit (%text-length s)))
           (splits
            (cond ((= 0 (%text-length delimiter))
                   (%text-split-into-characters s limit))
                  (else
                   (%text-split-using-word s delimiter limit)))))
      (case grammar
        ((infix strict-infix)
         (if (= 0 (%text-length s))
             (if (eq? grammar 'infix)
                 '()
                 (bad-arguments))
             splits))
        ((prefix)
         (if (and (pair? splits)
                  (= 0 (%text-length (car splits))))
             (cdr splits)
             splits))
        ((suffix)
         (if (and (pair? splits)
                  (= 0 (%text-length (car (last-pair splits)))))
             (reverse (cdr (reverse splits)))
             splits))
        (else
         (bad-arguments)))))))

(: %text-split-into-characters (text integer -> (list-of text)))
(define (%text-split-into-characters s limit)
  (let ((n (%text-length s)))
    (cond ((> n (+ limit 1))
           (append (%text-split-into-characters (subtext s 0 limit) limit)
                   (list (subtext s limit n))))
          (else
           (map text (textual->list s))))))

;;; FIXME: inefficient

(: %text-split-using-word (text text integer -> (list-of text)))
(define (%text-split-using-word txt sep limit)
  (let loop ((i 0)
             (limit limit)
             (texts '()))
    (if (= 0 limit)
        (reverse (cons (subtext txt i (%text-length txt)) texts))
        (let ((i2 (textual-contains txt sep i)))
          (if i2
              (loop (+ i2 (%text-length sep))
                    (- limit 1)
                    (cons (subtext txt i i2) texts))
              (loop i 0 texts))))))

;;; eof
