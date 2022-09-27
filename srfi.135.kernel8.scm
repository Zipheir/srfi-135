;;; Copyright (C) William D Clinger (2016).
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

(module (srfi 135 kernel8)
  (text? text-length text-ref text-tabulate subtext textual-concatenate
   write-text text-ref/no-checks
   chunk-size
   string->text-1
   )

(import (except scheme string-length string-ref)
        (only r7rs bytevector-length bytevector-u8-ref write-bytevector
                   bytevector-u8-set! make-bytevector make-list)
        (only utf8 string-length string-ref)
        (chicken base)
        (chicken condition)
        (chicken type)
        typed-records)

(define-type bytevector u8vector)

(include "exceptions.scm")
(include "util.scm")

;;; 1-argument version

(: string->text-1 (string -> text))
(define (string->text-1 s)
  (if (string? s)
      (text-tabulate (lambda (i) (string-ref s i))
                     (string-length s))
      (complain 'string->text s)))

(: chunk-size fixnum)
(define chunk-size 128)

(: length&i0 (fixnum fixnum -> fixnum))
(define (length&i0 len i0)
  (+ (* chunk-size len) i0))

(: length&i0.length (fixnum -> fixnum))
(define (length&i0.length k)
  (quotient k chunk-size))

(: length&i0.i0 (fixnum -> fixnum))
(define (length&i0.i0 k)
  (remainder k chunk-size))

(define-record-type text-rtd
  (new-text0 k chunks)
  text?
  (k      text.k : fixnum)
  (chunks text.chunks : (vector-of bytevector)))

(define-type text (struct text-rtd))
(define-type textual (or text string))

(: %new-text (fixnum fixnum (vector-of bytevector) -> text))
(define (%new-text len i0 chunks)
  (new-text0 (length&i0 len i0) chunks))

(: the-empty-text text)
(define the-empty-text
  (%new-text 0 0 (vector (make-bytevector 0))))

;;; text? is defined by the record definition above.

(: text-length (text -> fixnum))
(define (text-length txt)
  (assert-type 'text-length (text? txt))
  (length&i0.length (text.k txt)))

(: text-ref (text fixnum -> char))
(define (text-ref txt i)
  (assert-type 'text-ref (text? txt))
  (assert-type 'text-ref (fixnum? i))
  (let* ((k      (text.k txt))
         (chunks (text.chunks txt))
         (len    (length&i0.length k))
         (i0     (length&i0.i0 k))
         (i+i0   (+ i i0))
         (j      (quotient i+i0 chunk-size))
         (ii     (remainder i+i0 chunk-size)))
    (unless (and (>= i 0) (< i len))
      (bounds-exception 'text-ref "index out of range" txt i))
    (let* ((sj (vector-ref chunks j))
           (sjn (bytevector-length sj)))
      (if (if (< j (- (vector-length chunks) 1))
              (= sjn chunk-size)
              (= sjn (remainder (+ i0 len) chunk-size)))
          (integer->char (bytevector-u8-ref sj ii))
          (%utf8-ref sj ii)))))

;;; Non-checking versions

(: %text-length (text -> fixnum))
(define (%text-length txt)
  (length&i0.length (text.k txt)))

(: text-ref/no-checks (text fixnum -> char))
(define (text-ref/no-checks txt i)
  (let* ((k      (text.k txt))
         (chunks (text.chunks txt))
         (len    (length&i0.length k))
         (i0     (length&i0.i0 k))
         (i+i0   (+ i i0))
         (j      (quotient i+i0 chunk-size))
         (ii     (remainder i+i0 chunk-size))
         (sj     (vector-ref chunks j))
         (sjn    (bytevector-length sj)))
    (if (if (< j (- (vector-length chunks) 1))
            (= sjn chunk-size)
            (= sjn (remainder (+ i0 len) chunk-size)))
        (integer->char (bytevector-u8-ref sj ii))
        (%utf8-ref sj ii))))

;;; Returns character i of the UTF-8.

(: %utf8-ref (bytevector fixnum -> char))
(define (%utf8-ref bv i)
  (let loop ((j 0)     ; character index of (bytevector-u8-ref bv k)
             (k 0))    ; byte index into bv
    (if (= i j)
        (%utf8-char-at bv k)
        (let ((byte (bytevector-u8-ref bv k)))
          (cond ((< byte 128)
                 (loop (+ j 1) (+ k 1)))
                ((< byte #b11100000)
                 (loop (+ j 1) (+ k 2)))
                ((< byte #b11110000)
                 (loop (+ j 1) (+ k 3)))
                (else
                 (loop (+ j 1) (+ k 4))))))))

(: %utf8-ref (bytevector fixnum -> char))
(define (%utf8-char-at bv k)
  (let ((byte (bytevector-u8-ref bv k)))
    (cond ((< byte 128)
           (integer->char byte))
          ((< byte #b11100000)
           (let* ((byte2 (bytevector-u8-ref bv (+ k 1)))
                  (bits1 (- byte  #b11000000))
                  (bits2 (- byte2 #b10000000))
                  (cp    (+ (* 64 bits1) bits2)))
             (integer->char cp)))
          ((< byte #b11110000)
           (let* ((byte2 (bytevector-u8-ref bv (+ k 1)))
                  (byte3 (bytevector-u8-ref bv (+ k 2)))
                  (bits1 (- byte  #b11100000))
                  (bits2 (- byte2 #b10000000))
                  (bits3 (- byte3 #b10000000))
                  (cp    (+ (* 64 64 bits1) (* 64 bits2) bits3)))
             (integer->char cp)))
          (else
           (let* ((byte2 (bytevector-u8-ref bv (+ k 1)))
                  (byte3 (bytevector-u8-ref bv (+ k 2)))
                  (byte4 (bytevector-u8-ref bv (+ k 3)))
                  (bits1 (- byte  #b11110000))
                  (bits2 (- byte2 #b10000000))
                  (bits3 (- byte3 #b10000000))
                  (bits4 (- byte4 #b10000000))
                  (cp    (+ (* 64 64 64 bits1)
                            (* 64 64 bits2)
                            (* 64 bits3)
                            bits4)))
             (integer->char cp))))))

;;; text-tabulate avoids side effects (in case proc returns more than once)

(: text-tabulate ((fixnum -> char) fixnum -> text))
(define (text-tabulate proc len)
  (assert-type 'text-tabulate (procedure? proc))
  (assert-type 'text-tabulate (fixnum? len))
  (if (= 0 len)
      the-empty-text
      (let loop ((i len)       ; highest index that's been tabulated
                 (chunks '())
                 (bytes '()))
        (cond ((= 0 i)
               (%new-text len
                           0
                           (list->vector
                            (cons (list->bytevector bytes)
                                  chunks))))
              ((and (= 0 (remainder i chunk-size))
                    (not (null? bytes)))
               (loop i
                     (cons (list->bytevector bytes) chunks)
                     '()))
              (else
               (let* ((i-1 (- i 1))
                      (c (proc i-1)))
                 (assert-type 'text-tabulate (char? c))
                 (let ((cp (char->integer c)))
                   (loop i-1
                         chunks
                         (cond ((< cp #x0080)
                                (cons cp bytes))
                               ((< cp #x0800)
                                (let* ((bits1 (quotient cp 64))
                                       (bits2 (remainder cp 64))
                                       (byte1 (+ bits1 #b11000000))
                                       (byte2 (+ bits2 #b10000000)))
                                  (cons byte1 (cons byte2 bytes))))
                               ((< cp #x10000)
                                (let* ((bits1 (quotient cp (* 64 64)))
                                       (bits2 (quotient
                                               (remainder cp (* 64 64))
                                               64))
                                       (bits3 (remainder cp 64))
                                       (byte1 (+ bits1 #b11100000))
                                       (byte2 (+ bits2 #b10000000))
                                       (byte3 (+ bits3 #b10000000)))
                                  (cons byte1
                                        (cons byte2
                                              (cons byte3 bytes)))))
                               (else
                                (let* ((bits1 (quotient cp (* 64 64 64)))
                                       (bits2 (quotient
                                               (remainder cp (* 64 64 64))
                                               (* 64 64)))
                                       (bits3 (quotient
                                               (remainder cp (* 64 64))
                                               64))
                                       (bits4 (remainder cp 64))
                                       (byte1 (+ bits1 #b11110000))
                                       (byte2 (+ bits2 #b10000000))
                                       (byte3 (+ bits3 #b10000000))
                                       (byte4 (+ bits4 #b10000000)))
                                  (cons byte1
                                        (cons byte2
                                              (cons byte3
                                                    (cons byte4
                                                          bytes)))))))))))))))

;;; FIXME: should the fast case do something different
;;; if the length of the result is sufficiently small?
;;; Probably not: splitting a 100-character text into
;;; 100 1-character texts should be fast, and programmers
;;; can now use text-copy instead if they're worried about it.
;;;
;;; subtext is now defined only for texts; use subtextual
;;; if the first argument might be a string.

(: subtext (text fixnum fixnum -> text))
(define (subtext txt start end)
  (assert-type 'subtext (text? txt))
  (assert-type 'subtext (fixnum? start))
  (assert-type 'subtext (fixnum? end))
  (unless (<= 0 start end (%text-length txt))
    (bounds-exception 'subtext "start/end out of range" txt start end))
  (%subtext txt start end))

(: %subtext (text fixnum fixnum -> text))
(define (%subtext txt start end)
  (let* ((k      (text.k txt))
         (chunks (text.chunks txt))
         (len    (length&i0.length k))
         (i0     (length&i0.i0 k))
         (i+i0   (+ start i0))
         (end+i0 (+ end i0))
         (len+i0 (+ len i0))
         (jstart (quotient i+i0 chunk-size))
         (jend   (quotient end+i0 chunk-size))
         (jlen   (quotient len chunk-size)))
    (cond ((= start end)
           the-empty-text)
          ((and (= 0 jstart)
                (= jlen jend))
           ;; the fast case
           (%new-text (- end start) i+i0 chunks))
          (else
           (let* ((v (make-vector (+ 1 (- jend jstart)))))
             (do ((j jstart (+ j 1)))
                 ((> j jend))
               (vector-set! v (- j jstart) (vector-ref chunks j)))
             (%new-text (- end start)
                        (remainder i+i0 chunk-size)
                        v))))))

;;; There are a lot of special cases that could be exploited here:
;;;     share the characters of the longest text
;;;     share the characters of the longest run of texts
;;;         whose characters don't have to be copied
;;;             if (text-length txt1) is a multiple of chunk-size,
;;;                 and txt2 starts at offset 0,
;;;                 then txt1 and txt2 can be concatenated
;;;                 without copying any of their characters
;;;
;;; That's a partial list.
;;; It would be easy to spend more time detecting special cases
;;; than would be saved on average.
;;; In the interest of simplicity and reliability, this code
;;; currently implements only two special cases:
;;;     share the full chunks of the longest text
;;;         provided
;;;             it contains at least chunk-size characters
;;;             it contains at least half the characters of the result
;;;             its characters start at offset zero
;;;     share the full chunks of the first text
;;;         provided
;;;             it contains at least chunk-size characters
;;;             its characters start at offset zero

(: textual-concatenate ((list-of textual) -> text))
(define (textual-concatenate texts)
  ;; Not foolproof, but cheaper than list?.
  (assert-type 'textual-concatenate (pair-or-null? texts))
  (cond ((null? texts) the-empty-text)
        ((null? (cdr texts))
         (let ((txt (car texts)))
           (cond ((text? txt) txt)
                 ((string? txt)
                  (string->text-1 txt))
                 (else
                  (type-exception 'textual-concatenate
                                  "invalid list element"
                                  texts)))))
        (else
         (let loop ((items (reverse texts))
                    (real-texts '())
                    (n 0)
                    (longest #f)
                    (longest-length 0))
           (cond ((null? items)
                  (%text-concatenate-n real-texts n longest longest-length))
                 ((text? (car items))
                  (let* ((txt (car items))
                         (k (%text-length txt)))
                    (loop (cdr items)
                          (cons txt real-texts)
                          (+ n k)
                          (if (> k longest-length) txt longest)
                          (max k longest-length))))
                 ((string? (car items))
                  (loop (cons (string->text-1 (car items)) (cdr items))
                        real-texts n longest longest-length))
                 (else
                  (type-exception 'textual-concatenate
                                  "invalid list element"
                                  texts)))))))

;;; All of the texts are really texts.  No strings.
;;; n is the length of the result.
;;; longest is #f or the longest of the texts, and
;;; longest-length is its length (or zero).

(: %textual-concatenate-n
   ((list-of text) fixnum (or fixnum false) fixnum -> text))
(define (%text-concatenate-n texts n longest longest-length)
  (if (and longest
           (> longest-length chunk-size)
           (< n (+ longest-length longest-length))
           (= 0 (length&i0.i0 (text.k longest))))
      (if (eq? longest (car texts))
          (%%text-concatenate-n texts n)
          (let loop ((texts texts)
                     (front '())
                     (front-length 0))
            (cond ((eq? longest (car texts))
                   (%%text-concatenate-front
                    (reverse front)
                    (%%text-concatenate-n texts (- n front-length))
                    front-length
                    n))
                  (else
                   (let ((txt (car texts)))
                     (loop (cdr texts)
                           (cons txt front)
                           (+ front-length (%text-length txt))))))))
      (%%text-concatenate-n texts n)))

;;; texts is a non-empty list of texts, with no strings.
;;; n is the length of the result.
;;;
;;; The text returned has a start index of zero.
;;;
;;; Special case:
;;; If the first text has a start index of zero,
;;; then its full chunks don't have to be copied.

(: %%text-concatenate-n ((list-of text) fixnum -> text))
(define (%%text-concatenate-n texts n)
  (if (= 0 n)
      the-empty-text
      (let* ((n/chunk-size     (quotient n chunk-size))
             (m       (remainder n chunk-size))
             (nchunks (+ n/chunk-size (if (= 0 m) 0 1)))
             (chunks  (make-vector nchunks 'bug-in-text-concatenate))
             (txt (car texts))
             (k   (text.k txt))
             (len (length&i0.length k))
             (i0  (length&i0.i0 k)))
        (if (and (> len chunk-size)
                 (= 0 i0))
            (let* ((j (quotient len chunk-size))
                   (ti (* j chunk-size))
                   (chunks0 (text.chunks txt)))
              (do ((i 0 (+ i 1)))
                  ((= i j))
                (vector-set! chunks i (vector-ref chunks0 i)))
              (%%text-concatenate-finish n 0 chunks j texts ti))
            (%%text-concatenate-finish n 0 chunks 0 texts 0)))))

;;; All of the texts are really texts.  No strings.
;;; The second argument is a text that starts at offset zero.
;;; k is the total length of the texts passed as first argument.
;;; n is the length of the result.
;;;
;;; Returns the texts concatenated with the second argument,
;;; without copying any chunks of the second argument.

(: %%text-concatenate-front
   ((list-of text) text fixnum fixnum -> text))
(define (%%text-concatenate-front texts txt k n)
  (let* ((k/chunk-size     (quotient k chunk-size))
         (mk      (remainder k chunk-size))
         (i0      (if (= 0 mk) 0 (- chunk-size mk)))  ; start offset for result
         (kchunks (+ k/chunk-size (if (= 0 mk) 0 1))) ; number of new chunks
         (n-k     (- n k))
         (n-k/chunk-size   (quotient n-k chunk-size))
         (m       (remainder n-k chunk-size))
         (nchunks (+ kchunks
                     n-k/chunk-size
                     (if (= 0 m) 0 1)))
         (chunks  (make-vector nchunks 'bug-in-text-concatenate))
         (chunks2 (text.chunks txt)))

    ;; copy extant chunks

    (do ((i kchunks (+ i 1)))
        ((= i nchunks))
      (vector-set! chunks i (vector-ref chunks2 (- i kchunks))))

    (%%text-concatenate-finish n i0 chunks 0 texts 0)))

;;; Given:
;;;
;;;     n      : the length of a text to be created
;;;     i0     : start offset for the text to be created
;;;     chunks : the vector of chunks for that new text
;;;     j      : vector index of first uninitialized chunk
;;;     texts  : a non-empty list of texts to be copied into the chunks
;;;     ti     : index of first uncopied character in the first text
;;;
;;; Creates new chunks as necessary, copies the texts into those chunks
;;; and returns a new text.
;;; The given texts may not fill the chunks because the chunks
;;; of some shared text may already have been copied into some
;;; tail of the chunks vector.

(: %%text-concatenate-finish
   (fixnum fixnum (vector-of bytevector) fixnum (list-of text) fixnum
     -> text))
(define (%%text-concatenate-finish n i0 chunks j texts ti)
  (let loop ((texts (cdr texts))
             (txt (car texts))
             (j j)    ; index into chunks
             (k i0)   ; index into (vector-ref chunks j)
             (ti ti)  ; index into txt
             (bytes (make-list i0 0))) ; bytes being collected for next chunk
    (cond ((= k chunk-size)
           (let ((bv (list->bytevector (reverse bytes))))
             (vector-set! chunks j bv))
           (loop texts txt (+ j 1) 0 ti '()))
          ((= ti (%text-length txt))
           (if (null? texts)
               (begin (if (not (null? bytes))
                          (let ((bv (list->bytevector (reverse bytes))))
                            (vector-set! chunks j bv)))
                      (%new-text n i0 chunks))
               (loop (cdr texts) (car texts) j k 0 bytes)))
          (else
           (let* ((cp (char->integer (text-ref/no-checks txt ti)))
                  (bytes (cond ((< cp #x0080)
                                (cons cp bytes))
                               ((< cp #x0800)
                                (let* ((bits1 (quotient cp 64))
                                       (bits2 (remainder cp 64))
                                       (byte1 (+ bits1 #b11000000))
                                       (byte2 (+ bits2 #b10000000)))
                                  (cons byte2 (cons byte1 bytes))))
                               ((< cp #x10000)
                                (let* ((bits1 (quotient cp (* 64 64)))
                                       (bits2 (quotient
                                               (remainder cp (* 64 64))
                                               64))
                                       (bits3 (remainder cp 64))
                                       (byte1 (+ bits1 #b11100000))
                                       (byte2 (+ bits2 #b10000000))
                                       (byte3 (+ bits3 #b10000000)))
                                  (cons byte3
                                        (cons byte2
                                              (cons byte1 bytes)))))
                               (else
                                (let* ((bits1 (quotient cp (* 64 64 64)))
                                       (bits2 (quotient
                                               (remainder cp (* 64 64 64))
                                               (* 64 64)))
                                       (bits3 (quotient
                                               (remainder cp (* 64 64))
                                               64))
                                       (bits4 (remainder cp 64))
                                       (byte1 (+ bits1 #b11110000))
                                       (byte2 (+ bits2 #b10000000))
                                       (byte3 (+ bits3 #b10000000))
                                       (byte4 (+ bits4 #b10000000)))
                                  (cons byte4
                                        (cons byte3
                                              (cons byte2
                                                    (cons byte1
                                                          bytes)))))))))
             (loop texts txt j (+ k 1) (+ ti 1) bytes))))))

;;; Primitive output procedure

(: write-text (text output-port -> void))
(define (write-text t port)
  (let* ((chunks (text.chunks t))
         (chunks-len (vector-length chunks)))
    (write-bytevector (vector-ref chunks 0)
                      port
                      (length&i0.i0 (text.k t)))
    (let loop ((i 1))
      (unless (= i chunks-len)
        (write-bytevector (vector-ref chunks i) port)
        (loop (+ i 1))))))

)
