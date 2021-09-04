;;; Copyright (C) William D Clinger (2016).
;;; Copyright (C) 2020 Wolfgang Corcoran-Mathe.
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

(import (except (scheme) string-ref)
        (chicken base)
        (chicken eval)
        (only (utf8) string-ref)
        (prefix (only (r7rs) char<=?) r7#)
        (only (r7rs) bytevector-length bytevector-u8-ref guard utf8->string)
        (test)
        (srfi 135))

(define-syntax OR
  (syntax-rules ()
    ((_ expr1 expr ...)
     (begin (set! current-test 'expr1)
            ;;          (write current-test)
            ;;          (newline)
            (or expr1 expr ...)))))

;;; Help functions for testing.

(define (as-text . args)
  (textual-concatenate (map (lambda (x)
                              (cond ((text? x) x)
                                    ((string? x) (string->text x))
                                    ((char? x) (text x))
                                    (else
                                     (error "as-text: illegal argument" x))))
                            args)))

(define (result=? str txt)
  (and (text? txt)
       (textual=? str txt)))

;;; Non-ASCII test texts.

(define ABC (string->text "αβγ"))

(define ABCDEF (string->text "ÀbÇdÉf"))

(define DEFABC (string->text "dÉfÀbÇ"))

(define eszett "ß")

(define fuss (string->text "Fuß"))

(define chaos0 (string->text "ΞΑΟΣ"))

(define chaos1 (string->text "ξαος"))

(define chaos2 (string->text "ξαοσ"))

(define beyondBMP (string->text "aÀο𝑁𝄓𝄐z"))

(include "test-predicates.scm")
(include "test-constructors.scm")
(include "test-conversions.scm")
(include "test-selection.scm")
(include "test-replacement.scm")
(include "test-comparison.scm")
(include "test-affixes.scm")
(include "test-searching.scm")
(include "test-case-conversion.scm")

;;; Concatenation

(or (result=? "" (textual-append))
    (fail 'textual-append))

(or (result=? "abcdef"

              (textual-append (as-text "")
                              (as-text "a")
                              (as-text "bcd")
                              "" "ef" "" ""))
    (fail 'textual-append))

(or (result=? "" (textual-concatenate '()))
    (fail 'textual-concatenate))

(or (result=? "abcdef"
              (textual-concatenate
               (map string->text '("" "a" "bcd" "" "ef" "" ""))))
    (fail 'textual-concatenate))

;;; textual-concatenate is likely to have special cases for longer texts.

(let* ((alphabet "abcdefghijklmnopqrstuvwxyz")
       (str1 alphabet)
       (str10 (apply string-append (vector->list (make-vector 10 str1))))
       (str100 (apply string-append (vector->list (make-vector 10 str10))))
       (str100-500 (substring str100 100 500))
       (str600-999 (substring str100 600 999))
       (alph1 (textual-copy alphabet))
       (alph10 (textual-concatenate (vector->list (make-vector 10 alph1))))
       (alph100 (textual-concatenate (vector->list (make-vector 10 alph10))))
       (t100-500 (subtext alph100 100 500))
       (t600-999 (subtext alph100 600 999)))

  (or (result=? str10 alph10)
      (fail 'textual-concatenate))

  (or (result=? str100 alph100)
      (fail 'textual-concatenate))

  (or (result=? str100-500 t100-500)
      (fail 'textual-concatenate))

  (or (result=? str600-999 t600-999)
      (fail 'textual-concatenate))

  ;; concatenating a short text with a long text

  (or (result=? (string-append str1 str600-999)
                (textual-concatenate (list alph1 t600-999)))
      (fail 'textual-concatenate))

  (or (result=? (string-append str1 str600-999)
                (textual-concatenate (list alph1 (textual-copy t600-999))))
      (fail 'textual-concatenate))

  (or (result=? (string-append str600-999 str1)
                (textual-concatenate (list t600-999 alph1)))
      (fail 'textual-concatenate))

  (or (result=? (string-append str600-999 str1)
                (textual-concatenate (list (textual-copy t600-999) alph1)))
      (fail 'textual-concatenate)))


(or (result=? "" (textual-concatenate-reverse '()))
    (fail 'textual-concatenate-reverse))

(or (result=? "efbcda"
              (textual-concatenate-reverse
               (map string->text '("" "a" "bcd" "" "ef" "" ""))))
    (fail 'textual-concatenate-reverse))

(or (result=? "huh?"
              (textual-concatenate-reverse '() "huh?"))
    (fail 'textual-concatenate-reverse))

(or (result=? "efbcdaxy"
              (textual-concatenate-reverse '("" "a" "bcd" "" "ef" "" "") "xy"))
    (fail 'textual-concatenate-reverse))

(or (result=? "huh"
              (textual-concatenate-reverse '() "huh?" 3))
    (fail 'textual-concatenate-reverse))

(or (result=? "efbcdax"
              (textual-concatenate-reverse
               '("" "a" "bcd" "" "ef" "" "") "x" 1))
    (fail 'textual-concatenate-reverse))


(or (result=? "" (textual-join '()))
    (fail 'textual-join))

(or (result=? " ab cd  e f "
              (textual-join (map string->text '("" "ab" "cd" "" "e" "f" ""))))
    (fail 'textual-join))

(or (result=? ""
              (textual-join '() ""))
    (fail 'textual-join))

(or (result=? "abcdef"
              (textual-join '("" "ab" "cd" "" "e" "f" "") ""))
    (fail 'textual-join))

(or (result=? ""
              (textual-join '() "xyz"))
    (fail 'textual-join))

(or (result=? "xyzabxyzcdxyzxyzexyzfxyz"
              (textual-join '("" "ab" "cd" "" "e" "f" "") "xyz"))
    (fail 'textual-join))

(or (result=? ""
              (textual-join '() "" 'infix))
    (fail 'textual-join))

(or (result=? "abcdef"
              (textual-join '("" "ab" "cd" "" "e" "f" "") "" 'infix))
    (fail 'textual-join))

(or (result=? ""
              (textual-join '() "xyz" 'infix))
    (fail 'textual-join))

(or (result=? "xyzabxyzcdxyzxyzexyzfxyz"
              (textual-join '("" "ab" "cd" "" "e" "f" "") (as-text "xyz") 'infix))
    (fail 'textual-join))

(or (equal? 'horror
            (guard (exn (#t 'horror))
                   (textual-join '() "" 'strict-infix)))
    (fail 'textual-join))

(or (result=? "abcdef"
              (textual-join '("" "ab" "cd" "" "e" "f" "") "" 'strict-infix))
    (fail 'textual-join))

(or (equal? 'wham
            (guard (exn (else 'wham))
                   (textual-join '() "xyz" 'strict-infix)))
    (fail 'textual-join))

(or (result=? "xyzabxyzcdxyzxyzexyzfxyz"
              (textual-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'strict-infix))
    (fail 'textual-join))

(or (result=? ""
              (textual-join '() "" 'suffix))
    (fail 'textual-join))

(or (result=? "abcdef"
              (textual-join '("" "ab" "cd" "" "e" "f" "") "" 'suffix))
    (fail 'textual-join))

(or (result=? ""
              (textual-join '() "xyz" 'suffix))
    (fail 'textual-join))

(or (result=? "xyzabxyzcdxyzxyzexyzfxyzxyz"
              (textual-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'suffix))
    (fail 'textual-join))

(or (result=? ""
              (textual-join '() "" 'prefix))
    (fail 'textual-join))

(or (result=? "abcdef"
              (textual-join '("" "ab" "cd" "" "e" "f" "") "" 'prefix))
    (fail 'textual-join))

(or (result=? ""
              (textual-join '() "xyz" 'prefix))
    (fail 'textual-join))

(or (result=? "xyzxyzabxyzcdxyzxyzexyzfxyz"
              (textual-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'prefix))
    (fail 'textual-join))


;;; Fold & map & friends

(or (= 8
       (textual-fold (lambda (c count)
                       (if (char-whitespace? c)
                           (+ count 1)
                           count))
                     0
                     (as-text " ...a couple of spaces in this one... ")))
    (fail 'textual-fold))

(or (= 7
       (textual-fold (lambda (c count)
                       (if (char-whitespace? c)
                           (+ count 1)
                           count))
                     0
                     " ...a couple of spaces in this one... "
                     1))
    (fail 'textual-fold))

(or (= 6
       (textual-fold (lambda (c count)
                       (if (char-whitespace? c)
                           (+ count 1)
                           count))
                     0
                     " ...a couple of spaces in this one... "
                     1
                     32))
    (fail 'textual-fold))

(or (equal? (string->list "abcdef")
            (textual-fold-right cons '() "abcdef"))
    (fail 'textual-fold-right))

(or (equal? (string->list "def")
            (textual-fold-right cons '() (as-text "abcdef") 3))
    (fail 'textual-fold-right))

(or (equal? (string->list "cde")
            (textual-fold-right cons '() (as-text "abcdef") 2 5))
    (fail 'textual-fold-right))

(or (string=? "aabraacaadaabraa"
              (let* ((s (as-text "abracadabra"))
                     (ans-len (textual-fold (lambda (c sum)
                                              (+ sum (if (char=? c #\a) 2 1)))
                                            0 s))
                     (ans (make-string ans-len)))
                (textual-fold (lambda (c i)
                                (let ((i (if (char=? c #\a)
                                             (begin (string-set! ans i #\a)
                                                    (+ i 1))
                                             i)))
                                  (string-set! ans i c)
                                  (+ i 1)))
                              0 s)
                ans))
    (fail 'textual-fold))


(or (result=? "abc" (textual-map string (as-text "abc")))
    (fail 'textual-map))

(or (result=? "ABC" (textual-map char-upcase "abc"))
    (fail 'textual-map))

(or (result=? "Hear-here!"
              (textual-map (lambda (c0 c1 c2)
                             (case c0
                               ((#\1) c1)
                               ((#\2) (string c2))
                               ((#\-) (text #\- c1))))
                           (string->text "1222-1111-2222")
                           (string->text "Hi There!")
                           (string->text "Dear John")))
    (fail 'textual-map))

(or (string=? "abc"
              (let ((q (open-output-string)))
                (textual-for-each (lambda (c) (write-char c q))
                                  (as-text "abc"))
                (get-output-string q)))
    (fail 'textual-for-each))

(or (equal? '("cfi" "beh" "adg")
            (let ((x '()))
              (textual-for-each (lambda (c1 c2 c3)
                                  (set! x (cons (string c1 c2 c3) x)))
                                "abc"
                                (as-text "defxyz")
                                (as-text "ghijklmnopqrstuvwxyz"))
              x))
    (fail 'textual-for-each))

(or (result=? "abc"
              (textual-map-index (lambda (i)
                                   (integer->char (+ i (char->integer #\a))))
                                 "xyz"))
    (fail 'textual-map-index))

(or (result=? "def"
              (textual-map-index (lambda (i)
                                   (integer->char (+ i (char->integer #\a))))
                                 "xyz***" 3))
    (fail 'textual-map-index))

(or (result=? "cde"
              (textual-map-index (lambda (i)
                                   (integer->char (+ i (char->integer #\a))))
                                 "......" 2 5))
    (fail 'textual-map-index))

(or (equal? '(101 100 99 98 97)
            (let ((s (as-text "abcde"))
                  (v '()))
              (textual-for-each-index
               (lambda (i)
                 (set! v (cons (char->integer (textual-ref s i)) v)))
               s)
              v))
    (fail 'textual-for-each-index))

(or (equal? '(101 100 99)
            (let ((s (as-text "abcde"))
                  (v '()))
              (textual-for-each-index
               (lambda (i)
                 (set! v (cons (char->integer (textual-ref s i)) v)))
               s 2)
              v))
    (fail 'textual-for-each-index))

(or (equal? '(99 98)
            (let ((s (as-text "abcde"))
                  (v '()))
              (textual-for-each-index
               (lambda (i)
                 (set! v (cons (char->integer (textual-ref s i)) v)))
               s 1 3)
              v))
    (fail 'textual-for-each-index))

(or (= 6 (textual-count "abcdef" char?))
    (fail 'textual-count))

(or (= 4 (textual-count "counting  whitespace, again " char-whitespace? 5))
    (fail 'textual-count))

(or (= 3 (textual-count "abcdefwxyz"
                        (lambda (c) (odd? (char->integer c)))
                        2 8))
    (fail 'textual-count))


(or (result=? "aiueaaaoi"
              (textual-filter (lambda (c) (memv c (textual->list "aeiou")))
                              (as-text "What is number, that man may know it?")))
    (fail 'textual-filter))

(or (result=? "And wmn, tht sh my knw nmbr?"
              (textual-remove (lambda (c) (memv c (textual->list "aeiou")))
                              "And woman, that she may know number?"))
    (fail 'textual-remove))

(or (result=? "iueaaaoi"
              (textual-filter (lambda (c) (memv c (textual->list "aeiou")))
                              (as-text "What is number, that man may know it?")
                              4))
    (fail 'textual-filter))

(or (result=? "mn, tht sh my knw nmbr?"
              (textual-remove (lambda (c) (memv c (textual->list "aeiou")))
                              "And woman, that she may know number?"
                              6))
    (fail 'textual-remove))

(or (result=? "aaao"
              (textual-filter (lambda (c) (memv c (textual->list "aeiou")))
                              (as-text "What is number, that man may know it?")
                              16 32))
    (fail 'textual-filter))

(or (result=? "And woman, that sh may know"
              (textual-remove (lambda (c) (memv c (textual->list "eiu")))
                              "And woman, that she may know number?"
                              0 28))
    (fail 'textual-remove))


#|
(or (result=? "" (textual-reverse ""))
    (fail 'textual-reverse))

(or (result=? "fedcba" (textual-reverse "abcdef"))
    (fail 'textual-reverse))

(or (result=? "" (textual-reverse (as-text "") 0))
    (fail 'textual-reverse))

(or (result=? "fedcba" (textual-reverse "abcdef" 0))
    (fail 'textual-reverse))

(or (result=? "fedc" (textual-reverse (as-text "abcdef") 2))
    (fail 'textual-reverse))

(or (result=? "" (textual-reverse "" 0 0))
    (fail 'textual-reverse))

(or (result=? "fedcba" (textual-reverse "abcdef" 0 6))
    (fail 'textual-reverse))

(or (result=? "edc" (textual-reverse "abcdef" 2 5))
    (fail 'textual-reverse))
|#


;;; Replication and splitting

(or (result=? "cdefabcdefabcd"
              (textual-replicate "abcdef" -4 10))
    (fail 'textual-replicate))

(or (result=? "bcdefbcdefbcd"
              (textual-replicate "abcdef" 90 103 1))
    (fail 'textual-replicate))

(or (result=? "ecdecdecde"
              (textual-replicate "abcdef" -13 -3 2 5))
    (fail 'textual-replicate))

(or (equal? '() (map textual->string (textual-split "" "")))
    (fail 'textual-split))

(or (equal? '("a" "b" "c") (map textual->string (textual-split "abc" "")))
    (fail 'textual-split))

(or (equal? '("too" "" "much" "" "data")
            (map textual->string
                 (textual-split "too  much  data" " ")))
    (fail 'textual-split))

(or (equal? '("" "there" "ya" "go" "")
            (map textual->string
                 (textual-split "***there***ya***go***" "***")))
    (fail 'textual-split))

(or (equal? '() (map textual->string (textual-split "" "" 'infix)))
    (fail 'textual-split))

(or (equal? '("a" "b" "c")
            (map textual->string (textual-split "abc" "" 'infix)))
    (fail 'textual-split))

(or (equal? '("too" "" "much" "" "data")
            (map textual->string
                 (textual-split "too  much  data" " " 'infix)))
    (fail 'textual-split))

(or (equal? '("" "there" "ya" "go" "")
            (map textual->string
                 (textual-split "***there***ya***go***" "***" 'infix)))
    (fail 'textual-split))

(or (equal? 'error
            (guard (exn (else 'error))
                   (map textual->string
                        (textual-split "" "" 'strict-infix))))
    (fail 'textual-split))

(or (equal? '("a" "b" "c")
            (map textual->string
                 (textual-split "abc" "" 'strict-infix)))
    (fail 'textual-split))

(or (equal? '("too" "" "much" "" "data")
            (map textual->string
                 (textual-split "too  much  data" " " 'strict-infix)))
    (fail 'textual-split))

(or (equal? '("" "there" "ya" "go" "")
            (map textual->string
                 (textual-split "***there***ya***go***" "***" 'strict-infix)))
    (fail 'textual-split))

(or (equal? '()
            (map textual->string
                 (textual-split "" "" 'prefix)))
    (fail 'textual-split))

(or (equal? '("a" "b" "c")
            (map textual->string
                 (textual-split "abc" "" 'prefix)))
    (fail 'textual-split))

(or (equal? '("too" "" "much" "" "data")
            (map textual->string
                 (textual-split "too  much  data" " " 'prefix)))
    (fail 'textual-split))

(or (equal? '("there" "ya" "go" "")
            (map textual->string
                 (textual-split "***there***ya***go***" "***" 'prefix)))
    (fail 'textual-split))

(or (equal? '()
            (map textual->string
                 (textual-split "" "" 'suffix)))
    (fail 'textual-split))

(or (equal? '("a" "b" "c")
            (map textual->string
                 (textual-split "abc" "" 'suffix)))
    (fail 'textual-split))

(or (equal? '("too" "" "much" "" "data")
            (map textual->string
                 (textual-split "too  much  data" " " 'suffix)))
    (fail 'textual-split))

(or (equal? '("" "there" "ya" "go")
            (map textual->string
                 (textual-split "***there***ya***go***" "***" 'suffix)))
    (fail 'textual-split))


(or (equal? '()
            (map textual->string
                 (textual-split "" "" 'infix #f)))
    (fail 'textual-split))

(or (equal? '("a" "b" "c")
            (map textual->string
                 (textual-split "abc" "" 'infix #f)))
    (fail 'textual-split))

(or (equal? '("too" "" "much" "" "data")
            (map textual->string
                 (textual-split "too  much  data" " " 'infix #f)))
    (fail 'textual-split))

(or (equal? '("" "there" "ya" "go" "")
            (map textual->string
                 (textual-split "***there***ya***go***" "***" 'infix #f)))
    (fail 'textual-split))

(or (equal? 'error
            (guard (exn (else 'error))
                   (map textual->string
                        (textual-split "" "" 'strict-infix #f))))
    (fail 'textual-split))

(or (equal? '("a" "b" "c")
            (map textual->string
                 (textual-split "abc" "" 'strict-infix #f)))
    (fail 'textual-split))

(or (equal? '("too" "" "much" "" "data")
            (map textual->string
                 (textual-split "too  much  data" " " 'strict-infix #f)))
    (fail 'textual-split))

(or (equal? '("" "there" "ya" "go" "")
            (map textual->string
                 (textual-split "***there***ya***go***" "***" 'strict-infix #f)))
    (fail 'textual-split))

(or (equal? '()
            (map textual->string
                 (textual-split "" "" 'prefix #f)))
    (fail 'textual-split))

(or (equal? '("a" "b" "c")
            (map textual->string
                 (textual-split "abc" "" 'prefix #f)))
    (fail 'textual-split))

(or (equal? '("too" "" "much" "" "data")
            (map textual->string
                 (textual-split "too  much  data" " " 'prefix #f)))
    (fail 'textual-split))

(or (equal? '("there" "ya" "go" "")
            (map textual->string
                 (textual-split "***there***ya***go***" "***" 'prefix #f)))
    (fail 'textual-split))

(or (equal? '()
            (map textual->string
                 (textual-split "" "" 'suffix #f)))
    (fail 'textual-split))

(or (equal? '("a" "b" "c")
            (map textual->string
                 (textual-split "abc" "" 'suffix #f)))
    (fail 'textual-split))

(or (equal? '("too" "" "much" "" "data")
            (map textual->string
                 (textual-split "too  much  data" " " 'suffix #f)))
    (fail 'textual-split))

(or (equal? '("" "there" "ya" "go")
            (map textual->string
                 (textual-split "***there***ya***go***" "***" 'suffix #f)))
    (fail 'textual-split))


(or (equal? 'error
            (guard (exn (else 'error))
                   (map textual->string
                        (textual-split "" "" 'strict-infix 3))))
    (fail 'textual-split))

(or (equal? '("a" "b" "c")
            (map textual->string
                 (textual-split "abc" "" 'strict-infix 3)))
    (fail 'textual-split))

(or (equal? '("too" "" "much" " data")
            (map textual->string
                 (textual-split "too  much  data" " " 'strict-infix 3)))
    (fail 'textual-split))

(or (equal? '("" "there" "ya" "go***")
            (map textual->string
                 (textual-split "***there***ya***go***" "***" 'strict-infix 3)))
    (fail 'textual-split))

(or (equal? '()
            (map textual->string
                 (textual-split "" "" 'prefix 3)))
    (fail 'textual-split))

(or (equal? '("a" "b" "c")
            (map textual->string
                 (textual-split "abc" "" 'prefix 3)))
    (fail 'textual-split))

(or (equal? '("too" "" "much" " data")
            (map textual->string
                 (textual-split "too  much  data" " " 'prefix 3)))
    (fail 'textual-split))

(or (equal? '("there" "ya" "go***")
            (map textual->string
                 (textual-split "***there***ya***go***" "***" 'prefix 3)))
    (fail 'textual-split))

(or (equal? '()
            (map textual->string
                 (textual-split "" "" 'suffix 3)))
    (fail 'textual-split))

(or (equal? '("a" "b" "c")
            (map textual->string
                 (textual-split "abc" "" 'suffix 3)))
    (fail 'textual-split))

(or (equal? '("too" "" "much" " data")
            (map textual->string
                 (textual-split "too  much  data" " " 'suffix 3)))
    (fail 'textual-split))

(or (equal? '("" "there" "ya" "go***")
            (map textual->string
                 (textual-split "***there***ya***go***" "***" 'suffix 3)))
    (fail 'textual-split))


(or (equal? 'error
            (guard (exn (else 'error))
                   (map textual->string
                        (textual-split "" "" 'strict-infix 3 0))))
    (fail 'textual-split))

(or (equal? '("b" "c")
            (map textual->string
                 (textual-split "abc" "" 'strict-infix 3 1)))
    (fail 'textual-split))

(or (equal? '("oo" "" "much" " data")
            (map textual->string
                 (textual-split "too  much  data" " " 'strict-infix 3 1)))
    (fail 'textual-split))

(or (equal? '("**there" "ya" "go" "")
            (map textual->string
                 (textual-split "***there***ya***go***" "***" 'strict-infix 3 1)))
    (fail 'textual-split))

(or (equal? '()
            (map textual->string
                 (textual-split "" "" 'prefix 3 0)))
    (fail 'textual-split))

(or (equal? '("b" "c")
            (map textual->string
                 (textual-split "abc" "" 'prefix 3 1)))
    (fail 'textual-split))

(or (equal? '("oo" "" "much" " data")
            (map textual->string
                 (textual-split "too  much  data" " " 'prefix 3 1)))
    (fail 'textual-split))

(or (equal? '("**there" "ya" "go" "")
            (map textual->string
                 (textual-split "***there***ya***go***" "***" 'prefix 3 1)))
    (fail 'textual-split))

(or (equal? '()
            (map textual->string
                 (textual-split "" "" 'suffix 3 0)))
    (fail 'textual-split))

(or (equal? '("b" "c")
            (map textual->string
                 (textual-split "abc" "" 'suffix 3 1)))
    (fail 'textual-split))

(or (equal? '("oo" "" "much" " data")
            (map textual->string
                 (textual-split "too  much  data" " " 'suffix 3 1)))
    (fail 'textual-split))

(or (equal? '("**there" "ya" "go")
            (map textual->string
                 (textual-split "***there***ya***go***" "***" 'suffix 3 1)))
    (fail 'textual-split))


(or (equal? 'error
            (guard (exn (else 'error))
                   (map textual->string
                        (textual-split "" "" 'strict-infix 3 0 0))))
    (fail 'textual-split))

(or (equal? '("b")
            (map textual->string
                 (textual-split "abc" "" 'strict-infix 3 1 2)))
    (fail 'textual-split))

(or (equal? '("oo" "" "much" " ")
            (map textual->string
                 (textual-split "too  much  data" " " 'strict-infix 3 1 11)))
    (fail 'textual-split))

(or (equal? '()
            (map textual->string
                 (textual-split "" "" 'prefix 3 0 0)))
    (fail 'textual-split))

(or (equal? '("b")
            (map textual->string
                 (textual-split "abc" "" 'prefix 3 1 2)))
    (fail 'textual-split))

(or (equal? '("oo" "" "much" " ")
            (map textual->string
                 (textual-split "too  much  data" " " 'prefix 3 1 11)))
    (fail 'textual-split))

(or (equal? '()
            (map textual->string
                 (textual-split "" "" 'suffix 3 0 0)))
    (fail 'textual-split))

(or (equal? '("b")
            (map textual->string
                 (textual-split "abc" "" 'suffix 3 1 2)))
    (fail 'textual-split))

(or (equal? '("oo" "" "much" " ")
            (map textual->string
                 (textual-split "too  much  data" " " 'suffix 3 1 11)))
    (fail 'textual-split))

(writeln "Done.")
(exit 0)
