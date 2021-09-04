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
(include "test-concatenation.scm")
(include "test-traversal.scm")

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
