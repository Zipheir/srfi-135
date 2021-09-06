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
        (utf8)
        (prefix (only (r7rs) char<=?) r7#)
        (only (r7rs) bytevector-length bytevector-u8-ref guard utf8->string)
        (test)
        (srfi 4)
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
(include "test-replication.scm")
(include "test-io.scm")

(test-exit)
