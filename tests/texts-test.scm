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

;;; Comparison
;;;
;;; The comparison tests aren't perfectly black-box because the
;;; specification of these comparison procedures allows them to
;;; use an ordering other than the usual lexicographic ordering.
;;; The sample implementations use lexicographic ordering, however,
;;; and a test program that discourages implementations from using
;;; orderings that differ from the usual on such simple cases is
;;; probably doing a public service.

(or (equal? #t (textual=? (as-text "Strasse") (as-text "Strasse")))
    (fail 'textual=?))

(or (equal? #t (textual=? "Strasse" (as-text "Strasse") "Strasse"))
    (fail 'textual=?))

(or (equal? #f (textual<? (as-text "z") (as-text "z")))
    (fail 'textual<?))
(or (equal? #t (textual<? (as-text "z") "zz"))
    (fail 'textual<?))
(or (equal? #f (textual<? (as-text "z") (as-text "Z")))
    (fail 'textual<?))
(or (equal? #t (textual<=? (as-text "z") "zz"))
    (fail 'textual<=?))
(or (equal? #f (textual<=? "z" "Z"))
    (fail 'textual<=?))
(or (equal? #t (textual<=? "z" (as-text "z")))
    (fail 'textual<=?))

(or (equal? #f (textual<? "z" (as-text "z")))
    (fail 'textual<?))
(or (equal? #f (textual>? (as-text "z") "zz"))
    (fail 'textual>?))
(or (equal? #t (textual>? "z" (as-text "Z")))
    (fail 'textual>?))
(or (equal? #f (textual>=? (as-text "z") "zz"))
    (fail 'textual>=?))
(or (equal? #t (textual>=? "z" "Z"))
    (fail 'textual>=?))
(or (equal? #t (textual>=? (as-text "z") (as-text "z")))
    (fail 'textual>=?))


(let* ((w "a")
       (x "abc")
       (y "def")
       (z (text #\a #\b #\c)))

  (or (equal? (textual=? x y z)                           #f)
      (fail 'textual=?))
  (or (equal? (textual=? x x z)                           #t)
      (fail 'textual=?))
  (or (equal? (textual=? w x y)                           #f)
      (fail 'textual=?))
  (or (equal? (textual=? y x w)                           #f)
      (fail 'textual=?))

  (or (equal? (textual<? x y z)                           #f)
      (fail 'textual<?))
  (or (equal? (textual<? x x z)                           #f)
      (fail 'textual<?))
  (or (equal? (textual<? w x y)                           #t)
      (fail 'textual<?))
  (or (equal? (textual<? y x w)                           #f)
      (fail 'textual<?))

  (or (equal? (textual>? x y z)                           #f)
      (fail 'textual>?))
  (or (equal? (textual>? x x z)                           #f)
      (fail 'textual>?))
  (or (equal? (textual>? w x y)                           #f)
      (fail 'textual>?))
  (or (equal? (textual>? y x w)                           #t)
      (fail 'textual>?))

  (or (equal? (textual<=? x y z)                          #f)
      (fail 'textual<=?))
  (or (equal? (textual<=? x x z)                          #t)
      (fail 'textual<=?))
  (or (equal? (textual<=? w x y)                          #t)
      (fail 'textual<=?))
  (or (equal? (textual<=? y x w)                          #f)
      (fail 'textual<=?))

  (or (equal? (textual>=? x y z)                          #f)
      (fail 'textual>=?))
  (or (equal? (textual>=? x x z)                          #t)
      (fail 'textual>=?))
  (or (equal? (textual>=? w x y)                          #f)
      (fail 'textual>=?))
  (or (equal? (textual>=? y x w)                          #t)
      (fail 'textual>=?))


  (or (equal? (textual=? x x)                             #t)
      (fail 'textual=?))
  (or (equal? (textual=? w x)                             #f)
      (fail 'textual=?))
  (or (equal? (textual=? y x)                             #f)
      (fail 'textual=?))

  (or (equal? (textual<? x x)                             #f)
      (fail 'textual<?))
  (or (equal? (textual<? w x)                             #t)
      (fail 'textual<?))
  (or (equal? (textual<? y x)                             #f)
      (fail 'textual<?))

  (or (equal? (textual>? x x)                             #f)
      (fail 'textual>?))
  (or (equal? (textual>? w x)                             #f)
      (fail 'textual>?))
  (or (equal? (textual>? y x)                             #t)
      (fail 'textual>?))

  (or (equal? (textual<=? x x)                            #t)
      (fail 'textual<=?))
  (or (equal? (textual<=? w x)                            #t)
      (fail 'textual<=?))
  (or (equal? (textual<=? y x)                            #f)
      (fail 'textual<=?))

  (or (equal? (textual>=? x x)                            #t)
      (fail 'textual>=?))
  (or (equal? (textual>=? w x)                            #f)
      (fail 'textual>=?))
  (or (equal? (textual>=? y x)                            #t)
      (fail 'textual>=?)))


(or (equal? #t (textual-ci<? "a" "Z"))
    (fail 'textual-ci<?))
(or (equal? #t (textual-ci<? "A" "z"))
    (fail 'textual-ci<?))
(or (equal? #f (textual-ci<? "Z" "a"))
    (fail 'textual-ci<?))
(or (equal? #f (textual-ci<? "z" "A"))
    (fail 'textual-ci<?))
(or (equal? #f (textual-ci<? "z" "Z"))
    (fail 'textual-ci<?))
(or (equal? #f (textual-ci<? "Z" "z"))
    (fail 'textual-ci<?))
(or (equal? #f (textual-ci>? "a" "Z"))
    (fail 'textual-ci>?))
(or (equal? #f (textual-ci>? "A" "z"))
    (fail 'textual-ci>?))
(or (equal? #t (textual-ci>? "Z" "a"))
    (fail 'textual-ci>?))
(or (equal? #t (textual-ci>? "z" "A"))
    (fail 'textual-ci>?))
(or (equal? #f (textual-ci>? "z" "Z"))
    (fail 'textual-ci>?))
(or (equal? #f (textual-ci>? "Z" "z"))
    (fail 'textual-ci>?))
(or (equal? #t (textual-ci=? "z" "Z"))
    (fail 'textual-ci=?))
(or (equal? #f (textual-ci=? "z" "a"))
    (fail 'textual-ci=?))
(or (equal? #t (textual-ci<=? "a" "Z"))
    (fail 'textual-ci<=?))
(or (equal? #t (textual-ci<=? "A" "z"))
    (fail 'textual-ci<=?))
(or (equal? #f (textual-ci<=? "Z" "a"))
    (fail 'textual-ci<=?))
(or (equal? #f (textual-ci<=? "z" "A"))
    (fail 'textual-ci<=?))
(or (equal? #t (textual-ci<=? "z" "Z"))
    (fail 'textual-ci<=?))
(or (equal? #t (textual-ci<=? "Z" "z"))
    (fail 'textual-ci<=?))
(or (equal? #f (textual-ci>=? "a" "Z"))
    (fail 'textual-ci>=?))
(or (equal? #f (textual-ci>=? "A" "z"))
    (fail 'textual-ci>=?))
(or (equal? #t (textual-ci>=? "Z" "a"))
    (fail 'textual-ci>=?))
(or (equal? #t (textual-ci>=? "z" "A"))
    (fail 'textual-ci>=?))
(or (equal? #t (textual-ci>=? "z" "Z"))
    (fail 'textual-ci>=?))
(or (equal? #t (textual-ci>=? "Z" "z"))
    (fail 'textual-ci>=?))

;;; The full-unicode feature doesn't imply full Unicode in strings,
;;; so these tests might fail even in a conforming implementation.
;;; Implementations that support full Unicode strings often have
;;; this feature, however, even though it isn't listed in the R7RS.

(cond-expand
 (full-unicode-strings
  (or (equal? #f (textual=? ABCDEF DEFABC))
      (fail 'textual=?))
  (or (equal? #f (textual=? DEFABC ABCDEF))
      (fail 'textual=?))
  (or (equal? #t (textual=? DEFABC DEFABC))
      (fail 'textual=?))

  (or (equal? #f (textual<? ABCDEF DEFABC))
      (fail 'textual=?))
  (or (equal? #t (textual<? DEFABC ABCDEF))
      (fail 'textual=?))
  (or (equal? #f (textual<? DEFABC DEFABC))
      (fail 'textual=?))

  (or (equal? #t (textual>? ABCDEF DEFABC))
      (fail 'textual=?))
  (or (equal? #f (textual>? DEFABC ABCDEF))
      (fail 'textual=?))
  (or (equal? #f (textual>? DEFABC DEFABC))
      (fail 'textual=?))

  (or (equal? #f (textual<=? ABCDEF DEFABC))
      (fail 'textual=?))
  (or (equal? #t (textual<=? DEFABC ABCDEF))
      (fail 'textual=?))
  (or (equal? #t (textual<=? DEFABC DEFABC))
      (fail 'textual=?))

  (or (equal? #t (textual>=? ABCDEF DEFABC))
      (fail 'textual=?))
  (or (equal? #f (textual>=? DEFABC ABCDEF))
      (fail 'textual=?))
  (or (equal? #t (textual>=? DEFABC DEFABC))
      (fail 'textual=?))

  (or (equal? #f (textual=? "Fuss" fuss))
      (fail 'textual=?:unicode))
  (or (equal? #f (textual=? "Fuss" "Fuss" fuss))
      (fail 'textual=?:unicode))
  (or (equal? #f (textual=? "Fuss" fuss "Fuss"))
      (fail 'textual=?:unicode))
  (or (equal? #f (textual=? fuss "Fuss" "Fuss"))
      (fail 'textual=?:unicode))
  (or (equal? #t (textual<? "z" (as-text eszett)))
      (fail 'textual<?:unicode))
  (or (equal? #f (textual<? (as-text eszett) "z"))
      (fail 'textual<?:unicode))
  (or (equal? #t (textual<=? "z" (as-text eszett)))
      (fail 'textual<=?:unicode))
  (or (equal? #f (textual<=? (as-text eszett) "z"))
      (fail 'textual<=?:unicode))
  (or (equal? #f (textual>? "z" (as-text eszett)))
      (fail 'textual>?:unicode))
  (or (equal? #t (textual>? (as-text eszett) "z"))
      (fail 'textual>?:unicode))
  (or (equal? #f (textual>=? "z" (as-text eszett)))
      (fail 'textual>=?:unicode))
  (or (equal? #t (textual>=? (as-text eszett) "z"))
      (fail 'textual>=?:unicode))
  (or (textual-ci=? fuss "Fuss")
      (fail 'textual-ci=?:unicode))
  (or (textual-ci=? fuss "FUSS")
      (fail 'textual-ci=?:unicode))
  (or (textual-ci=? chaos0 chaos1 chaos2)
      (fail 'textual-ci=?:chaos)))
 (else))


;;; Prefixes and suffixes

(or (= 0 (textual-prefix-length ABC ABCDEF))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length ABCDEF ABC))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length ABCDEF DEFABC))
    (fail 'textual-prefix-length))

(or (= 6 (textual-prefix-length DEFABC DEFABC))
    (fail 'textual-prefix-length))

(or (= 6 (textual-prefix-length (textual->string DEFABC) DEFABC))
    (fail 'textual-prefix-length))

(or (= 6 (textual-prefix-length DEFABC (textual->string DEFABC)))
    (fail 'textual-prefix-length))

(or (= 6 (textual-prefix-length (textual->string DEFABC)
                                (textual->string DEFABC)))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "") (as-text "")))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "") (as-text "aabbccddee")))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "aisle") (as-text "")))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "") (as-text "aabbccddee")))
    (fail 'textual-prefix-length))

(or (= 1 (textual-prefix-length (as-text "aisle") (as-text "aabbccddee")))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "bail") (as-text "aabbccddee")))
    (fail 'textual-prefix-length))

(or (= 4 (textual-prefix-length (as-text "prefix") (as-text "preface")))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "") (as-text "") 0))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "") (as-text "aabbccddee") 0))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "aisle") (as-text "") 0))
    (fail 'textual-prefix-length))

(or (= 1 (textual-prefix-length (as-text "aisle") (as-text "aabbccddee") 0))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "bail") (as-text "aabbccddee") 0))
    (fail 'textual-prefix-length))

(or (= 4 (textual-prefix-length (as-text "prefix") (as-text "preface") 0))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "aisle") (as-text "") 1))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "aisle") (as-text "aabbccddee") 1))
    (fail 'textual-prefix-length))

(or (= 1 (textual-prefix-length (as-text "bail") (as-text "aabbccddee") 1))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "prefix") (as-text "preface") 1))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "") (as-text "") 0 0))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "") (as-text "aabbccddee") 0 0))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "aisle") (as-text "") 0 4))
    (fail 'textual-prefix-length))

(or (= 1 (textual-prefix-length (as-text "aisle") (as-text "aabbccddee") 0 4))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "bail") (as-text "aabbccddee") 0 1))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "aisle") (as-text "") 1 4))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "aisle") (as-text "aabbccddee") 1 4))
    (fail 'textual-prefix-length))

(or (= 1 (textual-prefix-length (as-text "bail") (as-text "aabbccddee") 1 4))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "prefix") (as-text "preface") 1 5))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "") (as-text "") 0 0 0))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "") (as-text "aabbccddee") 0 0 0))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "aisle") (as-text "") 0 4 0))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "aisle") (as-text "aabbccddee") 0 4 2))
    (fail 'textual-prefix-length))

(or (= 1 (textual-prefix-length (as-text "bail") (as-text "aabbccddee") 0 1 2))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "prefix") (as-text "preface") 0 5 1))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "aisle") (as-text "") 1 4 0))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "aisle") (as-text "aabbccddee") 1 4 3))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "bail") (as-text "aabbccddee") 1 4 3))
    (fail 'textual-prefix-length))

(or (= 3 (textual-prefix-length (as-text "prefix") (as-text "preface") 1 5 1))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "") (as-text "") 0 0 0 0))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "") (as-text "aabbccddee") 0 0 0 0))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "aisle") (as-text "") 0 4 0 0))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "aisle") "aabbccddee" 0 4 2 10))
    (fail 'textual-prefix-length))

(or (= 1 (textual-prefix-length (as-text "bail") (as-text "aabbccddee") 0 1 2 10))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "prefix") (as-text "preface") 0 5 1 6))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "aisle") (as-text "") 1 4 0 0))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "aisle") (as-text "aabbccddee") 1 4 3 3))
    (fail 'textual-prefix-length))

(or (= 0 (textual-prefix-length (as-text "bail") (as-text "aabbccddee") 1 4 3 6))
    (fail 'textual-prefix-length))

(or (= 3 (textual-prefix-length (as-text "prefix") (as-text "preface") 1 5 1 7))
    (fail 'textual-prefix-length))


(or (= 0 (textual-suffix-length ABC ABCDEF))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length ABCDEF ABC))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length ABCDEF DEFABC))
    (fail 'textual-suffix-length))

(or (= 6 (textual-suffix-length DEFABC DEFABC))
    (fail 'textual-suffix-length))

(or (= 6 (textual-suffix-length (textual->string DEFABC) DEFABC))
    (fail 'textual-suffix-length))

(or (= 6 (textual-suffix-length DEFABC (textual->string DEFABC)))
    (fail 'textual-suffix-length))

(or (= 6 (textual-suffix-length (textual->string DEFABC) (textual->string DEFABC)))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "") (as-text "")))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "") (as-text "aabbccddee")))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "aisle") (as-text "")))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "") (as-text "aabbccddee")))
    (fail 'textual-suffix-length))

(or (= 1 (textual-suffix-length (as-text "aisle") (as-text "aabbccddee")))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "bail") (as-text "aabbccddee")))
    (fail 'textual-suffix-length))

(or (= 3 (textual-suffix-length (as-text "place") (as-text "preface")))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "") (as-text "") 0))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "") (as-text "aabbccddee") 0))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "aisle") (as-text "") 0))
    (fail 'textual-suffix-length))

(or (= 1 (textual-suffix-length (as-text "aisle") (as-text "aabbccddee") 0))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "bail") (as-text "aabbccddee") 0))
    (fail 'textual-suffix-length))

(or (= 3 (textual-suffix-length (as-text "place") (as-text "preface") 0))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "aisle") (as-text "") 1))
    (fail 'textual-suffix-length))

(or (= 1 (textual-suffix-length (as-text "aisle") (as-text "aabbccddee") 1))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "bail") (as-text "aabbccddee") 1))
    (fail 'textual-suffix-length))

(or (= 3 (textual-suffix-length (as-text "place") (as-text "preface") 1))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "") (as-text "") 0 0))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "") (as-text "aabbccddee") 0 0))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "aisle") (as-text "") 0 4))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "aisle") (as-text "aabbccddee") 0 4))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "bail") (as-text "aabbccddee") 0 1))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "aisle") (as-text "") 1 4))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "aisle") (as-text "aabbccddee") 1 4))
    (fail 'textual-suffix-length))

(or (= 1 (textual-suffix-length (as-text "aisle") (as-text "aabbccddee") 1 5))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "bail") (as-text "aabbccddee") 1 4))
    (fail 'textual-suffix-length))

(or (= 3 (textual-suffix-length (as-text "place") (as-text "preface") 1 5))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "") (as-text "") 0 0 0))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "") (as-text "aabbccddee") 0 0 0))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "aisle") (as-text "") 0 4 0))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "aisle") (as-text "aabbccddee") 0 4 2))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "bail") (as-text "aabbccddee") 0 1 2))
    (fail 'textual-suffix-length))

(or (= 3 (textual-suffix-length (as-text "place") (as-text "preface") 0 5 1))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "aisle") (as-text "") 1 4 0))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "aisle") (as-text "aabbccddee") 1 4 3))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "bail") (as-text "aabbccddee") 1 4 3))
    (fail 'textual-suffix-length))

(or (= 3 (textual-suffix-length (as-text "place") (as-text "preface") 1 5 1))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "") (as-text "") 0 0 0 0))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "") (as-text "aabbccddee") 0 0 0 0))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "aisle") (as-text "") 0 4 0 0))
    (fail 'textual-suffix-length))

(or (= 1 (textual-suffix-length "aisle" (as-text "aabbccddee") 0 5 2 10))
    (fail 'textual-suffix-length))

(or (= 1 (textual-suffix-length (as-text "bail") (as-text "aabbccddee") 0 1 2 4))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "place") (as-text "preface") 0 5 1 6))
    (fail 'textual-suffix-length))

(or (= 2 (textual-suffix-length (as-text "place") (as-text "preface") 0 4 1 6))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "aisle") (as-text "") 1 4 0 0))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "aisle") (as-text "aabbccddee") 1 4 3 3))
    (fail 'textual-suffix-length))

(or (= 0 (textual-suffix-length (as-text "bail") (as-text "aabbccddee") 1 4 3 6))
    (fail 'textual-suffix-length))

(or (= 3 (textual-suffix-length (as-text "place") (as-text "preface") 1 5 1 7))
    (fail 'textual-suffix-length))


(or (eq? #f (textual-prefix? ABC ABCDEF))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? ABCDEF ABC))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? ABCDEF DEFABC))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? DEFABC DEFABC))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (textual->string DEFABC) DEFABC))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? DEFABC (textual->string DEFABC)))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (textual->string DEFABC) (textual->string DEFABC)))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "") (as-text "")))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "") (as-text "abc")))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "a") (as-text "abc")))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "c") (as-text "abc")))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "ab") (as-text "abc")))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "ac") (as-text "abc")))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "abc") (as-text "abc")))
    (fail 'textual-prefix?))

(or (eq? #f (textual-suffix? ABC ABCDEF))
    (fail 'textual-suffix?))

(or (eq? #f (textual-suffix? ABCDEF ABC))
    (fail 'textual-suffix?))

(or (eq? #f (textual-suffix? ABCDEF DEFABC))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? DEFABC DEFABC))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (textual->string DEFABC) DEFABC))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? DEFABC (textual->string DEFABC)))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "") (as-text "")))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "") (as-text "abc")))
    (fail 'textual-suffix?))

(or (eq? #f (textual-suffix? (as-text "a") (as-text "abc")))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "c") (as-text "abc")))
    (fail 'textual-suffix?))

(or (eq? #f (textual-suffix? (as-text "ac") (as-text "abc")))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "bc") (as-text "abc")))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "abc") (as-text "abc")))
    (fail 'textual-suffix?))

(or (eq? #t (textual-prefix? (as-text "") (as-text "") 0))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "") (as-text "abc") 0))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "a") (as-text "abc") 0))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "c") (as-text "abc") 0))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "ab") (as-text "abc") 0))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "ac") (as-text "abc") 0))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "abc") (as-text "abc") 0))
    (fail 'textual-prefix?))

(or (eq? #t (textual-suffix? (as-text "") (as-text "") 0))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "") (as-text "abc") 0))
    (fail 'textual-suffix?))

(or (eq? #f (textual-suffix? (as-text "a") (as-text "abc") 0))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "c") (as-text "abc") 0))
    (fail 'textual-suffix?))

(or (eq? #f (textual-suffix? (as-text "ac") (as-text "abc") 0))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "bc") (as-text "abc") 0))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "abc") (as-text "abc") 0))
    (fail 'textual-suffix?))

(or (eq? #t (textual-prefix? (as-text "ab") (as-text "abc") 2))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "ac") (as-text "abc") 2))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "abc") (as-text "abc") 2))
    (fail 'textual-prefix?))

(or (eq? #t (textual-suffix? (as-text "ac") (as-text "abc") 2))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "bc") (as-text "abc") 2))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "abc") (as-text "abc") 2))
    (fail 'textual-suffix?))


(or (eq? #t (textual-prefix? (as-text "") (as-text "") 0 0))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "") (as-text "abc") 0 0))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "a") (as-text "abc") 0 0))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "c") (as-text "abc") 0 1))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "ab") (as-text "abc") 0 1))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "ab") (as-text "abc") 0 2))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "ac") (as-text "abc") 0 2))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "abc") (as-text "abc") 0 3))
    (fail 'textual-prefix?))

(or (eq? #t (textual-suffix? (as-text "") (as-text "") 0 0))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "") (as-text "abc") 0 0))
    (fail 'textual-suffix?))

(or (eq? #f (textual-suffix? (as-text "a") (as-text "abc") 0 1))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "c") (as-text "abc") 0 1))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "ac") (as-text "abc") 1 2))
    (fail 'textual-suffix?))

(or (eq? #f (textual-suffix? (as-text "ac") (as-text "abc") 0 2))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "bc") (as-text "abc") 0 2))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "abc") (as-text "abc") 0 3))
    (fail 'textual-suffix?))

(or (eq? #t (textual-prefix? (as-text "ab") (as-text "abc") 2 2))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "ac") (as-text "abc") 2 2))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "abc") (as-text "abc") 2 3))
    (fail 'textual-prefix?))

(or (eq? #t (textual-suffix? (as-text "ac") (as-text "abc") 2 2))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "bc") (as-text "abc") 2 2))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "abc") (as-text "abc") 2 3))
    (fail 'textual-suffix?))


(or (eq? #t (textual-prefix? (as-text "") (as-text "") 0 0 0))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "") (as-text "abc") 0 0 0))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "a") (as-text "abc") 0 0 0))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "c") (as-text "abc") 0 1 0))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "ab") (as-text "abc") 0 1 0))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "ab") (as-text "abc") 0 2 0))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "ac") (as-text "abc") 0 2 0))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "abc") (as-text "abc") 0 3 0))
    (fail 'textual-prefix?))

(or (eq? #t (textual-suffix? (as-text "") (as-text "") 0 0 0))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "") (as-text "abc") 0 0 0))
    (fail 'textual-suffix?))

(or (eq? #f (textual-suffix? (as-text "a") (as-text "abc") 0 1 0))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "c") (as-text "abc") 0 1 0))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "ac") (as-text "abc") 1 2 0))
    (fail 'textual-suffix?))

(or (eq? #f (textual-suffix? (as-text "ac") (as-text "abc") 0 2 0))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "bc") (as-text "abc") 0 2 0))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "abc") (as-text "abc") 0 3 0))
    (fail 'textual-suffix?))

(or (eq? #t (textual-prefix? (as-text "ab") (as-text "abc") 2 2 0))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "ac") (as-text "abc") 2 2 0))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "abc") (as-text "abc") 2 3 0))
    (fail 'textual-prefix?))

(or (eq? #t (textual-suffix? (as-text "ac") (as-text "abc") 2 2 0))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "bc") (as-text "abc") 2 2 0))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "abc") (as-text "abc") 2 3 0))
    (fail 'textual-suffix?))

(or (eq? #t (textual-prefix? (as-text "") (as-text "abc") 0 0 1))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "a") (as-text "abc") 0 0 1))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "c") (as-text "abc") 0 1 2))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "ab") (as-text "abc") 0 1 2))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "ab") (as-text "abc") 0 2 1))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "ac") (as-text "abc") 0 2 1))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "abc") (as-text "abc") 0 3 1))
    (fail 'textual-prefix?))

(or (eq? #f (textual-suffix? (as-text "a") (as-text "abc") 0 1 2))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "c") (as-text "abc") 0 1 1))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "ac") (as-text "abc") 1 2 2))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "bc") (as-text "abc") 0 2 1))
    (fail 'textual-suffix?))

(or (eq? #f (textual-suffix? (as-text "bc") (as-text "abc") 0 2 2))
    (fail 'textual-suffix?))


(or (eq? #t (textual-prefix? (as-text "") (as-text "") 0 0 0 0))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "") (as-text "abc") 0 0 0 3))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "a") (as-text "abc") 0 0 0 3))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "c") (as-text "abc") 0 1 0 3))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "ab") (as-text "abc") 0 1 0 3))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "ab") (as-text "abc") 0 2 0 3))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "ac") (as-text "abc") 0 2 0 3))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "abc") (as-text "abc") 0 3 0 3))
    (fail 'textual-prefix?))

(or (eq? #t (textual-suffix? (as-text "") (as-text "abc") 0 0 0 3))
    (fail 'textual-suffix?))

(or (eq? #f (textual-suffix? (as-text "a") (as-text "abc") 0 1 0 3))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "c") (as-text "abc") 0 1 0 3))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "ac") (as-text "abc") 1 2 0 3))
    (fail 'textual-suffix?))

(or (eq? #f (textual-suffix? (as-text "ac") (as-text "abc") 0 2 0 3))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "bc") (as-text "abc") 0 2 0 3))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "abc") (as-text "abc") 0 3 0 3))
    (fail 'textual-suffix?))

(or (eq? #t (textual-prefix? (as-text "ab") (as-text "abc") 2 2 0 3))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "ac") (as-text "abc") 2 2 0 3))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "abc") (as-text "abc") 2 3 0 3))
    (fail 'textual-prefix?))

(or (eq? #t (textual-suffix? (as-text "ac") (as-text "abc") 2 2 0 3))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "bc") (as-text "abc") 2 2 0 3))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "abc") (as-text "abc") 2 3 0 3))
    (fail 'textual-suffix?))

(or (eq? #t (textual-prefix? (as-text "") (as-text "abc") 0 0 1 3))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "a") (as-text "abc") 0 0 1 3))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "c") (as-text "abc") 0 1 2 3))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "ab") (as-text "abc") 0 1 2 3))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "ab") (as-text "abc") 0 2 1 3))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "ac") (as-text "abc") 0 2 1 3))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "abc") (as-text "abc") 0 3 1 3))
    (fail 'textual-prefix?))

(or (eq? #f (textual-suffix? (as-text "a") (as-text "abc") 0 1 2 3))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "c") (as-text "abc") 0 1 1 3))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "ac") (as-text "abc") 1 2 2 3))
    (fail 'textual-suffix?))

(or (eq? #t (textual-suffix? (as-text "bc") (as-text "abc") 0 2 1 3))
    (fail 'textual-suffix?))

(or (eq? #f (textual-suffix? (as-text "bc") (as-text "abc") 0 2 2 3))
    (fail 'textual-suffix?))


(or (eq? #t (textual-prefix? (as-text "") (as-text "abc") 0 0 0 2))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "a") (as-text "abc") 0 0 0 2))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "c") (as-text "abc") 0 1 0 2))
    (fail 'textual-prefix?))

(or (eq? #t (textual-prefix? (as-text "ab") (as-text "abc") 0 1 0 2))
    (fail 'textual-prefix?))

(or (eq? #f (textual-prefix? (as-text "abc") (as-text "abc") 0 3 0 2))
    (fail 'textual-prefix?))

(or (eq? #t (textual-suffix? (as-text "") (as-text "abc") 0 0 0 2))
    (fail 'textual-suffix?))

(or (eq? #f (textual-suffix? (as-text "c") (as-text "abc") 0 1 0 2))
    (fail 'textual-suffix?))

(or (eq? #f (textual-suffix? (as-text "ac") (as-text "abc") 1 2 0 2))
    (fail 'textual-suffix?))


;;; Searching

(or (eqv? #f (textual-index (as-text "") char?))
    (fail 'textual-index))

(or (eqv? 0 (textual-index (as-text "abcdef") char?))
    (fail 'textual-index))

(or (eqv? 4 (textual-index (as-text "abcdef") (lambda (c) (char>? c #\d))))
    (fail 'textual-index))

(or (eqv? #f (textual-index (as-text "abcdef") char-whitespace?))
    (fail 'textual-index))

(or (eqv? #f (textual-index-right (as-text "") char?))
    (fail 'textual-index-right))

(or (eqv? 5 (textual-index-right (as-text "abcdef") char?))
    (fail 'textual-index-right))

(or (eqv? 5 (textual-index-right (as-text "abcdef")
                                 (lambda (c) (char>? c #\d))))
    (fail 'textual-index-right))


(or (eqv? #f (textual-index-right (as-text "abcdef") char-whitespace?))
    (fail 'textual-index-right))

(or (eqv? #f (textual-skip (as-text "") string?))
    (fail 'textual-skip))

(or (eqv? 0 (textual-skip (as-text "abcdef") string?))
    (fail 'textual-skip))

(or (eqv? 4 (textual-skip (as-text "abcdef") (lambda (c) (char<=? c #\d))))
    (fail 'textual-skip))

(or (eqv? #f (textual-skip (as-text "abcdef") char?))
    (fail 'textual-skip))

(or (eqv? #f (textual-skip-right (as-text "") string?))
    (fail 'textual-skip-right))

(or (eqv? 5 (textual-skip-right (as-text "abcdef") string?))
    (fail 'textual-skip-right))

(or (eqv? 5 (textual-skip-right (as-text "abcdef")
                                (lambda (c) (char<=? c #\d))))
    (fail 'textual-skip-right))

(or (eqv? #f (textual-skip-right (as-text "abcdef") char?))
    (fail 'textual-skip-right))


(or (eqv? 2 (textual-index "abcdef" char? 2))
    (fail 'textual-index))

(or (eqv? 4 (textual-index "abcdef" (lambda (c) (char>? c #\d)) 2))
    (fail 'textual-index))

(or (eqv? #f (textual-index "abcdef" char-whitespace? 2))
    (fail 'textual-index))

(or (eqv? 5 (textual-index-right "abcdef" char? 2))
    (fail 'textual-index-right))

(or (eqv? 5 (textual-index-right "abcdef"
                                 (lambda (c)
                                   (char>? c #\d)) 2))
    (fail 'textual-index-right))

(or (eqv? #f (textual-index-right "abcdef" char-whitespace? 2))
    (fail 'textual-index-right))

(or (eqv? 2 (textual-skip "abcdef" string? 2))
    (fail 'textual-skip))

(or (eqv? 4 (textual-skip "abcdef"
                          (lambda (c)
                            (char<=? c #\d)) 2))
    (fail 'textual-skip))

(or (eqv? #f (textual-skip "abcdef" char? 2))
    (fail 'textual-skip))

(or (eqv? 5 (textual-skip-right "abcdef" string? 2))
    (fail 'textual-skip-right))

(or (eqv? 5 (textual-skip-right "abcdef"
                                (lambda (c)
                                  (char<=? c #\d)) 2))
    (fail 'textual-skip-right))

(or (eqv? #f (textual-skip-right "abcdef" char? 2))
    (fail 'textual-skip-right))


(or (eqv? 2 (textual-index (as-text "abcdef") char? 2 5))
    (fail 'textual-index))

(or (eqv? 4 (textual-index (as-text "abcdef")
                           (lambda (c) (char>? c #\d)) 2 5))
    (fail 'textual-index))

(or (eqv? #f (textual-index (as-text "abcdef") char-whitespace? 2 5))
    (fail 'textual-index))

(or (eqv? 4 (textual-index-right (as-text "abcdef") char? 2 5))
    (fail 'textual-index-right))

(or (eqv? 4 (textual-index-right (as-text "abcdef")
                                 (lambda (c)
                                   (char>? c #\d)) 2 5))
    (fail 'textual-index-right))

(or (eqv? #f (textual-index-right (as-text "abcdef")
                                  char-whitespace? 2 5))
    (fail 'textual-index-right))


(or (eqv? 2 (textual-skip (as-text "abcdef") string? 2 5))
    (fail 'textual-skip))

(or (eqv? 4 (textual-skip (as-text "abcdef")
                          (lambda (c) (char<=? c #\d)) 2 5))
    (fail 'textual-skip))

(or (eqv? #f (textual-skip (as-text "abcdef") char? 2 5))
    (fail 'textual-skip))

(or (eqv? 4 (textual-skip-right (as-text "abcdef") string? 2 5))
    (fail 'textual-skip-right))

(or (eqv? 4 (textual-skip-right (as-text "abcdef")
                                (lambda (c)
                                  (char<=? c #\d)) 2 5))
    (fail 'textual-skip-right))

(or (eqv? #f (textual-skip-right (as-text "abcdef") char? 2 5))
    (fail 'textual-skip-right))


(or (eqv? 0 (textual-contains (as-text "") (as-text "")))
    (fail 'textual-contains))

(or (eqv? 0 (textual-contains (as-text "abcdeffffoo") (as-text "")))
    (fail 'textual-contains))

(or (eqv? 0 (textual-contains (as-text "abcdeffffoo") (as-text "a")))
    (fail 'textual-contains))

(or (eqv? 5 (textual-contains (as-text "abcdeffffoo") (as-text "ff")))
    (fail 'textual-contains))

(or (eqv? 4 (textual-contains (as-text "abcdeffffoo") (as-text "eff")))
    (fail 'textual-contains))

(or (eqv? 8 (textual-contains (as-text "abcdeffffoo") (as-text "foo")))
    (fail 'textual-contains))

(or (eqv? #f (textual-contains (as-text "abcdeffffoo") (as-text "efffoo")))
    (fail 'textual-contains))

(or (eqv? 0 (textual-contains-right (as-text "") (as-text "")))
    (fail 'textual-contains-right))

(or (eqv? 11 (textual-contains-right (as-text "abcdeffffoo") (as-text "")))
    (fail 'textual-contains-right))

(or (eqv? 0 (textual-contains-right (as-text "abcdeffffoo") (as-text "a")))
    (fail 'textual-contains-right))

(or (eqv? 7 (textual-contains-right (as-text "abcdeffffoo") (as-text "ff")))
    (fail 'textual-contains-right))

(or (eqv? 4 (textual-contains-right (as-text "abcdeffffoo") (as-text "eff")))
    (fail 'textual-contains-right))

(or (eqv? 8 (textual-contains-right (as-text "abcdeffffoo") (as-text "foo")))
    (fail 'textual-contains-right))

(or (eqv? #f (textual-contains-right (as-text "abcdeffffoo")
                                     (as-text "efffoo")))
    (fail 'textual-contains-right))


(or (eqv? 0 (textual-contains "" "" 0))
    (fail 'textual-contains))

(or (eqv? 2 (textual-contains "abcdeffffoo" "" 2))
    (fail 'textual-contains))

(or (eqv? #f (textual-contains "abcdeffffoo" "a" 2))
    (fail 'textual-contains))

(or (eqv? 5 (textual-contains "abcdeffffoo" "ff" 2))
    (fail 'textual-contains))

(or (eqv? 4 (textual-contains "abcdeffffoo" "eff" 2))
    (fail 'textual-contains))

(or (eqv? 8 (textual-contains "abcdeffffoo" "foo" 2))
    (fail 'textual-contains))

(or (eqv? #f (textual-contains "abcdeffffoo" "efffoo" 2))
    (fail 'textual-contains))

(or (eqv? 0 (textual-contains-right "" "" 0))
    (fail 'textual-contains-right))

(or (eqv? 11 (textual-contains-right "abcdeffffoo" "" 2))
    (fail 'textual-contains-right))

(or (eqv? #f (textual-contains-right "abcdeffffoo" "a" 2))
    (fail 'textual-contains-right))

(or (eqv? 7 (textual-contains-right "abcdeffffoo" "ff" 2))
    (fail 'textual-contains-right))

(or (eqv? 4 (textual-contains-right "abcdeffffoo" "eff" 2))
    (fail 'textual-contains-right))

(or (eqv? 8 (textual-contains-right "abcdeffffoo" "foo" 2))
    (fail 'textual-contains-right))

(or (eqv? #f (textual-contains-right "abcdeffffoo" "efffoo" 2))
    (fail 'textual-contains-right))


(or (eqv? 0 (textual-contains (as-text "") (as-text "") 0 0))
    (fail 'textual-contains))

(or (eqv? 2 (textual-contains (as-text "abcdeffffoo") (as-text "") 2 10))
    (fail 'textual-contains))

(or (eqv? #f (textual-contains (as-text "abcdeffffoo") (as-text "a") 2 10))
    (fail 'textual-contains))

(or (eqv? 5 (textual-contains (as-text "abcdeffffoo") (as-text "ff") 2 10))
    (fail 'textual-contains))

(or (eqv? 4 (textual-contains (as-text "abcdeffffoo") (as-text "eff") 2 10))
    (fail 'textual-contains))

(or (eqv? #f (textual-contains (as-text "abcdeffffoo") (as-text "foo") 2 10))
    (fail 'textual-contains))

(or (eqv? #f (textual-contains (as-text "abcdeffffoo") (as-text "efffoo") 2 10))
    (fail 'textual-contains))

(or (eqv? 0 (textual-contains-right (as-text "") (as-text "") 0 0))
    (fail 'textual-contains-right))

(or (eqv? 10 (textual-contains-right (as-text "abcdeffffoo") (as-text "") 2 10))
    (fail 'textual-contains-right))

(or (eqv? #f (textual-contains-right (as-text "abcdeffffoo") (as-text "a") 2 10))
    (fail 'textual-contains-right))

(or (eqv? 7 (textual-contains-right (as-text "abcdeffffoo") (as-text "ff") 2 10))
    (fail 'textual-contains-right))

(or (eqv? 4 (textual-contains-right (as-text "abcdeffffoo") (as-text "eff") 2 10))
    (fail 'textual-contains-right))

(or (eqv? #f (textual-contains-right (as-text "abcdeffffoo") "foo" 2 10))
    (fail 'textual-contains-right))

(or (eqv? #f (textual-contains-right "abcdeffffoo" (as-text "efffoo") 2 10))
    (fail 'textual-contains-right))


(or (eqv? 0 (textual-contains "" "" 0 0 0))
    (fail 'textual-contains))

(or (eqv? 2 (textual-contains "abcdeffffoo" "" 2 10 0))
    (fail 'textual-contains))

(or (eqv? 2 (textual-contains "abcdeffffoo" "a" 2 10 1))
    (fail 'textual-contains))

(or (eqv? 5 (textual-contains "abcdeffffoo" "ff" 2 10 1))
    (fail 'textual-contains))

(or (eqv? 5 (textual-contains "abcdeffffoo" "eff" 2 10 1))
    (fail 'textual-contains))

(or (eqv? #f (textual-contains "abcdeffffoo" "foo" 2 10 1))
    (fail 'textual-contains))

(or (eqv? #f (textual-contains "abcdeffffoo" "efffoo" 2 10 1))
    (fail 'textual-contains))

(or (eqv? 0 (textual-contains-right "" "" 0 0 0))
    (fail 'textual-contains-right))

(or (eqv? 10 (textual-contains-right "abcdeffffoo" "" 2 10 0))
    (fail 'textual-contains-right))

(or (eqv? 10 (textual-contains-right "abcdeffffoo" "a" 2 10 1))
    (fail 'textual-contains-right))

(or (eqv? 8 (textual-contains-right "abcdeffffoo" "ff" 2 10 1))
    (fail 'textual-contains-right))

(or (eqv? 7 (textual-contains-right "abcdeffffoo" "eff" 2 10 1))
    (fail 'textual-contains-right))

(or (eqv? #f (textual-contains-right "abcdeffffoo" "foo" 2 10 1))
    (fail 'textual-contains-right))

(or (eqv? #f (textual-contains-right "abcdeffffoo" "efffoo" 2 10 1))
    (fail 'textual-contains-right))


(or (eqv? 0 (textual-contains "" "" 0 0 0 0))
    (fail 'textual-contains))

(or (eqv? 2 (textual-contains "abcdeffffoo" "" 2 10 0 0))
    (fail 'textual-contains))

(or (eqv? 2 (textual-contains "abcdeffffoo" "a" 2 10 1 1))
    (fail 'textual-contains))

(or (eqv? 5 (textual-contains "abcdeffffoo" "ff" 2 10 1 2))
    (fail 'textual-contains))

(or (eqv? 5 (textual-contains "abcdeffffoo" "eff" 2 10 1 2))
    (fail 'textual-contains))

(or (eqv? 9 (textual-contains "abcdeffffoo" "foo" 2 10 1 2))
    (fail 'textual-contains))

(or (eqv? 4 (textual-contains "abcdeffffoo" "efffoo" 2 10 0 2))
    (fail 'textual-contains))

(or (eqv? 0 (textual-contains-right "" "" 0 0 0 0))
    (fail 'textual-contains-right))

(or (eqv? 10 (textual-contains-right "abcdeffffoo" "" 2 10 0 0))
    (fail 'textual-contains-right))

(or (eqv? 10 (textual-contains-right "abcdeffffoo" "a" 2 10 1 1))
    (fail 'textual-contains-right))

(or (eqv? 8  (textual-contains-right "abcdeffffoo" "ff" 2 10 1 2))
    (fail 'textual-contains-right))

(or (eqv? 8 (textual-contains-right "abcdeffffoo" "eff" 2 10 1 2))
    (fail 'textual-contains-right))

(or (eqv? 9 (textual-contains-right "abcdeffffoo" "foo" 2 10 1 2))
    (fail 'textual-contains-right))

(or (eqv? 7 (textual-contains-right "abcdeffffoo" "efffoo" 2 10 1 3))
    (fail 'textual-contains-right))


;;; Case conversion

;;; FIXME: should test some non-ASCII cases here.

(or (result=? "1234STRIKES" (textual-upcase (as-text "1234Strikes")))
    (fail 'textual-upcase))

(or (result=? "1234STRIKES" (textual-upcase (as-text "1234strikes")))
    (fail 'textual-upcase))

(or (result=? "1234STRIKES" (textual-upcase (as-text "1234STRIKES")))
    (fail 'textual-upcase))

(or (result=? "1234strikes" (textual-downcase (as-text "1234Strikes")))
    (fail 'textual-downcase))

(or (result=? "1234strikes" (textual-downcase (as-text "1234strikes")))
    (fail 'textual-downcase))

(or (result=? "1234strikes" (textual-downcase (as-text "1234STRIKES")))
    (fail 'textual-downcase))

(or (result=? "1234strikes" (textual-foldcase (as-text "1234Strikes")))
    (fail 'textual-foldcase))

(or (result=? "1234strikes" (textual-foldcase (as-text "1234strikes")))
    (fail 'textual-foldcase))

(or (result=? "1234strikes" (textual-foldcase (as-text "1234STRIKES")))
    (fail 'textual-foldcase))

(or (result=? "And With Three Strikes You Are Out"
              (textual-titlecase
               (as-text "and with THREE STRIKES you are oUT")))
    (fail 'textual-titlecase))

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
