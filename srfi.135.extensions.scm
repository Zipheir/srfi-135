;;; © 2022 Wolfgang Corcoran-Mathe
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

;;; I/O and other additions for SRFI 135.

(module (srfi 135 extensions)
  (textual->generator generator->text text-accumulator
   text-read-line read-text text-read-lines write-textual
   open-input-textual open-output-text get-output-text
   )

  (import scheme
          (chicken base)
          (chicken condition)
          (chicken port)
          (chicken type)
          (only (r7rs) bytevector-length)
          (chicken type)
          (srfi 1)
          (only utf8 read-string write-string)
          (only (utf8-srfi-13) substring/shared)
          (srfi 135 kernel8)
          (srfi 135))

  (define-type text (struct text-rtd))
  (define-type textual (or text string))

  (include "exceptions.scm")

  ;; Check that i is a valid index into t.
  (: %check-index (symbol textual integer -> undefined))
  (define (%check-index loc t i)
    (unless (and (>= i 0) (< i (textual-length t)))
      (bounds-exception loc "index out of bounds" i t)))

  ;; Check that [start, end) defines a valid range of t.
  (define (%check-range loc t start end)
    (unless (<= 0 start end (textual-length t))
      (bounds-exception loc "invalid range" start end t)))

  (include "util.scm")
  (include "135.gen-acc.scm")
  (include "135.io.scm")
)
