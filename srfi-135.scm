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

(module (srfi 135)

  (;; Predicates

   text?                 textual?
   textual-null? 
   textual-every         textual-any

   ;; Constructors

   make-text             text
   text-tabulate
   text-unfold           text-unfold-right

   ;; Conversion

   textual->text
   textual->string       textual->vector      textual->list
   string->text          vector->text         list->text    reverse-list->text
   textual->utf8         textual->utf16be
   textual->utf16        textual->utf16le
   utf8->text            utf16be->text
   utf16->text           utf16le->text

   ;; Selection

   text-length           textual-length
   text-ref              textual-ref
   subtext               subtextual
   textual-copy
   textual-take          textual-take-right
   textual-drop          textual-drop-right
   textual-pad           textual-pad-right 
   textual-trim          textual-trim-right   textual-trim-both

   ;; Replacement

   textual-replace

   ;; Comparison

   textual=?             textual-ci=?
   textual<?             textual-ci<?
   textual>?             textual-ci>?
   textual<=?            textual-ci<=?
   textual>=?            textual-ci>=?

   ;; Prefixes & suffixes

   textual-prefix-length textual-suffix-length
   textual-prefix?       textual-suffix?    

   ;; Searching

   textual-index         textual-index-right
   textual-skip          textual-skip-right
   textual-contains      textual-contains-right

   ;; Case conversion

   textual-upcase        textual-downcase
   textual-foldcase      textual-titlecase

   ;; Concatenation

   textual-append        textual-concatenate  textual-concatenate-reverse
   textual-join

   ;; Fold & map & friends

   textual-fold          textual-fold-right
   textual-map           textual-for-each
   textual-map-index     textual-for-each-index
   textual-count
   textual-filter        textual-remove
;  textual-reverse

   ;; Replication & splitting

   textual-replicate     textual-split
   )

  ;; Don't import non-Unicode-aware base procedures.
  (import (except (scheme) string-length string-ref string-set!
                           make-string string substring string->list
                           list->string string-fill! write-char
                           read-char display vector->list char<=?
                           )
          (only (r7rs) make-bytevector bytevector? bytevector-u8-set!
                       bytevector-length bytevector-u8-ref utf8->string
                       string->utf8 make-list exact-integer? vector->list
                       char<=?
                       )
          (only (chicken base) define-record-type error include case-lambda
                               set-record-printer!)
          (only (chicken io) write-string)
          (chicken type)
          (only (srfi 1) last-pair take)
          (only (srfi 141) euclidean-remainder)
          typed-records
          (utf8)
          (only (utf8-srfi-13) string-upcase string-downcase
                               string-titlecase string-copy!)
          (only (utf8-case-map) char-downcase-single))

  (define-type bytevector u8vector)

  (define (assertion-violation procname msg . irritants)
    (apply error msg irritants))

  ;; CHICKEN: We need a Unicode-aware foldcase, so this can't be
  ;; imported from the r7rs egg.
  (define (string-foldcase s) (string-downcase s))
  (define (char-foldcase c) (char-downcase-single c))

  (include "kernel8.body.scm")

  (define-type text text-rtd)
  (define-type textual (or text string))

  (set-record-printer!
   text-rtd
   (lambda (text port)
     (write-string "«" port)
     (write-string (textual->string text) port)
     (write-string "»" port)))

  (include "135.body.scm"))

;;; eof
