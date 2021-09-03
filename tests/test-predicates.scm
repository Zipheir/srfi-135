;;; Predicates

(test-group "Predicates"
  (test-assert (text? (text)))

  (test-assert (not (text? #\a)))

  (test-assert (textual? (text)))

  (test-assert (textual? (string)))

  (test-assert (not (textual? #\a)))

  (test-assert (textual-null? (text)))

  (test-assert (not (textual-null? ABC)))

  ;;; textual-every & textual-any

  (test #t (textual-every (lambda (c) (if (char? c) c #f))
                          (text)))

  (test #\c (textual-every (lambda (c) (if (char? c) c #f))
                           (as-text "abc")))

  (test #f (textual-every (lambda (c) (if (char>? c #\b) c #f))
                          (as-text "abc")))

  (test #\c (textual-every (lambda (c) (if (char>? c #\b) c #f))
                           (as-text "abc") 2))

  (test #t (textual-every (lambda (c) (if (char>? c #\b) c #f))
                          (as-text "abc") 1 1))

  (test #f (textual-any (lambda (c) (if (char? c) c #f))
                        (text)))

  (test #\a (textual-any (lambda (c) (if (char? c) c #f))
                         (as-text "abc")))

  (test #\c (textual-any (lambda (c) (if (char>? c #\b) c #f))
                         (as-text "abc")))

  (test #\c (textual-any (lambda (c) (if (char>? c #\b) c #f))
                         (as-text "abc") 2))

  (test #f (textual-any (lambda (c) (if (char>? c #\b) c #f))
                        (as-text "abc") 0 2))


  (test #t (textual-every (lambda (c) (if (char? c) c #f)) ""))

  (test #\c (textual-every (lambda (c) (if (char? c) c #f)) "abc"))

  (test #f (textual-every (lambda (c) (if (char>? c #\b) c #f)) "abc"))

  (test #\c (textual-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 2))

  (test #t (textual-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 1 1))

  (test #f (textual-any (lambda (c) (if (char? c) c #f)) ""))

  (test #\a (textual-any (lambda (c) (if (char? c) c #f)) "abc"))

  (test #\c (textual-any (lambda (c) (if (char>? c #\b) c #f)) "abc"))

  (test #\c (textual-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 2))

  (test #f (textual-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 0 2))
  )
