;;; The comparison tests aren't perfectly black-box because the
;;; specification of these comparison procedures allows them to
;;; use an ordering other than the usual lexicographic ordering.
;;; The sample implementations use lexicographic ordering, however,
;;; and a test program that discourages implementations from using
;;; orderings that differ from the usual on such simple cases is
;;; probably doing a public service.

(test-group "Comparison"
  (test #t (textual=? (as-text "Strasse") (as-text "Strasse")))

  (test #t (textual=? "Strasse" (as-text "Strasse") "Strasse"))

  (test #f (textual<? (as-text "z") (as-text "z")))
  (test #t (textual<? (as-text "z") "zz"))
  (test #f (textual<? (as-text "z") (as-text "Z")))
  (test #t (textual<=? (as-text "z") "zz"))
  (test #f (textual<=? "z" "Z"))
  (test #t (textual<=? "z" (as-text "z")))

  (test #f (textual<? "z" (as-text "z")))
  (test #f (textual>? (as-text "z") "zz"))
  (test #t (textual>? "z" (as-text "Z")))
  (test #f (textual>=? (as-text "z") "zz"))
  (test #t (textual>=? "z" "Z"))
  (test #t (textual>=? (as-text "z") (as-text "z")))


  (let ((w "a")
        (x "abc")
        (y "def")
        (z (text #\a #\b #\c)))

    (test #f (textual=? x y z))
    (test #t (textual=? x x z))
    (test #f (textual=? w x y))
    (test #f (textual=? y x w))

    (test #f (textual<? x y z))
    (test #f (textual<? x x z))
    (test #t (textual<? w x y))
    (test #f (textual<? y x w))

    (test #f (textual>? x y z))
    (test #f (textual>? x x z))
    (test #f (textual>? w x y))
    (test #t (textual>? y x w))

    (test #f (textual<=? x y z))
    (test #t (textual<=? x x z))
    (test #t (textual<=? w x y))
    (test #f (textual<=? y x w))

    (test #f (textual>=? x y z))
    (test #t (textual>=? x x z))
    (test #f (textual>=? w x y))
    (test #t (textual>=? y x w))

    (test #t (textual=? x x))
    (test #f (textual=? w x))
    (test #f (textual=? y x))

    (test #f (textual<? x x))
    (test #t (textual<? w x))
    (test #f (textual<? y x))

    (test #f (textual>? x x))
    (test #f (textual>? w x))
    (test #t (textual>? y x))

    (test #t (textual<=? x x))
    (test #t (textual<=? w x))
    (test #f (textual<=? y x))

    (test #t (textual>=? x x))
    (test #f (textual>=? w x))
    (test #t (textual>=? y x))
    )

  (test #t (textual-ci<? "a" "Z"))
  (test #t (textual-ci<? "A" "z"))
  (test #f (textual-ci<? "Z" "a"))
  (test #f (textual-ci<? "z" "A"))
  (test #f (textual-ci<? "z" "Z"))
  (test #f (textual-ci<? "Z" "z"))
  (test #f (textual-ci>? "a" "Z"))
  (test #f (textual-ci>? "A" "z"))
  (test #t (textual-ci>? "Z" "a"))
  (test #t (textual-ci>? "z" "A"))
  (test #f (textual-ci>? "z" "Z"))
  (test #f (textual-ci>? "Z" "z"))
  (test #t (textual-ci=? "z" "Z"))
  (test #f (textual-ci=? "z" "a"))
  (test #t (textual-ci<=? "a" "Z"))
  (test #t (textual-ci<=? "A" "z"))
  (test #f (textual-ci<=? "Z" "a"))
  (test #f (textual-ci<=? "z" "A"))
  (test #t (textual-ci<=? "z" "Z"))
  (test #t (textual-ci<=? "Z" "z"))
  (test #f (textual-ci>=? "a" "Z"))
  (test #f (textual-ci>=? "A" "z"))
  (test #t (textual-ci>=? "Z" "a"))
  (test #t (textual-ci>=? "z" "A"))
  (test #t (textual-ci>=? "z" "Z"))
  (test #t (textual-ci>=? "Z" "z"))

  ;;; The full-unicode feature doesn't imply full Unicode in strings,
  ;;; so these tests might fail even in a conforming implementation.
  ;;; Implementations that support full Unicode strings often have
  ;;; this feature, however, even though it isn't listed in the R7RS.

  (test #f (textual=? ABCDEF DEFABC))
  (test #f (textual=? DEFABC ABCDEF))
  (test #t (textual=? DEFABC DEFABC))

  (test #f (textual<? ABCDEF DEFABC))
  (test #t (textual<? DEFABC ABCDEF))
  (test #f (textual<? DEFABC DEFABC))

  (test #t (textual>? ABCDEF DEFABC))
  (test #f (textual>? DEFABC ABCDEF))
  (test #f (textual>? DEFABC DEFABC))

  (test #f (textual<=? ABCDEF DEFABC))
  (test #t (textual<=? DEFABC ABCDEF))
  (test #t (textual<=? DEFABC DEFABC))

  (test #t (textual>=? ABCDEF DEFABC))
  (test #f (textual>=? DEFABC ABCDEF))
  (test #t (textual>=? DEFABC DEFABC))

  (test #f (textual=? "Fuss" fuss))
  (test #f (textual=? "Fuss" "Fuss" fuss))
  (test #f (textual=? "Fuss" fuss "Fuss"))
  (test #f (textual=? fuss "Fuss" "Fuss"))
  (test #t (textual<? "z" (as-text eszett)))
  (test #f (textual<? (as-text eszett) "z"))
  (test #t (textual<=? "z" (as-text eszett)))
  (test #f (textual<=? (as-text eszett) "z"))
  (test #f (textual>? "z" (as-text eszett)))
  (test #t (textual>? (as-text eszett) "z"))
  (test #f (textual>=? "z" (as-text eszett)))
  (test #t (textual>=? (as-text eszett) "z"))
  )
