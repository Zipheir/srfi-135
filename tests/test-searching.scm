(test-group "Searching"
  (test #f (textual-index (as-text "") char?))

  (test 0 (textual-index (as-text "abcdef") char?))

  (test 4 (textual-index (as-text "abcdef") (lambda (c) (char>? c #\d))))

  (test #f (textual-index (as-text "abcdef") char-whitespace?))

  (test #f (textual-index-right (as-text "") char?))

  (test 5 (textual-index-right (as-text "abcdef") char?))

  (test 5 (textual-index-right (as-text "abcdef")
                                   (lambda (c) (char>? c #\d))))


  (test #f (textual-index-right (as-text "abcdef") char-whitespace?))

  (test #f (textual-skip (as-text "") string?))

  (test 0 (textual-skip (as-text "abcdef") string?))

  (test 4 (textual-skip (as-text "abcdef") (lambda (c) (char<=? c #\d))))

  (test #f (textual-skip (as-text "abcdef") char?))

  (test #f (textual-skip-right (as-text "") string?))

  (test 5 (textual-skip-right (as-text "abcdef") string?))

  (test 5 (textual-skip-right (as-text "abcdef")
                                  (lambda (c) (char<=? c #\d))))

  (test #f (textual-skip-right (as-text "abcdef") char?))


  (test 2 (textual-index "abcdef" char? 2))

  (test 4 (textual-index "abcdef" (lambda (c) (char>? c #\d)) 2))

  (test #f (textual-index "abcdef" char-whitespace? 2))

  (test 5 (textual-index-right "abcdef" char? 2))

  (test 5 (textual-index-right "abcdef"
                                   (lambda (c)
                                     (char>? c #\d)) 2))

  (test #f (textual-index-right "abcdef" char-whitespace? 2))

  (test 2 (textual-skip "abcdef" string? 2))

  (test 4 (textual-skip "abcdef"
                            (lambda (c)
                              (char<=? c #\d)) 2))

  (test #f (textual-skip "abcdef" char? 2))

  (test 5 (textual-skip-right "abcdef" string? 2))

  (test 5 (textual-skip-right "abcdef"
                                  (lambda (c)
                                    (char<=? c #\d)) 2))

  (test #f (textual-skip-right "abcdef" char? 2))


  (test 2 (textual-index (as-text "abcdef") char? 2 5))

  (test 4 (textual-index (as-text "abcdef")
                             (lambda (c) (char>? c #\d)) 2 5))

  (test #f (textual-index (as-text "abcdef") char-whitespace? 2 5))

  (test 4 (textual-index-right (as-text "abcdef") char? 2 5))

  (test 4 (textual-index-right (as-text "abcdef")
                                   (lambda (c)
                                     (char>? c #\d)) 2 5))

  (test #f (textual-index-right (as-text "abcdef")
                                    char-whitespace? 2 5))


  (test 2 (textual-skip (as-text "abcdef") string? 2 5))

  (test 4 (textual-skip (as-text "abcdef")
                            (lambda (c) (char<=? c #\d)) 2 5))

  (test #f (textual-skip (as-text "abcdef") char? 2 5))

  (test 4 (textual-skip-right (as-text "abcdef") string? 2 5))

  (test 4 (textual-skip-right (as-text "abcdef")
                                  (lambda (c)
                                    (char<=? c #\d)) 2 5))

  (test #f (textual-skip-right (as-text "abcdef") char? 2 5))


  (test 0 (textual-contains (as-text "") (as-text "")))

  (test 0 (textual-contains (as-text "abcdeffffoo") (as-text "")))

  (test 0 (textual-contains (as-text "abcdeffffoo") (as-text "a")))

  (test 5 (textual-contains (as-text "abcdeffffoo") (as-text "ff")))

  (test 4 (textual-contains (as-text "abcdeffffoo") (as-text "eff")))

  (test 8 (textual-contains (as-text "abcdeffffoo") (as-text "foo")))

  (test #f (textual-contains (as-text "abcdeffffoo") (as-text "efffoo")))

  (test 0 (textual-contains-right (as-text "") (as-text "")))

  (test 11 (textual-contains-right (as-text "abcdeffffoo") (as-text "")))

  (test 0 (textual-contains-right (as-text "abcdeffffoo") (as-text "a")))

  (test 7 (textual-contains-right (as-text "abcdeffffoo") (as-text "ff")))

  (test 4 (textual-contains-right (as-text "abcdeffffoo") (as-text "eff")))

  (test 8 (textual-contains-right (as-text "abcdeffffoo") (as-text "foo")))

  (test #f (textual-contains-right (as-text "abcdeffffoo")
                                       (as-text "efffoo")))


  (test 0 (textual-contains "" "" 0))

  (test 2 (textual-contains "abcdeffffoo" "" 2))

  (test #f (textual-contains "abcdeffffoo" "a" 2))

  (test 5 (textual-contains "abcdeffffoo" "ff" 2))

  (test 4 (textual-contains "abcdeffffoo" "eff" 2))

  (test 8 (textual-contains "abcdeffffoo" "foo" 2))

  (test #f (textual-contains "abcdeffffoo" "efffoo" 2))

  (test 0 (textual-contains-right "" "" 0))

  (test 11 (textual-contains-right "abcdeffffoo" "" 2))

  (test #f (textual-contains-right "abcdeffffoo" "a" 2))

  (test 7 (textual-contains-right "abcdeffffoo" "ff" 2))

  (test 4 (textual-contains-right "abcdeffffoo" "eff" 2))

  (test 8 (textual-contains-right "abcdeffffoo" "foo" 2))

  (test #f (textual-contains-right "abcdeffffoo" "efffoo" 2))


  (test 0 (textual-contains (as-text "") (as-text "") 0 0))

  (test 2 (textual-contains (as-text "abcdeffffoo") (as-text "") 2 10))

  (test #f (textual-contains (as-text "abcdeffffoo") (as-text "a") 2 10))

  (test 5 (textual-contains (as-text "abcdeffffoo") (as-text "ff") 2 10))

  (test 4 (textual-contains (as-text "abcdeffffoo") (as-text "eff") 2 10))

  (test #f (textual-contains (as-text "abcdeffffoo") (as-text "foo") 2 10))

  (test #f (textual-contains (as-text "abcdeffffoo") (as-text "efffoo") 2 10))

  (test 0 (textual-contains-right (as-text "") (as-text "") 0 0))

  (test 10 (textual-contains-right (as-text "abcdeffffoo") (as-text "") 2 10))

  (test #f (textual-contains-right (as-text "abcdeffffoo") (as-text "a") 2 10))

  (test 7 (textual-contains-right (as-text "abcdeffffoo") (as-text "ff") 2 10))

  (test 4 (textual-contains-right (as-text "abcdeffffoo") (as-text "eff") 2 10))

  (test #f (textual-contains-right (as-text "abcdeffffoo") "foo" 2 10))

  (test #f (textual-contains-right "abcdeffffoo" (as-text "efffoo") 2 10))


  (test 0 (textual-contains "" "" 0 0 0))

  (test 2 (textual-contains "abcdeffffoo" "" 2 10 0))

  (test 2 (textual-contains "abcdeffffoo" "a" 2 10 1))

  (test 5 (textual-contains "abcdeffffoo" "ff" 2 10 1))

  (test 5 (textual-contains "abcdeffffoo" "eff" 2 10 1))

  (test #f (textual-contains "abcdeffffoo" "foo" 2 10 1))

  (test #f (textual-contains "abcdeffffoo" "efffoo" 2 10 1))

  (test 0 (textual-contains-right "" "" 0 0 0))

  (test 10 (textual-contains-right "abcdeffffoo" "" 2 10 0))

  (test 10 (textual-contains-right "abcdeffffoo" "a" 2 10 1))

  (test 8 (textual-contains-right "abcdeffffoo" "ff" 2 10 1))

  (test 7 (textual-contains-right "abcdeffffoo" "eff" 2 10 1))

  (test #f (textual-contains-right "abcdeffffoo" "foo" 2 10 1))

  (test #f (textual-contains-right "abcdeffffoo" "efffoo" 2 10 1))


  (test 0 (textual-contains "" "" 0 0 0 0))

  (test 2 (textual-contains "abcdeffffoo" "" 2 10 0 0))

  (test 2 (textual-contains "abcdeffffoo" "a" 2 10 1 1))

  (test 5 (textual-contains "abcdeffffoo" "ff" 2 10 1 2))

  (test 5 (textual-contains "abcdeffffoo" "eff" 2 10 1 2))

  (test 9 (textual-contains "abcdeffffoo" "foo" 2 10 1 2))

  (test 4 (textual-contains "abcdeffffoo" "efffoo" 2 10 0 2))

  (test 0 (textual-contains-right "" "" 0 0 0 0))

  (test 10 (textual-contains-right "abcdeffffoo" "" 2 10 0 0))

  (test 10 (textual-contains-right "abcdeffffoo" "a" 2 10 1 1))

  (test 8  (textual-contains-right "abcdeffffoo" "ff" 2 10 1 2))

  (test 8 (textual-contains-right "abcdeffffoo" "eff" 2 10 1 2))

  (test 9 (textual-contains-right "abcdeffffoo" "foo" 2 10 1 2))

  (test 7 (textual-contains-right "abcdeffffoo" "efffoo" 2 10 1 3))
  )
