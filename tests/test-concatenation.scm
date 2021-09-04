(test-group "Concatenation"
  (test "" (textual-append))

  (test "abcdef"
        (textual-append (as-text "")
                        (as-text "a")
                        (as-text "bcd")
                        "" "ef" "" ""))

  (test "" (textual-concatenate '()))

  (test "abcdef"
        (textual-concatenate
         (map string->text '("" "a" "bcd" "" "ef" "" ""))))

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

    (test str10 alph10)
    (test str100 alph100)
    (test str100-500 t100-500)
    (test str600-999 t600-999)

    ;; concatenating a short text with a long text

    (test (string-append str1 str600-999)
          (textual-concatenate (list alph1 t600-999)))

    (test (string-append str1 str600-999)
          (textual-concatenate (list alph1 (textual-copy t600-999))))

    (test (string-append str600-999 str1)
          (textual-concatenate (list t600-999 alph1)))

    (test (string-append str600-999 str1)
          (textual-concatenate (list (textual-copy t600-999) alph1)))
    )


  (test "" (textual-concatenate-reverse '()))

  (test "efbcda"
        (textual-concatenate-reverse
         (map string->text '("" "a" "bcd" "" "ef" "" ""))))

  (test "huh?" (textual-concatenate-reverse '() "huh?"))

  (test "efbcdaxy"
        (textual-concatenate-reverse '("" "a" "bcd" "" "ef" "" "") "xy"))

  (test "huh" (textual-concatenate-reverse '() "huh?" 3))

  (test "efbcdax"
        (textual-concatenate-reverse
         '("" "a" "bcd" "" "ef" "" "") "x" 1))


  (test "" (textual-join '()))

  (test " ab cd  e f "
        (textual-join (map string->text '("" "ab" "cd" "" "e" "f" ""))))

  (test "" (textual-join '() ""))

  (test "abcdef" (textual-join '("" "ab" "cd" "" "e" "f" "") ""))

  (test "" (textual-join '() "xyz"))

  (test "xyzabxyzcdxyzxyzexyzfxyz"
        (textual-join '("" "ab" "cd" "" "e" "f" "") "xyz"))

  (test "" (textual-join '() "" 'infix))

  (test "abcdef" (textual-join '("" "ab" "cd" "" "e" "f" "") "" 'infix))

  (test "" (textual-join '() "xyz" 'infix))

  (test "xyzabxyzcdxyzxyzexyzfxyz"
        (textual-join '("" "ab" "cd" "" "e" "f" "") (as-text "xyz") 'infix))

  (test 'horror
        (guard (exn (#t 'horror))
          (textual-join '() "" 'strict-infix)))

  (test "abcdef"
        (textual-join '("" "ab" "cd" "" "e" "f" "") "" 'strict-infix))

  (test 'wham
        (guard (exn (else 'wham))
          (textual-join '() "xyz" 'strict-infix)))

  (test "xyzabxyzcdxyzxyzexyzfxyz"
        (textual-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'strict-infix))

  (test "" (textual-join '() "" 'suffix))

  (test "abcdef" (textual-join '("" "ab" "cd" "" "e" "f" "") "" 'suffix))

  (test "" (textual-join '() "xyz" 'suffix))

  (test "xyzabxyzcdxyzxyzexyzfxyzxyz"
        (textual-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'suffix))

  (test "" (textual-join '() "" 'prefix))

  (test "abcdef" (textual-join '("" "ab" "cd" "" "e" "f" "") "" 'prefix))

  (test "" (textual-join '() "xyz" 'prefix))

  (test "xyzxyzabxyzcdxyzxyzexyzfxyz"
        (textual-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'prefix))

