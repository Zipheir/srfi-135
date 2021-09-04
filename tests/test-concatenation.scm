(test-group "Concatenation"
  (test-assert (result=? "" (textual-append)))

  (test-assert
   (result=? "abcdef"
             (textual-append (as-text "")
                             (as-text "a")
                             (as-text "bcd")
                             "" "ef" "" "")))

  (test-assert (result=? "" (textual-concatenate '())))

  (test-assert
   (result=? "abcdef"
             (textual-concatenate
              (map string->text '("" "a" "bcd" "" "ef" "" "")))))

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

    (test-assert (result=? str10 alph10))
    (test-assert (result=? str100 alph100))
    (test-assert (result=? str100-500 t100-500))
    (test-assert (result=? str600-999 t600-999))

    ;; concatenating a short text with a long text

    (test-assert
     (result=? (string-append str1 str600-999)
               (textual-concatenate (list alph1 t600-999))))

    (test-assert
     (result=? (string-append str1 str600-999)
               (textual-concatenate (list alph1 (textual-copy t600-999)))))

    (test-assert
     (result=? (string-append str600-999 str1)
               (textual-concatenate (list t600-999 alph1))))

    (test-assert
     (result=? (string-append str600-999 str1)
               (textual-concatenate (list (textual-copy t600-999) alph1))))
    )


  (test-assert (result=? "" (textual-concatenate-reverse '())))

  (test-assert
   (result=? "efbcda"
             (textual-concatenate-reverse
              (map string->text '("" "a" "bcd" "" "ef" "" "")))))

  (test-assert (result=? "huh?" (textual-concatenate-reverse '() "huh?")))

  (test-assert
   (result=? "efbcdaxy"
             (textual-concatenate-reverse '("" "a" "bcd" "" "ef" "" "") "xy")))

  (test-assert (result=? "huh" (textual-concatenate-reverse '() "huh?" 3)))

  (test-assert
   (result=? "efbcdax"
             (textual-concatenate-reverse '("" "a" "bcd" "" "ef" "" "") "x" 1)))


  (test-assert (result=? "" (textual-join '())))

  (test-assert
   (result=? " ab cd  e f "
             (textual-join (map string->text '("" "ab" "cd" "" "e" "f" "")))))

  (test-assert (result=? "" (textual-join '() "")))

  (test-assert
   (result=? "abcdef" (textual-join '("" "ab" "cd" "" "e" "f" "") "")))

  (test-assert (result=? "" (textual-join '() "xyz")))

  (test-assert
   (result=? "xyzabxyzcdxyzxyzexyzfxyz"
             (textual-join '("" "ab" "cd" "" "e" "f" "") "xyz")))

  (test-assert (result=? "" (textual-join '() "" 'infix)))

  (test-assert
   (result=? "abcdef"
             (textual-join '("" "ab" "cd" "" "e" "f" "") "" 'infix)))

  (test-assert (result=? "" (textual-join '() "xyz" 'infix)))

  (test-assert
   (result=? "xyzabxyzcdxyzxyzexyzfxyz"
             (textual-join '("" "ab" "cd" "" "e" "f" "")
                           (as-text "xyz")
                           'infix)))

  (test 'horror
        (guard (exn (#t 'horror))
          (textual-join '() "" 'strict-infix)))

  (test-assert
   (result=? "abcdef"
             (textual-join '("" "ab" "cd" "" "e" "f" "") "" 'strict-infix)))

  (test 'wham
        (guard (exn (else 'wham))
          (textual-join '() "xyz" 'strict-infix)))

  (test-assert
   (result=? "xyzabxyzcdxyzxyzexyzfxyz"
             (textual-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'strict-infix)))

  (test-assert (result=? "" (textual-join '() "" 'suffix)))

  (test-assert
   (result=? "abcdef"
             (textual-join '("" "ab" "cd" "" "e" "f" "") "" 'suffix)))

  (test-assert (result=? "" (textual-join '() "xyz" 'suffix)))

  (test-assert
   (result=? "xyzabxyzcdxyzxyzexyzfxyzxyz"
             (textual-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'suffix)))

  (test-assert (result=? "" (textual-join '() "" 'prefix)))

  (test-assert
   (result=? "abcdef"
             (textual-join '("" "ab" "cd" "" "e" "f" "") "" 'prefix)))

  (test-assert (result=? "" (textual-join '() "xyz" 'prefix)))

  (test-assert
   (result=? "xyzxyzabxyzcdxyzxyzexyzfxyz"
             (textual-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'prefix)))
  )
