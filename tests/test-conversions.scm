(test-group "Conversions"
  (test-assert (let ((txt (textual->text "str")))
                 (and (text? txt) (textual=? txt "str"))))

  (test-assert (let ((txt (textual->text (text #\s #\t #\r))))
                 (and (text? txt) (textual=? txt "str"))))


  (test-assert (string=? "" (textual->string (text))))

  (test-assert (string=? "" (textual->string (text) 0)))

  (test-assert (string=? "" (textual->string (text) 0 0)))

  (test-assert (string=? "abc" (textual->string (text #\a #\b #\c))))

  (test-assert (string=? "" (textual->string (text #\a #\b #\c) 3)))

  (test-assert (string=? "bc" (textual->string (text #\a #\b #\c) 1 3)))


  (test-assert (string=? "" (textual->string "")))

  (test-assert (string=? "" (textual->string "" 0)))

  (test-assert (string=? "" (textual->string "" 0 0)))

  (test-assert (string=? "abc" (textual->string "abc")))

  (test-assert (string=? "" (textual->string "abc" 3)))

  (test-assert (string=? "bc" (textual->string "abc" 1 3)))


  (test '#() (textual->vector (text)))

  (test '#() (textual->vector (text) 0))

  (test '#() (textual->vector (text) 0 0))

  (test '#(#\a #\b #\c) (textual->vector (text #\a #\b #\c)))

  (test '#() (textual->vector (text #\a #\b #\c) 3))

  (test '#(#\b #\c) (textual->vector (text #\a #\b #\c) 1 3))


  (test '#() (textual->vector ""))

  (test '#() (textual->vector "" 0))

  (test '#() (textual->vector "" 0 0))

  (test '#(#\a #\b #\c) (textual->vector "abc"))

  (test '#() (textual->vector "abc" 3))

  (test '#(#\b #\c) (textual->vector "abc" 1 3))


  (test '() (textual->list (text)))

  (test '() (textual->list (text) 0))

  (test '() (textual->list (text) 0 0))

  (test '(#\a #\b #\c) (textual->list (text #\a #\b #\c)))

  (test '() (textual->list (text #\a #\b #\c) 3))

  (test '(#\b #\c) (textual->list (text #\a #\b #\c) 1 3))


  (test '() (textual->list ""))

  (test '() (textual->list "" 0))

  (test '() (textual->list "" 0 0))

  (test '(#\a #\b #\c) (textual->list "abc"))

  (test '() (textual->list "abc" 3))

  (test '(#\b #\c) (textual->list "abc" 1 3))


  (test-assert (result=? "" (string->text "")))

  (test-assert (result=? "" (string->text "" 0)))

  (test-assert (result=? "" (string->text "" 0 0)))

  (test-assert (result=? "abc" (string->text "abc")))

  (test-assert (result=? "bc" (string->text "abc" 1)))

  (test-assert (result=? "" (string->text "abc" 3)))

  (test-assert (result=? "b" (string->text "abc" 1 2)))

  (test-assert (result=? "bc" (string->text "abc" 1 3)))


  (test-assert (result=? "" (vector->text '#())))

  (test-assert (result=? "" (vector->text '#() 0)))

  (test-assert (result=? "" (vector->text '#() 0 0)))

  (test-assert (result=? "abc" (vector->text '#(#\a #\b #\c))))

  (test-assert (result=? "bc" (vector->text '#(#\a #\b #\c) 1)))

  (test-assert (result=? "" (vector->text '#(#\a #\b #\c) 3)))

  (test-assert (result=? "b" (vector->text '#(#\a #\b #\c) 1 2)))

  (test-assert (result=? "bc" (vector->text '#(#\a #\b #\c) 1 3)))


  (test-assert (result=? "" (list->text '())))

  (test-assert (result=? "" (list->text '() 0)))

  (test-assert (result=? "" (list->text '() 0 0)))

  (test-assert (result=? "abc" (list->text '(#\a #\b #\c))))

  (test-assert (result=? "bc" (list->text '(#\a #\b #\c) 1)))

  (test-assert (result=? "" (list->text '(#\a #\b #\c) 3)))

  (test-assert (result=? "b" (list->text '(#\a #\b #\c) 1 2)))

  (test-assert (result=? "bc" (list->text '(#\a #\b #\c) 1 3)))


  (test-assert (result=? "" (reverse-list->text '())))

  (test-assert (result=? "cba" (reverse-list->text '(#\a #\b #\c))))


  (test-assert (equal? '#u8(97 98 99))
              (textual->utf8 (as-text "abc")))

  (test-assert (equal? '#u8(97 98 99))
              (textual->utf8 "abc"))

  (test-assert (equal? '#u8(97 98 99 121 121 121 122 122 122))
              (textual->utf8 (as-text "xxxabcyyyzzz") 3))

  (test-assert (equal? '#u8(97 98 99 121 121 121 122 122 122))
              (textual->utf8 "xxxabcyyyzzz" 3))

  (test-assert (equal? '#u8(97 98 99))
              (textual->utf8 (as-text "xxxabcyyyzzz") 3 6))

  (test-assert (equal? '#u8(97 98 99))
              (textual->utf8 "xxxabcyyyzzz" 3 6))


  (define assumed-endianness
    (let ((bom (textual->utf16 (as-text ""))))
      (test 2 (bytevector-length bom))
      (if (= (bytevector-u8-ref bom 0) 254)
          'big
          'little)))

  (test (if (eq? assumed-endianness 'big)
            '#u8(254 255 0 97 0 98 0 99)
            '#u8(255 254 97 0 98 0 99 0))
        (textual->utf16 (as-text "abc")))

  (test (if (eq? assumed-endianness 'big)
            '#u8(254 255 0 97 0 98 0 99)
            '#u8(255 254 97 0 98 0 99 0))
        (textual->utf16 "abc"))

  (test (if (eq? assumed-endianness 'big)
            '#u8(254 255 0 97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122)
            '#u8(255 254 97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122 0))
        (textual->utf16 (as-text "xxxabcyyyzzz") 3))

  (test (if (eq? assumed-endianness 'big)
            '#u8(254 255 0 97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122)
            '#u8(255 254 97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122 0))
        (textual->utf16 "xxxabcyyyzzz" 3))

  (test (if (eq? assumed-endianness 'big)
            '#u8(254 255 0 97 0 98 0 99)
            '#u8(255 254 97 0 98 0 99 0))
        (textual->utf16 (as-text "xxxabcyyyzzz") 3 6))

  (test (if (eq? assumed-endianness 'big)
            '#u8(254 255 0 97 0 98 0 99)
            '#u8(255 254 97 0 98 0 99 0))
        (textual->utf16 "xxxabcyyyzzz" 3 6))


  (test '#u8(0 97 0 98 0 99)
        (textual->utf16be (as-text "abc")))

  (test '#u8(0 97 0 98 0 99)
        (textual->utf16be "abc"))

  (test '#u8(0 97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122)
        (textual->utf16be (as-text "xxxabcyyyzzz") 3))

  (test '#u8(0 97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122)
        (textual->utf16be "xxxabcyyyzzz" 3))

  (test '#u8(0 97 0 98 0 99)
        (textual->utf16be (as-text "xxxabcyyyzzz") 3 6))

  (test '#u8(0 97 0 98 0 99)
        (textual->utf16be "xxxabcyyyzzz" 3 6))


  (test '#u8(97 0 98 0 99 0)
        (textual->utf16le (as-text "abc")))

  (test '#u8(97 0 98 0 99 0)
        (textual->utf16le "abc"))

  (test '#u8(97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122 0)
        (textual->utf16le (as-text "xxxabcyyyzzz") 3))

  (test '#u8(97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122 0)
        (textual->utf16le "xxxabcyyyzzz" 3))

  (test '#u8(97 0 98 0 99 0)
        (textual->utf16le (as-text "xxxabcyyyzzz") 3 6))

  (test '#u8(97 0 98 0 99 0)
        (textual->utf16le "xxxabcyyyzzz" 3 6))


  (test-assert (result=? "abc"
                         (utf8->text '#u8(97 98 99))))

  (test-assert
   (result=? "abcyyyzzz"
             (utf8->text '#u8(0 1 2 97 98 99 121 121 121 122 122 122) 3)))

  (test-assert (result=? "abc"
                         (utf8->text '#u8(41 42 43 97 98 99 100 101 102) 3 6)))


  (test-assert (result=? "abc"
                         (utf16->text '#u8(254 255 0 97 0 98 0 99))))

  (test-assert (result=? "abc"
                         (utf16->text '#u8(255 254 97 0 98 0 99 0))))

  (test-assert (result=? "abc"
                         (utf16->text (textual->utf16 "abc") 2)))

  (test-assert (result=? "bcdef"
                         (utf16->text (textual->utf16 "abcdef") 4)))

  (test-assert (result=? "bcd"
                         (utf16->text (textual->utf16 "abcdef") 4 10)))


  (test-assert (result=? "abc"
                         (utf16be->text '#u8(0 97 0 98 0 99))))

  (test-assert (result=? "bc"
                         (utf16be->text (textual->utf16be "abc") 2)))

  (test-assert (result=? "bcd"
                         (utf16be->text (textual->utf16be "abcdef") 2 8)))


  (test-assert (result=? "abc"
                         (utf16le->text '#u8(97 0 98 0 99 0))))

  (test-assert (result=? "bc"
                         (utf16le->text (textual->utf16le "abc") 2)))

  (test-assert (result=? "bcd"
                         (utf16le->text (textual->utf16le "abcdef") 2 8)))


  (test '#u8(97 195 128 206 191
             240 157 145 129 240 157 132 147 240 157 132 144 122)
        (textual->utf8 beyondBMP))

  (let ((bv (textual->utf16 beyondBMP)))
    (test-assert
      (or (equal? bv
                  '#u8(254 255 0 97 0 192 3 191
                       216 53 220 65 216 52 221 19 216 52 221 16 0 122))
          (equal? bv
                  '#u8(255 254 97 0 192 0 191 3
                       53 216 65 220 52 216 19 221 52 216 16 221 122 0)))))

  (test '#u8(0 97 0 192 3 191 216 53 220 65 216 52 221 19 216 52 221 16 0 122)
        (textual->utf16be beyondBMP))

  (test '#u8(97 0 192 0 191 3 53 216 65 220 52 216 19 221 52 216 16 221 122 0)
        (textual->utf16le beyondBMP))

  (test-assert
   (textual=? beyondBMP
              (utf8->text
               '#u8(97 195 128 206 191
                    240 157 145 129 240 157 132 147 240 157 132 144 122))))

  (test-assert (textual=? beyondBMP (utf16->text (textual->utf16 beyondBMP))))

  (test-assert (textual=? beyondBMP (utf16->text (textual->utf16 beyondBMP) 2)))

  (test-assert (textual=? beyondBMP
                          (utf16be->text (textual->utf16be beyondBMP))))

  (test-assert (textual=? beyondBMP
                          (utf16le->text (textual->utf16le beyondBMP))))

  (test-assert (result=? (string-append (string (integer->char #xfeff)) "abc")
                         (utf16be->text '#u8(254 255 0 97 0 98 0 99))))

  (test-assert (result=? (string-append (string (integer->char #xfeff)) "abc")
                         (utf16le->text '#u8(255 254 97 0 98 0 99 0))))
  )
