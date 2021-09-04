(test-group "Traversal"
  (test 8
        (textual-fold (lambda (c count)
                        (if (char-whitespace? c)
                            (+ count 1)
                            count))
                      0
                      (as-text " ...a couple of spaces in this one... ")))

  (test 7
        (textual-fold (lambda (c count)
                        (if (char-whitespace? c)
                            (+ count 1)
                            count))
                      0
                      " ...a couple of spaces in this one... "
                      1))

  (test 6
        (textual-fold (lambda (c count)
                        (if (char-whitespace? c)
                            (+ count 1)
                            count))
                      0
                      " ...a couple of spaces in this one... "
                      1
                      32))

  (test (string->list "abcdef") (textual-fold-right cons '() "abcdef"))

  (test (string->list "def") (textual-fold-right cons '() (as-text "abcdef") 3))

  (test (string->list "cde")
        (textual-fold-right cons '() (as-text "abcdef") 2 5))

  (test "aabraacaadaabraa"
        (let* ((s (as-text "abracadabra"))
               (ans-len (textual-fold (lambda (c sum)
                                        (+ sum (if (char=? c #\a) 2 1)))
                                      0
                                      s))
               (ans (make-string ans-len)))
          (textual-fold (lambda (c i)
                          (let ((i (if (char=? c #\a)
                                       (begin (string-set! ans i #\a)
                                              (+ i 1))
                                       i)))
                            (string-set! ans i c)
                            (+ i 1)))
                        0
                        s)
          ans))

  (test-assert (result=? "abc" (textual-map string (as-text "abc"))))

  (test-assert (result=? "ABC" (textual-map char-upcase "abc")))

  (test-assert
   (result=? "Hear-here!"
             (textual-map (lambda (c0 c1 c2)
                            (case c0
                              ((#\1) c1)
                              ((#\2) (string c2))
                              ((#\-) (text #\- c1))))
                          (string->text "1222-1111-2222")
                          (string->text "Hi There!")
                          (string->text "Dear John"))))

  (test "abc"
        (let ((q (open-output-string)))
          (textual-for-each (lambda (c) (write-char c q)) (as-text "abc"))
          (get-output-string q)))

  (test '("cfi" "beh" "adg")
        (let ((x '()))
          (textual-for-each (lambda (c1 c2 c3)
                              (set! x (cons (string c1 c2 c3) x)))
                            "abc"
                            (as-text "defxyz")
                            (as-text "ghijklmnopqrstuvwxyz"))
          x))

  (test-assert
   (result=? "abc"
             (textual-map-index (lambda (i)
                                  (integer->char (+ i (char->integer #\a))))
                                "xyz")))

  (test-assert
   (result=? "def"
             (textual-map-index (lambda (i)
                                  (integer->char (+ i (char->integer #\a))))
                                "xyz***"
                                3)))

  (test-assert
   (result=? "cde"
             (textual-map-index (lambda (i)
                                  (integer->char (+ i (char->integer #\a))))
                                 "......"
                                 2
                                 5)))

  (test '(101 100 99 98 97)
        (let ((s (as-text "abcde"))
              (v '()))
          (textual-for-each-index
           (lambda (i)
             (set! v (cons (char->integer (textual-ref s i)) v)))
           s)
          v))

  (test '(101 100 99)
        (let ((s (as-text "abcde"))
              (v '()))
          (textual-for-each-index
           (lambda (i)
             (set! v (cons (char->integer (textual-ref s i)) v)))
           s 2)
          v))

  (test '(99 98)
        (let ((s (as-text "abcde"))
              (v '()))
          (textual-for-each-index
           (lambda (i)
             (set! v (cons (char->integer (textual-ref s i)) v)))
           s 1 3)
          v))

  (test 6 (textual-count "abcdef" char?))

  (test 4 (textual-count "counting  whitespace, again " char-whitespace? 5))

  (test 3 (textual-count "abcdefwxyz"
                          (lambda (c) (odd? (char->integer c)))
                          2
                          8))


  (let ((vowels (textual->list "aeiou")))
    (test-assert
     (result=? "aiueaaaoi"
               (textual-filter
                (lambda (c) (memv c vowels))
                (as-text "What is number, that man may know it?"))))

    (test-assert
     (result=? "And wmn, tht sh my knw nmbr?"
               (textual-remove (lambda (c) (memv c vowels))
                               "And woman, that she may know number?")))

    (test-assert
     (result=? "iueaaaoi"
                  (textual-filter
                   (lambda (c) (memv c vowels))
                   (as-text "What is number, that man may know it?")
                   4)))

    (test-assert
     (result=? "mn, tht sh my knw nmbr?"
               (textual-remove (lambda (c) (memv c vowels))
                               "And woman, that she may know number?"
                               6)))

    (test-assert
     (result=? "aaao"
               (textual-filter (lambda (c) (memv c vowels))
                               (as-text "What is number, that man may know it?")
                               16
                               32)))

    (test-assert
      (result=? "And woman, that sh may know"
                (textual-remove (lambda (c) (memv c (textual->list "eiu")))
                                "And woman, that she may know number?"
                                0
                                28)))
    )


  (test-assert (result=? "" (textual-reverse "")))
  (test-assert (result=? "fedcba" (textual-reverse "abcdef")))
  (test-assert (result=? "" (textual-reverse (as-text "") 0)))
  (test-assert (result=? "fedcba" (textual-reverse "abcdef" 0)))
  (test-assert (result=? "fedc" (textual-reverse (as-text "abcdef") 2)))
  (test-assert (result=? "" (textual-reverse "" 0 0)))
  (test-assert (result=? "fedcba" (textual-reverse "abcdef" 0 6)))
  (test-assert (result=? "edc" (textual-reverse "abcdef" 2 5)))
  )
