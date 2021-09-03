(test-group "Selection"
  ;;; length & ref

  (test-assert (= 0 (text-length (text))))

  (test-assert (= 6 (text-length ABCDEF)))

  (test-assert (= 1234 (text-length (make-text 1234 (text-ref ABC 0)))))


  (test-assert (char=? #\a (text-ref (text #\a #\b #\c) 0)))

  (test-assert (char=? #\c (text-ref (text #\a #\b #\c) 2)))

  (test-assert (char=? (string-ref (textual->string ABCDEF) 3)
                       (text-ref ABCDEF 3)))


  (test-assert (= 0 (textual-length (text))))

  (test-assert (= 6 (textual-length ABCDEF)))

  (test-assert (= 1234 (textual-length (make-text 1234 (text-ref ABC 0)))))

  (test-assert (char=? #\a (textual-ref (text #\a #\b #\c) 0)))

  (test-assert (char=? #\c (textual-ref (text #\a #\b #\c) 2)))

  (test-assert (char=? (string-ref (textual->string ABCDEF) 3)
                       (textual-ref ABCDEF 3)))

  ;;; subtext

  (test-assert (result=? "" (subtext (text) 0 0)))

  (test-assert (result=? "" (subtext (string->text "abcdef") 0 0)))

  (test-assert (result=? "" (subtext (string->text "abcdef") 4 4)))

  (test-assert (result=? "" (subtext (string->text "abcdef") 6 6)))

  (test-assert (result=? "abcd" (subtext (string->text "abcdef") 0 4)))

  (test-assert (result=? "cde" (subtext (string->text "abcdef") 2 5)))

  (test-assert (result=? "cdef" (subtext (string->text "abcdef") 2 6)))

  (test-assert (result=? "abcdef" (subtext (string->text "abcdef") 0 6)))

  ;;; subtextual

  (test-assert (result=? "" (subtextual (text) 0 0)))

  (test-assert (result=? "" (subtextual (string->text "abcdef") 0 0)))

  (test-assert (result=? "" (subtextual (string->text "abcdef") 4 4)))

  (test-assert (result=? "" (subtextual (string->text "abcdef") 6 6)))

  (test-assert (result=? "abcd" (subtextual (string->text "abcdef") 0 4)))

  (test-assert (result=? "cde" (subtextual (string->text "abcdef") 2 5)))

  (test-assert (result=? "cdef" (subtextual (string->text "abcdef") 2 6)))

  (test-assert (result=? "abcdef" (subtextual (string->text "abcdef") 0 6)))


  (test-assert (result=? "" (subtextual "" 0 0)))

  (test-assert (result=? "" (subtextual "abcdef" 0 0)))

  (test-assert (result=? "" (subtextual "abcdef" 4 4)))

  (test-assert (result=? "" (subtextual "abcdef" 6 6)))

  (test-assert (result=? "abcd" (subtextual "abcdef" 0 4)))

  (test-assert (result=? "cde" (subtextual "abcdef" 2 5)))

  (test-assert (result=? "cdef" (subtextual "abcdef" 2 6)))

  (test-assert (result=? "abcdef" (subtextual "abcdef" 0 6)))

  ;;; textual-copy

  (test-assert (result=? "" (textual-copy (text))))

  (test-assert (let* ((txt (string->text "abcdef"))
                      (copy (textual-copy txt)))
                 (and (result=? "abcdef" copy)
                      (not (eqv? txt copy)))))


  (test-assert (result=? "" (textual-copy "")))

  (test-assert (result=? "abcdef" (textual-copy "abcdef")))


  (test-assert (result=? "" (textual-copy (text) 0)))

  (test-assert (result=? "abcdef" (textual-copy (string->text "abcdef") 0)))

  (test-assert (result=? "ef" (textual-copy (string->text "abcdef") 4)))

  (test-assert (result=? "" (textual-copy (string->text "abcdef") 6)))


  (test-assert (result=? "" (textual-copy "" 0)))

  (test-assert (result=? "abcdef" (textual-copy "abcdef" 0)))

  (test-assert (result=? "ef" (textual-copy "abcdef" 4)))

  (test-assert (result=? "" (textual-copy "abcdef" 6)))


  (test-assert (result=? "" (textual-copy (text) 0 0)))

  (test-assert (result=? "" (textual-copy (string->text "abcdef") 0 0)))

  (test-assert (result=? "" (textual-copy (string->text "abcdef") 4 4)))

  (test-assert (result=? "" (textual-copy (string->text "abcdef") 6 6)))

  (test-assert (result=? "abcd" (textual-copy (string->text "abcdef") 0 4)))

  (test-assert (result=? "cde" (textual-copy (string->text "abcdef") 2 5)))

  (test-assert (result=? "cdef" (textual-copy (string->text "abcdef") 2 6)))

  (test-assert (result=? "abcdef" (textual-copy (string->text "abcdef") 0 6)))


  (test-assert (result=? "" (textual-copy "" 0 0)))

  (test-assert (result=? "" (textual-copy "abcdef" 0 0)))

  (test-assert (result=? "" (textual-copy "abcdef" 4 4)))

  (test-assert (result=? "" (textual-copy "abcdef" 6 6)))

  (test-assert (result=? "abcd" (textual-copy "abcdef" 0 4)))

  (test-assert (result=? "cde" (textual-copy "abcdef" 2 5)))

  (test-assert (result=? "cdef" (textual-copy "abcdef" 2 6)))

  (test-assert (result=? "abcdef" (textual-copy "abcdef" 0 6)))

  ;;; take & drop

  (test-assert (result=? "" (textual-take (text) 0)))

  (test-assert (result=? "" (textual-take (string->text "abcdef") 0)))

  (test-assert (result=? "ab" (textual-take (string->text "abcdef") 2)))

  (test-assert (result=? "" (textual-drop (string->text "") 0)))

  (test-assert (result=? "abcdef" (textual-drop (string->text "abcdef") 0)))

  (test-assert (result=? "cdef" (textual-drop (string->text "abcdef") 2)))

  (test-assert (result=? "" (textual-take-right (text) 0)))

  (test-assert (result=? "" (textual-take-right (string->text "abcdef") 0)))

  (test-assert (result=? "ef" (textual-take-right (string->text "abcdef") 2)))

  (test-assert (result=? "" (textual-drop-right (text) 0)))

  (test-assert (result=? "abcdef"
                         (textual-drop-right (string->text "abcdef") 0)))

  (test-assert (result=? "abcd"
                         (textual-drop-right (string->text "abcdef") 2)))


  (test-assert (result=? "" (textual-take "" 0)))

  (test-assert (result=? "" (textual-take "abcdef" 0)))

  (test-assert (result=? "ab" (textual-take "abcdef" 2)))

  (test-assert (result=? "" (textual-drop "" 0)))

  (test-assert (result=? "abcdef" (textual-drop "abcdef" 0)))

  (test-assert (result=? "cdef" (textual-drop "abcdef" 2)))

  (test-assert (result=? "" (textual-take-right "" 0)))

  (test-assert (result=? "" (textual-take-right "abcdef" 0)))

  (test-assert (result=? "ef" (textual-take-right "abcdef" 2)))

  (test-assert (result=? "" (textual-drop-right "" 0)))

  (test-assert (result=? "abcdef" (textual-drop-right "abcdef" 0)))

  (test-assert (result=? "abcd" (textual-drop-right "abcdef" 2)))

  ;;; pad & trim

  (test-assert (result=? "" (textual-pad (string->text "") 0)))

  (test-assert (result=? "     " (textual-pad (string->text "") 5)))

  (test-assert (result=? "  325" (textual-pad (string->text "325") 5)))

  (test-assert (result=? "71325" (textual-pad (string->text "71325") 5)))

  (test-assert (result=? "71325" (textual-pad (string->text "8871325") 5)))

  (test-assert (result=? "" (textual-pad (string->text "") 0 #\*)))

  (test-assert (result=? "*****" (textual-pad (string->text "") 5 #\*)))

  (test-assert (result=? "**325" (textual-pad (string->text "325") 5 #\*)))

  (test-assert (result=? "71325" (textual-pad (string->text "71325") 5 #\*)))

  (test-assert (result=? "71325" (textual-pad (string->text "8871325") 5 #\*)))

  (test-assert (result=? "" (textual-pad (string->text "") 0 #\* 0)))

  (test-assert (result=? "*****" (textual-pad (string->text "") 5 #\* 0)))

  (test-assert (result=? "**325" (textual-pad (string->text "325") 5 #\* 0)))

  (test-assert (result=? "71325" (textual-pad (string->text "71325") 5 #\* 0)))

  (test-assert
   (result=? "71325" (textual-pad (string->text "8871325") 5 #\* 0)))

  (test-assert (result=? "***25" (textual-pad (string->text "325") 5 #\* 1)))

  (test-assert (result=? "*1325" (textual-pad (string->text "71325") 5 #\* 1)))

  (test-assert
   (result=? "71325" (textual-pad (string->text "8871325") 5 #\* 1)))

  (test-assert (result=? "" (textual-pad (string->text "") 0 #\* 0 0)))

  (test-assert (result=? "*****" (textual-pad (string->text "") 5 #\* 0 0)))

  (test-assert (result=? "**325" (textual-pad (string->text "325") 5 #\* 0 3)))

  (test-assert
   (result=? "**713" (textual-pad (string->text "71325") 5 #\* 0 3)))

  (test-assert
   (result=? "**887" (textual-pad (string->text "8871325") 5 #\* 0 3)))

  (test-assert (result=? "***25" (textual-pad (string->text "325") 5 #\* 1 3)))

  (test-assert
   (result=? "**132" (textual-pad (string->text "71325") 5 #\* 1 4)))

  (test-assert
   (result=? "*8713" (textual-pad (string->text "8871325") 5 #\* 1 5)))

  (test-assert (result=? "" (textual-pad-right (string->text "") 0)))

  (test-assert (result=? "     " (textual-pad-right (string->text "") 5)))

  (test-assert (result=? "325  " (textual-pad-right (string->text "325") 5)))

  (test-assert (result=? "71325" (textual-pad-right (string->text "71325") 5)))

  (test-assert
   (result=? "88713" (textual-pad-right (string->text "8871325") 5)))

  (test-assert (result=? "" (textual-pad-right (string->text "") 0 #\*)))

  (test-assert (result=? "*****" (textual-pad-right (string->text "") 5 #\*)))

  (test-assert
   (result=? "325**" (textual-pad-right (string->text "325") 5 #\*)))

  (test-assert (result=? "71325"
                         (textual-pad-right (string->text "71325") 5 #\*)))

  (test-assert (result=? "88713"
                         (textual-pad-right (string->text "8871325") 5 #\*)))

  (test-assert (result=? "" (textual-pad-right (string->text "") 0 #\* 0)))

  (test-assert (result=? "*****" (textual-pad-right (string->text "") 5 #\* 0)))

  (test-assert (result=? "325**"
                         (textual-pad-right (string->text "325") 5 #\* 0)))

  (test-assert (result=? "71325"
                         (textual-pad-right (string->text "71325") 5 #\* 0)))

  (test-assert (result=? "88713"
                         (textual-pad-right (string->text "8871325") 5 #\* 0)))

  (test-assert (result=? "25***"
                         (textual-pad-right (string->text "325") 5 #\* 1)))

  (test-assert (result=? "1325*"
                         (textual-pad-right (string->text "71325") 5 #\* 1)))

  (test-assert (result=? "87132"
                         (textual-pad-right (string->text "8871325") 5 #\* 1)))

  (test-assert (result=? "" (textual-pad-right (string->text "") 0 #\* 0 0)))

  (test-assert
   (result=? "*****" (textual-pad-right (string->text "") 5 #\* 0 0)))

  (test-assert (result=? "325**"
                         (textual-pad-right (string->text "325") 5 #\* 0 3)))

  (test-assert (result=? "713**"
                         (textual-pad-right (string->text "71325") 5 #\* 0 3)))

  (test-assert
   (result=? "887**" (textual-pad-right (string->text "8871325") 5 #\* 0 3)))

  (test-assert (result=? "25***"
                         (textual-pad-right (string->text "325") 5 #\* 1 3)))

  (test-assert (result=? "132**"
                         (textual-pad-right (string->text "71325") 5 #\* 1 4)))

  (test-assert
   (result=? "8713*" (textual-pad-right (string->text "8871325") 5 #\* 1 5)))


  (test-assert (result=? "" (textual-pad "" 0)))

  (test-assert (result=? "     " (textual-pad "" 5)))

  (test-assert (result=? "  325" (textual-pad "325" 5)))

  (test-assert (result=? "71325" (textual-pad "71325" 5)))

  (test-assert (result=? "71325" (textual-pad "8871325" 5)))

  (test-assert (result=? "" (textual-pad "" 0 #\*)))

  (test-assert (result=? "*****" (textual-pad "" 5 #\*)))

  (test-assert (result=? "**325" (textual-pad "325" 5 #\*)))

  (test-assert (result=? "71325" (textual-pad "71325" 5 #\*)))

  (test-assert (result=? "71325" (textual-pad "8871325" 5 #\*)))

  (test-assert (result=? "" (textual-pad "" 0 #\* 0)))

  (test-assert (result=? "*****" (textual-pad "" 5 #\* 0)))

  (test-assert (result=? "**325" (textual-pad "325" 5 #\* 0)))

  (test-assert (result=? "71325" (textual-pad "71325" 5 #\* 0)))

  (test-assert (result=? "71325" (textual-pad "8871325" 5 #\* 0)))

  (test-assert (result=? "***25" (textual-pad "325" 5 #\* 1)))

  (test-assert (result=? "*1325" (textual-pad "71325" 5 #\* 1)))

  (test-assert (result=? "71325" (textual-pad "8871325" 5 #\* 1)))

  (test-assert (result=? "" (textual-pad "" 0 #\* 0 0)))

  (test-assert (result=? "*****" (textual-pad "" 5 #\* 0 0)))

  (test-assert (result=? "**325" (textual-pad "325" 5 #\* 0 3)))

  (test-assert (result=? "**713" (textual-pad "71325" 5 #\* 0 3)))

  (test-assert (result=? "**887" (textual-pad "8871325" 5 #\* 0 3)))

  (test-assert (result=? "***25" (textual-pad "325" 5 #\* 1 3)))

  (test-assert (result=? "**132" (textual-pad "71325" 5 #\* 1 4)))

  (test-assert (result=? "*8713" (textual-pad "8871325" 5 #\* 1 5)))

  (test-assert (result=? "" (textual-pad-right "" 0)))

  (test-assert (result=? "     " (textual-pad-right "" 5)))

  (test-assert (result=? "325  " (textual-pad-right "325" 5)))

  (test-assert (result=? "71325" (textual-pad-right "71325" 5)))

  (test-assert (result=? "88713" (textual-pad-right "8871325" 5)))

  (test-assert (result=? "" (textual-pad-right "" 0 #\*)))

  (test-assert (result=? "*****" (textual-pad-right "" 5 #\*)))

  (test-assert (result=? "325**" (textual-pad-right "325" 5 #\*)))

  (test-assert (result=? "71325" (textual-pad-right "71325" 5 #\*)))

  (test-assert (result=? "88713" (textual-pad-right "8871325" 5 #\*)))

  (test-assert (result=? "" (textual-pad-right "" 0 #\* 0)))

  (test-assert (result=? "*****" (textual-pad-right "" 5 #\* 0)))

  (test-assert (result=? "325**" (textual-pad-right "325" 5 #\* 0)))

  (test-assert (result=? "71325" (textual-pad-right "71325" 5 #\* 0)))

  (test-assert (result=? "88713" (textual-pad-right "8871325" 5 #\* 0)))

  (test-assert (result=? "25***" (textual-pad-right "325" 5 #\* 1)))

  (test-assert (result=? "1325*" (textual-pad-right "71325" 5 #\* 1)))

  (test-assert (result=? "87132" (textual-pad-right "8871325" 5 #\* 1)))

  (test-assert (result=? "" (textual-pad-right "" 0 #\* 0 0)))

  (test-assert (result=? "*****" (textual-pad-right "" 5 #\* 0 0)))

  (test-assert (result=? "325**" (textual-pad-right "325" 5 #\* 0 3)))

  (test-assert (result=? "713**" (textual-pad-right "71325" 5 #\* 0 3)))

  (test-assert (result=? "887**" (textual-pad-right "8871325" 5 #\* 0 3)))

  (test-assert (result=? "25***" (textual-pad-right "325" 5 #\* 1 3)))

  (test-assert (result=? "132**" (textual-pad-right "71325" 5 #\* 1 4)))

  (test-assert (result=? "8713*" (textual-pad-right "8871325" 5 #\* 1 5)))


  (test-assert (result=? "" (textual-trim (string->text ""))))

  (test-assert
   (result=? "a  b  c  " (textual-trim (string->text "  a  b  c  "))))

  (test-assert
   (result=? "" (textual-trim (string->text "") char-whitespace?)))

  (test-assert (result=? "a  b  c  "
                         (textual-trim (string->text "  a  b  c  ")
                                       char-whitespace?)))

  (test-assert (result=? "" (textual-trim (string->text "  a  b  c  ") char?)))

  (test-assert
   (result=? "" (textual-trim (string->text "") char-whitespace? 0)))

  (test-assert (result=? "a  b  c  "
                         (textual-trim (string->text "  a  b  c  ")
                                       char-whitespace?
                                       0)))

  (test-assert
   (result=? "" (textual-trim (string->text "  a  b  c  ") char? 0)))

  (test-assert (result=? "b  c  "
                         (textual-trim (string->text "  a  b  c  ")
                                       char-whitespace?
                                       3)))

  (test-assert
   (result=? "" (textual-trim (string->text "  a  b  c  ") char? 3)))

  (test-assert
   (result=? "" (textual-trim (string->text "  a  b  c  ") char? 0 11)))

  (test-assert (result=? "b  c  "
                         (textual-trim (string->text "  a  b  c  ")
                                       char-whitespace?
                                       3
                                       11)))

  (test-assert
   (result=? "" (textual-trim (string->text "  a  b  c  ") char? 3 11)))

  (test-assert (result=? ""
                         (textual-trim (string->text "  a  b  c  ") char? 0 8)))

  (test-assert (result=? "b  "
                         (textual-trim (string->text "  a  b  c  ")
                                       char-whitespace?
                                       3
                                       8)))

  (test-assert (result=? ""
                         (textual-trim (string->text "  a  b  c  ")
                                       char?
                                       3
                                       8)))


  (test-assert (result=? "" (textual-trim-right (string->text ""))))

  (test-assert (result=? "  a  b  c"
                         (textual-trim-right (string->text "  a  b  c  "))))

  (test-assert
   (result=? "" (textual-trim-right (string->text "") char-whitespace?)))

  (test-assert (result=? "  a  b  c"
                         (textual-trim-right (string->text "  a  b  c  ")
                                             char-whitespace?)))

  (test-assert
   (result=? "" (textual-trim-right (string->text "  a  b  c  ") char?)))

  (test-assert (result=? ""
                         (textual-trim-right (string->text "")
                                             char-whitespace?
                                             0)))

  (test-assert (result=? "  a  b  c"
                         (textual-trim-right (string->text "  a  b  c  ")
                                             char-whitespace?
                                             0)))

  (test-assert (result=? ""
                         (textual-trim-right (string->text "  a  b  c  ")
                                             char?
                                             0)))

  (test-assert (result=? "  b  c"
                         (textual-trim-right (string->text "  a  b  c  ")
                                             char-whitespace?
                                             3)))

  (test-assert (result=? ""
                         (textual-trim-right (string->text "  a  b  c  ")
                                             char?
                                             3)))

  (test-assert (result=? ""
                         (textual-trim-right (string->text "  a  b  c  ")
                                             char?
                                             0
                                             11)))

  (test-assert (result=? "  b  c"
                         (textual-trim-right (string->text "  a  b  c  ")
                                             char-whitespace?
                                             3
                                             11)))

  (test-assert (result=? ""
                         (textual-trim-right (string->text "  a  b  c  ")
                                             char?
                                             3
                                             11)))

  (test-assert (result=? ""
                         (textual-trim-right (string->text "  a  b  c  ")
                                             char?
                                             0
                                             8)))

  (test-assert (result=? "  b"
                         (textual-trim-right (string->text "  a  b  c  ")
                                             char-whitespace?
                                             3
                                             8)))

  (test-assert (result=? ""
                         (textual-trim-right (string->text "  a  b  c  ")
                                             char?
                                             3
                                             8)))


  (test-assert (result=? "" (textual-trim-both (string->text ""))))

  (test-assert
   (result=? "a  b  c" (textual-trim-both (string->text "  a  b  c  "))))

  (test-assert
   (result=? "" (textual-trim-both (string->text "") char-whitespace?)))

  (test-assert
   (result=? "a  b  c"
             (textual-trim-both (string->text "  a  b  c  ") char-whitespace?)))

  (test-assert
   (result=? "" (textual-trim-both (string->text "  a  b  c  ") char?)))

  (test-assert
   (result=? "" (textual-trim-both (string->text "") char-whitespace? 0)))

  (test-assert
   (result=? "a  b  c"
             (textual-trim-both (string->text "  a  b  c  ")
                                char-whitespace?
                                0)))

  (test-assert
   (result=? "" (textual-trim-both (string->text "  a  b  c  ") char? 0)))

  (test-assert
   (result=? "b  c"
             (textual-trim-both (string->text "  a  b  c  ")
                                char-whitespace?
                                3)))

  (test-assert
   (result=? "" (textual-trim-both (string->text "  a  b  c  ") char? 3)))

  (test-assert
   (result=? "" (textual-trim-both (string->text "  a  b  c  ") char? 0 11)))

  (test-assert
   (result=? "b  c" (textual-trim-both (string->text "  a  b  c  ")
                                       char-whitespace? 3 11)))

  (test-assert
   (result=? "" (textual-trim-both (string->text "  a  b  c  ") char? 3 11)))

  (test-assert
   (result=? "" (textual-trim-both (string->text "  a  b  c  ") char? 0 8)))

  (test-assert
   (result=? "b" (textual-trim-both (string->text "  a  b  c  ")
                                    char-whitespace? 3 8)))

  (test-assert
   (result=? "" (textual-trim-both (string->text "  a  b  c  ") char? 3 8)))


  (test-assert (result=? "" (textual-trim "")))

  (test-assert (result=? "a  b  c  " (textual-trim "  a  b  c  ")))

  (test-assert (result=? "" (textual-trim "" char-whitespace?)))

  (test-assert
   (result=? "a  b  c  " (textual-trim "  a  b  c  " char-whitespace?)))

  (test-assert (result=? "" (textual-trim "  a  b  c  " char?)))

  (test-assert (result=? "" (textual-trim "" char-whitespace? 0)))

  (test-assert
   (result=? "a  b  c  " (textual-trim "  a  b  c  " char-whitespace? 0)))

  (test-assert (result=? "" (textual-trim "  a  b  c  " char? 0)))

  (test-assert
   (result=? "b  c  " (textual-trim "  a  b  c  " char-whitespace? 3)))

  (test-assert (result=? "" (textual-trim "  a  b  c  " char? 3)))

  (test-assert (result=? "" (textual-trim "  a  b  c  " char? 0 11)))

  (test-assert
   (result=? "b  c  " (textual-trim "  a  b  c  " char-whitespace? 3 11)))

  (test-assert (result=? "" (textual-trim "  a  b  c  " char? 3 11)))

  (test-assert (result=? "" (textual-trim "  a  b  c  " char? 0 8)))

  (test-assert
   (result=? "b  " (textual-trim "  a  b  c  " char-whitespace? 3 8)))

  (test-assert (result=? "" (textual-trim "  a  b  c  " char? 3 8)))


  (test-assert (result=? "" (textual-trim-right "")))

  (test-assert (result=? "  a  b  c" (textual-trim-right "  a  b  c  ")))

  (test-assert (result=? "" (textual-trim-right "" char-whitespace?)))

  (test-assert
   (result=? "  a  b  c" (textual-trim-right "  a  b  c  " char-whitespace?)))

  (test-assert (result=? "" (textual-trim-right "  a  b  c  " char?)))

  (test-assert (result=? "" (textual-trim-right "" char-whitespace? 0)))

  (test-assert
   (result=? "  a  b  c" (textual-trim-right "  a  b  c  " char-whitespace? 0)))

  (test-assert (result=? "" (textual-trim-right "  a  b  c  " char? 0)))

  (test-assert
   (result=? "  b  c" (textual-trim-right "  a  b  c  " char-whitespace? 3)))

  (test-assert (result=? "" (textual-trim-right "  a  b  c  " char? 3)))

  (test-assert (result=? "" (textual-trim-right "  a  b  c  " char? 0 11)))

  (test-assert
   (result=? "  b  c" (textual-trim-right "  a  b  c  " char-whitespace? 3 11)))

  (test-assert (result=? "" (textual-trim-right "  a  b  c  " char? 3 11)))

  (test-assert (result=? "" (textual-trim-right "  a  b  c  " char? 0 8)))

  (test-assert
   (result=? "  b" (textual-trim-right "  a  b  c  " char-whitespace? 3 8)))

  (test-assert (result=? "" (textual-trim-right "  a  b  c  " char? 3 8)))


  (test-assert (result=? "" (textual-trim-both "")))

  (test-assert (result=? "a  b  c" (textual-trim-both "  a  b  c  ")))

  (test-assert (result=? "" (textual-trim-both "" char-whitespace?)))

  (test-assert
   (result=? "a  b  c" (textual-trim-both "  a  b  c  " char-whitespace?)))

  (test-assert (result=? "" (textual-trim-both "  a  b  c  " char?)))

  (test-assert (result=? "" (textual-trim-both "" char-whitespace? 0)))

  (test-assert
   (result=? "a  b  c" (textual-trim-both "  a  b  c  " char-whitespace? 0)))

  (test-assert (result=? "" (textual-trim-both "  a  b  c  " char? 0)))

  (test-assert
   (result=? "b  c" (textual-trim-both "  a  b  c  " char-whitespace? 3)))

  (test-assert (result=? "" (textual-trim-both "  a  b  c  " char? 3)))

  (test-assert (result=? "" (textual-trim-both "  a  b  c  " char? 0 11)))

  (test-assert
   (result=? "b  c" (textual-trim-both "  a  b  c  " char-whitespace? 3 11)))

  (test-assert (result=? "" (textual-trim-both "  a  b  c  " char? 3 11)))

  (test-assert (result=? "" (textual-trim-both "  a  b  c  " char? 0 8)))

  (test-assert
   (result=? "b" (textual-trim-both "  a  b  c  " char-whitespace? 3 8)))

  (test-assert (result=? "" (textual-trim-both "  a  b  c  " char? 3 8)))
  )
