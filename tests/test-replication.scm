(test-group "Replication and splitting"
  ;;; textual-replicate

  (test-assert
   (result=? "cdefabcdefabcd" (textual-replicate "abcdef" -4 10)))

  (test-assert
   (result=? "bcdefbcdefbcd" (textual-replicate "abcdef" 90 103 1)))

  (test-assert
   (result=? "ecdecdecde" (textual-replicate "abcdef" -13 -3 2 5)))

  ;;; textual-split

  (test '() (map textual->string (textual-split "" "")))

  (test '("a" "b" "c") (map textual->string (textual-split "abc" "")))

  (test '("too" "" "much" "" "data")
         (map textual->string
              (textual-split "too  much  data" " ")))

  (test '("" "there" "ya" "go" "")
         (map textual->string
              (textual-split "***there***ya***go***" "***")))

  (test '() (map textual->string (textual-split "" "" 'infix)))

  (test '("a" "b" "c")
         (map textual->string (textual-split "abc" "" 'infix)))

  (test '("too" "" "much" "" "data")
         (map textual->string
              (textual-split "too  much  data" " " 'infix)))

  (test '("" "there" "ya" "go" "")
         (map textual->string
              (textual-split "***there***ya***go***" "***" 'infix)))

  (test 'error
         (guard (exn (else 'error))
                (map textual->string
                     (textual-split "" "" 'strict-infix))))

  (test '("a" "b" "c")
         (map textual->string
              (textual-split "abc" "" 'strict-infix)))

  (test '("too" "" "much" "" "data")
         (map textual->string
              (textual-split "too  much  data" " " 'strict-infix)))

  (test '("" "there" "ya" "go" "")
         (map textual->string
              (textual-split "***there***ya***go***" "***" 'strict-infix)))

  (test '()
         (map textual->string
              (textual-split "" "" 'prefix)))

  (test '("a" "b" "c")
         (map textual->string
              (textual-split "abc" "" 'prefix)))

  (test '("too" "" "much" "" "data")
         (map textual->string
              (textual-split "too  much  data" " " 'prefix)))

  (test '("there" "ya" "go" "")
         (map textual->string
              (textual-split "***there***ya***go***" "***" 'prefix)))

  (test '()
         (map textual->string
              (textual-split "" "" 'suffix)))

  (test '("a" "b" "c")
         (map textual->string
              (textual-split "abc" "" 'suffix)))

  (test '("too" "" "much" "" "data")
         (map textual->string
              (textual-split "too  much  data" " " 'suffix)))

  (test '("" "there" "ya" "go")
         (map textual->string
              (textual-split "***there***ya***go***" "***" 'suffix)))


  (test '()
         (map textual->string
              (textual-split "" "" 'infix #f)))

  (test '("a" "b" "c")
         (map textual->string
              (textual-split "abc" "" 'infix #f)))

  (test '("too" "" "much" "" "data")
         (map textual->string
              (textual-split "too  much  data" " " 'infix #f)))

  (test '("" "there" "ya" "go" "")
         (map textual->string
              (textual-split "***there***ya***go***" "***" 'infix #f)))

  (test 'error
         (guard (exn (else 'error))
                (map textual->string
                     (textual-split "" "" 'strict-infix #f))))

  (test '("a" "b" "c")
         (map textual->string
              (textual-split "abc" "" 'strict-infix #f)))

  (test '("too" "" "much" "" "data")
         (map textual->string
              (textual-split "too  much  data" " " 'strict-infix #f)))

  (test '("" "there" "ya" "go" "")
         (map textual->string
              (textual-split "***there***ya***go***" "***" 'strict-infix #f)))

  (test '()
         (map textual->string
              (textual-split "" "" 'prefix #f)))

  (test '("a" "b" "c")
         (map textual->string
              (textual-split "abc" "" 'prefix #f)))

  (test '("too" "" "much" "" "data")
         (map textual->string
              (textual-split "too  much  data" " " 'prefix #f)))

  (test '("there" "ya" "go" "")
         (map textual->string
              (textual-split "***there***ya***go***" "***" 'prefix #f)))

  (test '()
         (map textual->string
              (textual-split "" "" 'suffix #f)))

  (test '("a" "b" "c")
         (map textual->string
              (textual-split "abc" "" 'suffix #f)))

  (test '("too" "" "much" "" "data")
         (map textual->string
              (textual-split "too  much  data" " " 'suffix #f)))

  (test '("" "there" "ya" "go")
         (map textual->string
              (textual-split "***there***ya***go***" "***" 'suffix #f)))


  (test 'error
         (guard (exn (else 'error))
                (map textual->string
                     (textual-split "" "" 'strict-infix 3))))

  (test '("a" "b" "c")
         (map textual->string
              (textual-split "abc" "" 'strict-infix 3)))

  (test '("too" "" "much" " data")
         (map textual->string
              (textual-split "too  much  data" " " 'strict-infix 3)))

  (test '("" "there" "ya" "go***")
         (map textual->string
              (textual-split "***there***ya***go***" "***" 'strict-infix 3)))

  (test '()
         (map textual->string
              (textual-split "" "" 'prefix 3)))

  (test '("a" "b" "c")
         (map textual->string
              (textual-split "abc" "" 'prefix 3)))

  (test '("too" "" "much" " data")
         (map textual->string
              (textual-split "too  much  data" " " 'prefix 3)))

  (test '("there" "ya" "go***")
         (map textual->string
              (textual-split "***there***ya***go***" "***" 'prefix 3)))

  (test '()
         (map textual->string
              (textual-split "" "" 'suffix 3)))

  (test '("a" "b" "c")
         (map textual->string
              (textual-split "abc" "" 'suffix 3)))

  (test '("too" "" "much" " data")
         (map textual->string
              (textual-split "too  much  data" " " 'suffix 3)))

  (test '("" "there" "ya" "go***")
         (map textual->string
              (textual-split "***there***ya***go***" "***" 'suffix 3)))


  (test 'error
         (guard (exn (else 'error))
                (map textual->string
                     (textual-split "" "" 'strict-infix 3 0))))

  (test '("b" "c")
         (map textual->string
              (textual-split "abc" "" 'strict-infix 3 1)))

  (test '("oo" "" "much" " data")
         (map textual->string
              (textual-split "too  much  data" " " 'strict-infix 3 1)))

  (test '("**there" "ya" "go" "")
         (map textual->string
              (textual-split "***there***ya***go***" "***" 'strict-infix 3 1)))

  (test '()
         (map textual->string
              (textual-split "" "" 'prefix 3 0)))

  (test '("b" "c")
         (map textual->string
              (textual-split "abc" "" 'prefix 3 1)))

  (test '("oo" "" "much" " data")
         (map textual->string
              (textual-split "too  much  data" " " 'prefix 3 1)))

  (test '("**there" "ya" "go" "")
         (map textual->string
              (textual-split "***there***ya***go***" "***" 'prefix 3 1)))

  (test '()
         (map textual->string
              (textual-split "" "" 'suffix 3 0)))

  (test '("b" "c")
         (map textual->string
              (textual-split "abc" "" 'suffix 3 1)))

  (test '("oo" "" "much" " data")
         (map textual->string
              (textual-split "too  much  data" " " 'suffix 3 1)))

  (test '("**there" "ya" "go")
         (map textual->string
              (textual-split "***there***ya***go***" "***" 'suffix 3 1)))


  (test 'error
         (guard (exn (else 'error))
                (map textual->string
                     (textual-split "" "" 'strict-infix 3 0 0))))

  (test '("b")
         (map textual->string
              (textual-split "abc" "" 'strict-infix 3 1 2)))

  (test '("oo" "" "much" " ")
         (map textual->string
              (textual-split "too  much  data" " " 'strict-infix 3 1 11)))

  (test '()
         (map textual->string
              (textual-split "" "" 'prefix 3 0 0)))

  (test '("b")
         (map textual->string
              (textual-split "abc" "" 'prefix 3 1 2)))

  (test '("oo" "" "much" " ")
         (map textual->string
              (textual-split "too  much  data" " " 'prefix 3 1 11)))

  (test '()
         (map textual->string
              (textual-split "" "" 'suffix 3 0 0)))

  (test '("b")
         (map textual->string
              (textual-split "abc" "" 'suffix 3 1 2)))

  (test '("oo" "" "much" " ")
         (map textual->string
              (textual-split "too  much  data" " " 'suffix 3 1 11)))
  )
