(test-group "Prefixes and suffixes"
  ;;; textual-prefix-length

  (test 0 (textual-prefix-length ABC ABCDEF))

  (test 0 (textual-prefix-length ABCDEF ABC))

  (test 0 (textual-prefix-length ABCDEF DEFABC))

  (test 6 (textual-prefix-length DEFABC DEFABC))

  (test 6 (textual-prefix-length (textual->string DEFABC) DEFABC))

  (test 6 (textual-prefix-length DEFABC (textual->string DEFABC)))

  (test 6 (textual-prefix-length (textual->string DEFABC)
                                 (textual->string DEFABC)))

  (test 0 (textual-prefix-length (as-text "") (as-text "")))

  (test 0 (textual-prefix-length (as-text "") (as-text "aabbccddee")))

  (test 0 (textual-prefix-length (as-text "aisle") (as-text "")))

  (test 0 (textual-prefix-length (as-text "") (as-text "aabbccddee")))

  (test 1 (textual-prefix-length (as-text "aisle") (as-text "aabbccddee")))

  (test 0 (textual-prefix-length (as-text "bail") (as-text "aabbccddee")))

  (test 4 (textual-prefix-length (as-text "prefix") (as-text "preface")))

  (test 0 (textual-prefix-length (as-text "") (as-text "") 0))

  (test 0 (textual-prefix-length (as-text "") (as-text "aabbccddee") 0))

  (test 0 (textual-prefix-length (as-text "aisle") (as-text "") 0))

  (test 1 (textual-prefix-length (as-text "aisle") (as-text "aabbccddee") 0))

  (test 0 (textual-prefix-length (as-text "bail") (as-text "aabbccddee") 0))

  (test 4 (textual-prefix-length (as-text "prefix") (as-text "preface") 0))

  (test 0 (textual-prefix-length (as-text "aisle") (as-text "") 1))

  (test 0 (textual-prefix-length (as-text "aisle") (as-text "aabbccddee") 1))

  (test 1 (textual-prefix-length (as-text "bail") (as-text "aabbccddee") 1))

  (test 0 (textual-prefix-length (as-text "prefix") (as-text "preface") 1))

  (test 0 (textual-prefix-length (as-text "") (as-text "") 0 0))

  (test 0 (textual-prefix-length (as-text "") (as-text "aabbccddee") 0 0))

  (test 0 (textual-prefix-length (as-text "aisle") (as-text "") 0 4))

  (test 1 (textual-prefix-length (as-text "aisle") (as-text "aabbccddee") 0 4))

  (test 0 (textual-prefix-length (as-text "bail") (as-text "aabbccddee") 0 1))

  (test 0 (textual-prefix-length (as-text "aisle") (as-text "") 1 4))

  (test 0 (textual-prefix-length (as-text "aisle") (as-text "aabbccddee") 1 4))

  (test 1 (textual-prefix-length (as-text "bail") (as-text "aabbccddee") 1 4))

  (test 0 (textual-prefix-length (as-text "prefix") (as-text "preface") 1 5))

  (test 0 (textual-prefix-length (as-text "") (as-text "") 0 0 0))

  (test 0 (textual-prefix-length (as-text "") (as-text "aabbccddee") 0 0 0))

  (test 0 (textual-prefix-length (as-text "aisle") (as-text "") 0 4 0))

  (test 0 (textual-prefix-length (as-text "aisle")
                                 (as-text "aabbccddee")
                                 0
                                 4
                                 2))

  (test 1 (textual-prefix-length (as-text "bail") (as-text "aabbccddee") 0 1 2))

  (test 0 (textual-prefix-length (as-text "prefix") (as-text "preface") 0 5 1))

  (test 0 (textual-prefix-length (as-text "aisle") (as-text "") 1 4 0))

  (test 0 (textual-prefix-length (as-text "aisle")
                                 (as-text "aabbccddee")
                                 1
                                 4
                                 3))

  (test 0 (textual-prefix-length (as-text "bail") (as-text "aabbccddee") 1 4 3))

  (test 3 (textual-prefix-length (as-text "prefix") (as-text "preface") 1 5 1))

  (test 0 (textual-prefix-length (as-text "") (as-text "") 0 0 0 0))

  (test 0 (textual-prefix-length (as-text "") (as-text "aabbccddee") 0 0 0 0))

  (test 0 (textual-prefix-length (as-text "aisle") (as-text "") 0 4 0 0))

  (test 0 (textual-prefix-length (as-text "aisle") "aabbccddee" 0 4 2 10))

  (test 1 (textual-prefix-length (as-text "bail")
                                 (as-text "aabbccddee")
                                 0
                                 1
                                 2
                                 10))

  (test 0 (textual-prefix-length (as-text "prefix")
                                 (as-text "preface")
                                 0
                                 5
                                 1
                                 6))

  (test 0 (textual-prefix-length (as-text "aisle") (as-text "") 1 4 0 0))

  (test 0 (textual-prefix-length (as-text "aisle")
                                 (as-text "aabbccddee")
                                 1
                                 4
                                 3
                                 3))

  (test 0 (textual-prefix-length (as-text "bail")
                                 (as-text "aabbccddee")
                                 1
                                 4
                                 3
                                 6))

  (test 3 (textual-prefix-length (as-text "prefix")
                                 (as-text "preface")
                                 1
                                 5
                                 1
                                 7))

  ;;; textual-suffix-length

  (test 0 (textual-suffix-length ABC ABCDEF))

  (test 0 (textual-suffix-length ABCDEF ABC))

  (test 0 (textual-suffix-length ABCDEF DEFABC))

  (test 6 (textual-suffix-length DEFABC DEFABC))

  (test 6 (textual-suffix-length (textual->string DEFABC) DEFABC))

  (test 6 (textual-suffix-length DEFABC (textual->string DEFABC)))

  (test 6 (textual-suffix-length (textual->string DEFABC)
                                 (textual->string DEFABC)))

  (test 0 (textual-suffix-length (as-text "") (as-text "")))

  (test 0 (textual-suffix-length (as-text "") (as-text "aabbccddee")))

  (test 0 (textual-suffix-length (as-text "aisle") (as-text "")))

  (test 0 (textual-suffix-length (as-text "") (as-text "aabbccddee")))

  (test 1 (textual-suffix-length (as-text "aisle") (as-text "aabbccddee")))

  (test 0 (textual-suffix-length (as-text "bail") (as-text "aabbccddee")))

  (test 3 (textual-suffix-length (as-text "place") (as-text "preface")))

  (test 0 (textual-suffix-length (as-text "") (as-text "") 0))

  (test 0 (textual-suffix-length (as-text "") (as-text "aabbccddee") 0))

  (test 0 (textual-suffix-length (as-text "aisle") (as-text "") 0))

  (test 1 (textual-suffix-length (as-text "aisle") (as-text "aabbccddee") 0))

  (test 0 (textual-suffix-length (as-text "bail") (as-text "aabbccddee") 0))

  (test 3 (textual-suffix-length (as-text "place") (as-text "preface") 0))

  (test 0 (textual-suffix-length (as-text "aisle") (as-text "") 1))

  (test 1 (textual-suffix-length (as-text "aisle") (as-text "aabbccddee") 1))

  (test 0 (textual-suffix-length (as-text "bail") (as-text "aabbccddee") 1))

  (test 3 (textual-suffix-length (as-text "place") (as-text "preface") 1))

  (test 0 (textual-suffix-length (as-text "") (as-text "") 0 0))

  (test 0 (textual-suffix-length (as-text "") (as-text "aabbccddee") 0 0))

  (test 0 (textual-suffix-length (as-text "aisle") (as-text "") 0 4))

  (test 0 (textual-suffix-length (as-text "aisle") (as-text "aabbccddee") 0 4))

  (test 0 (textual-suffix-length (as-text "bail") (as-text "aabbccddee") 0 1))

  (test 0 (textual-suffix-length (as-text "aisle") (as-text "") 1 4))

  (test 0 (textual-suffix-length (as-text "aisle") (as-text "aabbccddee") 1 4))

  (test 1 (textual-suffix-length (as-text "aisle") (as-text "aabbccddee") 1 5))

  (test 0 (textual-suffix-length (as-text "bail") (as-text "aabbccddee") 1 4))

  (test 3 (textual-suffix-length (as-text "place") (as-text "preface") 1 5))

  (test 0 (textual-suffix-length (as-text "") (as-text "") 0 0 0))

  (test 0 (textual-suffix-length (as-text "") (as-text "aabbccddee") 0 0 0))

  (test 0 (textual-suffix-length (as-text "aisle") (as-text "") 0 4 0))

  (test 0 (textual-suffix-length (as-text "aisle")
                                 (as-text "aabbccddee")
                                 0
                                 4
                                 2))

  (test 0 (textual-suffix-length (as-text "bail") (as-text "aabbccddee") 0 1 2))

  (test 3 (textual-suffix-length (as-text "place") (as-text "preface") 0 5 1))

  (test 0 (textual-suffix-length (as-text "aisle") (as-text "") 1 4 0))

  (test 0 (textual-suffix-length (as-text "aisle")
                                 (as-text "aabbccddee")
                                 1
                                 4
                                 3))

  (test 0 (textual-suffix-length (as-text "bail") (as-text "aabbccddee") 1 4 3))

  (test 3 (textual-suffix-length (as-text "place") (as-text "preface") 1 5 1))

  (test 0 (textual-suffix-length (as-text "") (as-text "") 0 0 0 0))

  (test 0 (textual-suffix-length (as-text "") (as-text "aabbccddee") 0 0 0 0))

  (test 0 (textual-suffix-length (as-text "aisle") (as-text "") 0 4 0 0))

  (test 1 (textual-suffix-length "aisle" (as-text "aabbccddee") 0 5 2 10))

  (test 1 (textual-suffix-length (as-text "bail")
                                 (as-text "aabbccddee")
                                 0
                                 1
                                 2
                                 4))

  (test 0 (textual-suffix-length (as-text "place") (as-text "preface") 0 5 1 6))

  (test 2 (textual-suffix-length (as-text "place") (as-text "preface") 0 4 1 6))

  (test 0 (textual-suffix-length (as-text "aisle") (as-text "") 1 4 0 0))

  (test 0 (textual-suffix-length (as-text "aisle")
                                 (as-text "aabbccddee")
                                 1
                                 4
                                 3
                                 3))

  (test 0 (textual-suffix-length (as-text "bail")
                                 (as-text "aabbccddee")
                                 1
                                 4
                                 3
                                 6))

  (test 3 (textual-suffix-length (as-text "place") (as-text "preface") 1 5 1 7))

  ;;; textual-prefix?/-suffix?

  (test #f (textual-prefix? ABC ABCDEF))

  (test #f (textual-prefix? ABCDEF ABC))

  (test #f (textual-prefix? ABCDEF DEFABC))

  (test #t (textual-prefix? DEFABC DEFABC))

  (test #t (textual-prefix? (textual->string DEFABC) DEFABC))

  (test #t (textual-prefix? DEFABC (textual->string DEFABC)))

  (test #t (textual-prefix? (textual->string DEFABC) (textual->string DEFABC)))

  (test #t (textual-prefix? (as-text "") (as-text "")))

  (test #t (textual-prefix? (as-text "") (as-text "abc")))

  (test #t (textual-prefix? (as-text "a") (as-text "abc")))

  (test #f (textual-prefix? (as-text "c") (as-text "abc")))

  (test #t (textual-prefix? (as-text "ab") (as-text "abc")))

  (test #f (textual-prefix? (as-text "ac") (as-text "abc")))

  (test #t (textual-prefix? (as-text "abc") (as-text "abc")))

  (test #f (textual-suffix? ABC ABCDEF))

  (test #f (textual-suffix? ABCDEF ABC))

  (test #f (textual-suffix? ABCDEF DEFABC))

  (test #t (textual-suffix? DEFABC DEFABC))

  (test #t (textual-suffix? (textual->string DEFABC) DEFABC))

  (test #t (textual-suffix? DEFABC (textual->string DEFABC)))

  (test #t (textual-suffix? (as-text "") (as-text "")))

  (test #t (textual-suffix? (as-text "") (as-text "abc")))

  (test #f (textual-suffix? (as-text "a") (as-text "abc")))

  (test #t (textual-suffix? (as-text "c") (as-text "abc")))

  (test #f (textual-suffix? (as-text "ac") (as-text "abc")))

  (test #t (textual-suffix? (as-text "bc") (as-text "abc")))

  (test #t (textual-suffix? (as-text "abc") (as-text "abc")))

  (test #t (textual-prefix? (as-text "") (as-text "") 0))

  (test #t (textual-prefix? (as-text "") (as-text "abc") 0))

  (test #t (textual-prefix? (as-text "a") (as-text "abc") 0))

  (test #f (textual-prefix? (as-text "c") (as-text "abc") 0))

  (test #t (textual-prefix? (as-text "ab") (as-text "abc") 0))

  (test #f (textual-prefix? (as-text "ac") (as-text "abc") 0))

  (test #t (textual-prefix? (as-text "abc") (as-text "abc") 0))

  (test #t (textual-suffix? (as-text "") (as-text "") 0))

  (test #t (textual-suffix? (as-text "") (as-text "abc") 0))

  (test #f (textual-suffix? (as-text "a") (as-text "abc") 0))

  (test #t (textual-suffix? (as-text "c") (as-text "abc") 0))

  (test #f (textual-suffix? (as-text "ac") (as-text "abc") 0))

  (test #t (textual-suffix? (as-text "bc") (as-text "abc") 0))

  (test #t (textual-suffix? (as-text "abc") (as-text "abc") 0))

  (test #t (textual-prefix? (as-text "ab") (as-text "abc") 2))

  (test #t (textual-prefix? (as-text "ac") (as-text "abc") 2))

  (test #f (textual-prefix? (as-text "abc") (as-text "abc") 2))

  (test #t (textual-suffix? (as-text "ac") (as-text "abc") 2))

  (test #t (textual-suffix? (as-text "bc") (as-text "abc") 2))

  (test #t (textual-suffix? (as-text "abc") (as-text "abc") 2))


  (test #t (textual-prefix? (as-text "") (as-text "") 0 0))

  (test #t (textual-prefix? (as-text "") (as-text "abc") 0 0))

  (test #t (textual-prefix? (as-text "a") (as-text "abc") 0 0))

  (test #f (textual-prefix? (as-text "c") (as-text "abc") 0 1))

  (test #t (textual-prefix? (as-text "ab") (as-text "abc") 0 1))

  (test #t (textual-prefix? (as-text "ab") (as-text "abc") 0 2))

  (test #f (textual-prefix? (as-text "ac") (as-text "abc") 0 2))

  (test #t (textual-prefix? (as-text "abc") (as-text "abc") 0 3))

  (test #t (textual-suffix? (as-text "") (as-text "") 0 0))

  (test #t (textual-suffix? (as-text "") (as-text "abc") 0 0))

  (test #f (textual-suffix? (as-text "a") (as-text "abc") 0 1))

  (test #t (textual-suffix? (as-text "c") (as-text "abc") 0 1))

  (test #t (textual-suffix? (as-text "ac") (as-text "abc") 1 2))

  (test #f (textual-suffix? (as-text "ac") (as-text "abc") 0 2))

  (test #t (textual-suffix? (as-text "bc") (as-text "abc") 0 2))

  (test #t (textual-suffix? (as-text "abc") (as-text "abc") 0 3))

  (test #t (textual-prefix? (as-text "ab") (as-text "abc") 2 2))

  (test #t (textual-prefix? (as-text "ac") (as-text "abc") 2 2))

  (test #f (textual-prefix? (as-text "abc") (as-text "abc") 2 3))

  (test #t (textual-suffix? (as-text "ac") (as-text "abc") 2 2))

  (test #t (textual-suffix? (as-text "bc") (as-text "abc") 2 2))

  (test #t (textual-suffix? (as-text "abc") (as-text "abc") 2 3))


  (test #t (textual-prefix? (as-text "") (as-text "") 0 0 0))

  (test #t (textual-prefix? (as-text "") (as-text "abc") 0 0 0))

  (test #t (textual-prefix? (as-text "a") (as-text "abc") 0 0 0))

  (test #f (textual-prefix? (as-text "c") (as-text "abc") 0 1 0))

  (test #t (textual-prefix? (as-text "ab") (as-text "abc") 0 1 0))

  (test #t (textual-prefix? (as-text "ab") (as-text "abc") 0 2 0))

  (test #f (textual-prefix? (as-text "ac") (as-text "abc") 0 2 0))

  (test #t (textual-prefix? (as-text "abc") (as-text "abc") 0 3 0))

  (test #t (textual-suffix? (as-text "") (as-text "") 0 0 0))

  (test #t (textual-suffix? (as-text "") (as-text "abc") 0 0 0))

  (test #f (textual-suffix? (as-text "a") (as-text "abc") 0 1 0))

  (test #t (textual-suffix? (as-text "c") (as-text "abc") 0 1 0))

  (test #t (textual-suffix? (as-text "ac") (as-text "abc") 1 2 0))

  (test #f (textual-suffix? (as-text "ac") (as-text "abc") 0 2 0))

  (test #t (textual-suffix? (as-text "bc") (as-text "abc") 0 2 0))

  (test #t (textual-suffix? (as-text "abc") (as-text "abc") 0 3 0))

  (test #t (textual-prefix? (as-text "ab") (as-text "abc") 2 2 0))

  (test #t (textual-prefix? (as-text "ac") (as-text "abc") 2 2 0))

  (test #f (textual-prefix? (as-text "abc") (as-text "abc") 2 3 0))

  (test #t (textual-suffix? (as-text "ac") (as-text "abc") 2 2 0))

  (test #t (textual-suffix? (as-text "bc") (as-text "abc") 2 2 0))

  (test #t (textual-suffix? (as-text "abc") (as-text "abc") 2 3 0))

  (test #t (textual-prefix? (as-text "") (as-text "abc") 0 0 1))

  (test #t (textual-prefix? (as-text "a") (as-text "abc") 0 0 1))

  (test #t (textual-prefix? (as-text "c") (as-text "abc") 0 1 2))

  (test #f (textual-prefix? (as-text "ab") (as-text "abc") 0 1 2))

  (test #f (textual-prefix? (as-text "ab") (as-text "abc") 0 2 1))

  (test #f (textual-prefix? (as-text "ac") (as-text "abc") 0 2 1))

  (test #f (textual-prefix? (as-text "abc") (as-text "abc") 0 3 1))

  (test #f (textual-suffix? (as-text "a") (as-text "abc") 0 1 2))

  (test #t (textual-suffix? (as-text "c") (as-text "abc") 0 1 1))

  (test #t (textual-suffix? (as-text "ac") (as-text "abc") 1 2 2))

  (test #t (textual-suffix? (as-text "bc") (as-text "abc") 0 2 1))

  (test #f (textual-suffix? (as-text "bc") (as-text "abc") 0 2 2))


  (test #t (textual-prefix? (as-text "") (as-text "") 0 0 0 0))

  (test #t (textual-prefix? (as-text "") (as-text "abc") 0 0 0 3))

  (test #t (textual-prefix? (as-text "a") (as-text "abc") 0 0 0 3))

  (test #f (textual-prefix? (as-text "c") (as-text "abc") 0 1 0 3))

  (test #t (textual-prefix? (as-text "ab") (as-text "abc") 0 1 0 3))

  (test #t (textual-prefix? (as-text "ab") (as-text "abc") 0 2 0 3))

  (test #f (textual-prefix? (as-text "ac") (as-text "abc") 0 2 0 3))

  (test #t (textual-prefix? (as-text "abc") (as-text "abc") 0 3 0 3))

  (test #t (textual-suffix? (as-text "") (as-text "abc") 0 0 0 3))

  (test #f (textual-suffix? (as-text "a") (as-text "abc") 0 1 0 3))

  (test #t (textual-suffix? (as-text "c") (as-text "abc") 0 1 0 3))

  (test #t (textual-suffix? (as-text "ac") (as-text "abc") 1 2 0 3))

  (test #f (textual-suffix? (as-text "ac") (as-text "abc") 0 2 0 3))

  (test #t (textual-suffix? (as-text "bc") (as-text "abc") 0 2 0 3))

  (test #t (textual-suffix? (as-text "abc") (as-text "abc") 0 3 0 3))

  (test #t (textual-prefix? (as-text "ab") (as-text "abc") 2 2 0 3))

  (test #t (textual-prefix? (as-text "ac") (as-text "abc") 2 2 0 3))

  (test #f (textual-prefix? (as-text "abc") (as-text "abc") 2 3 0 3))

  (test #t (textual-suffix? (as-text "ac") (as-text "abc") 2 2 0 3))

  (test #t (textual-suffix? (as-text "bc") (as-text "abc") 2 2 0 3))

  (test #t (textual-suffix? (as-text "abc") (as-text "abc") 2 3 0 3))

  (test #t (textual-prefix? (as-text "") (as-text "abc") 0 0 1 3))

  (test #t (textual-prefix? (as-text "a") (as-text "abc") 0 0 1 3))

  (test #t (textual-prefix? (as-text "c") (as-text "abc") 0 1 2 3))

  (test #f (textual-prefix? (as-text "ab") (as-text "abc") 0 1 2 3))

  (test #f (textual-prefix? (as-text "ab") (as-text "abc") 0 2 1 3))

  (test #f (textual-prefix? (as-text "ac") (as-text "abc") 0 2 1 3))

  (test #f (textual-prefix? (as-text "abc") (as-text "abc") 0 3 1 3))

  (test #f (textual-suffix? (as-text "a") (as-text "abc") 0 1 2 3))

  (test #t (textual-suffix? (as-text "c") (as-text "abc") 0 1 1 3))

  (test #t (textual-suffix? (as-text "ac") (as-text "abc") 1 2 2 3))

  (test #t (textual-suffix? (as-text "bc") (as-text "abc") 0 2 1 3))

  (test #f (textual-suffix? (as-text "bc") (as-text "abc") 0 2 2 3))


  (test #t (textual-prefix? (as-text "") (as-text "abc") 0 0 0 2))

  (test #t (textual-prefix? (as-text "a") (as-text "abc") 0 0 0 2))

  (test #f (textual-prefix? (as-text "c") (as-text "abc") 0 1 0 2))

  (test #t (textual-prefix? (as-text "ab") (as-text "abc") 0 1 0 2))

  (test #f (textual-prefix? (as-text "abc") (as-text "abc") 0 3 0 2))

  (test #t (textual-suffix? (as-text "") (as-text "abc") 0 0 0 2))

  (test #f (textual-suffix? (as-text "c") (as-text "abc") 0 1 0 2))

  (test #f (textual-suffix? (as-text "ac") (as-text "abc") 1 2 0 2))
  )
