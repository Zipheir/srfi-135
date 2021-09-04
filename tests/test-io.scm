(test-group "I/O"
  ;;; Input
  (test-assert (eof-object? (text-read-line (open-input-string ""))))
  (test-assert (result=? "s" (text-read-line (open-input-string "s"))))
  (test-assert
   (result=? "hello" (text-read-line (open-input-string "hello\nworld\n"))))

  (test-assert (eof-object? (read-text 10 (open-input-string ""))))
  (test-assert (result=? "s" (read-text 10 (open-input-string "s"))))
  (test-assert
   (result=? "abc" (read-text 3 (open-input-string "abcdef"))))
  (test-assert
   (result=? "αβγ" (read-text 3 (open-input-string "αβγδεζ"))))

  (test '() (text-read-lines (open-input-string "")))
  (test-assert
   (result=? "helloworld"
             (textual-concatenate
              (text-read-lines (open-input-string "hello\nworld\n")))))
  (test '() (text-read-lines (open-input-string "hello\n") 0))
  (test-assert
   (result=? "hello"
             (textual-concatenate
              (text-read-lines (open-input-string "hello\nworld\n")
                               1))))

  ;;; Output

  (test ""
        (let ((p (open-output-string)))
          (write-textual (text) p)
          (get-output-string p)))

  (test "hello, world"
        (let ((p (open-output-string)))
          (write-textual "hello, world" p)
          (get-output-string p)))

  (test (textual->string ABCDEF)
        (let ((p (open-output-string)))
          (write-textual ABCDEF p)
          (get-output-string p)))

  (test "δεζ"
        (let ((p (open-output-string)))
          (write-textual "αβγδεζ" p 3)
          (get-output-string p)))

  (test "γδε"
        (let ((p (open-output-string)))
          (write-textual "αβγδεζ" p 2 5)
          (get-output-string p)))

  )
