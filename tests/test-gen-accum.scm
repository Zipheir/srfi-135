(test-group "Generators"
  (test-assert (eof-object? ((textual->generator (text)))))
  (test (string->list "abcdef")
        (generator->list (textual->generator "abcdef")))
  (test (string->list "αβγδεζ")
        (generator->list (textual->generator (textual->text "αβγδεζ"))))

  (test-assert (textual-null? (generator->text (constantly #!eof))))
  (test-assert
   (result=? "abcdef"
             (generator->text (string->generator "abcdef"))))
  (test-assert
   (result=? "αβγδεζ"
             (generator->text (textual->generator "αβγδεζ"))))
  )

(test-group "text-accumulator"
  (test-assert (textual-null? ((text-accumulator) #!eof)))
  (test-assert
   (result=? "abc"
             (let ((acc (text-accumulator)))
	       (acc #\a)
	       (acc #\b)
	       (acc #\c)
	       (acc #!eof))))
  (test-assert
   (textual=? ABCDEF
              (let ((acc (text-accumulator)))
                (textual-for-each acc ABCDEF)
                (acc #!eof))))
  )
