;;; FIXME: should test some non-ASCII cases here.

(test-group "Case conversion"
  (test-assert
   (result=? "1234STRIKES" (textual-upcase (as-text "1234Strikes"))))

  (test-assert
   (result=? "1234STRIKES" (textual-upcase (as-text "1234strikes"))))

  (test-assert
   (result=? "1234STRIKES" (textual-upcase (as-text "1234STRIKES"))))

  (test-assert
   (result=? "1234strikes" (textual-downcase (as-text "1234Strikes"))))

  (test-assert
   (result=? "1234strikes" (textual-downcase (as-text "1234strikes"))))

  (test-assert
   (result=? "1234strikes" (textual-downcase (as-text "1234STRIKES"))))

  (test-assert
   (result=? "1234strikes" (textual-foldcase (as-text "1234Strikes"))))

  (test-assert
   (result=? "1234strikes" (textual-foldcase (as-text "1234strikes"))))

  (test-assert
   (result=? "1234strikes" (textual-foldcase (as-text "1234STRIKES"))))

  (test-assert
   (result=? "And With Three Strikes You Are Out"
             (textual-titlecase
              (as-text "and with THREE STRIKES you are oUT"))))
  )
