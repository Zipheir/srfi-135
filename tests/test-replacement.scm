(test-group "Replacement"
  (test-assert
   (result=? "It's lots of fun to code it up in Scheme."
             (textual-replace (as-text "It's easy to code it up in Scheme.")
                              (as-text "lots of fun")
                              5 9)))

  (test-assert
   (result=? "The miserable perl programmer endured daily ridicule."
             (textual-replace "The TCL programmer endured daily ridicule."
                              (as-text "another miserable perl drone")
                              4
                              7
                              8
                              22)))

  (test-assert
   (result=? "It's really easy to code it up in Scheme."
             (textual-replace (as-text "It's easy to code it up in Scheme.")
                              "really "
                              5
                              5)))

  (test-assert
   (result=? "Runs in O(1) time." ; for texts (using sample implementations)
             (textual-replace "Runs in O(n) time." (text #\1) 10 11)))
  )
