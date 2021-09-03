;;; Constructors

(test-group "Constructors"
  (test #t (result=? ""
                     (text-tabulate
                      (lambda (i)
                        (integer->char (+ i (char->integer #\a))))
                      0)))

  (test #t (result=? "abc"
                     (text-tabulate
                      (lambda (i)
                        (integer->char (+ i (char->integer #\a))))
                      3)))


  (test #t (result=? "abc"
                     (let ((p (open-input-string "abc")))
                       (text-unfold eof-object?
                                    values
                                    (lambda (x) (read-char p))
                                    (read-char p)))))

  (test #t (result=? "" (text-unfold null? car cdr '())))

  (test #t (result=? "abc"
                     (text-unfold null? car cdr (string->list "abc"))))

  (test #t (result=? "def"
                     (text-unfold null? car cdr '() (string->text "def"))))

  (test #t (result=? "defabcG"
                     (text-unfold null?
                                  car
                                  cdr
                                  (string->list "abc")
                                  (string->text "def")
                                  (lambda (x) (if (null? x) (text #\G) "")))))

  (test #t (result=? "" (text-unfold-right null? car cdr '())))

  (test #t (result=? "cba"
                     (text-unfold-right null? car cdr (string->list "abc"))))

  (test #t (result=? "def"
                     (text-unfold-right null?
                                        car
                                        cdr
                                        '()
                                        (string->text "def"))))

  (test #t (result=? "Gcbadef"
                     (text-unfold-right
                      null?
                      car
                      cdr
                      (string->list "abc")
                      (string->text "def")
                      (lambda (x) (if (null? x) (text #\G) "")))))


  (test #t (result=? "def"
                     (text-unfold null? car cdr '() "def")))

  (test #t (result=? "defabcG"
                     (text-unfold null?
                                  car
                                  cdr
                                  (string->list "abc")
                                  "def"
                                  (lambda (x) (if (null? x) "G" "")))))

  (test #t (result=? "dabcG"
                     (text-unfold null?
                                  car
                                  cdr
                                  (string->list "abc")
                                  #\d
                                  (lambda (x) (if (null? x) "G" "")))))

  (test #t (result=? (string-append "%="
                                    (make-string 200 #\*)
                                    "A B C D E F G H I J K L M "
                                    "N O P Q R S T U V W X Y Z "
                                    (make-string (* 200 (- (char->integer #\a)
                                                           (char->integer #\Z)
                                                           1))
                                                 #\*)
                                    "abcdefghijklmnopqrstuvwxyz"
                                    " ")
                     (text-unfold (lambda (n) (char>? (integer->char n) #\z))
                                  (lambda (n)
                                    (let ((c (integer->char n)))
                                      (cond ((r7#char<=? #\a c #\z) c)
                                            ((r7#char<=? #\A c #\Z)
                                             (text c #\space))
                                            (else (make-string 200 #\*)))))
                                  (lambda (n) (+ n 1))
                                  (char->integer #\@)
                                  "%="
                                  (lambda (n) #\space))))

  (test #t (result=? "def"
                     (text-unfold-right null? car cdr '() "def")))

  (test #t (result=? "Gcbadef"
                     (text-unfold-right null?
                                        car
                                        cdr
                                        (string->list "abc")
                                        "def"
                                        (lambda (x) (if (null? x) "G" "")))))

  (test #t (result=? "Gcbad"
                     (text-unfold-right null?
                                        car
                                        cdr
                                        (string->list "abc")
                                        #\d
                                        (lambda (x) (if (null? x) "G" "")))))

  (test #t (result=? (string-append " "
                                    (list->string
                                     (reverse
                                      (string->list
                                       "abcdefghijklmnopqrstuvwxyz")))
                                    (make-string (* 200 (- (char->integer #\a)
                                                           (char->integer #\Z)
                                                           1))
                                                 #\*)
                                    "Z Y X W V U T S R Q P O N "
                                    "M L K J I H G F E D C B A "
                                    (make-string 200 #\*)
                                    "%=")
                     (text-unfold-right
                      (lambda (n) (char>? (integer->char n) #\z))
                      (lambda (n)
                        (let ((c (integer->char n)))
                          (cond ((r7#char<=? #\a c #\z) c)
                                ((r7#char<=? #\A c #\Z) (text c #\space))
                                (else (make-string 200 #\*)))))
                      (lambda (n) (+ n 1))
                      (char->integer #\@)
                      "%="
                      (lambda (n) #\space))))

  (test #t (result=? " The English alphabet: abcdefghijklmnopqrstuvwxyz "
                     (text-unfold-right (lambda (n) (< n (char->integer #\A)))
                                        (lambda (n)
                                          (char-downcase (integer->char n)))
                                        (lambda (n) (- n 1))
                                        (char->integer #\Z)
                                        #\space
                                        (lambda (n)
                                          " The English alphabet: "))))
  )
