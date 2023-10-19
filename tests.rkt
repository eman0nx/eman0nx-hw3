#lang racket

(require rackunit
         2htdp/image
         "hw3.rkt")

(provide TESTS)

(define TESTS
  (test-suite
   "hw3"
#|
   (test-case "Exercise 83: consumes an Editor and produces an image."
              (define test-editor (make-editor "hello" "I" "world"))
              (render test-editor))
|#

#|
   (test-case "Exercise 84,86: Produces another Editor, modified in 86 so that keystroke ignored if too big for text"
              (define e1 (make-editor "" "" ""))
              (define updated-e1 (edit e1 "a"))
              (define updated-e2 (edit updated-e1 "b"))
              (display (editor-pre updated-e2)) ;"ab"
|#

   #|
   (test-case "Exercise 85: Interactive editor"
              (define (test1)
                  (run "hello"))
              (test1)

              (define (test-editor)
                (define test-ed (editor "hello" 0))
                (check-equal? (editor-text test-ed) "hello")
                (check-equal? (editor-index test-ed) 0)
                (define ed1 (edit test-ed "w"))
                (check-equal? (editor-text ed1) "hellow") ;fails
                (check-equal? (editor-index ed1) 1)
                (run "hello"))
               (test-editor)
   |#
 #|
    (test-case
        "Exercise 177: Consume 2 strings, produce an Editor"
       
      (check-equal? (editor-kh (create-editor "" "") "e")
               (create-editor "e" ""))
      (check-equal? (editor-kh (create-editor "cd" "fgh") "e")
                    (create-editor "cde" "fgh")))))
|#

   #|
    (test-case
        "Exercise 180: Design editor-text w/o using implode"
        (define my-editor3 (create-editor "hello" "world"))
        (check-equal? (editor-rgt my-editor3) (create-editor "hellow" "orld")))
|#

#|
    (test-case
     "Exercise 179: Design editor-text using implode"
     (define my-editor (create-editor "pre" "post"))
     (editor-render my-editor)
     (check-equal?
      (editor-text
       (cons "p" (cons "o" (cons "s" (cons "t" '())))))
      (text "post" FONT-SIZE FONT-COLOR))
|#


    ))


(module+ main
  (require rackunit/text-ui)
  (run-tests TESTS 'verbose))
