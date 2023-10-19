#lang racket

(provide (all-defined-out))
(require rackunit)

(require 2htdp/batch-io)
(require 2htdp/universe)
(require 2htdp/image)

;; ex 87
;Develop a data representation for an editor based on our first idea,
;using a string and an index.
;Then solve the preceding exercises again. Retrace the design recipe.
;Hint if you haven’t done so, solve the exercises in Functions.

;(struct editor (text index))

#|
;; ex 83
; Design the function render, which consumes an Editor and produces an image.
; render the text within an empty scene of 200x20 pixels.
; For the cursor, use a 1x20 red rectangle and for the strings, black text of size 16.

; Extract text before and after cursor
; Creates shapes
; overlay/align 'beside' 'above' to arrange within 200x20 scene

(define-struct editor [pre cursor post])

; render : Image
(define (render ed)
  (define cursor (rectangle 1 20 "solid" "red"))
  (define left (text (editor-pre ed) 16 "black")) ;(define left (text (editor-text ed) 16 "black")) 
  (define right (text (editor-post ed) 16 "black")) ;(define right (text (substring (editor-text ed) (editor-index ed) (string-length (editor-text ed))) 16 "black")) 

  ; beside: combine left and right
  (define text-left-cursor-right (beside left cursor right))
  
  ; place in scene
  (overlay/align "left" "center" text-left-cursor-right (empty-scene 200 20)))
|#

;; ex 84, 86
;The function produces another editor.
; Modified so that keystroke ignored if text would be too big for canvas

; Get 1st char
(define (string-first s)
  (if (string=? s "") "" (substring s 0 1)))

; Get rest after 1st char
(define (string-rest s)
  (if (string=? s "") "" (substring s 1)))

; Get last char
(define (string-last s)
  (if (string=? s "") "" (substring s (- (string-length s) 1))))

; Remove last char
(define (string-remove-last s)
  ;(if (string=? s "") "" (substring s 0 (- (string-length s) 1))))
  (substring s 0 (- (string-length s) 1)))

; Check if string empty
(define (string-empty? s)
  (= (string-length s) 0))

; edit fn
;(define (edit ed ke)
  ; set text width
  ;(define (text-width txt)
   ; (image-width (text txt 16 "black")))
  
  ; Left and right arrow keys
  ;(define (arrow-keys ed ke)
    #|(cond
       [(string=? ke "left")
       (if (not (string-empty? (editor-pre ed)))
           (make-editor
            (string-remove-last (editor-pre ed))
            (string-last (editor-pre ed))
            (string-append (string-first (editor-post ed)) (editor-post ed)))
            ed)]
      [(string=? ke "right")
       (if (not (string-empty? (editor-post ed)))
           (make-editor
            (string-append (editor-pre ed) (string-first (editor-post ed)))
            ;(string-append (editor-pre ed) (string-first (editor-post ed)))
            (string-first (editor-post ed))
            (string-rest (editor-post ed)))
           ed)]
      [else ed]))
  |#
  
  ; Check backspace, deletes the character immediately to the left of the cursor

  #|(cond
    [(string=? ke "\b")
     (if (> (editor-index ed) 0)
         (editor (string-append (substring (editor-text ed) 0 (- (editor-index ed) 1))
                                (substring (editor-text ed) (editor-index ed)))
                                (- (editor-index ed) 1)) ed)]
     #| (if (not (string-empty? (editor-pre ed)))
         (make-editor
          (string-remove-last (editor-pre ed))
          (string-remove-last (editor-pre ed))
          (editor-post ed)) ed)] |#
    [(string=? ke "\t") ed]
    [(string=? ke "\r") ed]
    ;[(> (text-width (string-append (editor-pre ed) ke)) 200)
    ; ed]
    [(string=? ke "left")
     ;(arrow-keys ed ke)]
     (if (> (editor-index ed) 0)
         (editor (editor-text ed) (- (editor-index ed) 1)) ed)]
    [(string=? ke "right")
     ;(arrow-keys ed ke)]
     (if (< (editor-index ed) (string-length (editor-text ed)))
         (editor (editor-text ed) (+ (editor-index ed) 1)) ed)]
    ;[(> (string-length ke) 1)
     ;(arrow-keys ed ke)]
    [else
     ;(make-editor
     (editor (string-append
              (substring (editor-text ed) 0
                         (editor-index ed)) ke
                                            (substring (editor-text ed)
                                                       (editor-index ed)))
             (+ (editor-index ed) 1))]))
      ;(string-append (editor-pre ed) ke)
      ;(string-append (editor-pre ed) ke)
      ;(editor-post ed))]))
|#
; Something's wrong with "left": deleting everything after cursor
     


; ex 85
;Define the function run.
;Given the pre field of an editor, it launches an interactive editor,
;using render and edit for the to-draw and on-key clauses, respectively.

#|(define (to-draw ed)
  (define (render-expr ed)
    (render ed))
  (define width-expr 200)
  (define height-expr 20)
  
  (place-image (render-expr ed) (/ width-expr 2) (/ height-expr 2) (empty-scene width-expr height-expr)))

(define (handle-key ed ke)
  (edit ed ke))

(define (run pre)
  ;(big-bang (make-editor pre "" "")
  (big-bang (editor pre 0)
    (on-draw to-draw)
    (on-key handle-key)))
|#



;; ex 177
;The function consumes two strings and produces an Editor.
;The first string is the text to the left of the cursor
;and the second string is the text to the right of the cursor.

;(define-struct Editor (left right))
;(define (create-editor left right)
;  (make-Editor left right))

(define HEIGHT 20) ; the height of the editor 
(define WIDTH 200) ; its width 
(define FONT-SIZE 16) ; the font size 
(define FONT-COLOR "black") ; the font color 
     
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

#|
; Editor -> Image
; renders an editor as an image of the two texts 
; separated by the cursor 
(define (editor-render e)
  (let* ((left-text (Editor-left e))
         (right-text (Editor-right e))
         (left-image (text left-text FONT-SIZE FONT-COLOR))
         (right-image (text right-text FONT-SIZE FONT-COLOR))
    (place-image CURSOR (/ WIDTH 2) (/ HEIGHT 2)
                 (above left-image right-image))))

; Editor KeyEvent -> Editor
; deals with a key event, given some editor
;(define (editor-kh ed ke) ed)(index "editor-kh")
(define (editor-kh ed k)
  (cond
    [(key=? k "left")
     (match ed
       [(Editor left right)
        (if (string-empty? left)
            ed
            (make-Editor (substring left 0 (- (string-length left) 1))
                       (string-append (substring left (- (string-length left) 1)) right)))])]
    [(key=? k "right")
     (match ed
       [(Editor left right)
        (if (string-empty? right)
            ed
            (make-Editor (string-append left (substring right 0 1))
                  (substring right 1)))])]
    [(key=? k "\b")
     (match ed
       [(Editor left right)
        (if (string-empty? left)
            ed
            (make-Editor (substring left 0 (- (string-length left) 1)) right))])]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1)
     (match ed
       [(Editor left right)
        (make-Editor (string-append left k) right)])]
    [else ed]))

; main : String -> Editor
; launches the editor given some initial string 
(define (main s)
   (big-bang (create-editor s "")
     (on-key editor-kh)
     (on-draw editor-render)))
|#

;; ex 178
; Program handles checking for "\t" and "\r" first to handle special keys first, preventing conflicts
; that arise if you were to handling single-character inputs first

; Why the 'editor-ins' function performs the insertion of character 'k' at cursor position:
; Function constructs new Editor w/ 'k' inserted at cursor position using cons
; Adding 'k' to beginning of 'editor-pre' allows us to insert 'k' where the cursor
; is while keeping the rest of the text in 'editor-post'

; insert the 1String k between pre and post
#|(define (editor-ins ed k)
  (create-editor (cons k (editor-pre ed))
               (editor-post ed)))
(check-expect
 (editor-ins (make-editor '() '()) "e")
 (create-editor (cons "e" '()) '()))
     
(check-expect
 (editor-ins
  (create-editor (cons "d" '())
               (cons "f" (cons "g" '())))
  "e")
 (create-editor (cons "e" (cons "d" '()))
              (cons "f" (cons "g" '()))))
|#

#| 179 onwards

; ex 179
;Design editor-text using implode
(define-struct editor (pre post))
(define (create-editor pre post)
  (make-editor (string->list pre) (string->list post)))


(define (implode 1st)
  (list->string 1st))

; Editor -> Editor
; moves the cursor position one 1String left, 
; if possible 
(define (editor-lft ed)
  (editor (append (editor-pre ed) (list (first (editor-post ed))))
          (rest (editor-post ed))))
     
; Editor -> Editor
; moves the cursor position one 1String right, 
; if possible 
(define (editor-rgt ed)
  (editor (append (editor-pre ed) (list (first (editor-post ed))))
          (rest (editor-post ed))))
     
; Editor -> Editor
; deletes a 1String to the left of the cursor,
; if possible 
(define (editor-del ed)
  (editor (butlast (editor-pre ed))
          (editor-post ed)))


(define (butlast 1st)
  (if (null? (cdr 1st))
      '()
      (cons (car 1st) (butlast (cdr 1st)))))
      

; Editor -> Image
#|
(define (editor-render e)
  (place-image/align
    (beside (editor-text (editor-pre e))
            CURSOR
            (editor-text (editor-post e)))
    1 1
    "left" "top"
    MT))
|#

; Lo1s -> Image
; renders a list of 1Strings as a text image
#|(define (editor-text s)
  ;(if (null? s)
      (text (implode s) FONT-SIZE FONT-COLOR))
|#

;ex 180
; Design editor-text w/o using implode

; Lo1s -> Image
; renders a list of 1Strings as a text image 
(define (editor-text s)
  (text (list->string s) FONT-SIZE FONT-COLOR))

; Editor -> Image
(define (editor-render ed)
  (place-image/align
    (beside (editor-text (editor-pre ed))
            CURSOR
            (editor-text (editor-post ed)))
    1 1
    "left" "top"
    MT))

(define my-editor (create-editor "pre" "post"))
(editor-render my-editor)

|# ; 179 onwards

