(load "base.cl")
(load "repl-config.cl")

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(declaim #+sbcl(sb-ext:muffle-conditions warning))

(in-package arboreta)

(setf *random-state* (make-random-state t))

(defparameter *cursor-blink-status* t)
(defparameter *cursor-blink-time* 0)
(defparameter *cursor-blink-delay* 1000)

(defun cursor-blink-timer ()
   (let ((time (get-internal-real-time)))
         (when (>= time *cursor-blink-time*)
               (setf *cursor-blink-status* (not *cursor-blink-status*))
               (setf *cursor-blink-time* (+ (get-internal-real-time) *cursor-blink-delay*))
               (setf *buffer-needs-update* t))))

(defun repl-update-loop ()
   (iter (for x = (+ (get-internal-real-time) 20))
         (cursor-blink-timer)
         (when *buffer-needs-update*
            (draw root-window)
            (cairo::refresh context)
            (setf *buffer-needs-update* nil))
         (iter (while (cairo::handle-event context)))
         (sleep (let ((delay (/ (- x (get-internal-real-time)) 1000)))
                      ;; (print (* 1000.0 delay))
                      (if (> delay 0) delay 0)))))

(defparameter *current-input* "")
(defparameter *cursor-index* 0)
(defparameter *buffer-needs-update* t)

(defun editor-insert (str)
   (setf *current-input* 
      (concatenate 'string 
         (subseq *current-input* 0 *cursor-index*) 
         str 
         (subseq *current-input* *cursor-index*)))
   (incf *cursor-index* (length str)))

(defun editor-delete-char ()
   (setf *current-input* 
      (concatenate 'string 
         (subseq *current-input* 0 (- *cursor-index* 1))
         (subseq *current-input* *cursor-index*)))
   (decf *cursor-index*))

(defun simple-eval ()
   (let* ((str (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t))
          (res (with-output-to-string (stream str)
                 (let* ((*standard-output* stream)
                        (val (format nil "~s" (ignore-errors (eval (read-from-string *current-input*))))))
                        (list val str))))) 
          `((,(alexandria:copy-sequence 'string *current-input*) ,@res))))

(defun handle-repl-key-events ()
   (iter (for x = (+ (get-internal-real-time) 17))
      (if *key-events*
         (let ((kev (pop *key-events*)))
               ;; (print kev)
               (setf *cursor-blink-status* t)
               (setf *cursor-blink-time* (+ (get-internal-real-time) *cursor-blink-delay*))
               
               (setf *buffer-needs-update* t)
               (when (and (eql (keypress-code kev) 113) (eql (keypress-mods kev) 4)) ;; C-q
                     (cairo:destroy context)
                     (sb-ext:exit))
               (when (and (eql (keypress-code kev) 97) (eql (keypress-mods kev) 4)) ;; C-a
                     (setf *cursor-index* 0))
               (when (and (eql (keypress-code kev) 101) (eql (keypress-mods kev) 4)) ;; C-e
                     (setf *cursor-index* (length *current-input*)))
               (alexandria::switch ((keypress-str kev) :test #'equalp) 
                  ("Return" (alexandria:appendf *buffer-history* (simple-eval))
                            ;; (print *buffer-history*)
                            ;; (finish-output)
                            (setf *current-input* "")
                            (setf *cursor-index* 0))
                  ("left" (unless (eql *cursor-index* 0) (decf *cursor-index*)))
                  ("right" (unless (eql *cursor-index* (length *current-input*)) (incf *cursor-index*)))
                  ("Shift_L" nil)
                  ("Control_L" nil)
                  ("Control_R" nil)
                  ("Shift_R" nil)
                  ("BackSpace" (when (and (not (eql *cursor-index* 0)) (> (length *current-input*) 0)) 
                                     (editor-delete-char)))
                  (otherwise 
                     (when (<= #x20 (keypress-code kev) #x13be) ;; latin chars
                        (editor-insert (if (> (length (keypress-str kev)) 1) 
                                         (string (code-char (keypress-code kev))) 
                                         (keypress-str kev))))))))
      (sleep (let ((delay (/ (- x (get-internal-real-time)) 1000)))
                   ;; (print (* 1000.0 delay))
                   (if (> delay 0) delay 0)))))

(defparameter *heart* "♥")
(defparameter *heart-colors* 
   '(("ae82ff" "dc2566" "fa2772" "d4c96e" "e7db75" "8fc029" "a7e22e" "56b7a5" "66efd5" "55bcce" "66d9ee" "9358fe")
     ("1693A5" "02AAB0" "00CDAC" "7FFF24" "C3FF68")
     ("AAFF00" "FFAA00" "FF00AA" "AA00FF" "00AAFF")
     ("F6D76B" "FF9036" "D6254D" "FF5475" "FDEBA9")
     ("FDCFBF" "FEB89F" "E23D75" "742365")))

(defun get-block (index size seq)
   (nth index 
      (iter (for x from 0 to (length seq) by size)
            (for y from size to (length seq) by size)
            (collect (subseq seq x y)))))

(defun set-hex-color (hex)
   (let ((colors (mapcar (lambda (x) (/ (parse-integer x :radix 16) 256)) 
                         (iter (for i from 0 to 2) (collect (get-block i 2 hex))))))
         (set-source-rgb (first colors) (second colors) (third colors))))

(defun basic-write (str color x y)
   (let ((pango-layout (pango:pango_cairo_create_layout (slot-value cairo:*context* 'cairo::pointer))))
         (pango:pango_layout_set_font_description pango-layout font)
         
         (new-path)
         (move-to x y)
         (set-hex-color color)
         (pango:pango_layout_set_text pango-layout str -1)

         (pango:pango_cairo_update_layout (slot-value *context* 'cairo::pointer) pango-layout)
         (pango:pango_cairo_show_layout (slot-value *context* 'cairo::pointer) pango-layout)
         (pango:g_object_unref pango-layout)))

(defparameter *colorset* (alexandria:random-elt *heart-colors*))
(defparameter *offset* (alexandria:random-elt (alexandria:iota 5 :start -5)))

(defun draw-hearts ()
   (iter (for y from 0 to 65 by 15)
         (for y2 upfrom 0)
      (iter (for x from -7 to 607 by 14)
            (for x2 upfrom 0)
            (basic-write *heart*
               (nth (mod (+ x2 (* *offset* y2)) (length *colorset*)) *colorset*) x y))))

(defparameter *buffer-history* nil)
(defparameter *font-height* 0)

(defun triangle-offset () (round (* *font-height* 0.35)))
(defun outer-padding () (round (* *font-height* 0.25)))
(defun inner-padding () (round (* *font-height* 0.15)))
(defun prompt-offset () (round (* *font-height* 0.80)))
(defun text-offset () (prompt-offset))

(defun draw-prompt-bg (y-offset)
   (new-path)
   (set-hex-color *prompt-bg-color*)
   (rectangle 0 (+ (outer-padding) y-offset) w (+ *font-height* (* 2 (inner-padding))))
   (fill-path)
   
   (move-to 0 (+ (outer-padding) y-offset))
   (new-path)
   (set-hex-color *triangle-color*)
   (line-to (- (+ (ceiling (/ *font-height* 2)) (* 2 (inner-padding))) (triangle-offset)) 
            (+ y-offset (outer-padding) (ceiling (/ *font-height* 2)) (* 1 (inner-padding))))
   (line-to (- (triangle-offset)) (+ y-offset (outer-padding) *font-height* (* 2 (inner-padding))))
   (line-to (- (triangle-offset)) (+ (outer-padding) y-offset))
   (fill-path))

(defun draw-prompt (text y-offset)
   (draw-prompt-bg y-offset)
 
   (basic-write text *prompt-fg-color* (prompt-offset) (+ (outer-padding) (inner-padding) y-offset))
   (+ *font-height* (* 2 (outer-padding)) (* 2 (inner-padding))))

(defun draw-current-prompt (text y-offset)
   (draw-prompt-bg y-offset)

   (let ((str text) (color *prompt-fg-color*) (x (prompt-offset)) (y (+ (outer-padding) (inner-padding) y-offset)))
      (let ((pango-layout (pango:pango_cairo_create_layout (slot-value cairo:*context* 'cairo::pointer))))
            (pango:pango_layout_set_font_description pango-layout font)

            (new-path)
            (move-to x y)
            (set-hex-color color)
            (pango:pango_layout_set_text pango-layout str -1)

            (pango:pango_cairo_update_layout (slot-value *context* 'cairo::pointer) pango-layout)
            (pango:pango_cairo_show_layout (slot-value *context* 'cairo::pointer) pango-layout)
            
            (when *cursor-blink-status*
               (new-path)
               (set-hex-color *prompt-fg-color*)
               (let ((cursor (pango:get-cursor-pos pango-layout *cursor-index*)))
                     (rectangle (+ (first cursor) (prompt-offset) 0) (+ (second cursor) (outer-padding) (inner-padding) y-offset)
                                (+ 0.5 (third cursor)) (fourth cursor)))
               (fill-path))
            (pango:g_object_unref pango-layout)))

   (+ *font-height* (* 2 (outer-padding)) (* 2 (inner-padding))))

;; normal lines get 16 px, repl lines get (2 * (innter + outer)) + 16
;; this *really* needs to be relative to font size
(defun draw-repl-body (y-offset)
   (iter (for x in *buffer-history*)
         (incf y-offset (draw-prompt (first x) y-offset))
         (when (not (equalp (third x) ""))
            (basic-write (string-trim '(#\Newline) (third x)) *printed-fg-color* (text-offset) y-offset)
            (incf y-offset (* *font-height* (+ 1 (iter (for c in-string (string-trim '(#\Newline) (third x))) 
                                            (counting (eql c #\newline)))))))
         (basic-write (second x) *return-form-color* (text-offset) y-offset)
         (incf y-offset *font-height*))
   (draw-current-prompt *current-input* y-offset))

;; get the font height by making a layout and then finding the cursor size
(defun pango-font-height-hack ()
   (let ((pango-layout (pango:pango_cairo_create_layout (slot-value cairo:*context* 'cairo::pointer)))
         (size 0))
     (pango:pango_layout_set_font_description pango-layout font)
     (setf size (fourth (pango:get-cursor-pos pango-layout 0)))
     (pango:g_object_unref pango-layout)
     size))

(defun repl-test ()
   (setf layout (pango:pango_cairo_create_layout (slot-value cairo:*context* 'cairo::pointer)))
   (setf font (pango:pango_font_description_from_string "Fantasque Sans Mono 10"))
   (pango:pango_layout_set_font_description layout font)
   
   (setf *font-height* (+ 1 (pango-font-height-hack)))   

   (sb-thread:make-thread 'handle-repl-key-events :name "keyevents-thread")

   (scale 1.0 1.0)

   (setf root-window
      (make-window :draw
         (lambda (window) 
            (when *buffer-needs-update*
               (new-path)
               (set-hex-color *background-color*)
               (rectangle 0 0 w h)
               (fill-path)
               
               (draw-repl-body (- (outer-padding))))
            (draw-subwindows window))))   
   (repl-update-loop))


(defun toplevel-start-render ()
   (setf context (cairo::create-arboreta-window w h))
   (setf surface (get-target context))
   (with-context (context)
      (repl-test)))

(toplevel-start-render)
