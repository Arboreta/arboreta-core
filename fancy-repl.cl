(load "base.cl")

(in-package arboreta)

(defun repl-update-loop ()
   (iter (for x = (+ (get-internal-real-time) 33))
         (draw root-window)
         (when *buffer-needs-update*
            (cairo::refresh context)
            (setf *buffer-needs-update* nil))
         (iter (while (cairo::handle-event context)))
         (sleep (let ((delay (/ (- x (get-internal-real-time)) 1000)))
                      ;; (print (* 1000.0 delay))
                      (if (> delay 0) delay 0)))))

(defparameter *repl-buffer* (format nil "minirepl 0.1~%> "))
(defparameter *current-input* "")

(defparameter *buffer-needs-update* t)

(defun repl-append (str)
   (setf *repl-buffer* (concatenate 'string *repl-buffer* str))
   (setf *current-input* (concatenate 'string *current-input* str)))

(defun handle-repl-key-events ()
   (iter (for x = (+ (get-internal-real-time) 17))
      (if *key-events*
         (let ((kev (pop *key-events*)))
               ;; (print kev)
               (setf *buffer-needs-update* t)
               (when (and (eql (keypress-code kev) 113) (eql (keypress-mods kev) 4))
                     (cairo:destroy context)
                     (sb-ext:exit))
               (alexandria::switch ((keypress-str kev) :test #'equalp) 
                  ("Return" (repl-append (format nil "~%~a~%> " (ignore-errors (eval (read-from-string *current-input*)))))
                            (finish-output)
                            (setf *current-input* ""))
                  ("Shift_L" nil)
                  ("Control_L" nil)
                  ("Control_R" nil)
                  ("Shift_R" nil)
                  ("BackSpace" (when (> (length *current-input*) 0) 
                                     (setf *current-input* (subseq *current-input* 0 (- (length *current-input*) 1)))
                                     (setf *repl-buffer* (subseq *repl-buffer* 0 (- (length *repl-buffer*) 1)))))
                  (otherwise (repl-append (if (> (length (keypress-str kev)) 1) 
                                                (string (code-char (keypress-code kev))) 
                                                (keypress-str kev)))))))
      (sleep (let ((delay (/ (- x (get-internal-real-time)) 1000)))
                   ;; (print (* 1000.0 delay))
                   (if (> delay 0) delay 0)))))

(defparameter *heart* "♥")
(defparameter *heart-colors* 
   (list "ae82ff" "dc2566" "fa2772" "d4c96e" "e7db75" "8fc029" "a7e22e" "56b7a5" "66efd5" "55bcce" "66d9ee" "9358fe"))

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
         (pango:pango_cairo_show_layout (slot-value *context* 'cairo::pointer) pango-layout)))

(defun draw-hearts ()
   (iter (for y from 0 to 65 by 15)
      (iter (for x from -7 to 607 by 14)
            (basic-write *heart* 
               (nth (mod (+ (/ (+ 7 x) 14) (/ y 15)) (length *heart-colors*)) *heart-colors*) x y))))

(defun repl-test ()
   (setf layout (pango:pango_cairo_create_layout (slot-value cairo:*context* 'cairo::pointer)))
   (setf font (pango:pango_font_description_from_string "Fantasque Sans Mono 10"))
   (pango:pango_layout_set_font_description layout font)
   
   (sb-thread:make-thread 'handle-repl-key-events :name "keyevents-thread")

   (setf root-window
      (make-window :draw
         (lambda (window) 
            (when *buffer-needs-update*
               (new-path)
               (set-source-rgb 37/255 46/255 50/255)
               (rectangle 0 0 w h)
               (fill-path)
               
               (draw-hearts)
         
               (new-path)
               (move-to 2 78)
               (set-hex-color "CFD0C2")
               (pango:pango_layout_set_text layout *repl-buffer* -1)
               (pango-update))
            
            (setf *last-length* (length *repl-buffer*))
            (draw-subwindows window))))   
   (repl-update-loop))


(defun toplevel-start-render ()
   ;; (setf cairo::event-handling-function )
   (setf context (cairo::create-arboreta-window w h))
   (setf surface (get-target context))
   (with-context (context)
      (repl-test)))

(toplevel-start-render)
