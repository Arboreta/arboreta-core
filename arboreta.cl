(proclaim '(optimize (speed 0) (safety 3) (debug 3)))

(ql:quickload '(alexandria iterate anaphora cl-cairo2 cl-cairo2-xlib cl-pango) :silent t)

(defpackage arboreta
  (:use cl iterate anaphora cl-cairo2))

(in-package arboreta)

(defparameter context nil)
(defparameter layout nil)
(defparameter font nil)

(defparameter w 600)
(defparameter h 400)

(defun main ()
   (setf context (create-xlib-image-context w h :window-name "pango-test"))
   (let ((cairo::*context* context))
      (set-source-rgb 37/255 46/255 50/255)
      (paint)
      
      (translate 0 0)
      (set-source-rgb 148/255 163/255 165/255)
      
      ;; had to do it without the helper macros, because they cause context errors for some reason
      ;; you might not have this font, change it to something you do have.

      ;; sometimes the window doesn't display the text, it should work if you close out the window
      ;; and re-run (arboreta::main) from the REPL

      ;; it seems to be time based, I can't tell if the text isn't rendering at all, or if it's just
      ;; the same color as the background, or the background is getting rendered first.

      ;; we're going to need to macro the shit out of this

      (setf layout (pango:pango_cairo_create_layout (slot-value context 'cairo::pointer)))
      (setf font (pango:pango_font_description_from_string "Fantasque Sans Mono 12"))
      (pango:pango_layout_set_font_description layout font)

      (cairo:save)
      
      (pango:pango_layout_set_text layout "Hello, glorious pango text rendering!" -1)
      (pango:pango_cairo_update_layout (slot-value context 'cairo::pointer) layout)
      (pango:pango_cairo_show_layout (slot-value context 'cairo::pointer) layout)
      
      (cairo:restore)
      
      (sleep 5)))

;; (sb-ext:save-lisp-and-die "arboreta" :toplevel #'main :executable t)
(main)
