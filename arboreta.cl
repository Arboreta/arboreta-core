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
   (with-context (context)
      (set-source-rgb 1 1 1)
      (rectangle 0 0 w h)
      (fill-path)

      ;; had to do it without the helper macros, because they cause context errors for some reason
      ;; you might not have this font, change it to a good non-bitmap monospaced font you do have.

      ;; we're going to need to macro the shit out of this

      (setf layout (pango:pango_cairo_create_layout (slot-value cairo:*context* 'cairo::pointer)))
      (setf font (pango:pango_font_description_from_string "Fantasque Sans Mono 12"))
      (pango:pango_layout_set_font_description layout font)

      ;; (cairo:save)

      (pango:pango_layout_set_text layout 
         (format nil "~{~a~%~}" 
            '("Hello, glorious pango text rendering!" 
              "いあだ〜〜ずかしいです〜〜" 
              "Τη γλώσσα μου έδωσαν ελληνική" 
              "ᚠᛇᚻ᛫ᛒᛦᚦ᛫ᚠᚱᚩᚠᚢᚱ᛫ᚠᛁᚱᚪ᛫ᚷᛖᚻᚹᛦᛚᚳᚢᛗ"
              "ಬಾ ಇಲ್ಲಿ ಸಂಭವಿಸು ಇಂದೆನ್ನ ಹೃದಯದಲಿ "
              "मैं काँच खा सकता हूँ और मुझे उससे कोई चोट नहीं पहुंचती")) 
         -1)

      (new-path)
      (move-to 0 0)
      (set-source-rgb 81/255 81/255 81/255)

      (pango:pango_cairo_update_layout (slot-value *context* 'cairo::pointer) layout)
      (pango:pango_cairo_layout_path (slot-value *context* 'cairo::pointer) layout)
      (fill-preserve)

      ;; (cairo:restore)

      (princ "press enter here to exit")
      (finish-output)
      (when (read-line)
         (cairo:destroy context))      
         (sb-ext:exit)))

(sb-ext:save-lisp-and-die "arboreta" :toplevel #'main :executable t)
;;(main)
