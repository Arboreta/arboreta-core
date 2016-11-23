(proclaim '(optimize (speed 0) (safety 3) (debug 3)))

(ql:quickload '(alexandria iterate anaphora cl-cairo2 cl-cairo2-xlib cl-pango) :silent t)

(defpackage arboreta
  (:use cl iterate anaphora cl-cairo2))

(in-package arboreta)

;; TODO & NOTES
;; find a way to fix the screen tearing issue
;; can we blit windows, or do they just have to be redrawn?
;; does normal pango layouts allow for smooth scolling?
;; text selection is going to be really weird, if implemented
;;   can we pull the font height from pango or fc for highlighting?
;;   how do we change the cursor to the text selection one?
;;     do we even have a cursor in wm mode?
;;   "worse is better" solution
;;     use internal clipboard, disregard people trying to use the applications by themselves
;;   alternative ("the right thing") solutions
;;     try to interface with x font rendering
;;       that means the main clipboard would need to be used
;;       using cl-x, probably
;;     switch platforms -- low preference

(defparameter context nil)
(defparameter surface nil)
(defparameter layout nil)
(defparameter font nil)

(defparameter w 600)
(defparameter h 400)

(defstruct window
   (attributes (make-hash-table :test #'eq))
   (draw nil))

(defun draw (window)
   (funcall (window-draw window) window))

(defun draw-subwindows (window)
   (awhen (gethash 'subwindows (window-attributes window))
          (iter (for x in it)
                (draw x))))

(defparameter root-window 
   (make-window :draw 
      (lambda (window)
         (new-path)
         (set-source-rgb 37/255 46/255 50/255)
         (rectangle 0 0 w h)
         (fill-path)
         (draw-subwindows window))))

(defun add-as-subwindow (source-window target-window)
   (push source-window (gethash 'subwindows (window-attributes target-window))))

(defun pango-update ()
   (pango:pango_cairo_update_layout (slot-value *context* 'cairo::pointer) layout)
   (pango:pango_cairo_show_layout (slot-value *context* 'cairo::pointer) layout))

(defun flush-surface ()
   (surface-flush surface)
   (cairo::sync context))

(defun window-update-loop ()
   (iter (for x = (get-internal-real-time))
         (for y = (+ x 33))
         (draw root-window)
         (flush-surface)
         (sleep (let ((a (/ (- y (get-internal-real-time)) 1000)))
                      (if (> a 0) a 0)))))

(defun unicode-test ()
   (setf layout (pango:pango_cairo_create_layout (slot-value cairo:*context* 'cairo::pointer)))
   (setf font (pango:pango_font_description_from_string "Fantasque sans mono 10"))
   (pango:pango_layout_set_font_description layout font)
   
   (set-source-rgb 37/255 46/255 50/255)
   (rectangle 0 0 w h)
   (fill-path)
   (new-path)
   (move-to 0 0)
   (set-source-rgb 148/255 163/255 165/255)
         
   (pango:pango_layout_set_text layout 
     (format nil "~{~a~%~}" 
       '("Hello, glorious pango text rendering!"
         "いあだ〜〜ずかしいです〜〜" 
         "Τη γλώσσα μου έδωσαν ελληνική" 
         "ᚠᛇᚻ᛫ᛒᛦᚦ᛫ᚠᚱᚩᚠᚢᚱ᛫ᚠᛁᚱᚪ᛫ᚷᛖᚻᚹᛦᛚᚳᚢᛗ"
         "ಬಾ ಇಲ್ಲಿ ಸಂಭವಿಸು ಇಂದೆನ್ನ ಹೃದಯದಲಿ "
         "मैं काँच खा सकता हूँ और मुझे उससे कोई चोट नहीं पहुंचती "))
     -1)

   (pango-update)
   (flush-surface))

(defparameter raven "Once upon a midnight dreary, while I pondered, weak and weary...")

;; you might not have this font, change it to a good non-bitmap monospaced font you do have.
(defun update-test ()
   (setf layout (pango:pango_cairo_create_layout (slot-value cairo:*context* 'cairo::pointer)))
   (setf font (pango:pango_font_description_from_string "Fantasque sans mono 10"))
   (pango:pango_layout_set_font_description layout font)

   (iter (for x from 0 to (length raven))
         (set-source-rgb 37/255 46/255 50/255)
         (rectangle 0 0 w h)
         (fill-path)
         (new-path)
         (move-to 0 0)
         (set-source-rgb 148/255 163/255 165/255)
         (pango:pango_layout_set_text layout (subseq raven 0 x) -1)
         (pango-update)
         (flush-surface)
         (sleep 0.1)))

(defparameter index 0)
(defparameter timer 0)

(defun windowing-test ()
   (setf layout (pango:pango_cairo_create_layout (slot-value cairo:*context* 'cairo::pointer)))
   (setf font (pango:pango_font_description_from_string "Fantasque sans mono 10"))
   (pango:pango_layout_set_font_description layout font)
   
   (add-as-subwindow
      (make-window :draw
         (lambda (window)
            (incf timer) 
            (new-path)
            (set-source-rgb 47/255 56/255 60/255)
            (rectangle 20 (+ 20 (* 0.7 h (sin (/ timer 10)))) (- w 40) (- h 40))
            (fill-path)
            
            (new-path)
            (move-to 20 (+ 20 (* 0.7 h (sin (/ timer 10)))))
            (set-source-rgb 148/255 163/255 165/255)
            (if (< index (length raven)) (incf index))
            (pango:pango_layout_set_text layout (subseq raven 0 index) -1)
            (pango-update)
            
            (draw-subwindows window)))
      root-window)   
   (window-update-loop))

;; had to do it without the helper macros, because they cause context errors for some reason
(defun create-xlib-window ()
   (setf context (create-xlib-image-context w h :window-name "pango-test"))
   (setf surface (get-target context))
   (with-context (context)
      (windowing-test)))

(defun main ()
  (sb-thread:make-thread 'create-xlib-window :name "rendering-thread")
  
  (read-line)
  (cairo:destroy context)
  (sb-ext:exit))

;; (sb-ext:save-lisp-and-die "arboreta" :toplevel #'main :executable t)
(main)
