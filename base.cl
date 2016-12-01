(proclaim '(optimize (speed 3) (safety 0) (debug 0)))

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(declaim #+sbcl(sb-ext:muffle-conditions warning))

(ql:quickload '(alexandria iterate anaphora cl-cairo2 cl-cairo2-xlib cl-pango) :silent t)

(defpackage arboreta
  (:use cl iterate anaphora cl-cairo2))

(in-package cl-cairo2)

(defcstruct xbuttonevent
  (type :int)
  (serial :unsigned-long)
  (send-event bool)
  (display display)
  (window window)
  (root window)
  (subwindow window)
  (time :long)
  (x :int)
  (y :int)
  (x-root :int)
  (y-root :int)
  (state :int)
  (button :int))

(defcstruct xkeyevent
  (type :int)
  (serial :unsigned-long)
  (send-event bool)
  (display display)
  (window window)
  (root window)
  (subwindow window)
  (time :long)
  (x :int)
  (y :int)
  (x-root :int)
  (y-root :int)
  (state :int)
  (keycode :int))

(defcfun ("XKeycodeToKeysym" xkeycode->keysym) :int
  (display display)
  (keycode :int)
  (index :int))

(defcfun ("XKeysymToString" xkeysym->string) :string
  (keysym :int))

(defcfun ("XPending" xpending) :int
  (display display))

(defun refresh (xlib-image-context)
   ;; (print "refresh")
   (with-slots (context (display-pointer display) dest-surface) xlib-image-context
      (cairo_paint (xlib-context xlib-image-context))
      (cairo_surface_flush dest-surface)
      ))

(defparameter initialization-done? nil)

(defun handle-event (xlib-image-context)
   (with-foreign-object (xev :long 24)
     ;; get next event
     (with-slots (display) xlib-image-context
        (if (> (xpending display) 0)
            (xnextevent display xev)
            (return-from handle-event nil)))
     ;; decipher structure, at least partially
     (with-foreign-slots ((type window serial) xev xanyevent)
       ;; action based on event type
       (cond
         ;; expose events
         ((and (= type 12))
          (refresh xlib-image-context)
          nil)
         ;; clientnotify event
         ((= type 33)
          nil)
         ;; key press and release events
         ;; remember to update cursor position from here as well
         ((or (= type 2)) ;; type 3 for release
          (with-foreign-slots ((state keycode x y) xev xkeyevent)
            (let ((code 
               (with-slots (display) xlib-image-context 
                  (xkeycode->keysym display keycode state))))
              (alexandria::appendf arboreta::*key-events* 
                 (list (if (zerop code) 
                           (arboreta::make-keypress :mods state 
                                                    :code (with-slots (display) xlib-image-context 
                                                             (xkeycode->keysym display keycode 0)) 
                                                    :str nil)
                           (arboreta::make-keypress :mods state 
                                                    :code code 
                                                    :str (xkeysym->string code)))))))))
      t)))

(defun default-event-handling (xlib-image-context)
   (loop (handle-event xlib-image-context)))

(defparameter event-handling-function #'default-event-handling)

(defun start-event-loop (xlib-image-context width height window-name)
   (finish-output)
   (call-xinitthreads)
   (bind (((:slots display signal-window (this-window window)
                   wm-delete-window pointer graphics-context
                   xlib-context dest-surface)
           xlib-image-context)
          (screen (xdefaultscreen display))
          (root (xdefaultrootwindow display))
          (visual (xdefaultvisual display screen))
          (whitepixel (xwhitepixel display screen))
          (wm-protocols (xinternatom display "WM_PROTOCOLS" 1)))
         ;; we sync everything for initialization
         (xsynchronize display 1)
         ;; create signal window and window
         (setf this-window
               (create-window display root width height 'inputoutput
                              visual whitepixel 
                              (logior exposuremask
                                      keypressmask
                                      keyreleasemask
                                      structurenotifymask)
                              t))
         (setf signal-window (create-window display root 1 1 'inputonly visual whitepixel 0 nil))
         ;; create graphics-context
         (setf graphics-context (xcreategc display this-window 0 (null-pointer)))
         ;; set size hints on window (hoping that window managers will
         ;; respect this)
         (set-window-size-hints display this-window width width height height)
         ;; intern atom for window closing, set protocol on window
         (setf wm-delete-window (xinternatom display "WM_DELETE_WINDOW" 1))
         (with-foreign-object (prot 'xatom)
           (setf (mem-aref prot 'xatom) wm-delete-window)
           (xsetwmprotocols display this-window prot 1))
         ;; store name
         (xstorename display this-window window-name)
         ;; first we create an X11 surface and context on the window
         (let ((xlib-surface (cairo_xlib_surface_create display this-window visual width height)))
           (setf xlib-context (cairo_create xlib-surface))
           (setf dest-surface xlib-surface))
         ;; create cairo surface, then context, then set the
         ;; surface as the source of the xlib-context
         (let ((surface (cairo_image_surface_create :CAIRO_FORMAT_RGB24 width height)))
           (setf pointer (cairo_create surface))
           (cairo_set_source_surface xlib-context surface 0 0))
         ;; map window
         (xmapwindow display this-window)
         ;; end of synchronizing
         (xsynchronize display 0)
         ;; end of initialization
         (setf initialization-done? t)
         ;; EVENT LOOP
         (sb-thread:abort-thread)
         ;; (funcall event-handling-function xlib-image-context)
         )
   ;; close down everything
   (with-slots (display pixmap window signal-window pointer xlib-context dest-surface) xlib-image-context
     (xsynchronize display 1)
     (let ((saved-pointer pointer))
           (setf pointer nil) ; invalidate first so it can't be used
           (cairo_destroy saved-pointer))
     (cairo_surface_destroy dest-surface)
     (cairo_destroy xlib-context)
     ;; !! free xlib-context, surface
     (xdestroywindow display window)
     (xdestroywindow display signal-window)
     (xclosedisplay display)))

(defun create-arboreta-window (width height &key (background-color +white+))
  (let ((display (xopendisplay (null-pointer)))
        (window-name (next-xlib-image-context-name)))
        (when (null-pointer-p display)
              (error "couldn't open display"))
        (let ((xlib-image-context 
                (make-instance 'xlib-image-context
                   :display display
                   :width width
                   :height height
                   :pixel-based-p t
                   :background-color background-color)))
      ;; start event loop thread
      (setf (slot-value xlib-image-context 'thread)
         (sb-thread:make-thread #'start-event-loop
                 :name (format nil "thread for display")
                 :arguments (list xlib-image-context width height window-name)))
      ;; wait for initialization to finish
      (loop until initialization-done?)
      ;; paint it if we are given a background color
      (when background-color
            (set-source-color background-color xlib-image-context)
            (paint xlib-image-context)
            (sync xlib-image-context))
      ;; return context
      xlib-image-context)))

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

(defstruct keypress mods code str)
(defparameter *key-events* nil)

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
   (cairo::refresh context))

(defun window-update-loop ()
   (iter (for x = (+ (get-internal-real-time) 16))
         (draw root-window)        
         (cairo::refresh context)
         (sleep (let ((delay (/ (- x (get-internal-real-time)) 1000)))
                      ;; (print (* 1000.0 delay))
                      (if (> delay 0) delay 0)))))

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
   (setf font (pango:pango_font_description_from_string "Blueberry 10"))
   (pango:pango_layout_set_font_description layout font)
   
   (sb-thread:make-thread 'handle-key-events-test :name "keyevents-thread")

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

(defparameter *typing-buffer* "")

(defun buffer-append (str)
   (setf *typing-buffer* (concatenate 'string *typing-buffer* str)))

(defun handle-key-events-test ()
   (iter
      (if *key-events*
         (let ((kev (pop *key-events*)))
               (print kev)
               (when (and (eql (keypress-code kev) 113) (eql (keypress-mods kev) 4))
                     (cairo:destroy context)
                     (sb-ext:exit))
               (alexandria::switch ((keypress-str kev) :test #'equalp) 
                  ("Return" (buffer-append (format nil "~%")))
                  ("Shift_L" nil)
                  ("Control_L" nil)
                  ("Control_R" nil)
                  ("Shift_R" nil)
                  ("BackSpace" (when (> (length *typing-buffer*) 0) 
                                     (setf *typing-buffer* (subseq *typing-buffer* 0 (- (length *typing-buffer*) 1)))))
                  (otherwise (buffer-append (if (> (length (keypress-str kev)) 1) 
                                                (string (code-char (keypress-code kev))) 
                                                (keypress-str kev)))))))))

(defun typing-test ()
   (setf layout (pango:pango_cairo_create_layout (slot-value cairo:*context* 'cairo::pointer)))
   (setf font (pango:pango_font_description_from_string "Fantasque Sans Mono 10"))
   (pango:pango_layout_set_font_description layout font)
   
   (sb-thread:make-thread 'handle-key-events-test :name "keyevents-thread")

   (add-as-subwindow
      (make-window :draw
         (lambda (window) 
            (new-path)
            (set-source-rgb 47/255 56/255 60/255)
            (rectangle 20 20 (- w 40) (- h 40))
            (fill-path)
               
            (new-path)
            (move-to 20 20)
            (set-source-rgb 148/255 163/255 165/255)
            (pango:pango_layout_set_text layout *typing-buffer* -1)
            (pango-update)
            
            (draw-subwindows window)))
      root-window)   
   (window-update-loop))

(defparameter *repl-buffer* "> ")
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

(defun repl-test ()
   (setf layout (pango:pango_cairo_create_layout (slot-value cairo:*context* 'cairo::pointer)))
   (setf font (pango:pango_font_description_from_string "Fantasque Sans Mono 10"))
   (pango:pango_layout_set_font_description layout font)
   
   (sb-thread:make-thread 'handle-repl-key-events :name "keyevents-thread")

   (add-as-subwindow
      (make-window :draw
         (lambda (window) 
            (when *buffer-needs-update*
               (new-path)
               (set-source-rgb 47/255 56/255 60/255)
               (rectangle 20 20 (- w 40) (- h 40))
               (fill-path)
            
               (new-path)
               (move-to 20 20)
               (set-source-rgb 148/255 163/255 165/255)
               (pango:pango_layout_set_text layout *repl-buffer* -1)
               (pango-update))
            
            (setf *last-length* (length *repl-buffer*))
            (draw-subwindows window)))
      root-window)   
   (repl-update-loop))
