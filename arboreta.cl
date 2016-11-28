(proclaim '(optimize (speed 0) (safety 3) (debug 3)))

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

;; copied from cl-cairo2-xlib, redefined
(defun create-xlib-image-context 
   (width height &key (display-name nil) 
                      (window-name (next-xlib-image-context-name))
                      (background-color +white+))
  "Create a window mapped to an xlib-image-context, with given width,
height (non-resizable) and window-name on display-name.  If
background-color is not nil, the window will be painted with it."
  (let ((display (xopendisplay (if display-name display-name (null-pointer)))))
    (when (null-pointer-p display)
          (error "couldn't open display ~a" display-name))
    (let ((xlib-image-context 
            (make-instance 'xlib-image-context
              :display display
              :width width
              :height height
              :pixel-based-p t
              :background-color background-color))
          initialization-done?)
      (labels ( ;; Repaint the xlib context with the image surface
          ;; (previously set as source during initialization.
          (refresh ()
            (with-slots (context dest-surface) xlib-image-context
              (cairo_paint (xlib-context xlib-image-context))
              (cairo_surface_flush dest-surface)))
          ;; The main event loop, started as a separate thread
          ;; when initialization is complete.  The main thread is
          ;; supposed to communicate with this one via X signals
          ;; using an unmapped InputOnly window (see
          ;; send-message-to-signal-window).
          (event-loop ()
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
                   (setf signal-window
                         (create-window display root 1 1 'inputonly visual
                                        whitepixel 0 nil))
                   ;; create graphics-context
                   (setf graphics-context
                         (xcreategc display this-window 0 (null-pointer)))
                   ;; set size hints on window (hoping that window managers will
                   ;; respect this)
                   (set-window-size-hints display this-window 
                                          width width height height)
                   ;; intern atom for window closing, set protocol on window
                   (setf wm-delete-window 
                         (xinternatom display "WM_DELETE_WINDOW" 1))
                   (with-foreign-object (prot 'xatom)
                     (setf (mem-aref prot 'xatom) wm-delete-window)
                     (xsetwmprotocols display this-window prot 1))
                   ;; store name
                   (xstorename display this-window window-name)
                   ;; first we create an X11 surface and context on the window
                   (let ((xlib-surface 
                           (cairo_xlib_surface_create display this-window visual
                                                      width height)))
                     (setf xlib-context (cairo_create xlib-surface))
                     (setf dest-surface xlib-surface))
                   ;; create cairo surface, then context, then set the
                   ;; surface as the source of the xlib-context
                   (let ((surface (cairo_image_surface_create :CAIRO_FORMAT_RGB24
                                                              width height)))
                     (setf pointer (cairo_create surface))
                     (cairo_set_source_surface xlib-context surface 0 0))
                   ;; map window
                   (xmapwindow display this-window)
                   ;; end of synchronizing
                   (xsynchronize display 0)
                   ;; end of initialization
                   (setf initialization-done? t)
                   ;; EVENT LOOP
         (with-foreign-object (xev :long 24)
            (do ((got-close-signal nil))
                 (got-close-signal)
               ;; get next event
               (xnextevent display xev)
               ;; decipher structure, at least partially
               (with-foreign-slots ((type window serial) xev xanyevent)
                 ;; action based on event type
                 (cond
                   ;; expose events
                   ((and (= type 12) (= window this-window))
                    (refresh))
                   ;; clientnotify event
                   ((= type 33)
                    (with-foreign-slots ((message-type data0) xev 
                                         xclientmessageevent)
                      (cond
                        ((or (and (= window signal-window)
                                  (= data0 +destroy-message+))
                             (and (= window this-window)
                                  (= message-type wm-protocols)
                                  (= data0 wm-delete-window)))
                         (setf got-close-signal t))
                        ((and (= window signal-window)
                              (= data0 +refresh-message+))
                         (refresh)))))
                   ;; key press and release events
                   ;; remember to update cursor position from here as well
                   ((or (= type 2)) ;; type 3 for release
                    (with-foreign-slots ((state keycode x y) xev xkeyevent)
                     (print (list state (xkeysym->string (xkeycode->keysym display keycode 0))))
                     (finish-output))))))))
       ;; close down everything
       (with-slots (display pixmap window signal-window pointer
                              xlib-context dest-surface)
           xlib-image-context
         (xsynchronize display 1)
         (let ((saved-pointer pointer))
           (setf pointer nil) ; invalidate first so it can't be used
           (cairo_destroy saved-pointer))
                   (cairo_surface_destroy dest-surface)
         (cairo_destroy xlib-context)
         ;; !! free xlib-context, surface
         (xdestroywindow display window)
         (xdestroywindow display signal-window)
         (xclosedisplay display))))
   ;; start even loop thread
   (setf (slot-value xlib-image-context 'thread)
              (start-thread
               #'event-loop
               (format nil "thread for display ~a" display-name))))
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

(defparameter std-out *standard-output*)

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
   (setf font (pango:pango_font_description_from_string "Blueberry 10"))
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
