(declaim (optimize (speed 0) (safety 3) (debug 3)))

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(declaim #+sbcl(sb-ext:muffle-conditions warning))

;; I've left this stuff as-is so that arboreta-repl still works.
(ql:quickload '(alexandria iterate anaphora cl-cairo2 cl-cairo2-xlib cl-pango cl-colors cl-ppcre) :silent t)
(load "cl-xkb.cl" :if-does-not-exist nil)

(defpackage arboreta
  (:use cl iterate anaphora cl-cairo2))

(in-package cl-cairo2)

(defcfun ("XGetDefault" x-get-defualt) :string
  (display display)
  (program :string)
  (option :string))

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
  (with-slots ((display-pointer display) dest-surface) xlib-image-context
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
         ;; buttonpress (mouse and scrolling) event
         ((= type 4)
          (with-foreign-slots ((state button) xev xbuttonevent)
            (alexandria::appendf arboreta::*mouse-events*
               (list (list state button)))
            (finish-output)))
         ;; key press and release events
         ;; remember to update cursor position from here as well
         ((or (= type 2)) ;; type 3 for release
          (with-foreign-slots ((state keycode x y) xev xkeyevent)
            (setf state (logand state (lognot 16)))
            (let ((code
               (with-slots (display) xlib-image-context 
                  (xkb::xkb-keycode->keysym display keycode 0 state))))
              (alexandria::appendf arboreta::*key-events* 
                 (list (if (zerop code) 
                           (arboreta::make-keypress :mods state 
                                                    :code (with-slots (display) xlib-image-context 
                                                             (xkb::xkb-keycode->keysym display keycode 0 0)) 
                                                    :str nil)
                           (arboreta::make-keypress :mods state
                                                    :code code
                                                    :str (xkb::get-keysym-name code)))))))))
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
                                      buttonpressmask
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

(defparameter arboreta-display nil)

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
      
      (setf arboreta-display (slot-value xlib-image-context 'display))
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

(defstruct keypress mods code str)
(defparameter *key-events* nil)

(defparameter *mouse-events* nil)

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
