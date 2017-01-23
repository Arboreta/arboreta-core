(declaim (optimize (speed 0) (safety 3) (debug 3)))

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(declaim #+sbcl(sb-ext:muffle-conditions warning))

;; I've left this stuff as-is so that arboreta-repl still works.
(ql:quickload '(alexandria iterate anaphora cl-cairo2 cl-cairo2-xlib cl-pango cl-colors cl-ppcre) :silent t)
(load "cl-xkb.cl" :if-does-not-exist nil)

(defpackage arboreta
  (:use cl iterate anaphora cl-cairo2))

(in-package cl-cairo2)

(defcfun ("XChangeProperty" xchangeproperty) :int
  (display display)
  (window window)
  (property xatom)
  (atom-type xatom)
  (format :int)
  (mode :int)
  (data :pointer)
  (nelements :int))

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
    (cairo_surface_flush dest-surface)))

(defstruct keypress mods code str)

(defun handle-event (window)
   (with-foreign-object (xev :long 24)
     ;; get next event
     (with-slots (display) (arboreta::image-context window)
        (if (> (xpending display) 0)
            (xnextevent display xev)
            (return-from handle-event nil)))
     ;; decipher structure, at least partially
     (with-foreign-slots ((type window serial) xev xanyevent)
       ;; action based on event type
       (cond
         ;; expose events
         ((and (= type 12))
          (update window)
          nil)
         ;; buttonpress (mouse and scrolling) event
         ((= type 4)
          (with-foreign-slots ((state button) xev xbuttonevent)
            (alexandria::appendf (arboreta::event-queue window)
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
              (alexandria::appendf (arboreta::event-queue window) 
                 (list (if (zerop code) 
                           (make-keypress 
                              :mods state 
                              :code (with-slots (display) xlib-image-context 
                                      (xkb::xkb-keycode->keysym display keycode 0 0)) 
                              :str nil)
                           (make-keypress 
                              :mods state
                              :code code
                              :str (xkb::get-keysym-name code)))))))))
      t)))

(defparameter net-wm-type nil)
(defparameter net-wm-type-target nil)

(defun setup-window (xlib-image-context width height window-name)
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
         ;; set window type
         (setf net-wm-type (xinternatom display "_NET_WM_WINDOW_TYPE" 1))
         (setf net-wm-type-target (xinternatom display "_NET_WM_WINDOW_TYPE_NORMAL" 1))
         
         (with-foreign-object (prop2 'xatom)
           (setf (mem-aref prop2 'xatom) net-wm-type-target)
           (xchangeproperty display this-window net-wm-type 4 32 0 prop2 1))
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
         (xsynchronize display 0)))

(defun clean-shutdown (xlib-image-context)
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

(defun create-window* (width height &key (background-color +white+))
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
      (setup-window xlib-image-context width height window-name)
      ;; paint it if we are given a background color
      (when background-color
            (set-source-color background-color xlib-image-context)
            (paint xlib-image-context)
            (sync xlib-image-context))
      ;; return context
      xlib-image-context)))

(in-package arboreta)

(defclass arboreta-window ()
   ((image-context 
      :initarg :image-context
      :initform (error "context must be supplied")
      :accessor image-context)
    (event-queue
      :initarg :event-queue
      :initform nil
      :accessor event-queue)
    (root-container
		:initform nil
		:accessor root-container)))

(defmethod update ((window arboreta-window))
   (cairo::refresh (image-context window)))

(defmethod shutdown ((window arboreta-window))
   (cairo::clean-shutdown (image-context window)))

(defun make-window (width height)
   (make-instance 'arboreta-window :image-context (cairo::create-window* width height)))

(defmethod start-update-loop ((window arboreta-window))
   (iter (for x = (+ (get-internal-real-time) 20))
			(if (root-container window)
				 ())
         (when *buffer-needs-update*
            (draw root-window)
            (cairo::refresh context)
            (setf *buffer-needs-update* nil))
         (iter (while (cairo::handle-event context)))
         (sleep (let ((delay (/ (- x (get-internal-real-time)) 1000)))
                      ;; (print (* 1000.0 delay))
                      (if (> delay 0) delay 0)))))

(defclass container ()
	(x y subwindows))

(defmethod )
