;;(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)))

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(declaim #+sbcl(sb-ext:muffle-conditions warning))

;; I've left this stuff as-is so that arboreta-repl still works.
(ql:quickload '(alexandria iterate anaphora cl-cairo2 cl-cairo2-xlib cl-pango cl-colors cl-ppcre dynamic-classes) :silent t)
(load "cl-xkb.cl" :if-does-not-exist nil)

(defpackage arboreta
  (:shadowing-import-from dynamic-classes defclass make-instance)
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

(defcstruct xmotionevent
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
  (state :int))

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

(defun handle-event (arboreta-window)
   (with-foreign-object (xev :long 24)
     ;; get next event
     (with-slots (display) (arboreta::image-context arboreta-window)
        (if (> (xpending display) 0)
            (xnextevent display xev)
            (return-from handle-event nil)))
     ;; decipher structure, at least partially
     (with-foreign-slots ((type window serial) xev xanyevent)
       ;; action based on event type
       (cond
         ;; expose events
         ((and (= type 12))
          (arboreta::update arboreta-window)
          nil)
         ;; button press (mouse) events
         ((= type 4)
          (with-foreign-slots ((state button x y) xev xbuttonevent)
            (alexandria::appendf (arboreta::event-queue arboreta-window)
                                 (list (list :mouse state button x y)))))
         ((= type 6)
            (with-foreign-slots ((state x y) xev xmotionevent)
            (alexandria::appendf (arboreta::event-queue arboreta-window)
                                 (list (list :hover state x y)))))
         ;; key press and release events
         ;; remember to update cursor position from here as well
         ((or (= type 2)) ;; type 3 for release
          (with-foreign-slots ((state keycode x y) xev xkeyevent)
            (setf state (logand state (lognot 16)))
            (let ((code
               (with-slots (display) (arboreta::image-context arboreta-window) 
                  (xkb::xkb-keycode->keysym display keycode 0 state))))
              (alexandria::appendf (arboreta::event-queue arboreta-window)
                 (if (zerop code) 
                     (list
                        (list :keypress 
                            state 
                            (with-slots (display) (arboreta::image-context arboreta-window) 
                              (xkb::xkb-keycode->keysym display keycode 0 0)) 
                            nil))
                     (list 
                        (list :keypress 
                            state
                            code
                            (xkb::get-keysym-name code)))))))))
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
                                      pointermotionmask
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

(defun get-block (index size seq)
   (nth index 
      (iter (for x from 0 to (length seq) by size)
            (for y from size to (length seq) by size)
            (collect (subseq seq x y)))))

(defun set-hex-color (hex)
   (let ((colors (mapcar (lambda (x) (/ (parse-integer x :radix 16) 256)) 
                         (iter (for i from 0 to 2) (collect (get-block i 2 hex))))))
         (set-source-rgb (first colors) (second colors) (third colors))))

(defun draw-rectangle (x y w h)
   (new-path)
   (rectangle x y w h)
   (fill-path))

(defun basic-write (str font color x y)
   (let ((pango-layout (pango:pango_cairo_create_layout (slot-value cairo:*context* 'cairo::pointer))))
         (pango:pango_layout_set_font_description pango-layout font)             
         (new-path)
         (move-to x y)
         (set-hex-color color)
         (pango:pango_layout_set_text pango-layout str -1)
                                    
         (pango:pango_cairo_update_layout (slot-value *context* 'cairo::pointer) pango-layout)
            (pango:pango_cairo_show_layout (slot-value *context* 'cairo::pointer) pango-layout)
               (pango:g_object_unref pango-layout)))

(defclass window ()
   (width (error "must supply width"))
   (height (error "must supply height"))
   (image-context nil)
   (event-queue nil)

   (update ((window window))
      (cairo::refresh (image-context window)))
   
   (shutdown ((window window))
      (cairo::clean-shutdown (image-context window)))
   
   (:after initialize-instance ((window window) &key)
      (with-slots (width height) window
         (setf (image-context window) (cairo::create-window* width height))))

   (start-drawing ((window window))
      (iter (for x = (+ (get-internal-real-time) 20))
            (handle-events window)
            (update window)
            (iter (while (cairo::handle-event window)))
            (let ((delay (/ (- x (get-internal-real-time)) 1000)))
                  (sleep (if (> delay 0) delay 0)))))
   
   (handle-events ((window window))
      (setf (event-queue window) nil)))
