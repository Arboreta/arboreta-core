(load "base.cl")

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(declaim #+sbcl(sb-ext:muffle-conditions warning))

(in-package arboreta)

(setf *random-state* (make-random-state t))

(defparameter *blink-status* t)
(defparameter *blink-time* 0)
(defparameter *blink-delay* 500)

(defun blink-timer ()
   (let ((time (get-internal-real-time)))
         (when (>= time *blink-time*)
               (setf *blink-status* (not *blink-status*))
               (setf *blink-time* (+ (get-internal-real-time) *blink-delay*))
               (setf *buffer-needs-update* t))))

(defun update-loop ()
   (iter (for x = (+ (get-internal-real-time) 30))
         (blink-timer)
         (when *buffer-needs-update*
            (draw root-window)
            (cairo::refresh context)
            (setf *buffer-needs-update* nil))
         (iter (while (cairo::handle-event context)))
         (sleep (let ((delay (/ (- x (get-internal-real-time)) 1000)))
                      ;; (print (* 1000.0 delay))
                      (if (> delay 0) delay 0)))))

(defparameter *cooldown-delta* 170)
(defparameter *cooldown-time* 0)

(defun off-cd? ()
   (> (get-internal-real-time) *cooldown-time*))

(defun cooldown-lock (&optional (cd *cooldown-delta*))
   (setf *cooldown-time* (+ (get-internal-real-time) cd)))

(defun player-action () nil)

(defun handle-repl-key-events ()
   (iter (for x = (+ (get-internal-real-time) 30))
      (if *key-events*
         (let ((kev (pop *key-events*)))
               (when (and (eql (keypress-code kev) 113) (eql (keypress-mods kev) 4)) ;; C-q
                     (cairo:destroy context)
                     (sb-ext:exit))
               (setf *buffer-needs-update* t)
               (alexandria::switch ((keypress-str kev) :test #'equalp) 
                  ("Return" nil)
                  ("left" 
                     (when (off-cd?) 
                        (decf (third *player-object*)) 
                        (setf (fifth *player-object*) t)
                        (cooldown-lock)))
                  ("right" 
                     (when (off-cd?) 
                        (incf (third *player-object*))
                        (setf (fifth *player-object*) nil)
                        (cooldown-lock)))
                  ("up" 
                     (when (off-cd?) 
                        (decf (fourth *player-object*))
                        (cooldown-lock)))
                  ("down" 
                     (when (off-cd?)
                        (incf (fourth *player-object*))
                        (cooldown-lock)))
                  ("z"
                     (when (off-cd?)
                        ))
                  ("Shift_L" nil)
                  ("Control_L" nil)
                  ("Control_R" nil)
                  ("Shift_R" nil)
                  ("BackSpace" nil)
                  (otherwise 
                     (when (<= #x20 (keypress-code kev) #x13be) ;; latin chars
                        nil)))))
      (sleep (let ((delay (/ (- x (get-internal-real-time)) 1000)))
                   (if (> delay 0) delay 0)))))

(defun handle-repl-mouse-events ()
   (if *mouse-events*
       (setf *mouse-events* nil))
   (sleep 1000))

(defun get-block (index size seq)
   (nth index 
      (iter (for x from 0 to (length seq) by size)
            (for y from size to (length seq) by size)
            (collect (subseq seq x y)))))

(defun set-hex-color (hex)
   (let ((colors (mapcar (lambda (x) (/ (parse-integer x :radix 16) 256)) 
                         (iter (for i from 0 to 2) (collect (get-block i 2 hex))))))
         (set-source-rgb (first colors) (second colors) (third colors))))

(defparameter *font-height* 0)

(defparameter *default-font* "blueberry 10")

;; get the font height by making a layout and then finding the cursor size
(defun pango-font-height-hack ()
   (let ((pango-layout (pango:pango_cairo_create_layout (slot-value cairo:*context* 'cairo::pointer)))
         (size 0))
     (pango:pango_layout_set_font_description pango-layout font)
     (setf size (fourth (pango:get-cursor-pos pango-layout 0)))
     (pango:g_object_unref pango-layout)
     size))

(defparameter *sprite-sheet* (image-surface-create-from-png "8x.png"))

(defun make-mob-object (name sprite-number x y rev)
   (list name (+ 4 (* 44 sprite-number)) x y rev))

(defparameter *player-object* (make-mob-object 'player 3 5 5 nil))

;; name sprite-x x y reverse?
(defparameter *world-objects*
   (list (make-mob-object 'enemy 7 12 5 t) *player-object*))

(defun render-mob-object (obj)
   (draw-from-sheet 
      (second obj) 
      (if *blink-status* 0 48) 44 48 (* 32 (third obj)) (* 32 (fourth obj))
      (fifth obj)))

(defun draw-from-sheet (src-x src-y width height dest-x dest-y reverse?)
   (new-path)
   (if reverse?
       (progn
         (translate w 0)
         (scale -1 1)
         (set-source-surface *sprite-sheet* (+ w (- dest-x) (- src-x) -40) (- dest-y src-y))
         (rectangle (+ w (- dest-x) -40) dest-y width height)
         (fill-path)
         (reset-trans-matrix))
      (progn
         (set-source-surface *sprite-sheet* (- dest-x src-x) (- dest-y src-y))
         (rectangle dest-x dest-y width height)
         (fill-path))))

(defun game-test ()
   (setf layout (pango:pango_cairo_create_layout (slot-value cairo:*context* 'cairo::pointer)))
   (setf font (pango:pango_font_description_from_string *default-font*))
   (pango:pango_layout_set_font_description layout font)
   
   (setf *font-height* (+ 0 (pango-font-height-hack)))

   (sb-thread:make-thread 'handle-repl-key-events :name "keyevents-thread")

   (setf root-window
      (make-window :draw
         (lambda (window) 
            (when *buffer-needs-update*
               (new-path)
               (set-hex-color "f8f8f8")
               (rectangle 0 0 w h)
               (fill-path)

               (new-path)
               (iter (for x in *world-objects*) (render-mob-object x))
               ))))
   (update-loop))

(defun toplevel-start-render ()
   (setf context (cairo::create-arboreta-window w h))
   (setf surface (get-target context))
   (with-context (context)
      (game-test)))

(toplevel-start-render)
