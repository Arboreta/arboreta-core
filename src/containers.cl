(in-package arboreta)

(defclass container-window (window)
   (root-container nil)

   (start-drawing ((window window))
      (iter (for x = (+ (get-internal-real-time) 20))
            (when (root-container window)
                  (with-context ((image-context window))
                    (draw (root-container window)))
                  (update window))
                  (handle-events window)
                  (iter (while (cairo::handle-event window)))
            (let ((delay (/ (- x (get-internal-real-time)) 1000)))
                  (sleep (if (> delay 0) delay 0))))))

(defclass container ()
   (x 0)
   (y 0)
   (width 0)
   (height 0)
   (subcontainers nil)
   (above nil)
   
   (draw-subs (*this*)
      (with-slots (subcontainers) this 
         (when subcontainers
           (iter (for c in subcontainers) (draw c)))))

   (draw (*this*)
      (draw-subs this))
   
   (:before draw (*this*)
      (with-slots (x y width height) this 
         (reset-clip)
         (reset-trans-matrix)
         (new-path)
         (rectangle x y width height)
         (clip)
         (translate x y))))

(defclass rect (container)
   (color "FFFFFF")
   (draw (*this*)
      (with-slots (color width height) this
         (set-hex-color color)
         (draw-rectangle 0 0 width height)
         (draw-subs this))))

(defmacro rect (&rest args)
   (let ((pargs (dynamic-classes::extract-keyargs args)))
      `(make-instance rect ,@(dynamic-classes::unpair (first pargs)) 
         :subcontainers (list ,@(second pargs)))))

(defclass vertical-list (container)
   (draw (*this*)
      (with-slots (subcontainers) this
         (when subcontainers
            (iter (for c in subcontainers)
                  (setf (y c) delta-y)
                  (draw c)
                  (reducing (height c) by #'+ into delta-y initial-value 0))))))

(defmacro vertical-list (&rest args)
   (let ((pargs (dynamic-classes::extract-keyargs args)))
      `(make-instance vertical-list ,@(dynamic-classes::unpair (first pargs)) 
         :subcontainers (list ,@(second pargs)))))
