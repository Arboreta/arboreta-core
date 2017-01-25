(ql:quickload 'arboreta-core)

(in-package arboreta)

(defparameter window nil)
(defparameter root nil)

(defun main ()
   (setf window 
      (make-instance window :width 600 :height 400
         (handle-events (*this*)
            (with-slots (event-queue) this
               (when event-queue
                  (print (pop event-queue))
                  (finish-output))))))
   (setf (root-container window) 
      (make-instance container :x 50 :y 50 :width 50 :height 50
         (red-value 0)
         (draw (*this*)
            (with-slots (x y width height) this
               (setf (red-value this) (mod (+ (red-value this) 1/255) 1))
               (set-source-rgb (red-value this) 50/255 50/255)
               (draw-rectangle x y (+ x width) (+ y height))))))
   (setf root (root-container window))
   (start-drawing window))

;; (sb-ext:save-lisp-and-die "test" :executable t :toplevel #'main)

