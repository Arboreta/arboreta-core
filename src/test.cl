(ql:quickload 'arboreta-core)

(in-package arboreta)

(defparameter window nil)

(defun main ()
   (setf window (make-instance window :width 600 :height 400))
   (setf (root-container window) 
      (make-instance container :x 50 :y 50 :width 200 :height 200
         (red-value 0)
         (draw (*this*)
            (with-slots (x y width height) this
               (setf (red-value this) (mod (+ (red-value this) 1/255) 1))
               (set-source-rgb (red-value this) 50/255 50/255)
               (draw-rectangle x y (+ x width) (+ y height))))))
   (start-drawing window))
