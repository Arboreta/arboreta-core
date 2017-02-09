(ql:quickload 'arboreta-core)

(in-package arboreta)

(defparameter window nil)

(defparameter w 600)
(defparameter h 400)

(defclass example-window (window)
   (width w)
   (height h)
   
   (handle-events (*this*)
      (with-slots (event-queue) this
         (iter (while event-queue)
            (let ((e (pop event-queue)))
               (when (and (eq (first e) :keypress) (equalp (second e) 4) (equalp (third e) 113)) ;; C-q
                  (shutdown window)
                  (sb-thread:terminate-thread sb-thread:*current-thread*)))))))

(defun main ()
   (setf window (make-instance example-window))
   (sb-thread:make-thread #'start-drawing :arguments (list window))
   (setf cairo::*context* (image-context window))
   (set-hex-color "DC2566")
   (draw-rectangle 10 10 50 50))

;; (sb-ext:save-lisp-and-die "test2" :executable t :toplevel #'main)

