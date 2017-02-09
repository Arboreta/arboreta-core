(ql:quickload 'arboreta-core)

(load "containers.cl")

(in-package arboreta)

(defparameter window nil)
(defparameter root nil)

(defparameter font-string "Fantasque Sans Mono 10")
(defparameter default-font nil)
(defparameter needs-update? t)

(defparameter w 1200)
(defparameter h 800)

(defclass example-container (rect)
   (width w)
   (height 50)
   
   (:after draw (*this*)
      (basic-write (format nil "#~a" (color this)) default-font "FFFFFF" 3 3)))

(defclass example-window (container-window)
   (width w)
   (height h)
   
   (handle-events (*this*)
      (with-slots (event-queue) this
         (iter (while event-queue)
            (let ((e (pop event-queue)))
               (when (and (eq (first e) :keypress) (equalp (second e) 4) (equalp (third e) 113)) ;; C-q
                  (sb-ext:exit))))))
   
   (start-drawing ((window example-window))
      (iter (for x = (+ (get-internal-real-time) 20))
            (when (and (root-container window) needs-update?)
                  (with-context ((image-context window))
                    (draw (root-container window)))
                  (update window)
                  (setf needs-update? nil))
                  (iter (while (cairo::handle-event window)))
                  (handle-events window)
            (let ((delay (/ (- x (get-internal-real-time)) 1000)))
                  (sleep (if (> delay 0) delay 0))))))

(defun main ()
   (setf window (make-instance example-window))
   (setf default-font (pango:pango_font_description_from_string font-string))
   (setf (root-container window)
      (rect :width w :height h :color "252E32"
         (vertical-list :width w :height h
            (make-instance example-container :color "8FC029")
            (make-instance example-container :color "DC2566")
            (make-instance example-container :color "55BCCE"))))
   (setf root (root-container window))
   (start-drawing window))

;; (sb-ext:save-lisp-and-die "test" :executable t :toplevel #'main)


