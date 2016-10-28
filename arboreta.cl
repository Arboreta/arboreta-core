(proclaim '(optimize (speed 0) (safety 3) (debug 3)))

(ql:quickload '(alexandria iterate anaphora cl-glut) :silent t)

(defpackage arboreta
  (:use cl iterate anaphora alexandria))

(in-package arboreta)

(defclass hello-window (glut:window)
  ()
  (:default-initargs :pos-x 100 :pos-y 100 :width 250 :height 250
                     :mode '(:single :rgb) :title "hello.lisp"))

(defmethod glut:display-window :before ((w hello-window))
  ;; Select clearing color.
  (gl:clear-color 0 0 0 0)
  ;; Initialize viewing values.
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 1 0 1 -1 1))

(defmethod glut:display ((w hello-window))
  (gl:clear :color-buffer)
  ;; Draw white polygon (rectangle) with corners at
  ;; (0.25, 0.25, 0.0) and (0.75, 0.75, 0.0).
  (gl:color 1 1 1)
  (gl:with-primitive :polygon
    (gl:vertex 0.25 0.25 0)
    (gl:vertex 0.75 0.25 0)
    (gl:vertex 0.75 0.75 0)
    (gl:vertex 0.25 0.75 0))
  ;; Start processing buffered OpenGL routines.
  (gl:flush))

(defun main () (glut:display-window (make-instance 'hello-window)))

(sb-ext:save-lisp-and-die "arboreta" :toplevel #'main :executable t)
