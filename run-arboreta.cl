(load "base.cl")

(in-package arboreta)

(defun toplevel-start-render ()
   (setf context (create-xlib-image-context w h :window-name "arboreta"))
   (setf surface (get-target context))
   (with-context (context)
      (repl-test)))

(toplevel-start-render)
