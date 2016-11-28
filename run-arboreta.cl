(load "base.cl")

(in-package arboreta)

(defun toplevel-start-render ()
   (setf context (create-xlib-image-context w h :window-name "pango-test"))
   (setf surface (get-target context))
   (with-context (context)
      (typing-test)))

(toplevel-start-render)
