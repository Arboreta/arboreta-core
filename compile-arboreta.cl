(load "base.cl")

(in-package arboreta)

(defun toplevel-start-render ()
   (setf context (create-xlib-image-context w h :window-name "pango-test"))
   (setf surface (get-target context))
   (with-context (context)
      (typing-test)))

(defun main ()
  (sb-thread:make-thread 'toplevel-start-render :name "rendering-thread")
  
  (read-line)
  (cairo:destroy context)
  (sb-ext:exit))

(sb-ext:save-lisp-and-die "arboreta" :toplevel #'main :executable t)
