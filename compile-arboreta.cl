(load "base.cl")

(in-package arboreta)

(defun toplevel-start-render ()
   (setf context (cairo::create-arboreta-window w h))
   (setf surface (get-target context))
   (with-context (context)
      (repl-test)))

(sb-ext:save-lisp-and-die "arboreta" :toplevel #'toplevel-start-render :executable t)
