(load "base.cl")

(in-package arboreta)

(defun toplevel-start-render ()
	;; (setf cairo::event-handling-function )
   (setf context (cairo::create-arboreta-window w h))
   (setf surface (get-target context))
   (with-context (context)
      (repl-test)))

(toplevel-start-render)
