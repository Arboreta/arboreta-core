example repl implementation

```cl
(defwindow prompt
	:editor
		(editor
			(keyevent (enter)
				(handle-evaluation (attr this input) (attr this parent parent parent))))
	(horizontal-align
		(promt-accent)
		(attr this editor)))

(defparameter accent-color (hex-color "#FFFFFF"))

(defwindow prompt-accent
	:height (* 2/3 (attr this parent height))
	(method draw (this)
		(move-to 0 0)
		(new-path)
		(set-color accent-color)
		(line-to )))

(defwindow repl-result
	:margin-top 2
	:margin-bottom 2
   :prompt (prompt)
	:printed-result nil
	:result nil
	(vertical-align 
		(attr this prompt) 
		(attr this printed-result) 
		(attr this result)))

(defwindow repl
	:subwindows (list (make-window repl-prompt))
	(setf (attr this (first subwindows) prompt editor active?) t)
	(scrolled-vertical-list (attr this subwindows)))
```