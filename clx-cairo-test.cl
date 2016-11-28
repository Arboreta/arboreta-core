(proclaim '(optimize (speed 0) (safety 3) (debug 3)))

(ql:quickload '(alexandria iterate anaphora cl-cairo2 cl-cairo2-xlib clx) :silent t)

(defpackage clx-cairo-test
  (:use cl iterate anaphora xlib))

(in-package clx-cairo-test)

(defparameter w 600)
(defparameter h 400)

(defun draw-stuff-plz (win gctx string x y)
   (CLEAR-AREA win
              :x 0 :y 0
              :width w :height h)
   (draw-glyphs win gctx x y string))

(defun clx-cairo-test (&key (string "Hello, lisp world!"))
   (let* ((display (open-default-display))
         (screen (display-default-screen display))
         (black (screen-black-pixel screen))
         (white (screen-white-pixel screen))
         (font (open-font display "fixed"))
         (x (truncate (- (screen-width screen) w) 2))
         (y (truncate (- (screen-height screen) h) 2))
         (window 
            (create-window
      :parent       (screen-root screen)
      :class        :input-output
      :x            x         ;temporary value
      :y            y         ;temporary value
      :width        w      ;temporary value
      :height       h      ;temporary value     
      :border-width 2
      :border       white
      :background   black
      :save-under   :on
      :override-redirect :on     ;override window mgr when positioning
      :event-mask   (make-event-mask :leave-window                    
                      :exposure)))
         (gcontext (create-gcontext :drawable window
                     :background black
                     :foreground white
                     :font font)))
      (print (list display screen window gcontext))
      (map-window window)
      (setf (wm-hints window) (make-wm-hints :input :on))
      
      (iter (repeat 10)
            (EVENT-CASE (display :force-output-p t)
              
              (:exposure (count)
                         
                         ;; Display prompt
                         (when (zerop count)
                           (draw-stuff-plz window gcontext string x y))
                         t)
              
              (:button-press (x y)
                             
                             ;; Pop up the menu
                             (print (list x y))
                             t)            
              
              (otherwise ()
                         ;;Ignore and discard any other event
                         t)))
      
      (close-display display :abort nil)
      (sb-ext:exit)))
