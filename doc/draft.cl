Arboreta container draft v1.0

(defcontainer paddle (rect physical)
	(width 40)
	(height 200)
	(color "#CCCCCC"))

(defcontainer ball (rect physical)
	(height 20)
	(width 20)
	(velocity-x 2.5)
	(velocity-y 2.5))

(defcontainer pong-container ()
	(width 600)
	(height 400)
	(:subcontainer player
		(make-container paddle
			(:initially (align this :horizontal 1/3 :vertical 1/2))))
	(:subcontainer enemy
		(make-container paddle
			(:initially (align this :horizontal 2/3 :vertical 1/2))))
	(:subcontainer ball
		(make-container ball))
	(:keyevent (arrow-down) (event)
		(decf (y (player this)) 30))
	(:keyevent (arrow-up) (event)
		(incf (y (player this)) 30)))

(defcontainer vertical-list
	(:default-constructor vertical-list)
	(draw (*this*)
      (with-slots (subcontainers) this
         (when subcontainers
            (iter (for c in subcontainers)
                  (setf (y c) delta-y)
                  (draw c)
                  (reducing (height c) by #'+ into delta-y initial-value 0))))))
