Arboreta container draft v1.0

(defcontainer paddle (rect)
	:width 40
	:height 200
	:color "#CCCCCC")

(defcontainer ball (rect)
	:height 20
	:width 20
	(velocity-x 0)
	(velocity-y 0))

(defcontainer pong-container ()
	:width 600
	:height 400
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
