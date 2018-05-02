(in-package :cl-user)
(defpackage #:net.hexapodia.games-3d
  (:use #:common-lisp)
  (:export #:*camera* #:*display* #:*font-preference* #:*grid-separation*
	   #:*score-table* #:+screen-side+ #:box #:camera #:camera-to-screen
	   #:collide-p #:collide-action #:coord #:draw-all-shapes #:draw-grid
	   #:draw-shape #:defvolume #:flat #:full-move #:3d-camera
	   #:get-centre #:move #:octaeder #:score-from-object #:shape
	   #:tetraeder #:tetragon
	   #:triangle #:turn #:volume #:world-to-camera #:world-to-screen))
