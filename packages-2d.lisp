(in-package #:cl-user)
(defpackage #:net.hexapodia.maps-2d
  (:nicknames #:maps-2d)
  (:use #:common-lisp)
  (:shadow #:map)
  (:export #:*current-map* #:*mapcount* #:activate-map #:define-generator
	   #:define-tile #:define-type-from-map #:find-starting-position
	   #:height #:mapdata #:read-all-maps #:seq #:tile-from-map #:width))

(defpackage :net.hexapodia.games-2d
  (:nicknames #:games-2d)
  (:use #:common-lisp #:xlib #:net.hexapodia.maps-2d #:net.hexapodia.games-ai)
  (:export #:*background* #:*display* #:*file-path* #:*font-preference*
	   #:*game-over* #:*game-window* #:*gcontext-set* #:*generator-ai-map*
	   #:*image-types* #:*moving-objects* #:*player* #:animated-object
	   #:check-move
	   #:collide #:clone #:damage #:defchecker #:def-partial-checker
	   #:default-interaction-loop #:delete-clone #:draw #:draw-map
	   #:ensure-animations  #:firing-object #:games-ignore
	   #:generator #:load-images #:make-generator #:map-animation
	   #:map-file #:map-perspective-animation #:monster #:move #:move-all
	   #:move-ok #:movement #:moving-object #:score #:target))	   

(defpackage #:net.hexapodia.compiled-image
  (:use #:common-lisp))
