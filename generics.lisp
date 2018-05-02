(in-package #:net.hexapodia.games-2d)

(defgeneric draw (object)
  (:documentation "Draw a graphics object into the relevant window
(objects that move will have their associated window tied from the object)"))

(defgeneric move (object)
  (:documentation "Make an object move, if it is an animated object, also switch
 to the next animation"))

(defgeneric movement (object)
  (:documentation "Returns movement (dx / dy) for a movable object."))
(defgeneric (setf movement) (value object)
  (:documentation "Sets movement (dx / dy) for a movable object."))
