(in-package #:net.hexapodia.games-2d)

(defvar *generator-ai-map* NIL
  "Variable to hold mappings from generator class to generator AI. This list should be on the format (TYPE1 AI1 TYPE2 AI2 ...)")

(defclass firing-object ()
  ((since-fired :accessor since-fired :initarg :since-fired)
   (max-delay :reader max-delay :initarg :max-delay)
   )
  (:default-initargs :since-fired 0 :max-delay 3))

(defclass monster (moving-object)
  ((score :reader score :initarg :score)
   (target :accessor target :initarg :target)
   (damage :reader damage :initarg :damage) 
   )
  (:default-initargs :damage 5 :score 20))

(defclass los-tracker (moving-object)
  ((target :reader target :initarg :target)))


(defclass generator ()
  ((the-map :reader the-map :initarg :map)
   (map-x :reader map-x :initarg :map-x)
   (map-y :reader map-y :initarg :map-y)
   (tile-type :reader tile-type :initarg :tile-type)
   (health :accessor health :initarg :health)
   (monster-type :reader monster-type :initarg :monster-type)
   (timer :accessor timer :initarg :timer)
   (score :reader score :initarg :score) 
   (respawn-max :reader respawn-max :initarg :respawn-max) 
   )
  (:default-initargs :timer 0))


(defun make-generator (tile type mx my)
  "Create a generator"
  (let ((new (make-instance type :tile-type tile :map-x mx :map-y my))
	(ai-type (getf *generator-ai-map* type)))
    (push new *generators*)
    (make-ai ai-type new)
    new))

(defmethod act ((obj moving-object))
  (act (ai obj)))

(defmethod act ((obj generator))
  (when (ai obj)
    (act (ai obj))))

(defgeneric projectile-speed (obj)
  (:documentation "How fast is a projectile?"))

(defgeneric range (obj)
  (:documentation "What's the viewing range of OBJ?"))

(defmacro defrange (class range)
  `(defmethod range ((ignore ,class))
     (declare (ignorable ignore))
     ,range))

(defgeneric object-speed (obj dir)
  (:documentation "How fast does an object move?")
  )

(defmacro defspeed (class base-speed)
  (let ((diag (round (sqrt (/ (* base-speed base-speed) 2)))))
    `(defmethod object-speed ((obj ,class) dir)
       (case dir
	 (0 '(0 ,(- base-speed)))
	 (1 '(,diag ,(- diag)))
	 (2 '(,base-speed 0))
	 (3 '(,diag ,diag))
	 (4 '(0 ,base-speed))
	 (5 '(,(- diag) ,diag))
	 (6 '(,(- base-speed) 0))
	 (7 '(,(- diag) ,(- diag)))))))

(defmacro bresenmacro (obj smooth-acc jerk-acc smooth-delta jerk-delta smooth-max jerk-max smooth-pos jerk-pos)
  (let ((store (gensym))
	(counter (gensym)))
    `(let ((,store (truncate ,jerk-max 2)))
       (setf (,smooth-acc ,obj) ,smooth-delta)
       (setf (,jerk-acc ,obj) 0)
       (loop for ,counter from 0 below ,smooth-max
	     if (> ,store ,smooth-max)
	     do (setf ,store (- ,store ,smooth-max)
		      (,jerk-acc ,obj) ,jerk-delta)
	     else
	     do (setf ,store (+ ,store ,jerk-max)
		      (,jerk-acc ,obj) 0)
	     while (move-ok ,obj)
	     do (incf (,smooth-pos ,obj) ,smooth-delta)
	     do (incf (,jerk-pos ,obj) (,jerk-acc ,obj))
	     do (xlib:draw-point *game-window* *white-gc* (,smooth-pos ,obj) (,jerk-pos ,obj))
	     do (xlib:display-force-output *display*)
	     finally (return t)))))

(defun line-of-sight-p (from to &optional max)
  (let ((fx (+ (x from) (truncate (width from) 2)))
	(fy (+ (y from) (truncate (width from) 2)))
	(tx (+ (x to) (truncate (width to) 2)))
	(ty (+ (y to) (truncate (width to) 2))))
    (let ((dx (- tx fx))
	  (dy (- ty fy))
	  (los-obj (make-instance 'los-tracker
				  :x fx
				  :y fy
				  :target to
				  :width 0)))
      (when (or (and max (<= (+ (* dx dx)
				(* dy dy))
			     (* max max)))
		(not max))
		
	(cond ((<= 0 dx dy)
	       (bresenmacro los-obj dy dx 1 1 dy dx y x))
	      ((<= 0 dy dx)
	       (bresenmacro los-obj dx dy 1 1 dx dy x y))
	      ((<= dx dy 0)
	       (bresenmacro los-obj dx dy -1 -1 dx dy x y))
	      ((<= dy dx 0)
	       (bresenmacro los-obj dy dx -1 -1 dy dx y x))
	      ((<= dx 0)
	       (cond ((<= 0 (abs dx) dy)
		      (bresenmacro los-obj dy dx 1 -1 dy (abs dx) y x))
		     ((<= 0 dy (abs dx))
		      (bresenmacro los-obj dx dy -1 1 (abs dx) dy x y))))
	      ((<= dy 0)
	       (cond ((<= 0 dx (abs dy))
		      (bresenmacro los-obj dy dx -1 1 (abs dy) dx y x))
		     ((<= 0 (abs dy) dx)
		      (bresenmacro los-obj dx dy 1 -1 dx (abs dy) x y)))))))))

(defun fire (object type)
  "Let an OBJECT fire a projectile of type TYPE. The projectile type
will be cloned from the relevant template."
  (let ((dir (current-animation object))
	(w (width object))
	(speed (projectile-speed object))
	(w2 (truncate (width object) 2))
	(bx (x object))
	(by (y object)))
    (labels ((fb (x y)
		 (clone type :x x :y y
			:direction dir
			:shooter object)))
      (when (>= (since-fired object)
	       (max-delay object))
	(setf (since-fired object) 0)
	(let ((fb
	     (cond ((oddp dir)
		    (let ((delta (round (sqrt (/ (* speed speed) 2)))))
		      (case dir
			(1 ;; NE
			 (let ((fireball (fb (+ bx w) (1- by))))
			   (setf (movement fireball) (list delta (- delta)))
			   fireball))
			(3 ;; SE
			 (let ((fireball (fb (+ bx w) (+ by w))))
			   (setf (movement fireball) (list delta delta))
			   fireball))
			(5 ;; SW
			 (let ((fireball (fb (1- bx) (+ by w))))
			   (setf (movement fireball) (list (- delta) delta))
			   fireball))
			(7 ;; NW
			 (let ((fireball (fb (- bx 17) (- by 17))))
			   (setf (movement fireball)
				 (list (- delta) (- delta)))
			   fireball)))))
		   (t (case dir
			(0 ;; N
			 (let ((fireball (fb (+ bx w2) (- by 16))))
			   (setf (movement fireball) (list 0 (- speed)))
			   fireball))
			(2 ;; E
			 (let ((fireball (fb (+ bx w) (+ by w2))))
			   (setf (movement fireball) (list speed 0))
			   fireball))
			(4 ;; S
			 (let ((fireball (fb (+ bx w2) (+ by w))))
			   (setf (movement fireball) (list 0 speed))
			   fireball))
			(6 ;; W
			 (let ((fireball (fb (- bx 16) (+ by w2))))
			   (setf (movement fireball) (list (- speed) 0))
			   fireball)))))))
	fb)))))

(defun find-direction (from to)
  (let ((fx (+ (x from) (truncate (width from) 2)))
	(fy (+ (y from) (truncate (width from) 2)))
	(tx (+ (x to) (truncate (width to) 2)))
	(ty (+ (y to) (truncate (width to) 2))))
    (let ((dx (- tx fx))
	  (dy (- ty fy)))
      (cond ((zerop dx)
	     (if (< dy 0)
		 0
	       4))
	    ((zerop dy)
	     (if (< dx 0)
		 6
	       2))
	    (t (let ((ratio (the double-float (/ (float dy 0.0d0)
						 (float dx 0.0d0)))))
		 (cond ((<= 2.0d0 ratio)
			(if (< dx 0)
			    0
			  4))
		       ((<= 0.5d0 ratio 2.0d0)
			(if (< dx 0)
			    7
			  3))
		       ((<= -0.5d0 ratio 0.5d0)
			(if (< dx 0)
			    6
			  2))
		       ((<= ratio -2.0d0)
			(if (< dx 0)
			    4
			  0))
		       (t (if (< dx 0)
			      5
			    1)))))))))
