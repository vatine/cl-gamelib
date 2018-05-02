(in-package #:net.hexapodia.games-2d)

(defvar *dir-anim* '((:north 0) (:north-east 1) (:east 2) (:south-east 3)
		     (:south 4) (:south-west 5) (:west 6) (:north-west 7)))
(defvar *clone-masters* nil)
(defparameter *animations* nil)
(defvar *allow-gc* nil)
(defvar *block-gc* nil)

(defvar *clip-debug* nil)

(defmacro map-animation (type axes diags)
  "Maps from an animated movable type to animations.

Each animation specifier is on the form:
  (<file name> <frame> ...)

The second argument is a list of animation frames for movement like +
The third argument is a list of animation frames for movement like X" 
  `(eval-when (:load-toplevel :execute)
     (push (list :classic ',type ',axes ',diags) *animations*)))

(defmacro map-perspective-animation (type n ne e se s sw w nw)
  "Maps from an animated movable type to animations.

Each animation specifier is on the form:
  (<file name> <frame> ...)

The second onwards argument is animation specifiers for movement starting
North, then clockwise around to North-west."
  `(eval-when (:load-toplevel :execute)
     (push (list :perspective ',type ',n ',ne ',e ',se ',s ',sw ',w ',nw) *animations*)))

(defun unmap-movable (obj win)
  (xlib:unmap-window win)
  (setf (unmapped obj) t)
  (when (or (old-x obj) (old-y obj) (old-dir obj))
    (setf (old-x obj) nil)
    (setf (old-y obj) nil)
    (setf (old-dir obj) nil)
    (draw *game-window*)))

(defun map-movable (obj)
  (when (unmapped obj)
    (setf (unmapped obj) nil)
    (xlib:map-window (window obj))
    (setf (old-x obj) (x obj))
    (setf (old-y obj) (y obj))
    ))


(defgeneric check-move (object tile off-x off-y)
  (:documentation "Check an object against a specific type of tile,
return NIL if movement is OK, <tile> if there's something blocking"))

(defgeneric collide (obj-1 obj-2)
  (:documentation "When CHECK-MOVE indicates there's movement problems, call
out to COLLIDE with the object that was trying to move and an indication as
to what is being collided with, so a proper handling can be done."))

(defmethod collide (o1 o2)
  (declare (ignore o1 o2))
  nil)


(defmacro defchecker (type (xl xh) (yl yh))
  (let ((typespec (if (keywordp type)
		      (list 'eql type)
		    type)))
    `(defmethod check-move ((obj moving-object) (tile ,typespec) off-x off-y)
       (when (and (and (<=  off-x ,xh)
		       (<= ,xl (+ (width obj) off-x)))
		  (and (<=  off-y ,yh)
		       (<= ,yl (+ off-y (width obj)))))
	 tile))))

(defmacro def-partial-checker (type (xl xh) (yl yh) &rest classes)
  (let ((typespec (if (keywordp type)
		      (list 'eql type)
		    type)))
    (append
     (list 'progn)
     `((defmethod check-move (obj (tile ,typespec) x y)
	 (declare (ignore obj tile x y))
	 nil))
     (loop for class in classes
	   collect `(defmethod check-move ((obj ,class) (tile ,typespec) off-x off-y)
		      (when (and (and (<=  off-x ,xh)
				      (<= ,xl (+ (width obj) off-x)))
				 (and (<=  off-y ,yh)
				      (<= ,yl (+ off-y (width obj)))))
			tile))))))

(defmethod check-move ((o1 moving-object) (o2 moving-object) off-x off-y)
  "First Approximation: When Sprites Collide"
  (let ((w1 (width o1))
	(w2 (width o2))
	(x1 (+ off-x (x o1)))
	(x2 (x o2))
	(y1 (+ off-y (y o1)))
	(y2 (y o2)))
    (and (<= x1 (+ x2 w2))
	 (<= x2 (+ x1 w1))
	 (<= y1 (+ y2 w2))
	 (<= y2 (+ y1 w1))
	 o2)))

(defun move-ok-1 (object)
  (let ((coll nil))
    (multiple-value-bind (tile-x off-x) (truncate (+ (x object) (dx object))
						  +unit-size+)
      (multiple-value-bind (tile-y off-y) (truncate (+ (y object) (dy object))
						    +unit-size+)
	(let ((w (width object)))
	  (if (setf coll
		    (or
		     (check-move object (tile-from-map tile-x tile-y)
				 off-x off-y)
		     (and
		      (>= (+ off-x w) +unit-size+)
		      (check-move object (tile-from-map (1+ tile-x) tile-y)
				  (- off-x +unit-size+) off-y))
		     (and
		      (>= (+ off-y w) +unit-size+)
		      (check-move object (tile-from-map tile-x (1+ tile-y))
				  off-x (- off-y +unit-size+)))
		     (and
		      (>= (+ off-y w) +unit-size+)
		      (>= (+ off-x w) +unit-size+)
		      (check-move
		       object (tile-from-map (1+ tile-x) (1+ tile-y))
		       (- off-x +unit-size+) (- off-y +unit-size+)))))
	      (values nil coll)
	    (values t nil)))))))

(defun move-ok-2 (object)
  (let ((off-x (dx object))
	(off-y (dy object)))
  (loop for obj in *moving-objects*
	unless (eql obj object)
	do (let ((coll (check-move object obj off-x off-y)))
	     (if coll
		 (return-from move-ok-2 (values nil coll))))))
  (values t nil))

(defun move-ok (object)
  (multiple-value-bind (ok obj) (move-ok-1 object)
    (if ok
	(move-ok-2 object)
      (values nil obj))))
      

(defmethod movement ((obj moving-object))
  (list (dx obj) (dy obj)))

(defmethod (setf movement) (delta (obj moving-object))
  (let ((dx (car delta))
	(dy (cadr delta)))
    (let ((sdx (signum dx))
	  (sdy (signum dy)))
      (setf (old-dir obj) (current-animation obj))
      (case sdx
	( 1 (case sdy
	      ( 1 (setf (direction obj) :south-east)
		  (setf (current-animation obj) 3))
	      ( 0 (setf (direction obj) :east)
		  (setf (current-animation obj) 2))
	      (-1 (setf (direction obj) :north-east)
		  (setf (current-animation obj) 1))))
	( 0 (case sdy
	      ( 1 (setf (direction obj) :south)
		  (setf (current-animation obj) 4))
	      ( 0 (setf (direction obj) :still))
	      (-1 (setf (direction obj) :north)
		  (setf (current-animation obj) 0))))
	(-1 (case sdy
	      ( 1 (setf (direction obj) :south-west)
		  (setf (current-animation obj) 5))
	      ( 0 (setf (direction obj) :west)
		  (setf (current-animation obj) 6))
	      (-1 (setf (direction obj) :north-west)
		  (setf (current-animation obj) 7))))))
    (setf (dx obj) dx)
    (setf (dy obj) dy)))

(defmethod move ((obj object))
  nil)

(defmethod move ((obj animated-object))
  (incf (pix-idx obj))
  (if (= (pix-idx obj) (pix-cnt obj))
      (setf (pix-idx obj) 0)))

(defmethod move ((obj moving-object))
  (unless (and (zerop (dx obj))
	       (zerop (dy obj)))
    
    (multiple-value-bind (move-ok cobj) (move-ok obj)
      (when move-ok
	(incf (x obj) (dx obj))
	(incf (y obj) (dy obj)))
      (when (not move-ok)
	(collide obj cobj)))
    (step-animation obj))
  ;;  (move (aref (animations obj) (current-animation obj)))
  )

(defmethod move :after ((obj firing-object))
  (incf (since-fired obj)))

;;;

(defun load-animation (list direction)
  "Load animation file and twist according to the direction specified,
Create an animation object and fill with pixmaps."
  (let ((ar (read-image-file (car (nth 0 list)))))
    (let ((max (car (array-dimensions ar)))
	  (len (1+
		(apply #'max (mapcar (lambda (l)
					(apply #'max (cdr l)))
			list)))))
      (flet ((transform (x y)
	       (case direction
		 ((0 1) (list y (- max x 1)))
		 ((2 3) (list x y))
		 ((4 5) (list (- max y 1) x))
		 ((6 7) (list (- max x 1) (- max y 1))))))
	(let ((anim (make-instance 'animated-object
				   :width max
				   :pixmaps (make-array len)
				   :gcontexts (make-array len)
				   :reverse-gcs (make-array len)
				   :pix-cnt len
				   :pix-idx 0)))
	  (loop
	   for (file . ixspec) in list
	   for ar = (read-image-file file)
	   do (let* ((px (create-pixmap
			  :width max
			  :height max
			  :depth *game-depth*
			  :drawable *game-window*))
		     (mask-px (xlib:create-pixmap
			       :width max
			       :height max
			       :depth 1
			       :drawable (xlib:screen-root *screen*)))
		     (rev-mask (xlib:create-pixmap
			       :width max
			       :height max
			       :depth 1
			       :drawable (xlib:screen-root *screen*)))
		     (*allow-gc* (xlib:create-gcontext
				  :drawable mask-px
				  :background 0
				  :foreground 1))
		     (*block-gc* (xlib:create-gcontext
				  :drawable mask-px
				  :background 0
				  :foreground 0)))
		(loop for ix in ixspec
		      do (progn
			   (setf (aref (pixmaps anim) ix) px)
			   (setf (aref (gcontexts anim) ix) mask-px)))
		(loop
		 for x from 0 below max
		 do (loop
		     for y from 0 below max
		     for arval = (aref ar x y)
		     for color = (map-color arval)
		     do (progn 
			  (apply #'xlib:draw-point
				 px color (transform x y)) 
			  (apply #'xlib:draw-point
				 mask-px (if (zerop arval)
					     *block-gc*
					   *allow-gc*)
				 (transform x y))
			  (apply #'xlib:draw-point
				 rev-mask (if (zerop arval)
					     *allow-gc*
					   *block-gc*)
				 (transform x y)))))
		(loop for ix in ixspec
		      do (progn
			   (setf (aref (pixmaps anim) ix) px)
			   (setf (aref (gcontexts anim) ix) mask-px)
			   (setf (aref (reverse-gcs anim) ix) rev-mask)))))
	  anim)))))

(defun load-movable (type anim-axis anim-diag)
  (let ((ar (make-array 8)))
    (loop for ix from 0 below 8 by 2
	  do (setf (aref ar ix) (load-animation anim-axis ix)))
    (loop for ix from 1 below 8 by 2
	  do (setf (aref ar ix) (load-animation anim-diag ix)))
    (make-instance type
		 :animations ar
		 :width (width (aref ar 0))
		 :x 0
		 :y 0)))

(defun load-perspective-animation (type axes)
  (let ((ar (make-array 8)))
    (loop for axis in axes
	  for ix from 0
	  do (setf (aref ar ix) (load-animation axis 2)))
    (make-instance type
		 :animations ar
		 :width (width (aref ar 0))
		 :x 0
		 :y 0)))

(defun ensure-animations ()
  (unless *block-gc*
    (setf *block-gc* (xlib:create-gcontext
		      :drawable *game-window*
		      :background 0
		      :foreground 0)))
  (unless *white-gc*
    (setf *white-gc* (xlib:create-gcontext
		      :drawable *game-window*
		      :background 0
		      :foreground (xlib:screen-white-pixel *screen*))))
  (unless *clone-masters*
    (setf *clone-masters* (make-hash-table))
    (ensure-colorstuff)
    (loop for (type1 . anim-spec) in *animations*
	  do (case type1
	       (:classic (apply 'load-animation anim-spec))
	       (:perspective (apply 'load-perspective-animation anim-spec))))))

(defun clone-animation (anim ix win)
  (let ((new (make-instance 'animated-object
			    :width (width anim)
			    :gcontexts (make-array (array-dimensions (gcontexts anim)))
			    :reverse-gcs (make-array (array-dimensions (gcontexts anim)))
			    :pix-cnt (pix-cnt anim)
			    :pixmaps (pixmaps anim)
			    :pix-idx (if ix ix (random (pix-cnt anim))))))
    (loop with dst = (gcontexts new)
	  with revd = (reverse-gcs new)
	  for ix from 0
	  for px across (gcontexts anim)
	  for rev across (reverse-gcs anim)
	  do (let ((new-px px)
		   (new-rev rev))
	       (setf (aref dst ix)
		     (xlib:create-gcontext :drawable win
					   :clip-mask new-px
					   :clip-x 0
					   :clip-y 0))
	       (setf (aref revd ix)
		     (xlib:create-gcontext :drawable win
					   :clip-mask new-rev
					   :clip-x 0
					   :clip-y 0))))
    new))

(defun wash-keywords (list &rest keys-to-zap)
  (loop for (key val . rest) on list by #'cddr
	unless (member key keys-to-zap)
	append (list key val)))

(defun clone (type &rest args &key (x 0 x-p) (y 0 y-p) (direction 0) &allow-other-keys)
  "Create a new object of TYPE, attach all relevant graphic details from the
clone master template, return the new object"
  (let* ((template (gethash type *clone-masters*))
	 (new (apply #'make-instance type
		     :width (width template)
		     :animations (make-array 8)
		     (wash-keywords args :x :y :direction))))
    (if x-p
	(setf (x new) x)
      (setf (x new) (x template)))
    (if y-p
	(setf (y new) y)
      (setf (y new) (y template)))
    (setf (current-animation new) direction)
    
    (setf (window new)
	  (xlib:create-window
	   :parent *game-window*
	   :x (x new)
	   :y (y new)
	   :background :parent-relative
	   :width (width new)
	   :height (width new)))
    
    (xlib:map-window (window new))
    (let ((anims (animations new))
	  (templates (animations template)))
      (loop for ix from 0
	    for tmp across templates
	    do (setf (aref anims ix)
		     (clone-animation tmp ix (window new))))
      ;;(setf (pixmap new) (pixmap (aref anims (current-animation new))))
      )
    (push new *moving-objects*)
    (step-animation new)
    (when (eql type 'player)
      (push new *players*)
      (unless *player*
	(setf *player* new)))
    new))

(defgeneric delete-clone (clone))

(defmethod delete-clone (clone)
  (setf *moving-objects* (delete clone *moving-objects*))

  (let ((win (window clone)))
    (xlib:unmap-window win)))

(defmethod delete-clone ((clone moving-object))
  (call-next-method)
  (when (and (slot-boundp clone 'ai)
	     (ai clone))
    (setf (ai clone) nil))
  (loop for anim across (animations clone)
	do (delete-clone anim)))

(defmethod delete-clone ((clone animated-object))
  (loop for gc across (gcontexts clone)
	do (xlib:free-gcontext gc)))

(defun move-all ()
  (loop for obj in *moving-objects*
	do (when (ai obj) (act obj))
	do (move obj))
  (loop for gen in *generators*
	do (when (ai gen) (act gen))))

