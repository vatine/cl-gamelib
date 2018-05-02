(in-package #:net.hexapodia.games-3d)

(defparameter *font-preference* '("-b&h-lucida-medium-i-normal-sans-12-120-75-75-p-71-iso8859-1" "fixed"))
(defvar *display* nil)
(defvar *screen* nil)
(defvar *backstore* nil)
(defvar *window* nil)
(defvar *green* nil)
(defvar *red* nil)
(defvar *blue* nil)
(defvar *colormap* nil)
(defvar *visual* nil)
(defvar *grid-separation* 5.0d0)
(defvar *check-timer* 0)


(defun ensure-display (&optional dpy)
  (unless *display*
    (setf *display* (funcall #'xlib:open-default-display dpy))
    (setf *screen* (car (xlib:display-roots *display*)))))

(defun closest-match (r g b)
  (flet ((color-dist (color r g b)
	   (let ((dr (* 0.52 (- r (xlib:color-red color))))
		 (dg (- g (xlib:color-green color)))
		 (db (* 0.21 (- b (xlib:color-blue color)))))
	     (+ (* dr dr) (* dg dg) (* db db)))))
    
    (let ((best-match nil)
	  (shortest 4.0))
      (dolist (pixel (xlib:query-colors *colormap*
					(loop for x from 0 to 255 collect x)))
	(when (< (color-dist pixel r g b) shortest)
	  (setf best-match pixel)
	  (setf shortest (color-dist pixel r g b))))
      (xlib:alloc-color *colormap* best-match))))

(defun get-colour (r g b)
  (case (xlib:visual-info-class (xlib:visual-info *display* *visual*))
    (:true-color (xlib:alloc-color *colormap*
				   (xlib:make-color :red r
						    :green g
						    :blue b)))
    (:pseudo-color (closest-match r g b))))

(defun ensure-camera ()
  (unless *camera*
    (setf *camera*
	  (make-instance 'camera
			 :x 0.0d0
			 :y 0.0d0
			 :z 1.0d0
			 :focal 1.0d0
			 :angle 0.0d0))
    (push *camera* *moving-objects*)))

(defun ensure-windows ()
  (unless *window*
    (ensure-display) 
    (setf *visual* (xlib:screen-root-visual *screen*))
    (setf *colormap* (xlib:screen-default-colormap *screen*))
    (setf *window* (xlib:create-window
		    :parent (xlib:screen-root *screen*)
		    :x 0 :y 0
		    :width +screen-side+ :height +screen-side+
		    :event-mask (xlib:make-event-mask
				 :button-press :button-release :focus-change
				 :exposure)))
    (let ((hints (xlib:make-wm-size-hints)))
      (setf (xlib:wm-size-hints-min-width  hints) 512)
      (setf (xlib:wm-size-hints-min-height hints) 532)
      (setf (xlib:wm-size-hints-max-width  hints) 512)
      (setf (xlib:wm-size-hints-max-height hints) 532)
;;;	(setf (xlib:program-specified-size-p hints) t)
      (setf (xlib:wm-normal-hints *window*) hints))
    (xlib:map-window *window*)
    (setf *backstore* (xlib:create-pixmap
		       :drawable *window*
		       :height +screen-side+ :width +screen-side+
		       :depth  (xlib:drawable-depth *window*)))
    (setf *black* (xlib:create-gcontext :background 0
				 :foreground (get-colour 0 0 0)
				 :drawable *window*))
    (setf *white* (xlib:create-gcontext :background 0
					:foreground (get-colour 1 1 1)
					:drawable *window*))
    (setf *green* (xlib:create-gcontext :background 0
					:foreground (get-colour 0 1 0)
					:drawable *window*))
    (setf *red* (xlib:create-gcontext :background 0
				      :foreground (get-colour 1 0 0)
				      :drawable *window*))
    (setf *blue* (xlib:create-gcontext :background 0
				      :foreground (get-colour 0 0 1)
				      :drawable *window*))
    (let ((font nil))
      (loop for font-designator in *font-preference*
	    until font
	    do (ignore-errors
		 (setf font (xlib:open-font *display* font-designator))))
      (when font
	(setf (xlib:gcontext-font *white*) font)
	(setf (xlib:gcontext-font *blue*) font))
      (unless font
	(error "No suitable font found!")))))

(defmethod draw-shape ((shape flat))
  (let ((vertexes (cam-vertexes shape)))
    (let ((points
	   (if (every 'in-view vertexes)
	       (loop for vertex across vertexes
		     append (camera-to-screen vertex *camera*))
	     (build-partially-clipped (loop for v across vertexes
					    collect v)))))
      (xlib:draw-lines *backstore* *white* points :fill-p t)
      (xlib:draw-lines *backstore* *black* points :fill-p nil))))

(defmethod draw-shape ((shape sphere))
  (let ((centre (get-vertex shape 0))
	(surface (get-vertex shape 1)))
    (let ((centre-xy (world-to-screen centre *camera*))
	  (surface-xy (world-to-screen surface *camera*)))
      (let ((dx (- (car surface-xy) (car  centre-xy)))
	    (dy (- (cadr surface-xy) (cadr centre-xy))))
	(let ((radius (round (sqrt (+ (* dx dx) (* dy dy))))))
	  (let ((x0 (- (car centre-xy) radius))
		(y0 (- (cadr centre-xy) radius)))
	    (xlib:draw-arc *backstore* *white*
			   x0 y0
			   (+ radius radius) (+ radius radius)
			   0 (* 2 pi) :fill-p t)
	    (xlib:draw-arc *backstore* *black*
			   x0 y0
			   (+ radius radius) (+ radius radius)
			   0 (* 2 pi) :fill-p nil)))))))

(defun draw-centred-glyphs (drawable gc x y seq)
  (multiple-value-bind (width ascent descent) (xlib:text-extents gc seq)
    (let ((w (truncate width 2))
	  (h (truncate (1+ (- ascent descent)) 2)))
      (xlib:draw-glyphs drawable gc (- x w) (+ y h) seq))))


(defun draw-coord-line-1 (w1 w2 cam draw)
  (if (and w1 w2)
    (let ((s1 (camera-to-screen w1 cam))
	  (s2 (camera-to-screen w2 cam)))
      (let ((d1 (line-start (car s1) (cadr s1) (car s2) (cadr s2)))
	    (d2 (line-start (car s2) (cadr s2) (car s1) (cadr s1))))
	(if (and d1 d2)
	  (xlib:draw-line draw *black*
			  (round (car d1)) (round (cadr d1))
			  (round (car d2)) (round (cadr d2)))
	  :line-start-failed)))
    :fed-nil))



(defun draw-coord-line (c1 c2 &optional (cam *camera*) (drawable *backstore*))
  (let ((c1 (world-to-camera c1 cam))
	(c2 (world-to-camera c2 cam)))
    (cond ((and (<= (abs (x c1)) (y c1)) ; Trivial visibility check
		(<= (abs (x c2)) (y c2)))
	   (draw-coord-line-1 c1 c2 cam drawable))
	  ((<= (abs (x c1)) (y c1)) ; Line start is within FOV
	   (draw-coord-line-1 c1 (clip-coord-line c1 c2) cam drawable))
	  ((<= (abs (x c2)) (y c2)) ; Line end is within FOV
	   (draw-coord-line-1 (clip-coord-line c1 c2) c2 cam drawable))
	  (t ; Neither end is within FOV
	   (draw-coord-line-1 (clip-coord-line c1 c2) (clip-coord-line c2 c1)
			      cam drawable)))))

(defun nearest (num base)
  (* base (round num base)))

(defun draw-grid (camera)
  (let ((delta (* 10.0d0 *grid-separation*))
	(xbase (nearest (x camera) *grid-separation*))
	(ybase (nearest (y camera) *grid-separation*)))
    (let ((cx1 (coord (- xbase delta) (- ybase delta) 0.0d0))
	  (cx2 (coord (- xbase delta) (+ ybase delta) 0.0d0))
	  (cy1 (coord (- xbase delta) (- ybase delta) 0.0d0))
	  (cy2 (coord (+ xbase delta) (- ybase delta) 0.0d0)))
      (loop for n from 0 to 21
	    do (progn
		 (draw-coord-line cx1 cx2)
		 (draw-coord-line cy1 cy2)
		 (incf (x cx1) *grid-separation*)
		 (incf (x cx2) *grid-separation*)
		 (incf (y cy1) *grid-separation*)
		 (incf (y cy2) *grid-separation*))))))




(defun fire (&key shooter (life-time 100))
  (let ((cam (or shooter *camera*)))
    (let ((angle (angle cam)))
      (let ((dy (* 2.0d0 (cos angle)))
	    (dx (* 2.0d0 (sin angle)))
	    (dz 0.0d0)
	    (x (x cam))
	    (y (y cam))
	    (z (z cam))
	    (delta 0.5d0))
	(let ((c1 (coord (- x delta) (- y delta) (- z delta)))
	      (c2 (coord (- x delta) (+ y delta) (- z delta)))
	      (c3 (coord (+ x delta) (+ y delta) (- z delta)))
	      (c4 (coord (- x delta) (+ y delta) (- z delta)))
	      (c5 (coord (- x delta) (- y delta) (+ z delta)))
	      (c6 (coord (- x delta) (+ y delta) (+ z delta)))
	      (c7 (coord (+ x delta) (+ y delta) (+ z delta)))
	      (c8 (coord (- x delta) (+ y delta) (+ z delta))))
	  (let ((missile (make-instance
			  'missile
			  :to-show 4
			  :shooter shooter
			  :vertexes (vector c1 c2 c3 c4 c5 c6 c7 c8)
			  :cam-vertexes (let ((vec (make-array 8)))
					  (loop for n below 8
						do (setf (aref vec n)
							 (coord 0.0d0
								0.0d0
								0.0d0)))
					  vec)
			  :delta (coord dx dy dz)
			  :rounds life-time)))
	    (when shooter
	      (move missile 6.0d0))
	    (push missile *moving-objects*)
	    (push missile *all-shapes*))))))
  20)

(defun detect-collisions ()
  (loop for obj2 in *all-shapes*
	do (loop for obj1 in *moving-objects*
		 do (when (collide-p obj1 obj2)
		      (collide-action obj1 obj2)))))

(defun ui-loop (&key (delay 0.1d0) display)
  (ensure-display display)
  (ensure-windows)
  (ensure-camera)
  (when (< *lives* 0)
    (setf *lives* +start-lives+)
    (setf *score* 0)
    (setf (x *camera*) 0.0d0)
    (setf (y *camera*) 0.0d0)
    (setf (angle *camera*) 0.0d0)
    (setf *all-shapes* nil)
    (setf *moving-objects* (delete-if-not (lambda (x)
					    (typep x 'camera))
					  *moving-objects*)))
  (let ((finish nil))
    (loop until (or (< *lives* 0) finish)
	  do (progn 
	       (decf *check-timer*)
	       (when (<= *check-timer* 0) 
		 (setf *check-timer* +check-interval+))
		   
	       (xlib:event-case (*display* :timeout delay)
		 (:button-press (window code x y)
		    (my-ignore window x y)
		    (ignore-errors
		      (case code
			(1 #-(AND)
			   (or *reload-timer* (setf *reload-timer* (fire)))
			   nil
			   )
			(2 nil)
			(3 (setf finish t))))
		    t)
		 (:button-release (window code x y)
		    (my-ignore window x y)
		    (case code
		      (1 nil)
		      (2 nil)
		      (3 nil))
		    t)
		 (:focus-out ()
		    t)
		 (:focus-in ()
		    t)
		 (:exposure ()
		    t))
	       (multiple-value-bind (x)
		   (xlib:query-pointer *window*)
		 (let ((angle (float
			       (/ (- x (/ +screen-side+ 2))
				 +screen-side+) 0.0d0)))
		   (turn *camera* angle)
		   (loop for obj in *moving-objects*
			 do (move obj))
		   (detect-collisions)
		   ;(update-screen)
		   ))))))
