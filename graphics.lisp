(in-package #:net.hexapodia.games-2d)

(defparameter *font-preference* '("-b&h-lucida-medium-i-normal-sans-12-120-75-75-p-71-iso8859-1" "fixed"))

(defconstant +unit-size+ 64 "Pixels per tile, as it were")

(defvar *display* nil)
(defvar *screen* nil)
(defvar *game-window* nil)
(defvar *background* nil)
(defvar *gcontext-set* nil)
(defvar *black* nil)
(defvar *player* nil)
(defvar *repaint* nil)
(defvar *view-x* 0)
(defvar *view-y* 0)
(defvar *window-width* 512)
(defvar *window-height* (+ *window-width* 20))
(defvar *external-key-repeat* nil)
(defvar *game-over* nil)
(defvar *white-gc* nil)

(defvar *controls* 0 "Control word for activated buttons,
b0 - left, b1 - up, b2 - right, b3 - down, b4 - fire")

(defvar *window-map* (make-hash-table))
(defvar *moving-objects* nil)
(defvar *players* nil)
(defvar *generators* nil)

;;; Functions ensuring important objects
(defun ensure-display (&optional dpy)
  (unless *display*
    (setf *display* (funcall #'xlib:open-default-display dpy))
    (setf *screen* (car (xlib:display-roots *display*)))))

(defun ensure-gamewindow ()
  (unless *game-window*
    (ensure-display)
    (let ((screen (xlib:display-default-screen *display*)))
      (setf *game-window*
	    (xlib:create-window
	     :parent (xlib:screen-root screen)
	     :x 0
	     :y 0
	     :width *window-width*
	     :height *window-height*
	     :event-mask (xlib:make-event-mask :key-press :key-release
					       :exposure :focus-change)))
      (let ((hints (make-wm-size-hints)))
	(setf (xlib:wm-size-hints-min-width  hints) *window-width*)
	(setf (xlib:wm-size-hints-min-height hints) *window-height*)
	(setf (xlib:wm-size-hints-max-width  hints) *window-width*)
	(setf (xlib:wm-size-hints-max-height hints) *window-height*)
;;;	(setf (xlib:program-specified-size-p hints) t)
	(setf (xlib:wm-normal-hints *game-window*) hints))
      (xlib:map-window *game-window*)
      (xlib:display-force-output *display*))))

(defun ensure-background ()
  (ensure-gamewindow)
  (unless *gcontext-set*
    (setf *gcontext-set*
	  (xlib:create-gcontext
	   :drawable *game-window*
	   :background (xlib:screen-black-pixel *screen*)
	   :foreground (xlib:screen-white-pixel *screen*)))
    (setf *black*
	  (xlib:create-gcontext
	   :drawable *game-window*
	   :background (xlib:screen-black-pixel *screen*)
	   :foreground (xlib:screen-black-pixel *screen*)))
    (let ((font (xlib:open-font *display* (car *font-preference*))))
      (when font
	(setf (xlib:gcontext-font *gcontext-set*) font)))) 
  (when net.hexapodia.maps-2d:*current-map*
    (when *background*
      (let ((w (xlib:drawable-width *background*))
	    (h (xlib:drawable-height *background*)))
	(when (or (< w (* +unit-size+ (net.hexapodia.maps-2d:width  net.hexapodia.maps-2d:*current-map*)))
		  (< h (* +unit-size+ (net.hexapodia.maps-2d:height net.hexapodia.maps-2d:*current-map*))))
	  (xlib:free-pixmap *background*)
	  (setf *background* nil))))
    (unless *background*
      (let ((w (* +unit-size+ (net.hexapodia.maps-2d:width  net.hexapodia.maps-2d:*current-map*)))
	    (h (* +unit-size+ (net.hexapodia.maps-2d:height net.hexapodia.maps-2d:*current-map*))))
	(setf *background* (xlib:create-pixmap
			    :width w
			    :height h
			    :depth (xlib:drawable-depth *game-window*)
			    :drawable *game-window*))))))

;;; Class definitions

(defclass object ()
  ((x :accessor x :initarg :x :type fixnum)
   (y :accessor y :initarg :y :type fixnum)
   (pixmap :accessor pixmap :initarg :pixmap)
   (window :accessor window :initarg :window) 
   ))

(defclass animated-object (object)
  ((pixmaps :reader pixmaps :initarg :pixmaps :type (array * (*)))
   (gcontexts :accessor gcontexts :initarg :gcontexts :type (array * (*)))
   (reverse-gcs :accessor reverse-gcs :initarg :reverse-gcs :type (array * (*))) 
   (pix-cnt :reader pix-cnt :initarg :pix-cnt :type fixnum)
   (pix-idx :accessor pix-idx :initarg :pix-idx :type fixnum)
   (width :reader width :initarg :width :type fixnum) 
   ))

(defclass moving-object (object)
  ((direction :accessor direction :initarg :direction)
   (unmapped :accessor unmapped :initform nil) 
   (dx :accessor dx :initarg :dx :type fixnum)
   (dy :accessor dy :initarg :dy :type fixnum)
   (old-x :accessor old-x :initarg :x :initform 0)
   (old-y :accessor old-y :initarg :y :initform 0)
   (old-dir :accessor old-dir :initarg :dir)
   (width :reader width :initarg :width :type fixnum) 
   (animations :accessor animations :initarg :animations :type (array * (*)))
   (ai :accessor ai :initarg :ai :initform nil) 
   (current-animation :accessor current-animation :initarg :current-animation) 
   )
  (:default-initargs :dx 0 :dy 0))

;;; Help functions
(defun displayable (obj)
  (let ((view-offset-x (- (x obj) *view-x*))
	(view-offset-y (- (y obj) *view-y*)))
    (and (or (<= 0 view-offset-x 511)
	     (<= 0 (+ view-offset-x +unit-size+) 511))
	 (or (<= 0 view-offset-y 511)
	     (<= 0 (+ view-offset-y +unit-size+) 511)))))

;;; Generics

(defgeneric step-animation (obj)
  )
(defgeneric get-gcontext (obj))
(defgeneric get-gcontext-rev (obj))
;;; Methods

(defmethod step-animation ((obj object))
  nil)
(defmethod step-animation ((obj animated-object))
  (let ((ix (1+ (pix-idx obj))))
    (when (>= ix (pix-cnt obj))
      (setf ix (mod ix (pix-cnt obj))))
    (setf (pix-idx obj) ix)
    (setf (pixmap obj) (aref (pixmaps obj) ix))))
(defmethod step-animation ((obj moving-object))
  (let ((an (aref (animations obj) (current-animation obj))))
    (step-animation an)
    (setf (pixmap obj) (pixmap an))))

(defmethod pixmap ((obj animated-object))
  (aref (pixmaps obj) (pix-idx obj)))
;;;(defmethod pixmap ((obj moving-object))
;;;  (let ((an (aref (animations obj) (current-animation obj))))
;;;    (pixmap an)))

(defmethod draw :before (obj)
  (when *repaint*
    (setf *repaint* nil)
    (draw-map)))

(defmethod draw ((obj object))
  (let ((win (window obj)))
    (if (or (xlib:drawable-equal win *game-window*)
	    (xlib:drawable-equal win *background*))
	(let ((pm (pixmap obj)))
	  (xlib:copy-area pm *gcontext-set* 0 0 +unit-size+ +unit-size+
			  *background* (x obj) (y obj)))
      (progn 
	(if (displayable obj)
	    (let ((pm (pixmap obj)))
	      (clear-area win)
	      (xlib:copy-area pm (get-gcontext obj) 0 0 +unit-size+ +unit-size+
			      win 0 0)
	      (setf (xlib:drawable-x win) (- (x obj) *view-x*))
	      (setf (xlib:drawable-y win) (- (y obj) *view-y*)))
	  (xlib:unmap-window win))))))

(defmethod draw ((obj moving-object))
  (let ((win (window obj)))
    (when (eq obj *player*)
      (cond ((< (x obj) 236) (setf *view-x* 0))
	    ((<= 236
		 (x obj)
		 (- (* +unit-size+ (width net.hexapodia.maps-2d:*current-map*)) 276))
	     (setf *view-x* (- (x obj) 236)))
	    (t (setf *view-x*
		     (- (* +unit-size+ (width net.hexapodia.maps-2d:*current-map*)) 512)))) 
      (cond ((< (y obj) 236) (setf *view-y* 0))
	    ((<= 236
		 (y obj)
		 (- (* +unit-size+ (height net.hexapodia.maps-2d:*current-map*)) 276))
	     (setf *view-y* (- (y obj) 236)))
	    (t (setf *view-y*
		     (- (* +unit-size+ (height net.hexapodia.maps-2d:*current-map*)) 512)))))
    (if (displayable obj)
	(let ((pm (pixmap obj))
	      (draw? t)
	      (draw-x (- (x obj) *view-x*))
	      (draw-y (- (y obj) *view-y*))) 

	  (map-movable obj)

	  (xlib:copy-area *background* *gcontext-set*
			  (old-x obj) (old-y obj)
			  (width obj) (width obj)
			  *game-window*
			  (- (old-x obj) *view-x*) (- (old-y obj) *view-y*))

	  (setf (old-x obj) (x obj))
	  (setf (old-y obj) (y obj))
	  (setf (old-dir obj) (current-animation obj))

	  (xlib:copy-area *background* (get-gcontext-rev obj)
			  (x obj) (y obj) (width obj) (width obj)
			  win 0 0)

	  (xlib:copy-area pm (get-gcontext obj) 0 0 (width obj) (width obj)
			  win 0 0)

	  (setf (xlib:drawable-x win) draw-x)
	  (setf (xlib:drawable-y win) draw-y)
	  
	  (xlib:display-force-output *display*) 
	  )
      (unmap-movable obj win))
    ))

(defun update-status-window ()
  (when *player*
    (xlib:draw-rectangle *game-window* *black* 0 512 512 20 t)
    (xlib:draw-glyphs
     *game-window* *gcontext-set* 10 527
     (format nil "Score: ~6,'0D  Health: ~4,'0D Keys: ~2,'0D ~6,'0B [~3,'0D ~3,'0D]"
	     (score *player*) (health *player*) (keys *player*)
	     *controls* (x *player*) (y *player*)))))

(defmethod draw ((obj xlib:window))
  (cond ((xlib:drawable-equal obj *game-window*)
	 (update-status-window)
	 (when *repaint*
	   (draw-map)
	   (setf *repaint* nil))
	 (copy-area *background* *gcontext-set* *view-x* *view-y* 512 512 *game-window* 0 0)
	 (loop for o in *moving-objects*
	       do (draw o)))
	(t (let ((new-obj (gethash obj *window-map*)))
	     (draw new-obj)))))

(defmethod get-gcontext ((obj animated-object))
  (aref (gcontexts obj) (pix-idx obj)))

(defmethod get-gcontext ((obj moving-object))
  (get-gcontext (aref (animations obj) (current-animation obj))))

(defmethod get-gcontext-rev ((obj animated-object))
  (aref (reverse-gcs obj) (pix-idx obj)))
(defmethod get-gcontext-rev ((obj moving-object))
  (get-gcontext-rev (aref (animations obj) (current-animation obj))))

(defun key-bit (code one-bit)
  (let ((keysym (xlib:keycode->keysym *display* code 0)))
    (case keysym
      (#xff51 (if one-bit #b00001 #b11110))
      (#xff52 (if one-bit #b00010 #b11101))
      (#xff53 (if one-bit #b00100 #b11011))
      (#xff54 (if one-bit #b01000 #b10111))
      (#x0020 (if one-bit #b10000 #b01111))
      (t      (if one-bit #b00000 #b11111)))))

(defun press-key (code)
  (setf *controls* (logior *controls* (key-bit code t))))

(defun release-key (code)
  (setf *controls* (logand *controls* (key-bit code nil))))

(defun deal-with-buttons ()
  (let ((move (logand *controls* #b01111)))
    (case move
      (#b0001 (setf (movement *player*) (object-speed *player* 6)))
      (#b0011 (setf (movement *player*) (object-speed *player* 7)))
      (#b0010 (setf (movement *player*) (object-speed *player* 0)))
      (#b0110 (setf (movement *player*) (object-speed *player* 1)))
      (#b0100 (setf (movement *player*) (object-speed *player* 2)))
      (#b1100 (setf (movement *player*) (object-speed *player* 3)))
      (#b1000 (setf (movement *player*) (object-speed *player* 4)))
      (#b1001 (setf (movement *player*) (object-speed *player* 5)))
      (t (setf (movement *player*) '(0 0))))
    (when (not (zerop (logand *controls* #b10000)))
      (fire *player*))))

(defun set-repeat ()
  (unless *external-key-repeat*
    (multiple-value-bind (click bell pitch dur led-mask global-repeat autos)
	(xlib:keyboard-control *display*)
      (games-ignore click bell pitch dur led-mask autos)
      (setf *external-key-repeat* global-repeat)
      (xlib:change-keyboard-control *display*
				    :auto-repeat-mode :off))))

(defun reset-repeat ()
  (when *external-key-repeat*
    (xlib:change-keyboard-control *display*
				  :auto-repeat-mode *external-key-repeat*)
    (setf *external-key-repeat* nil)))

(defun default-interaction-loop ()
  (let ((xy (net.hexapodia.maps-2d:find-starting-position net.hexapodia.maps-2d:*current-map*)))
    (setf *player*
	  (clone 'player
		 :x (* +unit-size+ (first xy))
		 :y (* +unit-size+ (second xy))))) 
  (loop until *game-over*
	for key-change-p = nil
	do (xlib:event-case (*display* :timeout 0)
	     (:key-press (window code)
		(games-ignore window)
	        (press-key code)
		(setf key-change-p t))
	     (:key-release (window code)
		(games-ignore window)
                (release-key code)
		(setf key-change-p t))
	     (:focus-in (window mode kind)
		(games-ignore window mode kind)
		(set-repeat)
		t)
	     (:focus-out (window mode kind)
		(games-ignore window mode kind)
		(reset-repeat)
		t))
	if key-change-p do (deal-with-buttons)
	do (progn
	     (move-all)
	     (draw *game-window*)
	     (sleep 0.01)
	     (if (<= (health *player*) 0)
		 (setf *game-over* t)))))
