(in-package :net.hexapodia.maps-2d)

(defvar *maps* nil "Array of maps")
(defvar *mapcount* 0 "Map count")
(defvar *map-path* #p"*.map" "Map path specifier")
(defvar *current-map* nil)
(defvar *symbol-map* nil "Hash table mapping map symbol to pixmap-family")
(defvar *symbol-map-data* '((#\S :floor)) "List of lists (CHAR SYMBOL) for map data")
(defvar *tile-map* nil "Hash table mapping map pixmap-family to symbol")
(defconstant +east+ 8)
(defconstant +north+ 4)
(defconstant +south+ 2)
(defconstant +west+ 1)

(defvar *tiles-with-direction* nil "List of lists (BASE TESTFUN) for tiles that should connect to something else but doesn't fill the tile on its own.")

(defvar *generator-map* (make-hash-table :test 'equal))
(defvar *tile-generator-data* nil "List of lists, like *SYMBOL-MAP-DATA*, but for generators")
(defvar *multi-tile-function-map* (make-hash-table))

(defclass map ()
  ((seq :reader seq :initarg :seq)
   (width :reader width :initarg :width)
   (height :reader height :initarg :height)
   (mapdata :reader mapdata :initarg :mapdata)
   ))

(defun make-map (width height seq)
  (let ((ar (make-array (list width height)
			:element-type 'character
			:initial-element #\space)))
    (setf (aref *maps* seq)
	  (make-instance 'map
			 :seq seq
			 :width width :height height
			 :mapdata ar))))

(defun read-map (fname)
  (with-open-file (map (merge-pathnames fname
					net.hexapodia.games-2d:*file-path*)
		       :direction :input)
    (let* ((seq (read map))
	   (w (read map))
	   (h (read map)))
      (let ((mapobj (make-map w h seq)))
	(loop for line from 0 below h
	      for ldata = (read-line map)
	      do (loop for pos from 0 below w
		       do (setf (aref (mapdata mapobj) pos line)
				(aref ldata pos))))
	mapobj))))

(defun read-all-maps (&optional spec)
  (let ((basepath (or spec
		      (merge-pathnames *map-path*
				       net.hexapodia.games-2d:*file-path*))))
    (let ((files (directory basepath)))
      (setf *mapcount* (length files))
      (setf *maps* (make-array *mapcount*))
      (loop for file in files
	    do (read-map file)))))

(defun activate-generators ()
  (clrhash *generator-map*)
  (let ((w (width *current-map*))
	(h (height *current-map*))
	(md (mapdata *current-map*)))
    (loop for x from 0 below w
	  do (loop for y from 0 below h
		   do (let* ((c (aref md x y))
			     (tv (member c *tile-generator-data* :key #'car)))
			(when tv
			  (let ((gen (second (first tv))))
			    (net.hexapodia.games-2d:make-generator
			     (gethash c *tile-map*)
			     gen x y))))))))

(defun activate-map (seq)
  (when (and (<= 0 seq)
	     (< seq *mapcount*))
    (setf *current-map* (aref *maps* seq))
    (activate-generators)))

(defun multi-tile-from-map (x y char)
  (let ((indicator 0)
	(map (mapdata *current-map*))
	(w (1- (width *current-map*)))
	(h (1- (height *current-map*))))
    (when (< 0 x)
      (if (char= (aref map (1- x) y) char)
	  (incf indicator +west+)))
    (when (< 0 y)
      (if (char= (aref map x (1- y)) char)
	  (incf indicator +north+)))
    (when (< x w)
      (if (char= (aref map (1+ x) y) char)
	  (incf indicator +east+)))
    (when (< y h)
      (if (char= (aref map x (1+ y)) char)
	  (incf indicator +south+)))
    indicator))

(defun make-keyword (name)
  (read-from-string (format nil ":~s" name)))

(defmacro define-type-from-map (key &rest chars)
  (let ((keys (loop for end in '("" -w -s -sw -n -nw -ns -nsw -e -ew -es
				 -esw -en -enw -ens -ensw)
		    collect (make-keyword
			     (concatenate 'string
					  (string key) (string end)))))
	(fun-name (intern (concatenate 'string (string key)
				       (string '-from-map)))))
    (push keys *tiles-with-direction*)
    `(progn
       (defun ,fun-name (x y)
	 (nth (logior ,@ (loop for char in chars
			       collect (list 'multi-tile-from-map
					     'x 'y char)))
	      ',keys))
       (setf (gethash (nth 0 ',keys) *multi-tile-function-map*)
	     (function ,fun-name)))))

(defmacro define-tile (char type)
  (let ((tile-type (make-keyword (string type))))
    `(push (list ,char ,tile-type) *symbol-map*)))

(defmacro define-generator (char tile-type generator-type)
  (let ((tile-type (make-keyword (string tile-type))))
    `(progn
       (push (list ,char ,tile-type) *symbol-map*)
       (push (list ,char ',generator-type) *tile-generator-data*))))

(defun tile-from-map (x y &optional actual-type)
  (unless *symbol-map*
    (setf *symbol-map* (make-hash-table))
    (loop for (char tile) in *symbol-map-data*
	  do (setf (gethash char *symbol-map*) tile)))
  (let* ((char (aref (mapdata *current-map*) x y))
	 (tile-type (gethash char *symbol-map*)))
    (cond ((null tile-type)
	   (error
	    "Unknown map symbol ~C, map reference (~D,~D), map sequence #~D~%"
	    char x y (seq *current-map*)))
	  ((gethash tile-type *multi-tile-function-map*)
	   (funcall (gethash tile-type *multi-tile-function-map*) x y))
	  ((member char *tile-generator-data* :key #'car)
	   (if actual-type
	       tile-type
	     (gethash (list x y) *generator-map*)))
	  (t tile-type))))

(defun set-tile-from-map (x y new)
  (unless *tile-map*
    (setf *tile-map* (make-hash-table))
        (loop for (char tile) in *symbol-map-data*
	  do (setf (gethash tile *tile-map*) char))

	(setf *tiles-with-direction*
	      (remove-duplicates *tiles-with-direction*
				 :key 'car))
	(loop for tile-map in *tiles-with-direction*
	      do (loop with base = (car tile-map)
		       for other in (cdr tile-map)
		       do (setf (gethash other *tile-map*)
				(gethash base *tile-map*)))))
  
  (let ((char (gethash new *tile-map*)))
    (when char
      (setf (aref (mapdata *current-map*) x y) char))))

(defsetf tile-from-map set-tile-from-map)

(defun find-starting-position (map)
  (let ((width (width map))
	(height (height map))
	(ar (mapdata map)))
    (loop for y from 0 below height
	  do (loop for x from 0 below width
		if (char= (aref ar x y) #\S)
		do (return-from find-starting-position (list x y))))))
