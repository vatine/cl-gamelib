(in-package #:net.hexapodia.games-2d)

(defvar *pixmaps* nil "Mapping from image keyword to pixmap")
(defvar *image-to-file* (make-hash-table))
(defvar *gc-cache* (make-hash-table))
(defvar *visual* nil)
(defvar *colormap* nil)
(defvar *image-types* (make-hash-table :test 'equal) "Mapping from image extension to image-reader")

(eval-when (:load-toplevel :execute)
  (setf (gethash "xpm" *image-types*) #'read-xpm))

(defmacro map-file (sym file)
  "Map a tile to a specific image file. SYM is a keyword designator"
  (let ((sym (intern (string sym) "KEYWORD")))
    `(eval-when (:load-toplevel :execute)
       (setf (gethash ,sym *image-to-file*) ,file))))


(defun ensure-colorstuff ()
  (when *game-window*
    (unless *visual*
      (setf *visual* (xlib:screen-root-visual *screen*))
      (setf *colormap* (xlib:screen-default-colormap *screen*))
      (setf *game-depth* (xlib:drawable-depth *game-window*)))))

(defun color-dist (color r g b)
  (let ((dr (* 0.52 (- r (xlib:color-red color))))
	(dg (- g (xlib:color-green color)))
	(db (* 0.21 (- b (xlib:color-blue color)))))
    (+ (* dr dr) (* dg dg) (* db db))))

(defun closest-match (r g b)
  (let ((red   (/ r 65535.0d0))
	(green (/ g 65535.0d0))
	(blue  (/ b 65535.0d0)))
    (let ((best-match nil)
	  (shortest 4.0))
      (dolist (pixel (xlib:query-colors *colormap*
					(loop for x from 0 to 255 collect x)))
	(when (< (color-dist pixel red green blue) shortest)
	  (setf best-match pixel)
	  (setf shortest (color-dist pixel r g b))))
      (xlib:alloc-color *colormap* best-match))))

(defun my-alloc (r g b)
  (case (xlib:visual-info-class (xlib:visual-info *display* *visual*))
    (:true-color (xlib:alloc-color *colormap*
				  (xlib:make-color :red (/ r 65535.0d0)
						   :green (/ g 65535.0d0)
						   :blue (/ b 65535.0d0))))
    (:pseudo-color (closest-match r g b))))

(defun map-color (rgb)
  (let ((cacheval (gethash rgb *gc-cache*)))
    (if cacheval
	cacheval
      (let ((red   (ldb (byte 16 32) rgb))
	    (green (ldb (byte 16 16) rgb))
	    (blue  (ldb (byte 16  0) rgb)))
	(let ((gc (xlib:create-gcontext
		   :drawable *game-window*
		   :background 0
		   :foreground (my-alloc red green blue))))
	  (setf (gethash rgb *gc-cache*) gc))))))

(defun read-image-file (file)
  (typecase file
    ((or string pathname)
     (let ((type (pathname-type file)))
       (let ((fun (gethash type *image-types*)))
	 (cond (fun (funcall fun file))
	       (t (error "Unknown image type, ~a" type))))))
    (symbol (let ((package (symbol-package file))
		  (image-package (find-package :compiled-image)))
	      (cond ((eql package image-package)
		     (symbol-value file))
		    (t (symbol-value (find-symbol (symbol-name file)
						  image-package))))))))

(defun read-image (file)
  (let ((array (read-image-file file)))
    (let* ((wh (array-dimensions array))
	   (w (car wh))
	   (h (cadr wh))
	   (px (create-pixmap
		:width w
		:height h
		:depth (xlib:drawable-depth *game-window*)
		:drawable *game-window*)))
      (loop for x from 0 below w
	    do (loop for y from 0 below h
		     do (xlib:draw-point
			 px
			 (map-color (aref array x y))
			 x y)))
      px)))

(defun compile-image (fname)
  (let ((type (pathname-type fname)))
    (cond ((string= type "xpm") (compile-xpm fname)))))

(defun compile-images (image-spec)
  (let ((files (directory image-spec)))
    (loop for file in files
	  do (compile-image file))))

(defun load-images ()
  (ensure-gamewindow)
  (ensure-colorstuff)
  (unless *pixmaps*
    (setf *pixmaps* (make-hash-table))
    (maphash (lambda (img file)
	       (setf (gethash img *pixmaps*) (read-image file)))
	     *image-to-file*)))


(defun draw-tile (x y)
  (let ((tile (tile-from-map x y t)))
    (xlib:copy-area (gethash tile *pixmaps*) *gcontext-set* 0 0 64 64
		    *background* (* x 64) (* y 64))))

(defun draw-map ()
  (ensure-background)
  (load-images)
  (loop for x from 0 below (width *current-map*)
	do (loop for y from 0 below (height *current-map*)
		 do (draw-tile x y))))
