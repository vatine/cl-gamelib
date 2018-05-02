(in-package #:net.hexapodia.games-2d)

(defvar *xpm-readtable* nil)
(defvar *compiled-xpm-file* "images-xpm.lisp")

(defconstant +xpm-colorchars+
  (if (boundp '+xpm-colorchars+)
      (symbol-value '+xpm-colorchars+)
    '(#\Space #\. #\x #\X #\+ #\@ #\# #\a #\b #\c #\d
      #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p
      #\q #\r #\s #\t #\u #\v #\y #\z #\A #\B #\C #\D
      #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P
      #\Q #\R #\S #\T #\U #\V #\Y #\Z)))

(defun init-readtable ()
  (unless *xpm-readtable*
    (setf *xpm-readtable* (copy-readtable))
    (set-syntax-from-char #\, #\Space *xpm-readtable*)))

(defun read-charspec (f cnt)
  (format nil "~{~c~}"
	  (loop for n from 0 below cnt
		collect (read-char f))))
(defun skip-whitespace (f)
  (loop for c = (read-char f)
	while (member c '(#\space #\tab) :test #'char=)
	finally (unread-char c f)
	))

(defun read-colour (f)
	   (let ((ctype (read-char f)))
	     (ecase ctype
	       (#\# (let ((*read-base* 16))
		      (read f)))
	       )))

(defun read-cspec (str cnt hash)
  (with-input-from-string (cs str)
     (let ((chars (read-charspec cs cnt)))
       (skip-whitespace cs)
       (let ((c (read-char cs)))
	 (if (char= c #\c)
	     (let ((col (progn
			  (skip-whitespace cs)
			  (read-colour cs))))
	       (setf (gethash chars hash) col))
	   (error "Unknown colourspec"))))))

(defun read-xpm (fspec)
  (init-readtable)
  (let ((*readtable* *xpm-readtable*))
    (with-open-file (image (merge-pathnames fspec
					    net.hexapodia.games-2d:*file-path*)
			   :direction :input) 
      (read-line image) ; Skip comment
      (read-line image) ; Skip C code
      (let ((colspec (read image))
	    width
	    height
	    (chartab (make-hash-table :test #'equal))
	    img)
	(with-input-from-string (cspec colspec)
	  (setf width (read cspec))
	  (setf height (read cspec))
	  (let ((colours (read cspec))
		(cpp (read cspec)))
	    (loop for cix from 0 below colours
		  for c = (read image)
		  do (read-cspec c cpp chartab))
	    (setf img (make-array (list width height)))
	    (loop for y from 0 below height
		  for line = (read image)
		  do (progn
		       (with-input-from-string (data line)
		       (loop for x from 0 below width
			     for cs = (read-charspec data cpp)
			     do (setf (aref img x y) (gethash cs chartab))))))
	    img))))))

(defun xpm-mem (n list)
  (loop for i in list
	while (<= i n)
	if (= i n)
	do (return t)))

(defun write-xpm (array fname)
  (let ((fspec (parse-namestring fname)))
    (unless (string= (pathname-type fspec) "xpm")
      (setf fspec (merge-pathnames (make-pathname :type "xpm")
				   fspec)))
    (let ((imgname (substitute #\- #\_ (pathname-name fspec)))
	  (wh (array-dimensions array))
	  (ctab (make-hash-table))
	  (cols nil))
      (let ((w (first wh))
	    (h (second wh)))
	(loop for x from 0 below w
	      do (loop for y from 0 below h
		       do (let ((col (aref array x y))) 
			    (unless (xpm-mem col cols)
			      (setf cols (sort (cons col cols) #'<)))
			    )))
	(loop for char in +xpm-colorchars+
	      for col in cols
	      do (setf (gethash col ctab) char))
	(with-open-file (img fname
			     :direction :output
			     :if-exists :supersede
			     :if-does-not-exist :create)
	   (format img "/* XPM */~%")
	   (format img "static char * ~a_xpm[] = {~%" (substitute #\_ #\- imgname))
	   (format img "\"~d ~d ~d 1\",~%" w h (length cols))
	   (loop for col in cols
		 for char = (gethash col ctab)
		 while char
		 do (format img "\"~c~cc #~12,'0X\",~%" char #\Tab col))
	   (loop for y from 0 below h
		 do (write-char #\" img)
		 do (loop for x from 0 below w
			  do (write-char (gethash (aref array x y) ctab) img))
		 if (< y (1- h))
		 do (format img "\",~%")
		 else do (format img "\"};~%"))
	   )))))

(defun rotate-xpm (in out)
  (let* ((source (read-xpm in))
	 (target (make-array (array-dimensions source)
			     :element-type 'integer
			     :initial-element 0))
	 (w (first (array-dimensions source)))
	 (h (second (array-dimensions source)))
	 (w2 (/ w 2))
	 (h2 (/ h 2)))
    (flet ((transform (x y)
	     (let ((dx (- x w2))
		   (dy (- y h2)))
	       (let ((ang (atan dx dy))
		     (l (sqrt (+ (* dx dx) (* dy dy)))))
		 (let ((nang (+ ang (/ pi 4))))
		   (let ((nx (round (+ w2 (* l (sin nang)))))
			 (ny (round (+ h2 (* l (cos nang))))))
		     (if (or (< nx 0)
			     (< ny 0)
			     (<= w nx)
			     (<= h ny))
			 0
		       (aref source nx ny))))))))
      (loop for x below w
	    do (loop for y below h
		     do (setf (aref target x y) (transform x y))))
      (write-xpm target out))))

(defun compile-xpm (in)
  (let ((source (read-xpm in))
	(existp (probe-file *compiled-xpm-file*)))
    (with-open-file (cimg *compiled-xpm-file*
			  :direction :output
			  :if-exists :append
			  :if-does-not-exist :create)
      (unless existp
	(format cimg "(in-package :compiled-image)~%"))
      (format cimg "(defvar ~a ~a)~%" (pathname-name in) source))))
