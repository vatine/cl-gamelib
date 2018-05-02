(in-package #:net.hexapodia.games-3d)

(defconstant +screen-side+ 512)
(defvar *pantzer-package* (find-package :pantzer))

(defvar *camera* nil)
(defvar *moving-objects* nil)
(defvar *all-shapes* nil)

(defvar *score* 0)
(defparameter *score-table* nil)

;;; Needed here, so we can handle multi-coloured shapes later
(defvar *white* nil)
(defvar *black* nil)


(defconstant +check-interval+ 30)
(defconstant +start-lives+ 3)
(defvar *lives* -1)


(defun decrease-if-positive (base n)
  (if (> base 0)
      (- base n)
    base))

(defun decrease-if-not-negative (base n)
  (if (>= base 0)
      (- base n)
    base))

(define-modify-macro decf-if-positive (&optional (delta 1))
  decrease-if-positive)
(define-modify-macro decf-if-not-negative (&optional (delta 1))
  decrease-if-not-negative)

(defmacro make-the (fun type)
  (let ((macro-name (intern (format nil "THE-~A" (symbol-name fun))
			    (symbol-package fun))))
    `(defmacro ,macro-name (obj)
       (list 'the ',type (list ',fun obj)))))

(make-the x double-float)
(make-the y double-float)
(make-the z double-float)
(make-the angle double-float)
(make-the focal double-float)

;; Utility functions
(defun random-pick (seq)
  (let ((n (random (length seq))))
    (elt seq n)))


(defun ran (b r &optional pos-only)
  (* (if pos-only
	 1
       (aref #(-1 1) (random 2)))
     (+ b (random (float r 0.0d0)))))

(declaim (ftype (function (double-float) double-float) square))
(defun square (n)
  (declare (double-float n))  
  (* n n))

(defun my-ignore (&rest ignore)
  (declare (ignore ignore))
  (values))

(defun in-view (cam-coord)
  (and (<= 0.0d0 (the-y cam-coord) 200.0d0)
       (<= (sqrt (+ (square (the-x cam-coord)) (square (the-z cam-coord))))
	   (the-y cam-coord))))