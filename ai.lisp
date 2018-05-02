(in-package #:net.hexapodia.games-ai)

(defclass ai ()
  ((object :reader object :initarg :object)
   (current :accessor current :initarg :current)))

(defun make-ai (ai-class object)
  "Create an AI instance for an object"
  (let ((new (make-instance ai-class :object object)))
    (setf (ai object) new)))

(defgeneric gen-act (ai state)
  (:documentation "Generic dispatch function for AI state machines,
normally dispatches on <state> and <ai>."))
(defgeneric act (frob)
  (:documentation "Make AI actions suitable for OBJ, this will act differently depending on OBJ
and if it can extract any AI info"))

(defmethod act ((ai ai))
  (gen-act ai (current ai)))

(defmethod act (ignore)
  (declare (ignorable ignore))
  t)




(defun extract-transit (body &optional acc)
  (cond ((atom body)
	 acc)
	((and (listp body)
	      (eql (car body) 'transit))
	 (cons (cadr body) acc))
	(t
	 (let ((headval (extract-transit (car body))))
	   (cond ((listp headval)
		  (extract-transit (cdr body) (append headval acc)))
		 (t (extract-transit (cdr body) (cons headval acc)))))))) 

(defun check-ai (states &optional class)
  (let ((state-table (make-hash-table)))
    (loop for (state . body) in states
	  do (setf (gethash state state-table) 'defined))
    (loop for (state . body) in states
	  for transits = (extract-transit body)
	  do (loop for transit in transits
		   if (gethash transit state-table)
		   do (setf (gethash transit state-table) 'used)
		   else
		   do (warn "~&Transition to unknown state ~S in state ~S, ~%when defining AI for ~S." transit state class)))
    (loop for state being the hash-keys of state-table
	  if (eql (gethash state state-table) 'defined)
	  do (warn "~&State ~S is defined but never used in ~S." state class))))

(defmacro def-ai (class &body states)
  (let ((initial (caar states))
	(methods
	 (loop for (state . body) in states
	       collect
	       `(defmethod gen-act ((ai ,class) (state (eql ',state)))
		  (macrolet ((transit (state)
			       (list 'setf (list 'current 'ai)
				     (list 'quote state))))
		    (flet ((object () (object ai)))
		      ,@body))))))
    (check-ai states class)
    `(progn
       (defclass ,class (ai)
	 ()
	 (:default-initargs :current ',initial))
       ,@methods)))



(defmethod gen-act (ai state)
  "Catch-all GEN-ACT"
  (format t ";;; Seems as if there's a lacking method for ~a as regards state ~a...~%" ai state))
