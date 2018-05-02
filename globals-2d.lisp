(in-package :net.hexapodia.games-2d)

(defvar *file-path* (merge-pathnames
		     (make-pathname :directory (list :relative "graphics"))
		     (asdf:component-pathname (asdf:find-system "games-2d"))))
(defvar *game-depth* nil)

(defun games-ignore (&rest ignore)
  (declare (ignore ignore))
  nil)
