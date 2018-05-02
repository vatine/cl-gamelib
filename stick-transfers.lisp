(in-package #:net.hexapodia.games-stick)

;;; Transfer functions for joysticks, map from linux "stick space"
;;; (16-bit signed integer) to gamelib "stick space" (single-float,
;;; -1.0s0 to 1.0s0)

(defun simple-transfer (in)
  (declare (type (signed-byte 16) in))
  (/ in 32768.0s0))

(defun square-transfer (in)
  (declare (type (signed-byte 16) in))
  (let ((val (simple-transfer in)))
    (* (signum val) val val)))
