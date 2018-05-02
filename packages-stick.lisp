(defpackage #:net.hexapodia.games-stick
  (:use :common-lisp)
  
  (:export #:update-stick #:make-stick #:simple-transfer #:square-transfer
	   #:set-transfer #:axes #:buttons
	   #+sb-thread #:continous-poll
	   #+sb-thread #:terminate-poll))

