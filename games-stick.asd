(asdf:defsystem "games-stick"
   :author "Ingvar Mattsson <ingvar@hexapodia.net>"
   :version "0.1"
   :components ((:file "packages-stick")
		(:file "stick-transfers" :depends-on ("packages-stick"))
		(:file "joystick" :depends-on ("packages-stick" "stick-transfers"))))

