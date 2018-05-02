(asdf:defsystem "games-3d"
   :author "Ingvar Mattsson <ingvar@hexapodia.net>"
   :version "0.3"
   :depends-on ("clx" "games-ai")
   :components ((:file "packages-3d")
		(:file "globals-3d" :depends-on ("packages-3d"))
		(:file "coords" :depends-on ("packages-3d" "globals-3d"))
		(:file "shapes" :depends-on ("coords" "packages-3d"
					     "globals-3d"))
		(:file "display" :depends-on ("coords" "packages-3d"
					      "shapes" "globals-3d"))))