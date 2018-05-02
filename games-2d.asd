(asdf:defsystem "games-2d"
   :author "Ingvar Mattsson <ingvar@hexapodia.net>"
   :version "0.3"
   :depends-on ("clx" "games-ai")
   :components ((:file "packages-2d")
		(:file "maps" :depends-on ("packages-2d"))
		(:file "globals-2d" :depends-on ("packages-2d"))
		(:file "xpm-lib" :depends-on ("packages-2d"))
		(:file "generics" :depends-on ("packages-2d"))
		(:file "graphics" :depends-on ("packages-2d" "xpm-lib"
					       "globals-2d" "generics"))
		(:file "gameplay" :depends-on ("packages-2d" "globals-2d"))
		(:file "movables" :depends-on ("packages-2d" "globals-2d"
					       "graphics" "generics"))))

