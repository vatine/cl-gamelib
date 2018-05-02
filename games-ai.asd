(asdf:defsystem "games-ai"
   :author "Ingvar Mattsson <ingvar@hexapodia.net>"
   :version "0.3"
   :components ((:file "packages-ai")
		(:file "ai" :depends-on ("packages-ai"))))
