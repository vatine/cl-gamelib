(asdf:defsystem "gamelib"
   :version "0.4"
   :author "Ingvar Mattsson <ingvar@hexapodia.net>"
   :depends-on ("games-ai" "games-2d" "games-3d"
		#+linux "games-stick"))
