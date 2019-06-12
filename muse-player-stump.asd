(asdf:defsystem #:muse-player-stump
  :description "Stumpwm module for lastfm muse player"
  :author "Mihai Olteanu"
  :license  "GPLv3"
  :version "0.1"
  :depends-on (:stumpwm :muse-player)
  :serial t
  :components ((:file "package")
               (:file "muse-player-stump")))
