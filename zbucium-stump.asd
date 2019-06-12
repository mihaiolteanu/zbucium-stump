(asdf:defsystem #:zbucium-stump
  :description "Stumpwm module for lastfm zbucium player"
  :author "Mihai Olteanu"
  :license  "GPLv3"
  :version "0.1"
  :depends-on (:stumpwm :lastfm :zbucium)
  :serial t
  :components ((:file "package")
               (:file "zbucium-stump")))
