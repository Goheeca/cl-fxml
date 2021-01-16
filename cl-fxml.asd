(in-package :asdf)
     
(defsystem "cl-fxml"
  :description "cl-fxml: Common Lisp - Finally eXtended Markup Language."
  :version "1.0.0"
  :author "Goheeca <goheeca@gmail.com>"
  :licence "Public Domain"
  :components ((:file "cl-fxml"))
  :depends-on ("agnostic-lizard" "named-readtables"))
