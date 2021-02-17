(in-package :asdf)

(defsystem "cl-fxml"
  :description "cl-fxml: Common Lisp - Finally eXtended Markup Language."
  :version "1.2.1"
  :author "Goheeca <goheeca@gmail.com>"
  :licence "MIT"
  :components ((:file "cl-fxml"))
  :depends-on ("agnostic-lizard" "named-readtables"))
