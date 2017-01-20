;;;; greed.asd

(asdf:defsystem #:greed
  :description "Playing the greed dice game."
  :author "Martin Buchmann <Martin.Buchmann@gmail.com>"
  :license "BSD"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
               (:file "debug")
               (:file "greed")
               (:file "tests")))

