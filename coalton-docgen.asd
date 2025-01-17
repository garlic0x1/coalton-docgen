(asdf:defsystem #:coalton-docgen
  :author "garlic0x1"
  :license "MIT"
  :depends-on (#:alexandria #:coalton #:markcl #:hiccl #:slynk)
  :components ((:file "package")
               (:file "patch")
               (:file "docgen")))
