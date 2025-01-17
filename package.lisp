(defpackage #:coalton-docgen
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria-2)
   (#:tc #:coalton-impl/typechecker)
   (#:entry #:coalton-impl/entry)
   (#:algo #:coalton-impl/algorithm)
   (#:env #:coalton-impl/typechecker/environment)
   (#:source #:coalton-impl/source))
  (:export
   #:*format*
   #:render-docs))
