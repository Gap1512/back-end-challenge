(asdf:defsystem #:back-end
  :description "Implementation of a RESTful API using Common Lisp"
  :author "Gustavo Alves Pacheco <gap1512@gmail.com>"
  :serial t
  :depends-on (#:postmodern #:snooze #:clack #:jonathan #:simple-date/postgres-glue)
  :components ((:file "package")
	       (:file "server/classes")
	       (:file "server/back-end")))
