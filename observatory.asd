;;;; observatory.asd
;;;; System definition for Observatory

(asdf:defsystem :observatory
  :description "A Gemini client"
  :author "Jeremiah Stoddard"
  :license "GPLv3"
  :depends-on (:cl-fad :mcclim :usocket :cl+ssl)
  :components ((:file "package")
	       (:file "request" :depends-on ("package"))
	       (:file "document" :depends-on ("package"))
	       (:file "select-file")
	       (:file "observatory" :depends-on ("package"
						 "request"
						 "document"
						 "select-file"))))
