;;;; observatory.asd
;;;; System definition for Observatory

(asdf:defsystem :observatory
  :description "A Gemini client"
  :author "Jeremiah Stoddard"
  :license "GPLv3"
  :depends-on (:cl-fad :mcclim :usocket :cl+ssl)
  :components ((:file "package")
	       (:file "defaults" :depends-on ("package"))
	       (:file "request" :depends-on ("package"))
	       (:file "document" :depends-on ("package"))
	       (:file "bookmarks" :depends-on ("package"))
	       (:file "bookmark-ui" :depends-on ("package"
						 "bookmarks"
						 "defaults"))
	       (:file "select-file")
	       (:file "observatory" :depends-on ("package"
						 "defaults"
						 "request"
						 "document"
						 "select-file"
						 "bookmarks"
						 "bookmark-ui"))))
