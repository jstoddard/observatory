;;;; bookmark-ui.lisp
;;;; User interface for managing bookmarks
;;;; Part of Observatory, by Jeremiah Stoddard

(in-package :observatory)

(defclass bookmark-holder ()
  ((bookmark :initform (error "bookmark can't be nil!")
	     :initarg :bookmark
	     :accessor bookmark-holder-bookmark)))

(defun make-bookmark-holder (bookmark)
  (make-instance 'bookmark-holder :bookmark bookmark))

(defun bookmark-ui (app-pane)
  "Display bookmarks with option to load page or delete."
  (window-clear app-pane)
  (with-text-style (t *h1-style*)
    (format t "Bookmarks~%"))
  (format t "Click on a bookmark link to follow. Right click for options.~%~%")
  (dolist (bm *bookmarks*)
    (let ((bmh (make-bookmark-holder bm)))
      (with-drawing-options (t :ink *link-color*)
	(with-output-as-presentation (t bmh 'bookmark-holder)
	  (format t "~a" (getf (bookmark-holder-bookmark bmh) :uri))))
      (format t " -- ~a~%" (getf (bookmark-holder-bookmark bmh) :title)))))
