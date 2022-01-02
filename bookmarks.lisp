;;;; bookmarks.lisp
;;;; Functions for handling bookmarks
;;;; Part of Observatory, by Jeremiah Stoddard

(in-package :observatory)

(defvar *bookmarks* nil)

(defun make-bookmark (uri title)
  (push (list :uri uri :title title) *bookmarks*))

(defun delete-bookmark (bookmark)
  "Delete a bookmark."
  (setf *bookmarks*
	(remove-if #'(lambda (b) (equalp b bookmark)) *bookmarks*)))

(defun load-bookmarks (filename)
  "Load bookmarks from file given in filename."
  (handler-case
      (with-open-file (stream filename :direction :input)
	(setf *bookmarks* (read stream)))
    (error (err) (setf *bookmarks* nil))))

(defun save-bookmarks (filename)
  "Save bookmarks to filename."
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (pprint *bookmarks* stream)))
