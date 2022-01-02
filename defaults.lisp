;;;; defaults.lisp
;;;; Default options/parameters for various items.
;;;; Part of Observatory, by Jeremiah Stoddard

(in-package :observatory)

;; Homepage
(defparameter *homepage* "gemini://gemini.circumlunar.space/")

;; Styles
(defparameter *h1-style* (make-text-style :sans-serif :bold :huge))
(defparameter *h2-style* (make-text-style :sans-serif :bold :very-large))
(defparameter *h3-style* (make-text-style :sans-serif :bold :large))
(defparameter *mono-style* (make-text-style :fix :roman :normal))
(defparameter *link-color* +blue+)

;; Where to save/load bookmarks
(defparameter *bookmarks-file* (merge-pathnames (user-homedir-pathname)
						(make-pathname :name ".observatory-bookmarks")))

