;;;; Observatory - A Gemini Client in Common Lisp
;;;; Written by Jeremiah Stoddard

(in-package :observatory)

(defparameter *h1-style* (make-text-style :sans-serif :bold :huge))
(defparameter *h2-style* (make-text-style :sans-serif :bold :very-large))
(defparameter *h3-style* (make-text-style :sans-serif :bold :large))
(defparameter *mono-style* (make-text-style :fix :roman :normal))

(define-application-frame observatory-app ()
  ((active-uri :initform "gemini://gemini.circumlunar.space/" :accessor active-uri))
  (:pretty-name "Observatory")
  (:menu-bar observatory-menu-bar)
  (:panes
   (app :application
	:display-time nil
	:width 800
	:height 400
	:end-of-line-action :wrap*)
   (uri-input :text-field
	      :value "gemini://gemini.circumlunar.space/"
	      :activate-callback 'uri-input-callback
	      :background +white+))
  (:layouts (default (vertically ()
		       (labelling (:label "URI:") uri-input)
		       app))))

(make-command-table 'observatory-menu-bar
		    :errorp nil
		    :menu '(("File" :menu file-menu)))

(make-command-table 'file-menu
		    :errorp nil
		    :menu '(("Home" :command com-homepage)
			    ;("Save" :command com-save)
			    ("Quit" :command com-quit)))

;;; TODO: Implement this
(define-observatory-app-command (com-save :name t)
    ()
  (let ((filename (subseq (active-uri *application-frame*)
			  (+ (position #\/ (active-uri *application-frame*) :from-end t) 1))))
    (when (= (length filename) 0) (setf filename "untitled.gmi"))
    (sf:select-file :title "Save" :prompt "Save As:" :dialog-type :save :default-fn filename)))

(define-observatory-app-command (com-quit :name t)
    ()
  (frame-exit *application-frame*))

(defmethod write-doc-part ((line doc-part))
  (format t "~a~%" (doc-part-text line)))

(defmethod write-doc-part ((line link-line))
  (let ((res (link-line-uri line)))
    (if res
	(if (string= (resource-protocol res) "gemini")
	    (with-output-as-presentation (t line 'link-line)
	      (format t "~a: " (resource-get-uri res)))
	    (format t "~a: " (resource-get-uri res)))))
  (format t "~a~%" (link-line-description line)))

(defmethod write-doc-part ((line mono-line))
  (with-text-style (t *mono-style*)
    (format t "~a~%" (doc-part-text line))))

(defmethod write-doc-part ((line heading1-line))
  (with-text-style (t *h1-style*)
    (format t "~a~%" (doc-part-text line))))

(defmethod write-doc-part ((line heading2-line))
  (with-text-style (t *h2-style*)
    (format t "~a~%" (doc-part-text line))))

(defmethod write-doc-part ((line heading3-line))
  (with-text-style (t *h3-style*)
    (format t "~a~%" (doc-part-text line))))

(defmethod write-doc-part ((line ul-line))
  #+sbcl (format t "~c~a~%" #\U+2022 (subseq (doc-part-text line) 1))
  #-sbcl (format t "~a~%" (doc-part-text line)))

(defun write-gemini-page (doc)
  (dolist (line (document-parts doc))
    (write-doc-part line)))

(defun load-page (uri)
  (let ((res (parse-uri uri))
	(app-pane (get-frame-pane *application-frame* 'app)))
    (when res
      (setf (active-uri *application-frame*) (resource-get-uri res))
      (window-clear app-pane)
      (let ((doc (get-gemini-page res)))
	(write-gemini-page doc))
      (setf (gadget-value (find-pane-named *application-frame* 'uri-input))
	    (resource-get-uri res))
      (scroll-extent app-pane 0 0))))

(define-observatory-app-command (com-homepage :name t)
    ()
  (load-page "gemini://gemini.circumlunar.space/"))

(define-observatory-app-command (com-follow-link :name t)
    ((link 'link-line))
  (load-page (resource-get-uri (link-line-uri link))))

(define-presentation-to-command-translator follow-link
  (link-line com-follow-link observatory-app
	     :gesture :select
	     :menu t)
  (object) (list object))

(defun uri-input-callback (gadget)
  "Load and display user-input page."
  (load-page (gadget-value gadget)))

(defun observatory-main ()
  "Display and run Gemini Client."
  (run-frame-top-level (make-application-frame 'observatory-app)))
