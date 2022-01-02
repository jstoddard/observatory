;;;; Observatory - A Gemini Client in Common Lisp
;;;; Written by Jeremiah Stoddard

(in-package :observatory)

;; Recently visited documents
(defvar *back-button-history* nil)
(defparameter *max-history-pages* 10)

(defun push-to-history (doc)
  "Add doc to *back-button-history*, removing old entries if needed. Returns doc."
  (push doc *back-button-history*)
  (when (> (length *back-button-history*) *max-history-pages*)
    (setf (cdr (nthcdr (- *max-history-pages* 1) *back-button-history*)) nil))
  doc)

(defun pop-from-history ()
  "Pop first entry from *back-button-history*, or return nil if none."
  (when *back-button-history*
    (pop *back-button-history*)))

;;; Utility function for URL encoding
;;; Based on a function from rosettacode.org
(defun uri-encode (str)
  "URI encode string str."
  (apply #'concatenate 'string
	 (map 'list #'(lambda (c)
			(if (not (alphanumericp c))
			    (format nil "%~2,'0x" (char-code c))
			    (string c)))
	      str)))

;;; Application GUI

(define-application-frame observatory-app ()
  ((current-doc :initform nil :accessor current-doc))
  (:pretty-name "Observatory")
  (:menu-bar observatory-menu-bar)
  (:panes
   (app :application
	:display-time nil
	:width 800
	:height 400
	:end-of-line-action :wrap*)
   (uri-input :text-field
	      :value *homepage*
	      :activate-callback 'uri-input-callback
	      :background +white+)
   (back-button :push-button
		:label (format nil "~a" (code-char 8592)) ; #\LEFTWARDS_ARROW
		:max-width 80
		:activate-callback 'back-button-callback))
  (:layouts (default (vertically ()
		       (horizontally ()
			 (spacing (:thickness 10) back-button)
			 (labelling (:label "URI:") uri-input))
		       app))))

(make-command-table 'observatory-menu-bar
		    :errorp nil
		    :menu '(("File" :menu file-menu)
			    ("Bookmarks" :menu bookmarks-menu)))

(make-command-table 'file-menu
		    :errorp nil
		    :menu '(("Home" :command com-homepage)
			    ;("Save" :command com-save)
			    ("Quit" :command com-quit)))

(make-command-table 'bookmarks-menu
		    :errorp nil
		    :menu '(("Add Bookmark" :command com-add-bookmark)
			    ("Manage Bookmarks" :command com-manage-bookmarks)))

;;; Make uri-input field active when application frame is run

(defmethod run-frame-top-level :before ((frame observatory-app) &key &allow-other-keys)
  (stream-set-input-focus (find-pane-named frame 'uri-input)))

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

;;; Document display functions

(defmethod write-doc-part ((line doc-part))
  (format t "~a~%" (doc-part-text line)))

(defmethod write-doc-part ((line link-line))
  (let ((res (link-line-uri line)))
    (if res
	(if (string= (resource-protocol res) "gemini")
	    (with-drawing-options (t :ink *link-color*)
	      (with-output-as-presentation (t line 'link-line)
		(format t "~a" (resource-get-uri res))))
	    (format t "~a" (resource-get-uri res)))))
  (format t " -- ~a~%" (link-line-description line)))

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

(defmethod write-doc-part ((line ul-line)) ; (code-char 8226) = #\Bullet
  (format t "~c~a~%" (code-char 8226) (subseq (doc-part-text line) 1)))

(defun write-gemini-page (doc)
  (let ((app-pane (get-frame-pane *application-frame* 'app)))
    (window-clear app-pane)
    (dolist (line (document-parts doc))
      (write-doc-part line))
      (setf (gadget-value (find-pane-named *application-frame* 'uri-input))
	    (resource-get-uri (document-resource doc)))
    (scroll-extent app-pane 0 0)))

(defun show-prompt (doc)
;;  (format t "Input: ")
  (let (response-field)
    (surrounding-output-with-border (t)
      (with-output-as-gadget (t)
	(setf response-field
	      (make-pane
	       'text-field
	       :min-width 100
	       :width 700
	       :activate-callback
	       #'(lambda (gadget)
		   (load-page (concatenate 'string
					   (resource-get-uri (document-resource doc))
					   "?"
					   (uri-encode (gadget-value gadget)))))))))
    (stream-set-input-focus response-field)))

(defun load-page (uri)
  (let ((res (parse-uri uri)))
    (when res
      (let ((doc (get-gemini-page res)))
	(when (eql (document-type doc) :redirect)
	  ;; Allow one redirect; a second will display a message to the user.
	  (setf doc (get-gemini-page (parse-uri (document-meta doc)))))
	(write-gemini-page doc)
	(when (eql (document-type doc) :input)
	  (show-prompt doc))
	(when (current-doc *application-frame*)
	  (push-to-history (current-doc *application-frame*)))
	(setf (current-doc *application-frame*) doc)))))

(defun go-back ()
  (let ((doc (pop-from-history)))
    (when doc
      (write-gemini-page doc)
      (setf (current-doc *application-frame*) doc))))

(define-observatory-app-command (com-homepage :name t)
    ()
  (load-page *homepage*))

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

(defun back-button-callback (gadget)
  (declare (ignore gadget))
  (go-back))

;;; Bookmark commands

(define-observatory-app-command (com-manage-bookmarks :name t)
    ()
  (push-to-history (current-doc *application-frame*))
  (bookmark-ui (get-frame-pane *application-frame* 'app)))

(define-observatory-app-command (com-add-bookmark :name t)
    ()
  (restart-case
      (when (current-doc *application-frame*)
	(let (name
	      (uri (resource-get-uri (document-resource (current-doc *application-frame*)))))
	  (accepting-values (t :own-window t)
	    (setf name (accept 'string :prompt "Bookmark Name:")))
	  (if name
	      (make-bookmark uri name)
	      (make-bookmark uri uri))
	  (save-bookmarks *bookmarks-file*)))
    (abort () nil)))

(define-observatory-app-command (com-load-bookmark :name t)
    ((bmh 'bookmark-holder))
  (load-page (getf (bookmark-holder-bookmark bmh) :uri)))

(define-presentation-to-command-translator load-bookmarked-page
  (bookmark-holder com-load-bookmark observatory-app
	     :gesture :select
	     :menu t)
    (object) (list object))

(define-observatory-app-command (com-delete-bookmark :name t)
    ((bmh 'bookmark-holder))
  (delete-bookmark (bookmark-holder-bookmark bmh))
  (save-bookmarks *bookmarks-file*)
  (bookmark-ui (get-frame-pane *application-frame* 'app)))

(define-presentation-to-command-translator delete-bookmark
    (bookmark-holder com-delete-bookmark observatory-app
		     :gesture :delete
		     :menu t)
    (object) (list object))

;; Run Application

(defun observatory-main ()
  "Display and run Gemini Client."
  (load-bookmarks *bookmarks-file*)
  (run-frame-top-level (make-application-frame 'observatory-app)))
