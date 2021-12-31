;;;; document.lisp
;;;; Document structure and functions for parsing text/gemini documents
;;;; Part of Observatory, by Jeremiah Stoddard

(in-package :observatory)

(defparameter +whitespace+ (format nil "~c~c~c~c~c"
				   #\Newline
				   #\Space
				   #\Tab
				   #\Return
				   #\Linefeed))

(defvar *parser-state* :normal)

(defun toggle-parser-state ()
  (if (eql *parser-state* :normal)
      (setf *parser-state* :pre)
      (setf *parser-state* :normal)))

(defclass document ()
  ((resource :initform (error "Resource required.") :initarg :resource :accessor document-resource)
   (response-code :initform 0 :initarg :response-code :accessor document-response-code)
   (meta :initform "" :initarg :meta :accessor document-meta)
   (type :initform :other :initarg :type :accessor document-type)
   (source :initform "" :initarg :source :accessor document-source)
   (parts :initform () :initarg :parts :accessor document-parts)))

(defun make-document (&key (resource (parse-uri "gemini://gemini.circumlunar.space/"))
			(response-code 0) (meta "") (type :other) (source "") (parts nil))
  (make-instance 'document
		 :resource resource
		 :response-code response-code
		 :meta meta
		 :type type
		 :source source
		 :parts parts))

(defclass doc-part ()
  ((text :initarg :text :initform "" :accessor doc-part-text))
  (:documentation "Base class for parts of a document."))

(defgeneric write-doc-part (line)
  (:documentation "Set appropriate style and write line to standard output."))

(defclass text-line (doc-part)
  ()
  (:documentation "A line of plain text."))

(defun make-text-line (text)
  (make-instance 'text-line :text text))

(defclass link-line (doc-part)
  ((uri :initarg :uri :initform nil :accessor link-line-uri)
   (description :initarg :description :initform "" :accessor link-line-description))
  (:documentation "A link."))

(defun ensure-complete-uri (uri res)
  "If uri is relative, add server, etc. from res and return absolute uri."
  (unless (position #\: uri)
    (if (eql (char uri 0) #\/)
	(setf uri (concatenate 'string
			       (resource-protocol res) "://"
			       (resource-server res) uri))
	(let* ((path (resource-get-uri res))
	       (last/ (position #\/ path :from-end t)))
	  (setf uri (concatenate 'string
				 (subseq path 0 last/) "/" uri)))))
  uri)

(defun make-link-line (text res)
  (let* ((trimmed-line (string-trim +whitespace+ (subseq text 3)))
	 (pos (min (or (position #\Space trimmed-line) (length trimmed-line))
		   (or (position #\Tab trimmed-line) (length trimmed-line)))))
    (make-instance 'link-line :text text
			      :uri (parse-uri
				    (ensure-complete-uri (subseq trimmed-line 0 pos) res))
			      :description (if (and pos (< pos (length trimmed-line)))
					       (string-trim +whitespace+
							    (subseq trimmed-line (+ pos 1)))
					       ""))))

(defclass toggle-line (doc-part)
  ()
  (:documentation "Following lines should be mono-line until next toggle-line."))

(defun make-toggle-line (text)
  (toggle-parser-state)
  (make-instance 'toggle-line :text (string-trim +whitespace+ (subseq text 3))))

(defclass mono-line (doc-part)
  ()
  (:documentation "Preformatted text for output in monospace font."))

(defun make-mono-line (text)
  (make-instance 'mono-line :text text))

(defclass heading1-line (doc-part)
  ()
  (:documentation "A heading."))

(defun make-heading1-line (text)
  (make-instance 'heading1-line :text (string-trim +whitespace+ (string-left-trim "#" text))))

(defclass heading2-line (doc-part)
  ()
  (:documentation "A subheading."))

(defun make-heading2-line (text)
  (make-instance 'heading2-line :text (string-trim +whitespace+ (string-left-trim "#" text))))

(defclass heading3-line (doc-part)
  ()
  (:documentation "A sub-subheading."))

(defun make-heading3-line (text)
  (make-instance 'heading3-line :text (string-trim +whitespace+ (string-left-trim "#" text))))

(defclass ul-line (doc-part)
  ()
  (:documentation "An unordered list item."))

(defun make-ul-line (text)
  (make-instance 'ul-line :text text))

(defclass quote-line (doc-part)
  ()
  (:documentation "Quoted text."))

(defun make-quote-line (text)
  (make-instance 'quote-line :text text))

;;; Parsing functions
;;; TODO: support for image files, at least png and jpeg, for inline
;;; display.

(defun parse-gemini-line (line resource)
  (cond
    ((and (>= (length line) 3)
	  (string= "```" (subseq line 0 3))) (make-toggle-line line))
    ((eql *parser-state* :pre) (make-mono-line line))
    ((= (length line) 0) (make-text-line line))
    ((and (>= (length line) 2) (string= "=>" (subseq line 0 2))) (make-link-line line resource))
    ((and (>= (length line) 3) (string= "###" (subseq line 0 3))) (make-heading3-line line))
    ((and (>= (length line) 2) (string= "##" (subseq line 0 2))) (make-heading2-line line))
    ((string= "#" (subseq line 0 1)) (make-heading1-line line))
    ((string= "*" (subseq line 0 1)) (make-ul-line line))
    ((string= ">" (subseq line 0 1)) (make-quote-line line))
    (t (make-text-line line))))

(defun parse-line (line document resource)
  "Parse a line received from Gemini server."
  (setf (document-source document) (concatenate 'string (document-source document) line))
  (case (document-type document)
    (:text (setf (document-parts document)
		 (append (document-parts document) (list (make-text-line line)))))
    (:gemini (setf (document-parts document)
		   (append (document-parts document)
			   (list (parse-gemini-line line resource))))))
  nil)

;;; Helper functions for parse-response-header

(defun make-unrecognized-response-document (res)
  (make-document :resource res
		 :response-code 0
		 :type :error
		 :parts (list (make-heading1-line "Error")
			      (make-text-line "Unrecognized response from server."))))

(defun make-unsupported-document (line res)
  (make-document :resource res
		 :response-code (parse-integer line :end 2 :junk-allowed t)
		 :type :error
		 :parts (list (make-heading1-line "Error")
			      (make-text-line
			       (format nil "Unsupported file type: ~a."
				       (subseq line 3))))))

(defun make-error-document (line res)
  (make-document :resource res
		 :response-code (parse-integer line :end 2 :junk-allowed t)
		 :type :error
		 :meta (subseq line 3)
		 :parts (list (make-heading1-line "Error")
			      (make-text-line
			       (format nil "Server returned error code ~a: ~a."
				       (subseq line 0 2)
				       (subseq line 3))))))

(defun make-observatory-error-document (line res)
  (make-document :resource res
		 :response-code 0
		 :type :error
		 :parts (list (make-heading1-line "Error")
			      (make-text-line line))))

(defun make-bad-protocol-document (line res)
  (make-document :resource res
		 :response-code 0
		 :type :error
		 :parts (list (make-heading1-line "Error")
			      (make-text-line
			       (format nil "Protocol not supported: ~a." line)))))

(defun make-redirect-document (line res)
  (make-document :resource res
		 :response-code (parse-integer line :end 2 :junk-allowed t)
		 :type :redirect
		 :meta (string-trim +whitespace+ (subseq line 3))
		 :parts (list (make-heading1-line "Redirect")
			      (make-text-line
			       (format nil "The server returned status code ~a: ~a."
				       (subseq line 0 2)
				       (string-trim +whitespace+ (subseq line 3)))))))

(defun make-input-document (line res)
  (make-document :resource res
		 :response-code (parse-integer line :end 2 :junk-allowed t)
		 :type :input
		 :meta (string-trim +whitespace+ (subseq line 3))
		 :parts (list (make-heading1-line "Input Requested")
			      (make-text-line
			       (string-trim +whitespace+ (subseq line 3))))))

(defun make-gemini-document (line res)
  (make-document :resource res
		 :response-code (parse-integer line :end 2 :junk-allowed t)
		 :type :gemini))

(defun make-text-document (line res)
  (make-document :resource res
		 :response-code (parse-integer line :end 2 :junk-allowed t)
		 :type :text))

(defun is-utf-8? (header-line)
  (if (search "charset" header-line)
      (if (search "utf-8" header-line)
	  t
	  nil)
      t)) ; if no charset indicated, default is utf8

(defun parse-response-header (line res)
  "Return document class based on provided header."
  (setf *parser-state* :normal)
  (let ((status-digit (parse-integer (subseq line 0 1) :junk-allowed t)))
    (cond
      ((not status-digit) (make-unrecognized-response-document res))
      ((= status-digit 1) (make-input-document line res))
      ((= status-digit 3) (make-redirect-document line res))
      ((/= status-digit 2) (make-error-document line res))
      ((< (length line) 4) (make-gemini-document line res))
      ((and (search "text/gemini" line) (is-utf-8? line))
       (make-gemini-document line res))
      ((and (search "text/" line) (is-utf-8? line))
       (make-text-document line res))
      (t (make-unsupported-document line res)))))
