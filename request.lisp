;;;; request.lisp
;;;; Functions for requesting a Gemini page
;;;; Part of Observatory, A Gemini client by Jeremiah Stoddard

(in-package :observatory)

(defstruct resource
  (raw-uri nil)
  (protocol "gemini")
  (server "gemini.circumlunar.space")
  (port nil)
  (page "/"))

(defun resource-get-uri (res)
  "Rebuild uri from components of resource res and return as string."
  (with-output-to-string (uri)
    (if (resource-port res)
	(format uri "~a://~a:~a~a"
		(resource-protocol res) (resource-server res)
		(resource-port res) (resource-page res))
	(format uri "~a://~a~a"
		(resource-protocol res) (resource-server res) (resource-page res)))))

(defun parse-protocol (uri)
  "Return the part of uri prior to the first colon."
  (let ((colon-pos (position #\: uri)))
    (if colon-pos
	(string-downcase (string-trim " " (subseq uri 0 colon-pos)))
	nil)))

(defun parse-server (uri)
  "Return the server part of uri."
  (let ((colon-pos (position #\: uri)))
    (unless colon-pos (setf colon-pos 0))
    (let* ((uri-without-protocol (string-trim "/" (subseq uri (+ colon-pos 1))))
	   (l (length uri-without-protocol))
	   (slash-or-colon-pos (min (or (position #\/ uri-without-protocol) l)
				    (or (position #\: uri-without-protocol) l))))
      (if slash-or-colon-pos
	  (string-downcase (string-trim " " (subseq uri-without-protocol 0 slash-or-colon-pos)))
	  (string-downcase (string-trim " " uri-without-protocol))))))

(defun parse-port (uri)
  "Return the port in uri, or nil if none."
  (let ((colon-pos (position #\: uri)))
    (unless colon-pos (setf colon-pos 0))
    (let* ((uri-without-protocol (string-trim "/" (subseq uri (+ colon-pos 1))))
	   (colon-pos (position #\: uri-without-protocol))
	   (after-colon (+ (or colon-pos -1) 1))
	   (slash-pos (position #\/ uri-without-protocol :start after-colon))
	   (port-str
	     (string-trim " "
			  (subseq uri-without-protocol after-colon (or slash-pos
								       (length uri-without-protocol))))))
      (if (and colon-pos (> (length port-str) 0))
	  (handler-case (parse-integer port-str)
	    (error (c) (values nil c)))
	  nil))))

(defun parse-page (uri)
  "Return the page from the uri, or a simple forward slash if none."
  (let ((colon-pos (position #\: uri)))
    (unless colon-pos (setf colon-pos 0))
    (let* ((uri-without-protocol (string-left-trim "/" (subseq uri (+ colon-pos 1))))
	   (colon-pos (position #\: uri-without-protocol))
	   (after-colon (+ (or colon-pos -1) 1))
	   (slash-pos (position #\/ uri-without-protocol :start after-colon)))
      (if slash-pos
	  (subseq uri-without-protocol slash-pos)
	  "/"))))

;;; This is not very efficient, since the whole uri string is repeatedly
;;; analyzed to pull out the protocol, server, port, and page.  It could
;;; be broken up as the protocol is pulled out, then the server, etc.,
;;; but passing the whole uri to separate little functions which return
;;; a specific component seemed a little more readable to me.  I don't
;;; think this is going to be a performance bottleneck in any significant
;;; manner, so I'm not too worried about it.
(defun parse-uri (uri)
  "Parse uri into a resource struct and return the struct. Return nil if not a uri."
  (let ((protocol (parse-protocol uri))
	(server (parse-server uri))
	(port (parse-port uri))
	(page (parse-page uri))
	(res nil))
    (when (not protocol) (setf protocol "gemini"))
    (when server
      (setf res (make-resource :raw-uri uri :protocol protocol :server server :page page))
      (when port (setf (resource-port res) port)))
    res))

;;; TODO: gemini-page class, which will include page contents when
;;; request is successful, and have additional response and error
;;; information whether request is successful or not.
(defun get-gemini-page (res)
  "Request page at uri identified by resource res and output to stream out."
  (when (not (string= (resource-protocol res) "gemini"))
    (return-from get-gemini-page (make-bad-protocol-document (resource-protocol res) res)))
  (usocket:with-client-socket (socket stream
				      (resource-server res)
				      (or (resource-port res) 1965))
    (let ((gem-stream (cl+ssl:make-ssl-client-stream stream
						     :external-format '(:utf-8 :eol-style :lf)
						     :unwrap-stream-p t
						     :verify nil
						     :hostname (resource-server res)))
	  doc)
      (unwind-protect
	   (progn
	     (format gem-stream "~a~c~c" (resource-get-uri res) #\Return #\Linefeed)
	     (force-output gem-stream)
	     (setf doc (parse-response-header (read-line gem-stream nil) res))
	     (loop :for line = (read-line gem-stream nil)
		   :while line :do
		     (parse-line line doc res)))
	(close gem-stream))
      doc)))
