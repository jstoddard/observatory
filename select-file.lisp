;;;; select-file.lisp
;;;;
;;;; A file selection dialog adapted by Jeremiah Stoddard for use with Observatory
;;;; Although Observatory as a whole is licensed under the terms of version 3 of the
;;;; GNU General Public License, the software in this file, which is a slightly
;;;; modified version of select-file originally written by the authors mentioned
;;;; in the copyright notice below, is distributed under the following license:
;;;;
;;;; Copyright (c) 2017-2020 John Carroll, Ann Copestake, Robert Malouf, Stephan Oepen, Angelo Rossi
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

(defpackage #:select-file
  (:nicknames #:sf)
  (:use #:cl)
  (:export #:select-file
           #:file-selector
           #:list-directory
           #:list-places
           #:list-devices))

(in-package #:select-file)

;;; Parameters in use by the application.

(defparameter +outline-gray+ #+:mcclim climi::*3d-dark-color* #-:mcclim (clim:make-gray-color 0.59))

(defparameter +text-gray+ (clim:make-gray-color 0.66))

(defparameter *cancel-button-string* "  Cancel  ")

;;; Files, folders and devices icon patterns

(defparameter +folder-icon+ (clim:make-pattern #2A((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                                                   (0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
                                                   (0 1 2 2 2 2 1 0 0 0 0 0 0 0 0 0)
                                                   (1 2 2 2 2 2 2 1 1 1 1 1 1 1 1 0)
                                                   (1 3 3 3 3 3 3 3 3 3 3 3 3 3 1 4)
                                                   (1 3 3 3 3 3 3 3 3 3 3 3 3 3 1 4)
                                                   (1 5 5 5 5 5 5 5 5 5 5 5 5 5 1 4)
                                                   (1 5 5 5 5 5 5 5 5 5 5 5 5 5 1 4)
                                                   (6 5 5 5 5 5 5 5 5 5 5 5 5 5 6 4)
                                                   (6 7 7 7 7 7 7 7 7 7 7 7 7 7 6 4)
                                                   (6 7 7 7 7 7 7 7 7 7 7 7 7 7 6 4)
                                                   (8 7 7 7 7 7 7 7 7 7 7 7 7 7 8 4)
                                                   (8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 4)
                                                   (0 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4)
                                                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                                                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
                              (list #+:mcclim clim:+transparent-ink+
                                    #-:mcclim clim:+white+ ; Allegro CLIM pattern limitation
                                    (clim:make-rgb-color 160/255 153/255 123/255)
                                    (clim:make-rgb-color 239/255 229/255 186/255)
                                    (clim:make-rgb-color 239/255 227/255 174/255)
                                    (clim:make-rgb-color 173/255 173/255 173/255)
                                    (clim:make-rgb-color 237/255 224/255 158/255)
                                    (clim:make-rgb-color 145/255 138/255 103/255)
                                    (clim:make-rgb-color 234/255 223/255 147/255)
                                    (clim:make-rgb-color 119/255 113/255 85/255))))

(defparameter +document-icon+ (clim:make-pattern #2A((0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0)
                                                     (0 0 1 2 2 2 2 2 2 2 1 1 0 0 0 0)
                                                     (0 0 1 2 2 2 2 2 2 2 1 3 1 0 0 0)
                                                     (0 0 1 2 2 2 2 2 2 2 1 3 3 1 0 0)
                                                     (0 0 1 2 2 2 2 2 2 2 1 1 1 1 4 0)
                                                     (0 0 1 2 2 2 2 2 2 2 2 4 4 5 4 0)
                                                     (0 0 1 2 2 2 2 2 2 2 2 2 2 1 4 0)
                                                     (0 0 1 2 2 2 2 2 2 2 2 2 2 1 4 0)
                                                     (0 0 1 2 2 2 2 2 2 2 2 2 2 1 4 0)
                                                     (0 0 1 2 2 2 2 2 2 2 2 2 2 1 4 0)
                                                     (0 0 1 2 2 2 2 2 2 2 2 2 2 1 4 0)
                                                     (0 0 1 2 2 2 2 2 2 2 2 2 2 1 4 0)
                                                     (0 0 1 2 2 2 2 2 2 2 2 2 2 1 4 0)
                                                     (0 0 1 2 2 2 2 2 2 2 2 2 2 1 4 0)
                                                     (0 0 1 1 1 1 1 1 1 1 1 1 1 1 4 0)
                                                     (0 0 0 4 4 4 4 4 4 4 4 4 4 4 4 0))
                                (list #+:mcclim clim:+transparent-ink+
                                      #-:mcclim clim:+white+ ; Allegro CLIM pattern limitation
                                      (clim:make-rgb-color 112/255 112/255 112/255)
                                      (clim:make-rgb-color 232/255 232/255 232/255)
                                      (clim:make-rgb-color 255/255 255/255 255/255)
                                      (clim:make-rgb-color 137/255 137/255 137/255)
                                      (clim:make-rgb-color 99/255 99/255 99/255))))

(defparameter +up-folder-icon+ (clim:make-pattern #2A((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                                                      (0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
                                                      (0 1 2 2 2 2 1 0 0 0 0 0 0 0 0 0)
                                                      (1 2 2 2 2 2 2 1 1 1 1 1 1 1 1 0)
                                                      (1 3 3 3 4 3 3 3 3 3 3 3 3 3 1 5)
                                                      (1 3 3 4 4 4 3 3 3 3 3 3 3 3 1 5)
                                                      (1 6 4 6 4 6 4 6 6 6 6 6 6 6 1 5)
                                                      (1 6 6 6 4 6 6 6 6 6 6 6 6 6 1 5)
                                                      (7 6 6 6 4 6 6 6 6 6 6 6 6 6 7 5)
                                                      (7 8 8 8 8 4 8 8 8 8 8 8 8 8 7 5)
                                                      (7 8 8 8 8 8 4 4 4 4 4 8 8 8 7 5)
                                                      (9 8 8 8 8 8 8 8 8 8 8 8 8 8 9 5)
                                                      (9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 5)
                                                      (0 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5)
                                                      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                                                      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
                                 (list #+:mcclim clim:+transparent-ink+
                                       #-:mcclim clim:+white+ ; Allegro CLIM pattern limitation
                                       (clim:make-rgb-color 109/255 158/255 176/255)
                                       (clim:make-rgb-color 189/255 232/255 252/255)
                                       (clim:make-rgb-color 176/255 229/255 253/255)
                                       (clim:make-rgb-color 0/255 0/255 0/255)
                                       (clim:make-rgb-color 188/255 188/255 188/255)
                                       (clim:make-rgb-color 154/255 219/255 253/255)
                                       (clim:make-rgb-color 81/255 133/255 152/255)
                                       (clim:make-rgb-color 140/255 211/255 251/255)
                                       (clim:make-rgb-color 58/255 96/255 109/255))))

(defparameter +device-icon+ (clim:make-pattern #2A((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                                                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                                                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                                                   (0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0)
                                                   (0 0 1 4 4 4 4 4 4 4 4 4 1 6 0 0)
                                                   (0 1 3 3 3 3 3 3 3 3 3 3 3 1 6 0)
                                                   (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 6)
                                                   (1 2 2 2 2 2 2 2 2 2 2 2 2 2 1 6)
                                                   (1 2 2 2 2 2 2 2 2 2 2 5 5 2 1 6)
                                                   (1 2 2 2 2 2 2 2 2 2 2 5 5 2 1 6)
                                                   (1 2 2 2 2 2 2 2 2 2 2 2 2 2 1 6)
                                                   (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 6)
                                                   (0 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6)
                                                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                                                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                                                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
                              (list #+:mcclim clim:+transparent-ink+
                                    #-:mcclim clim:+white+ ; Allegro CLIM pattern limitation
                                    (clim:make-rgb-color 112/255 112/255 112/255)
                                    (clim:make-rgb-color 190/255 190/255 190/255)
                                    (clim:make-rgb-color 220/255 220/255 220/255)
                                    (clim:make-rgb-color 240/255 240/255 240/255)
                                    (clim:make-rgb-color 48/255 240/255 48/255)
                                    (clim:make-rgb-color 173/255 173/255 173/255))))

;;; Main function.
;;; See the README file for documentation.

(defun select-file (&rest args &key
                                 (frame-name 'file-selector)
                                 (title nil)
                                 (prompt "Name:") ; "Save As:" is appropriate for dialog-type :save
                                 (directory (user-homedir-pathname))
				 (default-fn "untitled")
                                 (dialog-type :save)
                                 (show-hidden-p nil)
                                 (ok-label "OK") ; "Open", "Load" or "Save" may also be appropriate
                                 &allow-other-keys)
  (check-type frame-name symbol)
  (check-type title (or string null))
  (check-type prompt (or string null))
  (check-type directory (or string pathname))
  (check-type dialog-type (member :open :save :directory))
  (check-type ok-label (or string null))
  (when (eql dialog-type :directory)
    (setq default-fn nil))
  (unless title
    (setq title (ecase dialog-type
                  (:open
                   "Select File")
                  (:save
                   "Specify File Name")
                  (:directory
                   "Select Directory"))))
  (unless prompt
    (setq prompt "Name:"))
  (setq directory (ensure-valid-directory directory))
  (unless ok-label
    (setq ok-label "OK"))
  (setq args (loop
                for (k v) on args by #'cddr
                unless (or (eq k :frame-name) (eq k :title))
                nconc (list k v)))
  (let ((frame (apply #'clim:make-application-frame
                      frame-name
                      :pretty-name title
                      :prompt prompt
                      :directory directory
		      :default-fn default-fn
                      :dialog-type dialog-type
                      :show-hidden-p show-hidden-p
                      :ok-label ok-label
                      args)))
    (setf (file-selector-files-dirs frame) (list-directory frame directory show-hidden-p))
    (clim:run-frame-top-level frame)
    (file-selector-result frame)))

(defun ensure-valid-directory (x)
  (if x
      (cl-fad:pathname-as-directory (pathname x))
      (user-homedir-pathname)))

;;; Classes.

(cl:defclass files-dirs-application-pane (clim:application-pane)
  ;; inheriting instead from clim-stream-pane might be more appropriate, but in that case in
  ;; McCLIM we don't get scroll wheel support (we don't for either in Allegro CLIM)
  ())

;;; The file-selector application frame, including gadget definitions and layout

(clim:define-application-frame file-selector ()
  ((prompt :initform nil
           :initarg :prompt
           :reader file-selector-prompt)
   (directory :initform ""
              :initarg :directory
              :reader file-selector-directory)
   (default-fn :initform nil
               :initarg :default-fn
               :reader file-selector-default-fn)
   (dialog-type :initform nil
                :initarg :dialog-type
                :reader file-selector-dialog-type)
   (show-hidden-p :initform nil
                  :initarg :show-hidden-p
                  :accessor file-selector-show-hidden-p)
   (ok-label :initform nil
             :initarg :ok-label
             :reader file-selector-ok-label)
   (files-dirs :initform nil
               :accessor file-selector-files-dirs)
   (last-margin :initform nil
                :accessor file-selector-last-margin)
   (last-ncolumns :initform 0
                  :accessor file-selector-last-ncolumns)
   (result :initform nil
           :accessor file-selector-result))
  (:menu-bar nil)
  (:panes
   ;; In McCLIM, putting the file selection text-field here ensures it gets keyboard focus,
   ;; since frame-standard-output returns the first defined application-pane that's visible.
   ;; In Allegro CL this does not work - nor does specialising frame-standard-output to
   ;; return this pane since a text-field is not a stream; see 'bugs'
   (selection-pane
    (clim:make-pane 'clim:text-field
                    :foreground clim:+black+
                    :background clim:+white+
                    :editable-p (if (member (file-selector-dialog-type clim:*application-frame*)
                                            '(:open :directory))
                                    nil
                                    t)
                    :value (if (file-selector-default-fn clim:*application-frame*)
			       (namestring (fad:merge-pathnames-as-file
					    (file-selector-directory clim:*application-frame*)
					    (file-selector-default-fn clim:*application-frame*)))
			       (namestring (file-selector-directory clim:*application-frame*)))
                    :value-changed-callback #'(lambda (gadget new-value)
                                                (declare (ignore gadget))
                                                (clim:with-application-frame (frame)
                                                  (update-ok-button frame new-value)))
                    :max-width clim:+fill+))
   (places-devices-pane
    (clim:make-pane 'clim:application-pane
                    :foreground clim:+black+
                    :background clim:+white+
                    :text-cursor nil
                    :max-width 150 ; in Allegro CLIM, this is completely
                                   ; overridden by the hbox-pane spec
                    :display-time nil
                    :display-function #'display-places-devices))
   (files-dirs-pane
    (clim:make-pane 'files-dirs-application-pane
                    :foreground clim:+black+
                    :background clim:+white+
                    :text-cursor nil
                    :display-time nil
                    :display-function #'display-files-dirs))
   (prompt-pane
    (clim:make-pane 'clim:label-pane
                    :label (file-selector-prompt clim:*application-frame*)
                    #+:mcclim :max-width #+:mcclim '(:relative 0))) ; prevent the label stretching
   (show-hidden-files-check-box
    (clim:make-pane 'clim:toggle-button
                    :label "Show hidden files"
                    :indicator-type :some-of
                    :value (file-selector-show-hidden-p clim:*application-frame*)
                    :value-changed-callback 'show-hidden-files-callback))
   (ok-button
    (clim:make-pane 'clim:push-button
                    :label (concatenate 'string
                                        "   "
                                        (file-selector-ok-label clim:*application-frame*) "   ")
                    :align-x :center
                    :y-spacing 4
                    #-:mcclim :show-as-default
                    #+:mcclim :show-as-default-p t ; incorrect keyword in McCLIM
                    :activate-callback #'ok-callback))
   (cancel-button
    (clim:make-pane 'clim:push-button
                    :label *cancel-button-string*
                    :align-x :center
                    :y-spacing 4
                    :activate-callback #'close-callback)))
  (:geometry :left 100 :top 100 :width 600 :height 400) ; default placement and size
  (:layouts
   (default
       (clim:spacing (:thickness 15)
         (clim:vertically (:y-spacing 15)
           (clim:horizontally (:x-spacing 10 :equalize-height t)
             (1/4
              (clim:outlining (:thickness 1
                               ;; in Allegro CLIM, outlining is grey by default - and indeed if
                               ;; we were to specify the colour then the scroll bar would also
                               ;; pick it up, which we definitely don't want
                               #+:mcclim :background #+:mcclim +outline-gray+)
                (clim:scrolling (:scroll-bar :vertical :scroll-bars :vertical) ; CLIM spec ambiguous
                  places-devices-pane)))
             (3/4
              (clim:outlining (:thickness 1 #+:mcclim :background #+:mcclim +outline-gray+)
                (clim:scrolling (:scroll-bar :vertical :scroll-bars :vertical)
                  files-dirs-pane))))
           (clim:horizontally () show-hidden-files-check-box :fill)
           (clim:horizontally (:x-spacing 10 :align-y :center :equalize-height nil)
             prompt-pane

             ;; in McCLIM only, wrap whitespace and outline around text-field gadget, otherwise
             ;; it looks too tight and flat
             #-:mcclim selection-pane
             #+:mcclim
             (clim:outlining (:thickness 1 :background +outline-gray+)
               (clim:outlining (:thickness 3 :background clim:+white+) selection-pane)))

           ;; For two equal width push-buttons on the left side of the frame, an approach
           ;; that works in Allegro CLIM is an hbox-pane split 50-50 inside another hbox
           ;; with a right fill. Unfortunately, this doesn't work in McCLIM since the button
           ;; with the narrower label fails to expand to its allotted half. Instead, we use
           ;; a grid-pane (which is not implemented in Allegro CLIM), adding spacing manually
           ;; since the McCLIM grid-pane ignores :x-spacing. However there's still a bug
           ;; (probably in grid-pane allocate-space): if the left button label is wider than
           ;; the right, the right button won't move over to the right or grow to match it.
           (clim:horizontally ()
             #+:mcclim
             (clim:make-pane 'clim:grid-pane
                             :contents (list (list (clim:horizontally () ok-button 5)
                                                   (clim:horizontally () 5 cancel-button))))
             #-:mcclim
             (clim:horizontally (:x-spacing 10) (1/2 ok-button) (1/2 cancel-button))
             :fill))))))

;;; Methods related to file-selector class.

#+:mcclim
(defmethod clim-extensions:find-frame-type ((frame file-selector))
  ;; make file selector have more dialog-like window controls (e.g. no maximize button)
  :dialog)

(defmethod clim:run-frame-top-level :before ((frame file-selector) &key &allow-other-keys)
  ;; !!! The order in which threads run in McCLIM/CLX/X11 may prevent a new frame from opening
  ;; at the top of the stack, requiring the user to click on it to activate it. I can't pin
  ;; down the circumstances in which this happens, but calling sleep here seems to be an
  ;; effective workaround.
  #+:mcclim (sleep 0.05)
  (update-ok-button
   frame (clim:gadget-value (clim:find-pane-named frame 'selection-pane))))

;;; Callback functions related to the action gadgets.

(defun update-ok-button (frame new-value)
  (let ((ok-button
         (clim:find-pane-named frame 'ok-button))
        (require-directory-p
         (eq (file-selector-dialog-type frame) :directory)))
    (when ok-button ; the gadget might not be associated with this frame yet
      (if (eq (and (cl-fad:directory-pathname-p new-value) t) require-directory-p)
          (clim:activate-gadget ok-button)
          (clim:deactivate-gadget ok-button)))))

(defun ok-callback (button)
  (declare (ignore button))
  (clim:with-application-frame (frame)
    (setf (file-selector-result frame)
          (pathname (clim:gadget-value (clim:find-pane-named frame 'selection-pane))))
    (clim:frame-exit frame)))

(defun close-callback (button)
  (declare (ignore button))
  (clim:with-application-frame (frame)
    (clim:frame-exit frame)))

(defun show-hidden-files-callback (checkbox value)
  (declare (ignore checkbox))
  (clim:with-application-frame (frame)
    (with-slots (files-dirs show-hidden-p) frame
      (setf show-hidden-p value)
      (setf files-dirs
            (list-directory frame
                            (pathname-directory-pathname
                              (clim:gadget-value (clim:find-pane-named frame 'selection-pane)))
                            value))
      (display-files-dirs frame (clim:find-pane-named frame 'files-dirs-pane)))))

;;; Detect resizing of the dialog, by an :after method on allocate-space. A more direct
;;; solution might be be a handle-event method on window-configuration-event; however
;;; handle-event is called on the frame's top level sheet and it doesn't seem possible to
;;; specialise that sheet portably.

(define-file-selector-command com-resize-panes ((frame 'file-selector))
  (display-files-dirs
   frame (clim:find-pane-named frame 'files-dirs-pane) t))

(defmethod clim:allocate-space :after ((pane files-dirs-application-pane) width height)
  (declare (ignore width height))
  ;; in McCLIM, a horizontal resize of an application / scroll pane combo updates the
  ;; application pane's text-margin if the scroll pane has a horizontal scroll bar,
  ;; but not otherwise - I think it should always
  #+:mcclim
  (setf (clim:stream-text-margin pane)
        (clim:bounding-rectangle-width (clim:pane-viewport-region pane)))
  (let ((margin (clim:stream-text-margin pane)))
    (clim:with-application-frame (frame)
      (with-slots (last-margin) frame
        (cond
          ((null last-margin) ; a brand new frame, not a resize?
           (setf last-margin margin))
          ((> (abs (- margin last-margin)) 10) ; significant change since last redisplay attempt?
           (setf last-margin margin)
           (clim:execute-frame-command frame `(com-resize-panes ,frame))))))))

;;; Panes class definitions.
;;; Files and directories list pane. This is the pane on the right.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (clim:define-presentation-type file-dir-namestring ()))

(define-file-selector-command com-select-file-dir
    ((data 'file-dir-namestring :gesture :select))
  (select-file-dir data))

(defun select-file-dir (data &optional relist-if-file-p)
  (let* ((p (cadr data))
         (parent clim:*application-frame*)
         (selection-pane (clim:find-pane-named parent 'selection-pane)))
    (setf (clim:gadget-value selection-pane) (namestring p))
    (update-ok-button parent p)
    (when (and relist-if-file-p (not (cl-fad:directory-pathname-p p)))
      (setq p (cl-fad:pathname-as-directory p)))
    (when (cl-fad:directory-pathname-p p)
      (with-slots (files-dirs show-hidden-p) parent
        (setf files-dirs (list-directory parent p show-hidden-p)))
      (display-files-dirs parent (clim:find-pane-named parent 'files-dirs-pane)))))

(defun display-files-dirs (frame stream &optional lazyp)
  (let*
      ((files-dirs (file-selector-files-dirs frame))
       (items
        (nconc
         (if (car files-dirs)
             (list (list* "Parent directory" (car files-dirs) 'parent))
             nil)
         (mapcar
          #'(lambda (p)
              (list* (file-dir-namestring p) p
                     (if (cl-fad:directory-pathname-p p) 'folder 'document)))
          (cdr files-dirs))))
       (max-width
        (+ 12 ; x-spacing - see display-fs-items / draw-namestring for these spacing values
           (max
            (+ 16 4
               (reduce #'max
                       items
                       :key #'(lambda (x)
                                (clim:text-size stream (if (atom x)
                                                           x
                                                           (car x)))))
               2)
            120))) ; min cell width
       (margin
        ;; we don't need all of the last column to be visible so extend right margin a bit
        (+ (clim:stream-text-margin stream)
           (floor max-width 4) 3 12))
       (ncolumns
        (min (max (floor margin max-width) 1) (length items))))
    ;; the lazyp flag indicates that if there are the same number of columns as were displayed
    ;; last time around then no redisplay is necessary
    (when (or (not lazyp) (not (eql ncolumns (file-selector-last-ncolumns frame))))
      (setf (file-selector-last-ncolumns frame)
            ncolumns)
      (display-fs-items items stream 'file-dir-namestring ncolumns max-width))))

;;; Each item is of the form (namestring pathname . type) where type is one of the symbols
;;; {device, parent, folder, document}. This function doesn't use the formatting-table
;;; :multiple-columns facility because it's not guaranteed to use all the columns - but we
;;; want that since it might bring into view items near the bottom of the table that otherwise
;;; would have to be scrolled down to.

(defun display-fs-items (items stream presentation-type ncolumns &optional (col-width 0))
  (let*
      ((x-margin 3)
       (y-margin (clim:stream-vertical-spacing stream))
       (row-height
        (+ (max 16 (clim:stream-line-height stream)) (clim:stream-vertical-spacing stream)))
       (record
        (clim:with-output-recording-options (stream :draw nil :record t)
          (clim:with-new-output-record (stream)
            (clim:window-clear stream)
            (loop
               with last-row-index = (1- (ceiling (length items) ncolumns))
               for item in items
               for ri = 0 then (if (= ri last-row-index) 0 (1+ ri))
               for ci = 0 then (if (zerop ri) (1+ ci) ci) ; display in column-wise order
               do
                 (clim:stream-set-cursor-position stream
                                                  (+ (* ci col-width) x-margin)
                                                  (+ (* ri row-height) y-margin))
                 (if (atom item)
                           (draw-heading stream item)
                           (clim:with-output-as-presentation (stream item presentation-type :single-box t)
                       (let ((icon
                              (ecase (cddr item)
                                (device
                                 +device-icon+)
                                (parent
                                 +up-folder-icon+)
                                (folder
                                 +folder-icon+)
                                (document
                                 +document-icon+))))
                         (draw-namestring stream icon (car item))))))
            (clim:scroll-extent stream 0 0)))))
    (clim:replay record stream)

    ;; in McCLIM only, need to indicate that the content of this pane may have a new height
    ;; so the scroller should update - and a new width to prevent the pane being scrollable
    ;; horizontally (e.g. with touchpad) if it has got narrower
    #+:mcclim
    (multiple-value-bind (w h)
        (clim:bounding-rectangle-size (clim:stream-output-history stream))
      (clim:change-space-requirements stream :width w :height (+ h 4)))))

(defun draw-namestring (stream pattern text)
  (flet ((backup/lock-namestring (s)
           ;; starts with ~$ or ends with ~ / # / .bak
           (let ((len (length s)))
             (and (> len 1)
                  (or (string= s "~$" :end1 2)
                      (member (char s (1- len)) '(#\~ #\#))
                      (and (> len 4) (string-equal s ".bak" :start1 (- len 4))))))))

    ;; !!! in McCLIM only, draw invisible points immediately to the left and right, otherwise
    ;; presentation highlighting does not give enough room
    (multiple-value-bind (x y)
        (clim:stream-cursor-position stream)
      #+:mcclim
      (progn
        (clim:draw-point* stream x (1- y) :line-thickness 1 :ink clim:+white+)
        (incf x))
      (clim:draw-pattern* stream pattern x y)
      (incf x (+ (clim:pattern-width pattern) 4))
      (clim:draw-text* stream
                       text
                       x
                       (+ y 7)
                       :align-y :center
                       :ink (if (backup/lock-namestring text)
                                +text-gray+
                                clim:+black+))
      #+:mcclim
      (progn
        (incf x (1+ (clim:text-size stream text)))
        (clim:draw-point* stream
                          x
                          (1- y)
                          :line-thickness 1
                          :ink clim:+white+)))))

(defun draw-heading (stream text)
  (multiple-value-bind (x y)
      (clim:stream-cursor-position stream)
    (clim:draw-text* stream
                     text
                     x
                     y
                     :align-y :top
                     :ink +text-gray+
                     :text-style (clim:merge-text-styles (clim:make-text-style nil :bold :smaller)
                                                         (clim:medium-text-style stream)))))

;;; Create a list of pathnames in current directory, headed by the pathname of the
;;; parent of this directory (or nil if we're at the root of this file system).

(defgeneric list-directory (frame dir &optional show-hidden-p)
  (:documentation "Returns a list of pathnames, the first being the parent directory of dir (or NIL if dir is the root of a file system) and the rest being the contents of dir. The show-hidden-p argument is passed through from the top-level call, intended to control whether file names starting with a period should be filtered out or not."))

(defmethod list-directory ((frame file-selector) dir &optional (show-hidden-p nil))
  (flet ((sorted-filtered-ls (d)

           ;; macOS always encodes file names as Unicode NFD no matter what the locale setting
           ;; is, so avoid a possible error here (in sbcl it's sb-int:c-string-decoding-error)
           ;; by ignoring a possibly misleading setting
           (let* (#+(and :sbcl :darwin) (sb-alien::*default-c-string-external-format* :utf-8)

                  ;; this call to list-directory resolves symlinks otherwise it's not possible
                  ;; to follow a symbolic link to a directory (e.g. tmp -> private/tmp, since
                  ;; in this case tmp looks like a normal file)
                  (items (cl-fad:list-directory d)))
             (sort
              (if show-hidden-p
                  items
                  (remove-if #'(lambda (p)
                                 (eql (char (file-dir-namestring p) 0) #\.))
                             items))
              #'string<
              :key #'file-dir-namestring))))
    (cons
     (if (pathname-root-p dir)
         nil
         (pathname-parent-directory dir))
     (handler-case
         (sorted-filtered-ls dir)
       (error (e)
         (warn "Unable to list directory ~A: ~A" dir e)
         nil)))))

(defun file-dir-namestring (x &optional homedir-tilde-p)
  (cond
    ((not (cl-fad:directory-pathname-p x))
     (file-namestring x))
    ((pathname-root-p x) ; NB could encounter root here via a symbolic link
     (directory-namestring x))
    ((and homedir-tilde-p (equal x (user-homedir-pathname)))
     (let ((dir (car (last (pathname-directory x)))))
       (concatenate 'string #+:unix "~" (if (and dir (stringp dir)) dir "home"))))
    (t
     (car (last (pathname-directory x))))))

;;; Avoid problems if we're in an environment with an old version of cl-fad (before 0.7.0?)

(defun pathname-root-p (p)
  (if (fboundp (intern "PATHNAME-ROOT-P" :fad))
      (funcall (intern "PATHNAME-ROOT-P" :fad) p)
      (equal (pathname-directory p) '(:absolute))))

(defun pathname-parent-directory (p)
  ;; argument p known not to be root
  (if (fboundp (intern "PATHNAME-PARENT-DIRECTORY" :fad))
      (funcall (intern "PATHNAME-PARENT-DIRECTORY" :fad) p)
      (make-pathname :directory (butlast (pathname-directory p)) :defaults p)))

(defun pathname-directory-pathname (p)
  (if (fboundp (intern "PATHNAME-DIRECTORY-PATHNAME" :fad))
      (funcall (intern "PATHNAME-DIRECTORY-PATHNAME" :fad) p)
      (make-pathname :defaults p :name nil :type nil)))

;;; 'Places' and 'devices' pane containing user home directory, any useful files or directories
;;; (achieved by specialising list-places), and roots of file systems on mounted devices. This
;;; is the pane on the left.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (clim:define-presentation-type place-device-namestring ()))

(define-file-selector-command com-select-place-device-namestring
    ((data 'place-device-namestring :gesture :select))
  (display-places-devices
   clim:*application-frame*
   (clim:find-pane-named clim:*application-frame* 'places-devices-pane))
  (select-file-dir data t))

(defun display-places-devices (frame stream)
  (display-fs-items
   (nconc (list "PLACES")
          (mapcar
           #'(lambda (p)
               (list* (file-dir-namestring p t) p
                      (if (cl-fad:directory-pathname-p p)
                          'folder
                          'document)))
           (list-places frame))
          (list " " "DEVICES")
          (mapcar
           #'(lambda (p) (list* (place-device-namestring p) p 'device))
           (list-devices frame)))
   stream 'place-device-namestring 1))

(defun place-device-namestring (x)
  ;; return name containing pathname device and last component of directory (if any) - to
  ;; look like a "device" as might be shown to the user on the desktop
  (let ((dev (pathname-device x))
        (dir (car (last (pathname-directory x)))))
    (when dir
      (unless (stringp dir) (setq dir nil)))
    (cond
      ((and dev
            (or (stringp dev) (symbolp dev) (characterp dev))
            (not (member dev '(:unspecific :unc))))
       (concatenate 'string (string dev) ":" dir))
      (dir)
      (t
       (directory-namestring x)))))

;;; Create a list of pathnames representing common places (= directories) in which the user
;;; might want to select files.

(defgeneric list-places (frame)
  (:documentation "Returns a list of pathnames, each of which is a regularly-used directory in which the user might want to select files."))

(defmethod list-places ((frame file-selector))
  (list (user-homedir-pathname)))

;;; Return a sorted list of pathnames representing roots of all mounted file systems. In
;;; Allegro CL on Windows there is a built-in function that does most of the work. On macOS,
;;; file systems are available under /Volumes/

(defgeneric list-devices (frame)
  (:documentation "Returns a list of pathnames, each of which is the root of a currently mounted file system - either local or via a network."))

(defmethod list-devices ((frame file-selector))
  (sort
   #+(and :mswindows :allegro)
   (mapcar
    #'(lambda (dev) (make-pathname :device (string dev) :directory '(:absolute)))
    (windows:file-systems)) ; Allegro CL only, defined in the :winapi module
   #+(and :mswindows (not :allegro))
   (list (make-pathname :device "C" :directory '(:absolute)))
   #+:darwin ; = macOS
   (cl-fad:list-directory "/Volumes/")
   #+(or :freebsd :linux)
   (append (list (make-pathname :directory '(:absolute)))
           (cl-fad:list-directory "/media/")
           (cl-fad:list-directory "/mnt/"))
   #+:openbsd
   (cons (make-pathname :directory '(:absolute)) (cl-fad:list-directory "/mnt/"))
   #-(or :mswindows :darwin :linux :bsd)
   (list (make-pathname :directory '(:absolute)))
   #'(lambda (p1 p2)
       (let ((p1-dev (pathname-device p1)) (p1-dir (car (last (pathname-directory p1))))
             (p2-dev (pathname-device p2)) (p2-dir (car (last (pathname-directory p2)))))
         (cond
           ((and p1-dev (null p2-dev)) t)
           ((and p2-dev (null p1-dev)) nil)
           ((or p1-dev p2-dev) (string< (string p1-dev) (string p2-dev)))
           ((and p1-dir (symbolp p1-dir)) t)
           ((and p2-dir (symbolp p2-dir)) nil)
           (t (string< (string p1-dir) (string p2-dir))))))))

;;; Having successfully loaded, add :select-file as a feature.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :select-file *features*))
