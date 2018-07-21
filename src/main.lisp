;;;; This file contains the code for the program interface.

(ql:quickload :cl-ppcre)
(ql:quickload :cl-dbi)
(ql:quickload :quri)
(load "util.lisp")
(load "adb.lisp")
(load "db.lisp")
(load "gen.lisp")

(defparameter *output-dir* nil)

(defun conversations ()
  (db-connect)
  (mapc (lambda (convo) (format t "(~A)~5t~A~%" (car convo) (cdr convo))) (list-conversations))
  (db-free))

(defun mk-report (convo &optional path start end)
  (db-connect)
  (cond ((and path (uiop:absolute-pathname-p path)) (setf *output-dir* (absolute-path path)))
	(path (setf *output-dir* (relative-path path)))
	(t (setf *output-dir* (relative-path "./acs_report/"))))
  (let ((msgs (list-messages convo start end))
	(convo-name (lookup convo (list-conversations))))
    (mapc #'adb-fetch-media msgs)
    (write-to-file (gen-report convo-name msgs)))
  (db-free))
