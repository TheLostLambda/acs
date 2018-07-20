;;;; This file contains the code for the program interface.

(ql:quickload :cl-ppcre)
(ql:quickload :cl-dbi)
(ql:quickload :quri)
(load "util.lisp")
(load "adb.lisp")
(load "db.lisp")
(load "gen.lisp")

(defun conversations ()
  (db-connect)
  (mapc (lambda (convo) (format t "(~A)~5t~A~%" (car convo) (cdr convo))) (list-conversations))
  (db-free))

(defun mk-report (convo &optional start end)
  (db-connect)
  (let ((msgs (list-messages convo start end)))
    (mapc #'adb-fetch-media msgs)
    (write-to-file (gen-report (lookup convo (list-conversations)) msgs)))
  (db-free))
