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
  (mapc #'adb-fetch-media (list-messages convo start end))
  (write-to-file (gen-report (lookup convo (list-conversations)) (list-messages convo start end)))
  (db-free))
