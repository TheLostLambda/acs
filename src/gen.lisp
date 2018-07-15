;;;; This file contains the code used to generate the final html report.

(defparameter *report-path* (merge-pathnames "ACS.html" *output-dir*))

(defparameter *html-wrapper* '(
"<!DOCTYPE html>
<html>
  <head>
    <title>~A</title>
    <meta charset=\"UTF-8\">
    <style>
      .right {
        text-align: right;
      }
      .left {
        text-align: left;
      }
      img {
        max-height: 15rem
      }
    </style>
  </head>
  <body>
"
. ;; The messages are inserted here.
"  </body>
</html>
"))

(defun gen-report (title msgs)
  (let ((html-stream (make-string-output-stream)))
    (format html-stream (car *html-wrapper*) title)
    (mapc (lambda (msg) (princ (msg-to-html msg) html-stream)) msgs)
    (format html-stream (cdr *html-wrapper*))
    (get-output-stream-string html-stream)))

(defun msg-to-html (msg)
  (cond ((eq (car msg) :text)
	 (format nil "<p class=\"~A\">~A</p>~%" (if (equal (cadr msg) "Me") "right" "left") (car (last msg))))
	((or (eq (car msg) :image) (eq (car msg) :sticker))
	 (format nil "<p class=\"~A\"><img src=\"~A\" alt=\"COULDN'T LOCATE MEDIA\"></p>~%" (if (equal (cadr msg) "Me") "right" "left") (resolve-media (car (last msg)))))
	(t "")))

(defun write-to-file (report-str)
  (with-open-file (fh (ensure-directories-exist *report-path*) :direction :output :if-exists :supersede)
    (write-string report-str fh)))

