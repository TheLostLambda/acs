;;;; This file contains the code used to generate the final html report.

(defparameter *report-path* (merge-pathnames "ACS.html" *output-dir*))
(defparameter *last-sender* nil)

(defparameter *html-wrapper* '(
"<!DOCTYPE html>
<html>
  <head>
    <title>~A</title>
    <meta charset=\"UTF-8\">
    <style>
      .convo {
        width: 60%;
        margin: auto;
      }
      .right {
        text-align: right;
        margin-left: 40%;
      }
      .left {
        text-align: left;
        margin-right: 40%;
      }
      img, video {
        max-height: 15rem
      }
      .message-container {
        background-color: #89CFF0;
        border-radius: 1rem;
        max-width: 60%;
      }
      .message-container * {
        padding: 0.5rem;
      }
      .message-container:hover .timestamp {
        display: block;
      }
      .timestamp {
        display: none;
      }
    </style>
  </head>
  <body>
    <div class=\"convo\">
"
. ;; The messages are inserted here.
"    </div>
  </body>
</html>
"))

(defun gen-report (title msgs)
  (let ((html-stream (make-string-output-stream)))
    (format html-stream (car *html-wrapper*) title)
    (mapc (lambda (msg) (princ (msg-to-html msg) html-stream)) msgs)
    (format html-stream (cdr *html-wrapper*))
    (get-output-stream-string html-stream)))

(defun msg-to-html (msg)
  (let ((html-stream (make-string-output-stream)))
    (unless (or (equal (cadr msg) *last-sender*) (equal (cadr msg) "Me"))
      (format html-stream "<p class=\"sender-name\">~A</p>~%" (cadr msg))) ; Change this so these IDs display only in group messages
    (setf *last-sender* (cadr msg))
    (format html-stream "<div class=\"message-container ~A\">~%" (if (equal (cadr msg) "Me") "right" "left"))
    (cond ((eq (car msg) :text)
	   (format html-stream "<p class=\"message\">~A</p>~%" (car (last msg))))
	  ((member (car msg) '(:image :sticker))
	   (format html-stream "<p><img src=\"~A\" alt=\"COULDN'T DISPLAY MEDIA\"></p>~%" (resolve-media (car (last msg)))))
	  ((eq (car msg) :video)
	   (format html-stream "<video controls><source src=\"~A\">COULDN'T DISPLAY MEDIA</video>" (resolve-media (car (last msg)))))
	  (t ""))
    (format html-stream "<p class=\"timestamp\">~A</p>~%" (caddr msg))
    (format html-stream "</div>~%")
    (get-output-stream-string html-stream)))

(defun write-to-file (report-str)
  (with-open-file (fh (ensure-directories-exist *report-path*) :direction :output :if-exists :supersede)
    (write-string report-str fh)))

