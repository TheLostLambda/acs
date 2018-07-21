;;;; This file contains the code used to generate the final html report.

(defparameter *last-sender* nil "Tracks the last person who sent a message so repeat senders only have their name shown once.")

(defparameter *html-wrapper* '(
"<!DOCTYPE html>
<html>
  <head>
    <title>~A</title>
    <meta charset=\"UTF-8\">
    <style>
      .convo {
        width: 90%;
        margin: auto;
      }
      .right {
        text-align: right;
        margin-left: 40%;
        flex-direction: row;
      }
      .left {
        text-align: left;
        margin-right: 40%;
        flex-direction: row-reverse;
      }
      .sender-name {
        font-weight: bold;
      }
      img, video {
        max-width: 50%;
        border-radius: 0.5rem;
        line-height: initial;
      }
      audio {
        border-radius: 0.5rem;
      }
      .message {
        white-space: pre-wrap;
        border-radius: 1rem;
        padding: 0.5rem;
      }
      .right .message {
        background-color: #89CFF0;
      }
      .left .message {
        background-color: #77DD77;
      }
      .timestamp {
        margin-right: 0.5rem;
        min-width: 10rem;
        font-weight: lighter;
      }
      .message-container {
        max-width: 60%;
        display: flex;
        justify-content: flex-end;
      }
      .message-container p {
        margin-top: 0.1rem;
        margin-bottom: 0.1rem;
      }
      a {
        text-decoration: none;
      }
      p {
        font-family: \"Noto Sans\", Arial, sans-serif;
      }
      .media {
        line-height: 0;
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
") "This wrapper contains the shared html for all of the reports. This includes `<head>` data and the page's CSS.")

(defun gen-report (title msgs)
  "Generates an html report when given a list of messages. Builds the html using the wrapper and the repeated application of the `msg-to-html` function."
  (let ((html-stream (make-string-output-stream)))
    (format html-stream (car *html-wrapper*) title)
    (mapc (lambda (msg) (unless (eq (car msg) :unsupported) (princ (msg-to-html msg) html-stream))) msgs)
    (format html-stream (cdr *html-wrapper*))
    (get-output-stream-string html-stream)))

(defun msg-to-html (msg)
  "Takes a single message and represents it in html. It checks the message type before rendering it appropriately."
  (let ((html-stream (make-string-output-stream)))
    (unless (or (equal (cadr msg) *last-sender*) (equal (cadr msg) "Me"))
      (format html-stream "<p class=\"sender-name\">~A</p>~%" (cadr msg)))
    (setf *last-sender* (cadr msg))
    (format html-stream "<div class=\"message-container ~A\">~%" (if (equal (cadr msg) "Me") "right" "left"))
    (cond ((eq (car msg) :text)
	   (format html-stream "<p class=\"message\" style=\"font-size:~Apx\">~A</p>~%" (round (nth 3 msg)) (car (last msg))))
	  ((eq (car msg) :document)
	   (format html-stream "<p class=\"message\">Sent a file: <a href=\"~A\">~A</a></p>~%" (resolve-media (car (last msg))) (nth 3 msg)))
	  ((eq (car msg) :location)
	   (format html-stream "<p class=\"message\">Sent a location: ~A~%(~6$,~6$)</p>~%" (car (last msg)) (nth 3 msg) (nth 4 msg)))
	  ((member (car msg) '(:image :sticker))
	   (format html-stream "<p class=\"media\"><img src=\"~A\" alt=\"COULDN'T DISPLAY MEDIA\"></p>~%" (resolve-media (car (last msg)))))
	  ((eq (car msg) :video)
	   (format html-stream "<p class=\"media\"><video controls><source src=\"~A\">COULDN'T DISPLAY MEDIA</video></p>~%" (resolve-media (car (last msg)))))
	  ((eq (car msg) :audio)
	   (format html-stream "<p class=\"media\"><audio controls><source src=\"~A\">COULDN'T DISPLAY MEDIA</audio></p>~%" (resolve-media (car (last msg)))))
	  (t ""))
    (format html-stream "<p class=\"timestamp\">~A</p>~%" (caddr msg))
    (format html-stream "</div>~%")
    (get-output-stream-string html-stream)))
