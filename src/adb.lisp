;;;; This file contains all of the code that handles interactions with adb and the filesystem

(defparameter *REMOTE-DB-PATH* "/data/data/com.google.android.apps.fireball/databases/fireball.db" "This is the path to the Allo database on an Android phone.")
(defparameter *TEMP* (uiop:default-temporary-directory) "This is the path to the operating system's temporary directory, where the database from the phone will be stored.")

;; This only works on rooted devices. To change that I would need to use `adb backup` then untar that archive.
(defun adb-pull-database ()
  "Pulls the message database from the phone by calling `adb pull`."
  (format t "Pulling the database... ")
  (finish-output)
  (uiop:run-program (list "adb" "pull" *REMOTE-DB-PATH* (namestring *TEMP*)) :output nil)
  (format t "Done.~%"))

;; Seriously, .webp is the bane of my existence...
(defun adb-fetch-media (msg)
  "Takes a message, checks its type, then uses adb to fetch any media that it might reference. It also performs limited file-type conversion."
  (when (member (car msg) '(:image :sticker :video :document :audio))
    (let ((uri (format-uri (car (last msg))))
	  (media-dir (merge-pathnames "media/" *output-dir*)))
      (format t "Pulling media file ~A... " (file-namestring uri))
      (finish-output)
      (ensure-directories-exist media-dir)
      (cond ((eq (car msg) :sticker)
	     (let ((temp-file (namestring (merge-pathnames *TEMP* (file-namestring uri))))
		   (conv-file (namestring (merge-pathnames media-dir (concatenate 'string (pathname-name uri) ".png"))))
		   (copy-file (namestring (merge-pathnames media-dir (concatenate 'string (pathname-name uri) ".webp")))))
	       (unless (or (probe-file copy-file) (probe-file conv-file))
		 (uiop:run-program (list "adb" "pull" uri (namestring *TEMP*)) :output nil)
		 (handler-case (uiop:run-program (list "convert" temp-file conv-file))
		   (error () (uiop:run-program (list "cp" temp-file copy-file)))))))
	    (t (unless (probe-file (merge-pathnames media-dir (file-namestring uri))) (ignore-errors (uiop:run-program (list "adb" "pull" uri (namestring media-dir)) :output nil)))))
      (format t "Done.~%"))))
