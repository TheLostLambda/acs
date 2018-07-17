;;;; This file contains all of the code that handles interactions with adb and the filesystem

(defparameter *REMOTE-DB-PATH* "/data/data/com.google.android.apps.fireball/databases/fireball.db")
(defparameter *TEMP* (uiop:default-temporary-directory))

(defparameter *output-dir* (merge-pathnames (uiop:parse-unix-namestring "../acs_report/") (uiop:getcwd)))
(defparameter *media-dir* (merge-pathnames "media/" *output-dir*))

;; This only works on rooted devices. To change that I would need to use `adb backup` then untar that archive.
(defun adb-pull-database ()
  "Pulls the message database from the phone by calling `adb pull`"
  (uiop:run-program (list "adb" "pull" *REMOTE-DB-PATH* (namestring *TEMP*)) :output t))

(defun adb-fetch-media (msg)
  "Takes a message, checks its type, then uses adb to fetch any media that it might reference"
  (when (member (car msg) '(:image :sticker :video))
    (let ((uri (format-uri (car (last msg))))) 
      (ensure-directories-exist *media-dir*)
      (cond ((member (car msg) '(:image :video))
	     (unless (probe-file (merge-pathnames *media-dir* (file-namestring uri))) (ignore-errors (uiop:run-program (list "adb" "pull" uri (namestring *media-dir*)) :output t))))
	    ((eq (car msg) :sticker)
	     (let ((temp-file (namestring (merge-pathnames *TEMP* (file-namestring uri))))
		   (conv-file (namestring (merge-pathnames *media-dir* (concatenate 'string (pathname-name uri) ".png"))))
		   (copy-file (namestring (merge-pathnames *media-dir* (concatenate 'string (pathname-name uri) ".webp")))))
	       (unless (or (probe-file copy-file) (probe-file conv-file))
		 (uiop:run-program (list "adb" "pull" uri (namestring *TEMP*)) :output t)
		 (handler-case (uiop:run-program (list "convert" temp-file conv-file))
		   (error () (uiop:run-program (list "cp" temp-file copy-file)))))))))))
