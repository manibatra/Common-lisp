(defun make-cd (title artist rating ripped)
	(list :title title :artist artist :rating rating :ripped ripped))

(defvar *db* nil)

(defun add-record (cd) (push cd *db*))


(defun dump-db ()
	(format t "~{~{~a:~10t~a~%~}~%~}" *db*))


(defun prompt-read (prompt)
	(format *query-io* "~a:" prompt)
	(force-output *query-io*)
	(read-line *query-io*))


(defun prompt-for-cd ()
  	(make-cd 
	  (prompt-read "Title")
	  (prompt-read "Artist")
	  (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
	  (y-or-n-p "Ripped [y/n]: ")))

;;; Loop and ask for cds till the user is done

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
		(if (not (y-or-n-p "Another? [y/n]: ")) (return))))


;;; Print to file

(defun save-db (filename)
  (with-open-file (out filename
					   :direction :output
					   :if-exists :supersede)
	(with-standard-io-syntax
	  (print *db* out)))) 

;;; Load from file

(defun load-db (filename)
  (with-open-file (in filename)
	(with-standard-io-syntax
	  (setf *db* (read in)))))


;;; Query based on artist name
(defun select-by-artist (artist)
  (remove-if-not 
	#'(lambda (cd) (equal (getf cd :artist) artist))
  	*db*))

;;; Generalised query


(defun select (selector-fn)
  (remove-if-not selector-fn *db*))


;;; Artist selector function

(defun artist-selector (artist)
  (select #'(lambda (cd) (equal (getf cd :artist) artist))))
