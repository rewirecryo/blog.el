(require 'blog-base)
(require 'eieio)

(defclass blog-author ()
  ((nominal-id :accessor blog-author-nominal-id
	       :initarg :nominal-id)
   (first-name :accessor blog-author-first-name
	       :initarg :first-name)
   (last-name :accessor blog-author-last-name
	      :initarg :last-name)))

(defun blog-calculate-authors-file-hash (authors-file)
  "Calculate the hash of the authors file."
  (with-temp-buffer
    (insert-file-contents authors-file)
    (blog-hash (current-buffer))))

(defun blog-fetch-stored-authors-file-hash (db)
  "Query database DB for the hash value for author.json that was last stored."
  (let ((hash-row (nth 0 (sqlite-select db "SELECT hash FROM authors_file_hash_table WHERE id=1;"))))
    (if hash-row
	(nth 0 hash-row))))

(defun blog-stored-authors-needs-update (db authors-file)
  "Check whether the list of authors in database DB needs to be updated.

It's considered out of date in the following cases:

  1) The hash in DB is different from the hash of the author file AUTHOR-FILE.

     In this case, the calculated hash of the author file is returned.

  2) There is no hash stored in DB

     In this case, t is returned."
  (let ((stored-hash (blog-fetch-stored-authors-file-hash db)) (calculated-hash nil))
    (if (not stored-hash)
	t
      (setq calculated-hash (blog-calculate-authors-file-hash authors-file))
      (if (not (string-equal stored-hash calculated-hash))
	  calculated-hash)))) ;; Return the calculated hash if it's different from that in the database

(defun blog-load-authors-from-file (authors-file-path &optional as-alist)
    "Parse authors file AUTHORS-FILE-PATH and return a list of author objects
for each author read from the file.

If AS-ALIST is nil, return a list. If AS-ALIST is non-nil, return an
alist, whose keys are the nominal IDs of their corresponding blog-author
objects."
    (with-temp-buffer
      (insert-file-contents authors-file-path)
      (let ((parsed-json (json-parse-buffer))
	    (final-list ()))
	(if (not (vectorp parsed-json))
	    (error "Root element in author file `%s' is not an array" authors-file-path))
	(seq-do (lambda (j-current-author)
		   (let ((first-name (gethash "first_name" j-current-author))
			 (last-name (gethash "last_name" j-current-author))
			 (nominal-id (gethash "nominal_id" j-current-author)))
		     (if as-alist
			 (push nominal-id final-list))
		     (push (make-instance blog-author
					  :first-name first-name
					  :last-name last-name
					  :nominal-id nominal-id)
			   final-list)))
		parsed-json)
	(reverse final-list))))

(defun blog-push-authors-to-database (db authors &optional non-atomic)
  "Push list of author objects, AUTHORS, to the database DB.

If NON-ATOMIC is non-nil, the SQL statements will be executed without
creating a transaction or rolling back failed statements."
  (condition-case caught-err
      (progn (if (not non-atomic) (sqlite-transaction db))
	     (seq-map (lambda (current-author)
			(sqlite-execute db
					"INSERT INTO authors (name_id, first_name, last_name) VALUES (?, ?, ?) ON CONFLICT DO UPDATE SET name_id = ?, first_name = ?, last_name = ?;"
					(list (blog-author-nominal-id current-author)
					      (blog-author-first-name current-author)
					      (blog-author-last-name current-author)
					      (blog-author-nominal-id current-author)
					      (blog-author-first-name current-author)
					      (blog-author-last-name current-author))))
		      authors)
	     (if (not non-atomic) sqlite-commit db))
    ((error) (if (not non-atomic) (sqlite-rollback db))
	      (signal (car caught-err) (cdr caught-err)))))

(provide 'blog-author)
