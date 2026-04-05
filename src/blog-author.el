(require 'blog-base)
(require 'eieio)

(defclass blog-avatar ()
  ((path :reader blog-avatar-path
	 :initarg :path)
   (size :reader blog-avatar-size
	 :initarg :size)))

(defclass blog-avatar-set ()
  ((avatars :accessor blog-avatar-set-avatars
	    :initarg :avatars)
   (timestamp :accessor blog-avatar-set-timestamp
	      :initarg :timestamp)))

(defclass blog-author ()
  ((nominal-id :accessor blog-author-nominal-id
	       :initarg :nominal-id)
   (first-name :accessor blog-author-first-name
	       :initarg :first-name)
   (last-name :accessor blog-author-last-name
	      :initarg :last-name)
   (avatar-sets :accessor blog-author-avatar-sets
		:initarg :avatar-sets)))

(defun blog-fetch-stored-authors-file-hash (db)
  "Query database DB for the hash value for author.json that was last stored."
  (let ((hash-row (nth 0 (sqlite-select db "SELECT hash FROM authors_file_hash_table WHERE id=1;"))))
    (if hash-row
	(nth 0 hash-row))))

(defun blog-stored-authors-needs-update (db authors-file-object)
  "Check whether the list of authors in database DB needs to be updated.

It's considered out of date in the following cases:

  1) The hash in DB is different from the hash of the author file AUTHOR-FILE.

     In this case, the calculated hash of the author file is returned.

  2) There is no hash stored in DB

     In this case, t is returned."
  (let ((stored-hash (blog-fetch-stored-authors-file-hash db)) (calculated-hash nil))
    (if (not stored-hash)
	t
      (setq calculated-hash (blog-hash (blog-git-object-show authors-file-object)))
      (if (not (string-equal stored-hash calculated-hash))
	  calculated-hash)))) ;; Return the calculated hash if it's different from that in the database

(defun blog-load-authors-from-buffer (buffer &optional as-alist)
    "Parse authors in buffer BUFFER and return a list of author objects
for each author read from the buffer.

If AS-ALIST is nil, return a list. If AS-ALIST is non-nil, return an
alist, whose keys are the nominal IDs of their corresponding blog-author
objects."
    (with-current-buffer buffer
      (let ((parsed-json (json-parse-string (buffer-string)))
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

(defun blog-load-authors-from-git-object (git-object &optional as-alist)
  "Parse authors in Git object GIT-OBJECT and return a list of author
objects for each author read from the object.

If AS-ALIST is nil, return a list. If AS-ALIST is non-nil, return an
alist, whose keys are the nominal IDs of their corresponding blog-author
objects."
  (with-temp-buffer
    (insert (blog-git-object-show git-object))
    (blog-load-authors-from-buffer (current-buffer) as-alist)))

(defun blog-push-authors-to-database (db authors &optional non-atomic)
  "Push list of author objects, AUTHORS, to the database DB.

If NON-ATOMIC is non-nil, the SQL statements will be executed without
creating a transaction or rolling back failed statements."
  (condition-case caught-err
      (progn (if (not non-atomic) (sqlite-transaction db))
	     (seq-map (lambda (current-author)
			(sqlite-execute db
					"INSERT INTO authors (nominal_id, first_name, last_name) VALUES (?, ?, ?) ON CONFLICT DO UPDATE SET nominal_id = ?, first_name = ?, last_name = ?;"
					(list (blog-author-nominal-id current-author)
					      (blog-author-first-name current-author)
					      (blog-author-last-name current-author)
					      (blog-author-nominal-id current-author)
					      (blog-author-first-name current-author)
					      (blog-author-last-name current-author))))
		      authors)
	     (if (not non-atomic) sqlite-commit db))
    (error (if (not non-atomic) (sqlite-rollback db))
	   (signal (car caught-err) (cdr caught-err)))))

(provide 'blog-author)
