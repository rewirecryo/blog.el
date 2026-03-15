(require 'blog-base)

(defclass blog/author ()
  ((nominal-id :accessor blog/author-nominal-id
	       :initarg :nominal-id)
   (first-name :accessor blog/author-first-name
	       :initarg :first-name)
   (last-name :accessor blog/author-last-name
	      :initarg :last-name)))

(defun blog/calculate-authors-file-hash (authors-file)
  "Calculate the hash of the authors file."
  (with-temp-buffer
    (insert-file-contents authors-file)
    (blog/hash (current-buffer))))

(defun blog/fetch-stored-authors-file-hash (db)
  "Query database DB for the hash value for author.json that was last stored."
  (let ((hash-row (nth 0 (sqlite-select db "SELECT hash FROM authors_file_hash_table WHERE id=1;"))))
    (if hash-row
	(nth 0 hash-row))))

(defun blog/stored-authors-needs-update (db authors-file)
  "Check whether the list of authors in database DB needs to be updated.

It's considered out of date in the following cases:

  1) The hash in DB is different from the hash of the author file AUTHOR-FILE.

     In this case, the calculated hash of the author file is returned.

  2) There is no hash stored in DB

     In this case, t is returned."
  (let ((stored-hash (blog/fetch-stored-authors-file-hash db)) (calculated-hash nil))
    (if (not stored-hash)
	t
      (setq calculated-hash (blog/calculate-authors-file-hash authors-file))
      (if (not (string-equal stored-hash calculated-hash))
	  calculated-hash)))) ;; Return the calculated hash if it's different from that in the database

(provide 'blog-author)
