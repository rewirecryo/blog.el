(require 'blog-base)
(require 'eieio)

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

(defun blog-load-avatar-sets-from-array (j-avatar-set number-of-sizes)
  "Given an array from an author JSON object parsed from an author file,
J-AVATAR-SET, return an alist that could be used as an
avatar set in a (blog-author) object.

If the avatar set does not have NUMBER-OF-SIZES images, an error will be
thrown."
  (let ((found-timestamps ()))
    (seq-map (lambda (current-avatar-set)
	       (let* ((timestamp (gethash "timestamp" current-avatar-set))
		      (image-array (gethash "images" current-avatar-set))
		      (image-array-length (length image-array)))
		 (if (seq-position found-timestamps timestamp)
		     (signal 'blog-existence-error (list (format-message "Multiple avatar sets for timestamp %d" timestamp)
							 j-avatar-set
							 timestamp)))
		 (if (not (= number-of-sizes image-array-length))
		     (signal 'blog-length-error (list (format-message "Avatar set array has %d sizes when %d are required" image-array-length number-of-sizes)
						      j-avatar-set
						      image-array-length
						      number-of-sizes)))
		 (push timestamp found-timestamps)
		 (list timestamp
		       (seq-map-indexed (lambda (image idx)
					  (list (1+ idx) (if (eq image :null) nil image)))
					(gethash "images" current-avatar-set)))))
	     j-avatar-set)))

(defun blog-load-authors-from-buffer (buffer avatar-set-size &optional as-alist)
  "Parse authors in buffer BUFFER and return a list of author objects
for each author read from the buffer. Every one of the author's avatar
sets must have AVATAR-SET-SIZE number of images.

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
				       :nominal-id nominal-id
				       :avatar-sets (if (gethash "avatars" j-current-author)
							(blog-load-avatar-sets-from-array (gethash "avatars" j-current-author) avatar-set-size)
						      nil))
			final-list)))
	      parsed-json)
      (reverse final-list))))

(defun blog-load-authors-from-git-object (git-object avatar-set-size &optional as-alist)
  "Parse authors in Git object GIT-OBJECT and return a list of author
objects for each author read from the object.

Other parameters are identical to those of blog-load-authors-from-buffer"
  (with-temp-buffer
    (insert (blog-git-object-show git-object))
    (blog-load-authors-from-buffer (current-buffer) avatar-set-size as-alist)))

(defun blog-push-authors-to-database (db authors)
  "Push list of author objects, AUTHORS, to the database DB."
  (condition-case caught-err
      (progn (seq-map (lambda (current-author)
			(sqlite-execute db
					"INSERT INTO authors (nominal_id, first_name, last_name) VALUES (?, ?, ?) ON CONFLICT DO UPDATE SET nominal_id = ?, first_name = ?, last_name = ?;"
					(list (blog-author-nominal-id current-author)
					      (blog-author-first-name current-author)
					      (blog-author-last-name current-author)
					      (blog-author-nominal-id current-author)
					      (blog-author-first-name current-author)
					      (blog-author-last-name current-author)))
			(seq-map (lambda (current-avatar-set)
				   (sqlite-execute db "INSERT INTO avatar_sets (author, taken_time) VALUES ((SELECT id FROM authors WHERE nominal_id=?), ?) ON CONFLICT DO UPDATE SET author=(SELECT id FROM authors WHERE nominal_id=?), taken_time=?;"
						   (list (blog-author-nominal-id current-author)
							 (car current-avatar-set)
							 (blog-author-nominal-id current-author)
							 (car current-avatar-set)))
				   (seq-map (lambda (current-avatar-image)
					      (if (nth 1 current-avatar-image)
						  (sqlite-execute db "INSERT INTO avatars (avatar_set, screen_size, file_path) VALUES ((SELECT id FROM avatar_sets WHERE taken_time=?), ?, ?) ON CONFLICT DO UPDATE SET avatar_set=(SELECT id FROM avatar_sets WHERE taken_time=?), screen_size=?, file_path=?;"
								  (list (car current-avatar-set)
									(car current-avatar-image)
									(nth 1 current-avatar-image)
									(car current-avatar-set)
									(car current-avatar-image)
									(nth 1 current-avatar-image)))
						(progn
						  (sqlite-execute db
								  "DELETE FROM avatars WHERE avatar_set=(SELECT id FROM avatar_sets WHERE taken_time=?) AND screen_size=?;"
								  (list (car current-avatar-set)
									(car current-avatar-image))))))
					    (nth 1 current-avatar-set)))
				 (blog-author-avatar-sets current-author)))
		      authors))))

(provide 'blog-author)
