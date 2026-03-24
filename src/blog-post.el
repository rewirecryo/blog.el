(setq-default blog-post-tag "blog_post")

(defclass blog-post ()
  ((nominal-id :initarg :nominal-id
	       :reader blog-nominal-id)
   (title :initarg :title
	  :reader blog-post-title)
   (subtitle :initarg :subtitle
	     :reader blog-post-subtitle)
   (author :initarg :author
	   :reader blog-post-author)
   (date-published :initarg :date-published
		   :reader blog-post-date-published)
   (date-modified :initarg :date-modified
		  :reader blog-post-date-modified)
   (stub :initarg :stub
	 :reader blog-post-stub)
   (src-file-path :initarg :src-file-path
		  :reader src-file-path)
   (hash :initarg :hash
	 :reader blog-post-hash)
   (content :initarg :content
	    :reader blog-post-content)))

(defun blog-org-timestamp-string-to-unix-timestamp
    (timestamp-string)
  "Given a timestamp string in format that's compatible with Org Mode,
return a Unix timestamp."
  (string-to-number
   (org-timestamp-format
    (org-timestamp-from-string
     timestamp-string)
    "%s")))

(defun blog-get-stub-at-point
    ()
  "Either retrieve or create a stub for the blog post at (point)."
  (let ((preset-stub (org-entry-get (point) "stub" nil)))
    (or	preset-stub
	(progn
	  (replace-regexp-in-string "-$" ""
				    (replace-regexp-in-string "[^a-z0-9-]+" "-"
							      (downcase (nth 4 (org-heading-components)))))))))

(defun blog-post-at-point-p
    ()
  "Return whether the org entry at (point) is a blogpost."
  (if (member blog-post-tag (org-get-tags (point) t))
      t
    nil))

(defun blog-parse-post-at-point
    (authors-list blog-root file-path &optional ignore-check)
  "Parse the Org entry at (point), and return a (blog-post) object.

A post needs a valid author, which will be chosen from AUTHORS-LIST,
which is a list of (blog-author) objects. The last object with the
matching nominal ID will be used.

FILE-PATH is the desired value of the 'file' slot.

Unless IGNORE-CHECK is t, an error will be thrown if the org entry
doesn't meet the requirements of blog-post-at-point-p."
  (if (not ignore-check)
      (if (not (blog-post-at-point-p))
	  (error "Org entry at (point) is not a blog post")))

  (make-instance blog-post :title (or (nth 4 (org-heading-components))
				      (error "Empty title"))
		 :nominal-id (or (org-entry-get (point) "nominal_id")
				 (error "Missing nominal ID"))
		 :subtitle (org-entry-get (point) "subtitle" nil)
		 :author (let ((found-author nil) (author-to-try (org-entry-get (point) "author" nil)))
			   (dolist (current-author authors-list)
			     (if (string-equal (blog-author-nominal-id current-author)
					       author-to-try)
				 (setq found-author current-author)))
			   (if (not found-author)
			       (error (concat "Author `" author-to-try "' does not exist in given author list") (list author-to-try authors-list))
			     found-author))
		 :date-published (let ((timestamp-string (org-entry-get (point) "date_published")))
				   (if timestamp-string (blog-org-timestamp-string-to-unix-timestamp timestamp-string)
				     (error "Missing published date")))
		 :date-modified (if (org-entry-get (point) "date_modified") (blog-org-timestamp-string-to-unix-timestamp (org-entry-get (point) "date_modified")))
		 :stub (or (blog-get-stub-at-point)
			   (error "Missing stub"))
		 :src-file-path (file-relative-name (buffer-file-name) blog-root)
		 :hash (blog-hash (save-excursion (org-back-to-heading)
						  (buffer-substring (point)
								    (org-element-contents-end (org-element-at-point)))))
		 :content (save-excursion (let ((begin nil) (end nil))
					    (org-back-to-heading)
					    (org-end-of-meta-data t)
					    (setq begin (point))
					    (org-end-of-subtree)
					    (setq end (point))
					    (buffer-substring-no-properties begin end)))))

(cl-defmethod blog-post-calculate-hash (post blog-post)
  "Given a blog-post, POST, calculate the hash of its contents."
  (blog-hash ))

(provide 'blog-post)
