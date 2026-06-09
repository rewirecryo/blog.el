(require 'org)
(require 'org-element)

(setq-default blog-post-tag "blog_post")

(defclass blog-post ()
  ((nominal-id :initarg :nominal-id
	       :reader blog-post-nominal-id)
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
		  :reader blog-post-src-file-path)
   (hash :initarg :hash
	 :reader blog-post-hash)
   (content :initarg :content
	    :reader blog-post-content)))

(defun blog-org-timestamp-string-to-unix-timestamp
    (timestamp-string)
  "Given a timestamp string, TIMESTAMP-STRING, in format that's compatible
with Org Mode, return a Unix timestamp.

NOTE: This function does not understand timezones. TIMESTAMP-STRING will
be understood to be in whatever timezone Emacs's time functions are
using, as explained in 43.6 of the GNU Elisp Reference Manual. A
TIMESTAMP-STRING value with a timezone will be accepted as a
properly-formatted argument, but won't change the returned value result."
  (time-convert
   (org-timestamp-to-time
    (org-timestamp-from-string
     timestamp-string))
   'integer))

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
    (authors-list file-path &optional ignore-check blog-root src-file-path)
  "Parse the Org entry at (point), and return a (blog-post) object.

A post needs a valid author, which will be chosen from AUTHORS-LIST,
which is a list of (blog-author) objects. The last object with the
matching nominal ID will be used.

FILE-PATH is the desired value of the 'file' slot.

Unless IGNORE-CHECK is t, an error will be thrown if the org entry
doesn't meet the requirements of blog-post-at-point-p.

If SRC-FILE-PATH is non-nil, it will be set as the blog-post's source
file. Otherwise, the blog-post's source file will be (buffer-file-name),
relative to BLOG-ROOT."
  (if (not ignore-check)
      (if (not (blog-post-at-point-p))
	  (error "Org entry at (point) is not a blog post")))

  (make-instance blog-post :title (or (nth 4 (org-heading-components))
				      (error "Empty title"))
		 :nominal-id (or (org-entry-get (point) "nominal_id")
				 (error "Missing nominal ID"))
		 :subtitle (or (org-entry-get (point) "subtitle")
			       (error "Missing subtitle"))
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
		 :src-file-path (or src-file-path
				    (if (buffer-file-name)
					(file-relative-name (buffer-file-name) blog-root)
				      nil))
		 :hash (blog-hash (save-excursion (org-back-to-heading)
						  (buffer-substring (point)
								    (org-element-property :end (org-element-at-point)))))
		 :content (save-excursion (let ((begin nil) (end nil))
					    (org-back-to-heading)
					    (org-end-of-meta-data t)
					    (setq begin (point))
					    (org-end-of-subtree)
					    (setq end (point))
					    (buffer-substring-no-properties begin end)))))

(cl-defmethod blog-post-calculate-hash (post blog-post)
  "Given a blog-post, POST, calculate the hash of its contents."
  (blog-hash post))

(defun blog-read-posts-from-buffer (buffer authors-list &optional as-alist existing-posts src-file-path)
  "Return a list of all posts in a given BUFFER, with the authors being
stored in AUTHORS-LIST.

If AS-ALIST is non-nil, return the list of posts as an alist, with the
post's nominal ID as its key.

If AS-ALIST is non-nil, any post found in BUFFER whose alist key is the
same as one in EXISTING-POSTS, will result in an error.

SRC-FILE-PATH is the source .org file with which the posts should be
associated, if any."
  (let ((final-list ()))
    (with-current-buffer buffer
      (if (not (string-equal mode-name "Org"))
	  (error "Buffer is not an Org buffer") buffer)
      (save-excursion
	  (goto-char 1)
	  (org-map-entries (lambda ()
			     (if (blog-post-at-point-p)
				 (progn
				   (let* ((current-post (blog-parse-post-at-point authors-list "" nil nil src-file-path))
					  (current-post-nominal-id (blog-post-nominal-id current-post)))
				     (if as-alist
					 (progn (if (assoc current-post-nominal-id (append final-list existing-posts))
						    (error "Blog post `%s' was found twice" current-post-nominal-id)
						  (push (list current-post-nominal-id current-post) final-list)))
				       (push current-post final-list))))))))
	(reverse final-list))))

(defun blog-fetch-posts-from-git-object (git-object authors-list &optional as-alist existing-posts src-file-path)
  "Read the posts held in a GIT-OBJECT.

If SRC-FILE-PATH is t, it's taken to be the path of GIT-OBJECT.
Otherwise, it's interpreted the same as it is
in (blog-read-posts-from-buffer).

Other arguments are identical to those
in (blog-read-posts-from-buffer)."
  (with-temp-buffer (org-mode)
		    (insert (blog-git-object-show git-object))
		    (blog-read-posts-from-buffer (current-buffer)
						 authors-list
						 as-alist
						 existing-posts
						 (if (eq src-file-path t)
						     (blog-git-object-path git-object)
						   src-file-path))))

(cl-defmethod blog-post-publish ((post blog-post) (output-file string) &optional (overwrite bool))
  "Publish a blog post, POST to a file at OUTPUT-FILE.

If OVERWRITE is non-nil, an exception will be thrown if the file already
exists.

If the file is successfully written, the content of the HTML file will
be returned."
  (with-temp-buffer (insert (blog-post-content post))
		    (let ((post-html (org-export-as 'html nil nil t)))
		      (if (not (and (file-exists-p output-file)
				    (not overwrite)))
			  (progn (with-temp-file output-file
				   (insert post-html)
				   post-html))
			(error "File `%s' already exists" output-file)))))
(provide 'blog-post)
