(require 'blog-author)
(require 'blog-database)
(require 'blog-git)
(require 'blog-post)
(setq-default blog-post-file-extension ".html")



(defun blog-get-output-filename-at-point
    (&optional file-ext)
  "Return the filename that this post would have, if published

FILE-EXT is the extension of the file. If you want it to begin with
'.', make sure you add it manually, since it won't be automatically
included in the filename."
  (concat (blog-get-stub-at-point) (or file-ext blog-post-file-extension)))

(defun blog-publish-at-point
    (publish-dir &optional file-ext body-only overwrite)
  "Publish the blogpost that exists at the current point, to a file.

The file will be published under the directory specified by PUBLISH-DIR.

FILE-EXT is the extension of the file. If you want it to begin with
'.', make sure you add it manually, since it won't be automatically
included in the filename.

If BODY-ONLY is non-nil, org-export will be told to only export what
would be within the <body> tags. In other words, it won't be a whole
HTML document. This is useful if you simply want to use the resulting
file in a template (i.e. using PHP or server-side includes).

If OVERWRITE is non-nil, the function will is allowed to overwrite the
file if it already exists. Otherwise, the function will signal an
error upon trying to publish to an already-existing file."
  (interactive "DPlease provide parent directory: \ni\ni\ni")
  (if (blog-post-at-point-p)
      (let ((post-text (org-export-as 'html t nil)) (post-output-path (concat publish-dir "/" (blog-get-output-filename-at-point file-ext))))
	(with-temp-buffer
	  (insert post-text)
	  (write-region nil nil post-output-path nil nil nil (if overwrite nil 'excl)))
	post-output-path)))

(defun blog-publish-this-buffer
    (publish-dir)
  "Publish all posts in this buffer.

Posts will be published into the directory specified by PUBLISH-DIR."
  (interactive "DPlease provide parent directory: ")
  (org-map-entries (lambda () (condition-case e
				  (blog-publish-at-point publish-dir)
				(error (message "Skipped existing file: '%s'." (nth 2 e))) nil nil))))

(defun blog-upload-post-at-point (conn root-dir file-path)
  "Upload the post at (point) to a SQLite database, located at the path specified in CONN.

ROOT-DIR is the directory prepended to FILE-PATH. It exists so that you
can put a relative path in the database while still accessing the file
on the current filesystem so that its hash can be calculated.

FILE-PATH is where the post has been exported, relative to the ROOT-DIR.
It's also what will be stored in the database, verbatim.
"
  (interactive)
  (let ((computed-hash nil) (file-size nil) (full-path (concat root-dir "/" file-path)))
    (with-temp-buffer (insert-file-contents-literally full-path 'raw-text)
		      (setq computed-hash (secure-hash 'sha512 (current-buffer)))
		      (setq file-size (buffer-size (current-buffer))))
    (sqlite-pragma conn "foreign_keys = ON;")
    (unwind-protect
	(progn
	  (sqlite-transaction conn)
	  (sqlite-execute conn "INSERT INTO files (path, hash, size) VALUES (?, ?, ?) ON CONFLICT DO UPDATE SET path=?, hash=?, size=?;" (list file-path computed-hash file-size file-path computed-hash file-size))
	  (sqlite-execute conn "INSERT OR REPLACE INTO posts (title, subtitle, author, date_published, date_modified, stub, file) VALUES (?, ?, (SELECT id FROM authors WHERE nominal_id=?), ?, ?, ?, (SELECT id FROM files WHERE path=?));"
			  (blog-query-info-at-point file-path))
	  (sqlite-commit conn))
      (sqlite-rollback conn))))


(provide 'blog)
