(setq-default blog/post-tag "blog_post")
(setq-default blog/post-file-extension ".html")
	      
(defun blog/is-post-at-point
    ()
  "Return whether the org entry at (point) is a blogpost."
  (if (member blog/post-tag (org-get-tags (point) t))
      t
    nil))

(defun blog/get-stub-at-point
    ()
  "Either retrieve or create a stub for the current entry."
  (let ((preset-stub (org-entry-get (point) "stub" nil)))
    (if preset-stub
	preset-stub
      (progn
	(replace-regexp-in-string "-$" ""
	(replace-regexp-in-string "[^a-z0-9-]+" "-"
	(downcase (nth 4 (org-heading-components)))))))))

(defun blog/get-output-filename-at-point
    (&optional file-ext)
  "Return the filename that this post would have, if published

FILE-EXT is the extension of the file. If you want it to begin with
'.', make sure you add it manually, since it won't be automatically
included in the filename."
  (concat (blog/get-stub-at-point) (or file-ext blog/post-file-extension)))

(defun blog/publish-at-point
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
  (if (blog/is-post-at-point)
      (let ((post-text (org-export-as 'html t nil)) (post-output-path (concat publish-dir "/" (blog/get-output-filename-at-point file-ext))))
	(with-temp-buffer
	  (insert post-text)
	  (write-region nil nil post-output-path nil nil nil (if overwrite nil 'excl)))
	post-output-path)))

(defun blog/publish-this-buffer
    (publish-dir)
  "Publish all posts in this buffer.

Posts will be published into the directory specified by PUBLISH-DIR."
  (interactive "DPlease provide parent directory: ")
  (org-map-entries (lambda () (condition-case e
				  (blog/publish-at-point publish-dir)
				(error (message "Skipped existing file: '%s'." (nth 2 e))) nil nil))))

(provide 'blog)
