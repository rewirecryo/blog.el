(defun blog-generate-temporary-directory-path (&optional prefix suffix retries)
  "Generate a path for a name for a temporary directory.

NOTE: This function DOES NOT append a slash to the path it returns."
  (if (not retries) (setq retries 0))
  (let ((new-directory (file-name-as-directory
			(concat (temporary-file-directory)
			       (or prefix "")
			       (number-to-string (emacs-pid))
			       (number-to-string (time-convert (current-time) 'integer))
			       (number-to-string (random (min 999999 most-positive-fixnum)))
			       (or suffix "")))))
    (if (file-exists-p new-directory)
	(if (> retries 0)
	    (blog-generate-temporary-directory-path (- retries 1))
	  (signal 'blog-error "Could not find a temporary directory path that was available."))
      new-directory)))

(defun blog-make-temporary-directory (&optional prefix suffix retries)
  "Try to create a unique subdirectory in the OS's temporary files directory (i.e. /tmp).

Returns the path of the created directory.

The filename will be prepended with PREFIX, and SUFFIX will be appended to it.

NOTE: This function DOES NOT append a slash to the path it returns."

  (let ((directory (blog-generate-temporary-directory-path prefix suffix retries)))
    (make-directory directory)
    directory))

(provide 'blog-test-utils)
