(defclass blog-git-object ()
  ((mode :initarg :mode
	 :accessor blog-git-object-mode)
   (hash :initarg :hash
	 :accessor blog-git-object-hash)
   (path :initarg :path
	 :accessor blog-git-object-path))
   :documentation "Representation of a Git object.")

(defclass blog-git-diff-report-record ()
  ((old :initarg :old
	:accessor blog-git-diff-report-record-old
	:documentation "Version of the object in the old tree.")
   (new :initarg :new
	:accessor blog-git-diff-report-record-new
	:documentation "Version of the object in the new tree.")
   (action :initarg :action
	   :accessor blog-git-diff-report-record-action
	   :documentation "What happened to the object between the two trees."))
  :documentation "Stores a single Git object's changes between two trees.")

(defun blog-parse-git-diff-report-line (line &optional ignore-colon)
  "Given a segment from 'git diff-tree -r', LINE, return a blog-git-diff-report-record.

Normally, LINE should begin with ':', and the function will throw an
error if it doesn't. However, if IGNORE-COLON is non-nil, the function
will expect LINE to NOT begin with ':'."

  ;; If the line should start with ':', make the checks
  (if (not ignore-colon)
      (progn
	;; If the line doesn't start with ':', throw an error
	(if (not (= (string-to-char line) ?:))
	    (signal (error "String is not a readable line from 'git diff-tree'") line))

	;; If the line starts with ':', cut off the initial ':' before
	;; further processing
	(setq line (substring line 1 (length line)))))

  ;; Before we can parse the line, we need to split it into segments.
  ;;
  ;; First, split the line into three segments (split-line-w-paths):
  ;;
  ;;     1) misc.
  ;;
  ;;     2) path of old object
  ;;
  ;;     3) path of new object
  ;;
  ;; Then, the first "misc" segment is split into five segements (first-cols):
  ;;
  ;;     1) mode of old object
  ;;
  ;;     2) mode of new object
  ;;
  ;;     3) hash of old object
  ;;
  ;;     4) hash of new object
  ;;
  ;;     5) action that was done
  (let* ((split-line-w-paths (split-string line "\0"))
	 (first-cols (split-string (nth 0 split-line-w-paths) " ")))

    ;; Create (and return) a new git diff-report record
    (blog-git-diff-report-record :old (blog-git-object :mode (nth 0 first-cols)
						       :hash (nth 2 first-cols)
						       :path (nth 1 split-line-w-paths))  ; :path might be nil; that's okay.

				 :new (blog-git-object :mode (nth 1 first-cols)
						       :hash (nth 3 first-cols)
						       :path (nth 2 split-line-w-paths)) ; :path might be nil; that's okay.

				 :action (let ((action-char (string-to-char (nth 4 first-cols)))
					       (action-symbol nil))
					   (cond ((= action-char ?A) (setq action-symbol 'added))
						 ((= action-char ?D) (setq action-symbol 'deleted))
						 ((= action-char ?M) (setq action-symbol 'modified)))
					   action-symbol))))

(defun blog-git-generate-diff-report (before-commit after-commit &optional files-to-check)
  "Call 'git diff-tree -r --no-renames -z' to learn what files in
FILES-TO-CHECK have changed between commits BEFORE-COMMIT and
AFTER-COMMIT.

Return an alist of three lists:
  - :added Newly created files

  - :modified Changed files

  - :deleted Deleted files"
  (let ((added-files ()) (modified-files ()) (deleted-files ()) (cmd-output nil)
	(diff-line nil) (exit-status nil) (cmd nil))
    (with-temp-buffer
      (setq cmd (append (read "(call-process)") (list "git" nil (current-buffer) nil "diff-tree" "-r" "-z" "--no-renames" before-commit after-commit)))
      (setq exit-status (eval cmd))
      (if (not (= 0 exit-status))
	  (signal (error "git diff-tree command failed.") cmd (buffer-string)))
      (if (buffer-string)
	  (progn
	    (dolist (diff-line (split-string (buffer-string) "\0:"))
	      (let* ((diff-record (blog-parse-git-diff-report-line diff-line (not (= ?: (string-to-char diff-line)))))  ; Turn the line into a record; only the first line will begin with a colon
		     (action (blog-git-diff-report-record-action diff-record)))
		(if (or (not files-to-check)
			(cl-find-if (lambda (path-to-test) (or (string-equal path-to-test (blog-git-object-path (blog-git-diff-report-record-old diff-record)))
							       (string-equal path-to-test (blog-git-object-path (blog-git-diff-report-record-new diff-record))))) files-to-check))
		    (cond ((eq action 'added) (setq added-files (append added-files (list diff-record))))
			  ((eq action 'modified) (setq modified-files (append modified-files (list diff-record))))
			  ((eq action 'deleted) (setq deleted-files (append deleted-files (list diff-record))))))))))
      (list :added added-files :modified modified-files :deleted deleted-files))))

(provide 'blog-git)
