(setq-default blog/post-tag "blog_post")
(setq-default blog/post-file-extension ".html")

(setq blog/max-nominal-id-length 32)
(setq blog/max-first-name-length 40)
(setq blog/max-last-name-length 40)
(setq blog/max-file-path-length 128)
(setq blog/hash-length 128)
(setq blog/max-subtitle-length 240)
(setq blog/max-title-length 80)
(setq blog/max-stub-length 32)

(defun blog/create-database (filename &optional overwrite)
  "Create a SQLite database that can serve as a Zero database, at FILENAME.

If OVERWRITE is nil, FILENAME will be overwritten."
  (if (and (file-exists-p filename) (not overwrite))
      (error "File `%s' already exists" filename)
      (let ((conn (sqlite-open filename)))
	(sqlite-execute conn "CREATE TABLE IF NOT EXISTS screen_sizes (id INTEGER PRIMARY KEY, name TEXT NOT NULL UNIQUE, CHECK(LENGTH(name) <= 32));")
	(sqlite-execute conn (format "CREATE TABLE IF NOT EXISTS authors (id INTEGER PRIMARY KEY, name_id TEXT NOT NULL UNIQUE, first_name TEXT NOT NULL, last_name TEXT NOT NULL, CHECK(LENGTH(name_id) <= %d), CHECK(LENGTH(first_name) <= %d), CHECK(LENGTH(last_name) <= %d), UNIQUE(first_name,last_name));" blog/max-nominal-id-length blog/max-first-name-length blog/max-last-name-length))
	(sqlite-execute conn "CREATE TABLE IF NOT EXISTS avatar_sets (id INTEGER PRIMARY KEY, author INTEGER NOT NULL, taken_time INTEGER NOT NULL, UNIQUE(author, taken_time), FOREIGN KEY(author) REFERENCES authors(id));")
	(sqlite-execute conn (format "CREATE TABLE IF NOT EXISTS avatars (file_path TEXT NOT NULL, avatar_set INTEGER NOT NULL, screen_size INTEGER NOT NULL, CHECK(LENGTH(file_path) <= %d), UNIQUE(avatar_set, screen_size), FOREIGN KEY(avatar_set) REFERENCES avatar_sets(id), FOREIGN KEY(screen_size) REFERENCES screen_sizes(id));" blog/max-file-path-length))
	(sqlite-execute conn (format "CREATE TABLE IF NOT EXISTS files (id INTEGER PRIMARY KEY, path TEXT NOT NULL UNIQUE, hash TEXT NOT NULL, size INTEGER NOT NULL, CHECK(LENGTH(path) <= %d), CHECK(LENGTH(hash) == %d), CHECK(size > 0), CHECK(path NOT LIKE '%%..%%'), CHECK(path NOT LIKE '%%*%%'));" blog/max-file-path-length blog/hash-length))
	(sqlite-execute conn (format "CREATE TABLE IF NOT EXISTS posts (id INTEGER PRIMARY KEY, hash TEXT NOT NULL, title TEXT NOT NULL UNIQUE, subtitle TEXT NOT NULL, author INTEGER NOT NULL, date_published INTEGER NOT NULL, date_modified INTEGER, stub TEXT NOT NULL UNIQUE, file INTEGER NOT NULL UNIQUE, CHECK(LENGTH(hash) == %d) CHECK(LENGTH(subtitle) <= %d), CHECK(LENGTH(title) <= %d), FOREIGN KEY(author) REFERENCES authors(id), CHECK(date_published >= 0), CHECK(IIF(date_modified == NULL, TRUE, date_modified > date_published)), CHECK(LENGTH(stub) <= %d), FOREIGN KEY(file) REFERENCES files(id));" blog/hash-length blog/max-subtitle-length blog/max-title-length blog/max-stub-length))
	(sqlite-close conn))))

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

(defun blog/upload-post-at-point (conn root-dir file-path)
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
	  (sqlite-execute conn "INSERT OR REPLACE INTO posts (title, subtitle, author, date_published, date_modified, stub, file) VALUES (?, ?, (SELECT id FROM authors WHERE name_id=?), ?, ?, ?, (SELECT id FROM files WHERE path=?));"
			  (blog/query-info-at-point file-path))
	  (sqlite-commit conn))
      (sqlite-rollback conn))))

(defun blog/query-info-at-point
    (file-path)
  "Return all information about the post at (point) that you'd need to put
that post into the database.

FILE-PATH is the desired value of the 'file' column.
"
  (interactive)
  (list (nth 4 (org-heading-components))
    (org-entry-get (point) "subtitle" nil)
    (org-entry-get (point) "author" nil)
    (string-to-number
     (org-timestamp-format (org-timestamp-from-string
			    (org-entry-get (point) "date_published"))
			   "%s"))
    (if (org-entry-get (point) "date_modified")
	(string-to-number
	 (org-timestamp-format (org-timestamp-from-string
				(org-entry-get (point) "date_modified"))
			       "%s")))
    (blog/get-stub-at-point)
    file-path
    ))

(provide 'blog)
