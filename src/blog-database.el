(setq blog-max-nominal-id-length 32)
(setq blog-max-first-name-length 40)
(setq blog-max-last-name-length 40)
(setq blog-max-file-path-length 128)
(setq blog-hash-length 128)
(setq blog-max-subtitle-length 240)
(setq blog-max-title-length 80)
(setq blog-max-stub-length 32)

(defun blog-initialize-database (conn)
  "Turn database CONN, into a database that can serve as a Zero database."
      (condition-case caught-err
	  (progn
	    (sqlite-transaction conn)
	    (sqlite-execute conn "CREATE TABLE IF NOT EXISTS screen_sizes (id INTEGER PRIMARY KEY, name TEXT NOT NULL UNIQUE, CHECK(LENGTH(name) BETWEEN 1 AND 32));")
	    (sqlite-execute conn (format "CREATE TABLE IF NOT EXISTS authors (id INTEGER PRIMARY KEY, name_id TEXT NOT NULL UNIQUE, first_name TEXT NOT NULL, last_name TEXT NOT NULL, CHECK(LENGTH(name_id) BETWEEN 1 AND %d), CHECK(LENGTH(first_name) <= %d), CHECK(LENGTH(last_name) BETWEEN 1 AND %d), UNIQUE(first_name,last_name));" blog-max-nominal-id-length blog-max-first-name-length blog-max-last-name-length))
	    (sqlite-execute conn "CREATE TABLE IF NOT EXISTS avatar_sets (id INTEGER PRIMARY KEY, author INTEGER NOT NULL, taken_time INTEGER NOT NULL, UNIQUE(author, taken_time), FOREIGN KEY(author) REFERENCES authors(id));")
	    (sqlite-execute conn (format "CREATE TABLE IF NOT EXISTS avatars (file_path TEXT NOT NULL, avatar_set INTEGER NOT NULL, screen_size INTEGER NOT NULL, CHECK(LENGTH(file_path) BETWEEN 1 AND %d), UNIQUE(avatar_set, screen_size), FOREIGN KEY(avatar_set) REFERENCES avatar_sets(id), FOREIGN KEY(screen_size) REFERENCES screen_sizes(id));" blog-max-file-path-length))
	    (sqlite-execute conn (format "CREATE TABLE IF NOT EXISTS files (id INTEGER PRIMARY KEY, path TEXT NOT NULL UNIQUE, hash TEXT NOT NULL, size INTEGER NOT NULL, CHECK(LENGTH(path) BETWEEN 1 AND %d), CHECK(LENGTH(hash) == %d), CHECK(size > 0), CHECK(path NOT LIKE '%%..%%'), CHECK(path NOT LIKE '%%*%%'));" blog-max-file-path-length blog-hash-length))
	    (sqlite-execute conn (format "CREATE TABLE IF NOT EXISTS posts (id INTEGER PRIMARY KEY, hash TEXT NOT NULL, title TEXT NOT NULL UNIQUE, subtitle TEXT NOT NULL, author INTEGER NOT NULL, date_published INTEGER NOT NULL, date_modified INTEGER, stub TEXT NOT NULL UNIQUE, file INTEGER NOT NULL UNIQUE, CHECK(LENGTH(hash) == %d) CHECK(LENGTH(subtitle) BETWEEN 1 AND %d), CHECK(LENGTH(title) BETWEEN 1 AND %d), FOREIGN KEY(author) REFERENCES authors(id), CHECK(date_published >= 0), CHECK(IIF(date_modified == NULL, TRUE, date_modified > date_published)), CHECK(LENGTH(stub) BETWEEN 1 AND %d), FOREIGN KEY(file) REFERENCES files(id));" blog-hash-length blog-max-subtitle-length blog-max-title-length blog-max-stub-length))
	    (sqlite-execute conn (format "CREATE TABLE authors_file_hash_table (id INTEGER PRIMARY KEY, hash TEXT NOT NULL, CHECK(id = 1), CHECK(LENGTH(hash) = %d))" blog-hash-length))
	    (sqlite-commit conn)
	    t)
	((error) (sqlite-rollback conn)
	 (signal (car caught-err) (cdr caught-err)))))

(defun blog-create-database (filename &optional return-connection overwrite)
  "Create a SQLite database that can serve as a Zero database, at FILENAME.

If RETURN-CONNECTION is non-nil, a the resulting connection object is returned. Otherwise, the path to the newly-created database.

If OVERWRITE is nil, FILENAME will be overwritten."
  (if (and (file-exists-p filename) (not overwrite))
      (error "File `%s' already exists" filename)
    (let ((db (sqlite-open filename)))
      (unwind-protect
	  (blog-initialize-database db)
	(if (not return-connection) (sqlite-close db)))
      (if return-connection
	  db
	filename))))

(provide 'blog-database)
