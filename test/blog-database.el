(require 'blog)
(require 'blog-database)
(require 'blog-test-utils)

(defmacro blog-test-validate-tables (db)
  "Runs queries to ensure that database DB has all the tables a blog
database should have."
  '(progn
     (sqlite-select db "SELECT * FROM screen_sizes;")
     (sqlite-select db "SELECT * FROM authors;")
     (sqlite-select db "SELECT * FROM avatar_sets;")
     (sqlite-select db "SELECT * FROM avatars;")
     (sqlite-select db "SELECT * FROM files;")
     (sqlite-select db "SELECT * FROM posts;")
     (sqlite-select db "SELECT * FROM authors_file_hash_table;")))

(ert-deftest blog-test-initialize-database-tables ()
  "Test whether (blog-initialize-database) creates all the necessary tables."
  (let ((db (sqlite-open)))
    (unwind-protect
	(progn
	  (blog-initialize-database db '("small" "big"))
	  (blog-test-validate-tables db))
      (sqlite-close db))))

(ert-deftest blog-test-initialize-database-tables-incorrect ()
  "Test whether (blog-initialize-database) creates all the necessary tables."
  (let ((db (sqlite-open)))
    (unwind-protect
	(progn
	  (blog-initialize-database db '("small" "big"))
	  (should-error
	   (sqlite-select db "SELECT * FROM nonexistent_table;")
	   :type 'sqlite-error))
      (sqlite-close db))))

(ert-deftest blog-create-database ()
  "Test whether blog-create-database creates a database on the filesystem, with the necessary tables."
  (let* ((testdir (blog-make-temporary-directory nil nil))
	 (dbfile (concat testdir "blog.db"))
	 (db nil))
    (unwind-protect
	(progn
	  (blog-create-database (concat testdir "blog.db") '("small" "big"))
	  (setq db (sqlite-open dbfile))
	  (unwind-protect
	      (progn
		(blog-test-validate-tables db))
	    (sqlite-close db)))
      (delete-directory testdir t))))

(ert-deftest blog-create-database-incorrect ()
  "Test whether blog-create-database creates a database on the filesystem, with the necessary tables."
  (let* ((testdir (blog-make-temporary-directory nil nil))
	 (dbfile (concat testdir "blog.db"))
	 (db nil))
    (unwind-protect
	(progn
	  (blog-create-database (concat testdir "blog.db") '("small" "big"))
	  (setq db (sqlite-open dbfile))
	  (unwind-protect
	      (should-error
	       (sqlite-select db "SELECT * FROM nonexistent_table;")
	       :type 'sqlite-error)
	    (sqlite-close db)))
      (delete-directory testdir t))))
