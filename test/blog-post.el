(require 'blog-post)
(require 'blog-test-utils)

(defun blog-test-init ()
  (setq test-dir (blog-make-temporary-directory))
  (setq was-initialized t))

(defun blog-test-clean-up ()
  (delete-directory test-dir t))

(defun get-example-authors-list ()
  (list (make-instance 'blog-author
		       :nominal-id "adventure-blogger"
		       :first-name "Adventure"
		       :last-name "Blogger")))

(defun get-example-post ()
  (blog-post :content "I spent the day hiking, fishing and stargazing. It was great."))

(defun get-example-post-text ()
  "** Outdoor Adventure                                             :blog_post:
:PROPERTIES:
:nominal_id: outdoor-adventure-12345
:author:   adventure-blogger
:stub:     outdoor-adventure
:date_published: <2026-01-01 12:34>
:date_modified: <2026-01-01 23:45>
:subtitle: A day spent outside
:END:
I spent the day hiking, fishing and stargazing. It was great.")

(defun get-example-post-text-no-tag ()
  "** Outdoor Adventure
:PROPERTIES:
:nominal_id: outdoor-adventure-12345
:author:   adventure-blogger
:stub:     outdoor-adventure
:date_published: <2026-01-01 12:34>
:date_modified: <2026-01-01 23:45>
:subtitle: A day spent outside
:END:
I spent the day hiking, fishing and stargazing. It was great.")

(ert-deftest blog-org-timestamp-string-to-unix-timestamp-test-date-only ()
  (setenv "TZ" "GMT")
  (should (= (blog-org-timestamp-string-to-unix-timestamp "<2026-01-01>")
	     1767225600)))

(ert-deftest blog-org-timestamp-string-to-unix-timestamp-test-time-and-date ()
  (setenv "TZ" "GMT")
  (should (= (blog-org-timestamp-string-to-unix-timestamp "<2026-01-01 5:00>")
	     1767243600)))

(ert-deftest blog-post-at-point-p-test-valid-post ()
  (with-temp-buffer (insert (get-example-post-text))
		    (org-mode)
		    (goto-char (point-min))
		    (should (blog-post-at-point-p))))


(ert-deftest blog-post-at-point-p-test-invalid-post ()
  (with-temp-buffer (insert (get-example-post-text-no-tag))
		    (org-mode)
		    (goto-char (point-min))
		    (should-not (blog-post-at-point-p))))

(ert-deftest blog-get-stub-at-point-test-success ()
  (with-temp-buffer (org-mode)
		    (insert (get-example-post-text))
		    (goto-char (point-min))
		    (should (string-equal (blog-get-stub-at-point)
					  "outdoor-adventure"))))

(ert-deftest blog-parse-post-at-point-test-sets-values ()
  (with-temp-buffer (org-mode)
		    (insert (get-example-post-text))
		    (goto-char (point-min))
		    (setenv "TZ" "GMT")
		    (let* ((blog-hash-algorithm 'sha256)
			   (post (blog-parse-post-at-point (get-example-authors-list)
							   nil
							   nil
							   "posts.org")))
		      (should (string-equal (blog-post-nominal-id post) "outdoor-adventure-12345"))
		      (should (string-equal (blog-post-stub post) "outdoor-adventure"))
		      (should (string-equal (blog-author-nominal-id (blog-post-author post)) "adventure-blogger"))
		      (should (= (time-convert (blog-post-date-published post) 'integer) 1767270840))
		      (should (= (time-convert (blog-post-date-modified post) 'integer) 1767311100))
		      (should (string-equal (blog-post-subtitle post) "A day spent outside"))
		      (should (string-equal (blog-post-content post) "I spent the day hiking, fishing and stargazing. It was great."))
		      (should (string-equal (blog-post-src-file-path post) "posts.org"))
		      (should (string-equal (blog-post-hash post) "2d199145ec25cd7c27d335f706bcff1f577b24a2422e1cdb2833dc138be10d83")))))

(ert-deftest blog-parse-post-at-point-test-check-entry-validity ()
    (with-temp-buffer (insert (get-example-post-text-no-tag))
		      (org-mode)
		      (should-error (blog-parse-post-at-point (get-example-authors-list)
							      nil))))

(ert-deftest blog-parse-post-at-point-test-dont-check-entry-validity ()
    (with-temp-buffer (insert (get-example-post-text-no-tag))
		      (org-mode)
		      (should (blog-parse-post-at-point (get-example-authors-list)
							t))))

(ert-deftest blog-read-posts-from-buffer-test ()
  (with-temp-buffer (insert (get-example-post-text))
		    (org-mode)
		    (let ((post-list (blog-read-posts-from-buffer (current-buffer)
								  (get-example-authors-list))))
		      (should (string-equal (blog-post-nominal-id (nth 0 post-list))
					    "outdoor-adventure-12345")))))

(ert-deftest blog-read-posts-from-buffer-test-src-file-path ()
  (with-temp-buffer (insert (get-example-post-text))
		    (org-mode)
		    (let ((post-list (blog-read-posts-from-buffer (current-buffer)
								  (get-example-authors-list)
								  nil
								  ()
								  "posts.org")))
		      (should (string-equal (blog-post-src-file-path (nth 0 post-list))
					    "posts.org")))))


(ert-deftest blog-read-posts-from-buffer-test-error-if-already-exists ()
  (with-temp-buffer (insert (get-example-post-text))
		    (org-mode)
		    (should-error (blog-read-posts-from-buffer (current-buffer)
							       (get-example-authors-list)
							       t
							       (list (list "outdoor-adventure-12345" (get-example-post-text))))
				  :type 'blog-existence-error)))

;; End-to-end test
(ert-deftest blog-post-publish-test-success ()
  "End-to-end test for whether the function writes to the file it's
supposed to."
  (let ((filename (file-name-concat test-dir "new_post.html")))
    (blog-post-publish (get-example-post) filename)
    (should (file-exists-p filename))))

;; End-to-end test
(ert-deftest blog-post-publish-test-fail-on-overwrite ()
  (let ((filename (file-name-concat test-dir "existing_post_fail.html")))
    (with-temp-file filename (insert "test"))
    (should-error (blog-post-publish (get-example-post) filename nil)
		  :type 'blog-existence-error)))

;; End-to-end test
(ert-deftest blog-post-publish-test-allow-overwrite ()
  (let ((filename (file-name-concat test-dir "existing_post_okay.html")))
    (with-temp-file filename (insert "test"))
    (should (blog-post-publish (get-example-post) filename t))))
