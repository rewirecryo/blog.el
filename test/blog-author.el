(require 'blog)
(require 'blog-test-utils)


(defun blog-test-init ()
  (setq create-git-repo-script-path (file-name-concat default-directory "test/create-git-repo.sh"))
  (setq work-dir (blog-generate-temporary-directory-path))
  (setq authors-file-path (file-name-concat work-dir "authors.json"))
  (setq was-initialized t)
  (setq repo-path (blog-generate-temporary-directory-path nil nil))
  (let ((git-init-exit-code (call-process create-git-repo-script-path nil '(:file "/dev/stdout") nil work-dir)))
    (if (= 0 git-init-exit-code)
	(setq was-initialized t)))
  (cd work-dir))

(defun blog-test-clean-up ()
  (delete-directory work-dir t))

(defun get-authors-jstring ()
  "[{\"first_name\":\"Science\",\"last_name\":\"Writer\",\"nominal_id\":\"swriter\",\"avatars\":[{\"timestamp\":100,\"images\":[\"swriter_small_old.jpg\",\"swriter_large_old.jpg\"]},{\"timestamp\":100000,\"images\":[\"swriter_small_new.jpg\",\"swriter_large_new.jpg\"]}]},{\"first_name\":\"Politics\",\"last_name\":\"Columnist\",\"nominal_id\":\"pcolumnist\",\"avatars\":[{\"timestamp\":100000,\"images\":[\"pcolumnist_small_old.jpg\",\"pcolumnist_large_old.jpg\"]},{\"timestamp\":120000,\"images\":[\"pcolumnist_small_new.jpg\",\"pcolumnist_large_new.jpg\"]}]}]")

(defun get-authors-length()
  2)

(ert-deftest blog-load-authors-from-buffer-inserts-authors ()
  (with-temp-buffer (insert (get-authors-jstring))
		    (let ((authors (blog-load-authors-from-buffer (current-buffer) 2)))
		      (should (string-equal "swriter" (blog-author-nominal-id (nth 0 authors)))))))

(ert-deftest blog-load-authors-from-buffer-fails-when-root-not-array ()
  (should-error (with-temp-buffer (insert "{}")
				  (blog-load-authors-from-buffer (current-buffer) 2))
		:type 'blog-error))

(ert-deftest blog-load-authors-from-buffer-alist-inserts-authors ()
  (with-temp-buffer (insert (get-authors-jstring))
		    (let ((loaded-author-list (blog-load-authors-from-buffer (current-buffer) 2 t)))
		      (should (plist-get loaded-author-list "swriter" 'string-equal))
		      (should (plist-get loaded-author-list "pcolumnist" 'string-equal)))))

(ert-deftest blog-load-authors-from-buffer-alist-length ()
  (with-temp-buffer (insert (get-authors-jstring))
		    (let ((authors-list (blog-load-authors-from-buffer (current-buffer) (get-authors-length) t)))
		      (= (* (get-authors-length) 2) (length authors-list)))))

(defun get-avatar-sets-jstring ()
  "[{\"timestamp\":100000,\"images\":[\"pcolumnist_small_old.jpg\", \"pcolumnist_large_old.jpg\"]},{\"timestamp\":120000,\"images\":[\"pcolumnist_small_new.jpg\",\"pcolumnist_large_new.jpg\"]}]")

(defun get-avatar-sets-length ()
  2)

(defun get-loaded-avatar-sets ()
  (blog-load-avatar-sets-from-array (json-parse-string (get-avatar-sets-jstring)) (get-avatar-sets-length)))

(ert-deftest blog-load-avatar-sets-from-array-correct-timestamps ()
    (should (alist-get 100000 (get-loaded-avatar-sets)))
    (should (alist-get 120000 (get-loaded-avatar-sets))))

(ert-deftest blog-load-avatar-sets-from-array-alist-correct-timestamps ()
    (should (alist-get 100000 (get-loaded-avatar-sets)))
    (should (alist-get 120000 (get-loaded-avatar-sets))))

(ert-deftest blog-load-avatar-sets-from-array-length ()
  (should (= (get-avatar-sets-length) (length (get-loaded-avatar-sets)))))
    
(ert-deftest blog-load-avatar-sets-from-array-values ()
  (let ((pcolumnist-old-avatar-set (nth 0 (alist-get 100000 (get-loaded-avatar-sets)))))
    (should (string-equal "pcolumnist_small_old.jpg" (nth 0 (alist-get 1 pcolumnist-old-avatar-set))))
    (should (string-equal "pcolumnist_large_old.jpg" (nth 0 (alist-get 2 pcolumnist-old-avatar-set))))))

(ert-deftest blog-load-avatar-sets-from-array-error-on-wrong-number-of-sizes ()
  (should-error (blog-load-avatar-sets-from-array (json-parse-string (get-avatar-sets-jstring)) 999)
		:type 'blog-length-error))

(defun get-avatar-sets-jstring-duplicate-timestamps ()
  "[{\"timestamp\":100000,\"images\":[\"pcolumnist_small_old.jpg\", \"pcolumnist_large_old.jpg\"]},{\"timestamp\":100000,\"images\":[\"pcolumnist_small_new.jpg\",\"pcolumnist_large_new.jpg\"]}]")

(ert-deftest blog-load-avatar-sets-from-array-error-on-duplicate-timestamps ()
  (should-error (blog-load-avatar-sets-from-array (json-parse-string (get-avatar-sets-jstring-duplicate-timestamps))
						  (get-avatar-sets-length))
		:type 'blog-existence-error))

(defun get-authors-file-git-object ()
  (blog-git-object :hash "718ddd430c59cae64c76f2aa485bc373804f6240" :path "authors.json" :mode "100644"))

(ert-deftest blog-load-authors-from-git-object-success ()
  (let ((authors-list (blog-load-authors-from-git-object (get-authors-file-git-object)
							 (get-avatar-sets-length))))
    (should (string-equal "swriter" (blog-author-nominal-id (nth 0 authors-list))))))

(defun get-database-connection ()
  (let ((db (sqlite-open)))
    (blog-initialize-database db '("small" "large"))
    (sqlite-execute db "INSERT INTO avatar_sets (")))
    
