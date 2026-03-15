(setq blog/hash-algorithm 'sha512)

(defun blog/hash (object &optional start end binary)
  "Execute (secure-hash) with blog/hash-algorithm as the algorithm, on
OBJECT.

START, END and BINARY are passed to (secure-hash), too."
  (secure-hash blog/hash-algorithm object start end binary))

(provide 'blog-base)
