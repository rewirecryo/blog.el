(define-error 'blog-error "blog.el error")
(define-error 'blog-existence-error "Existence error" 'blog-error)
(define-error 'blog-length-error "Length error" 'blog-error)
(provide 'blog-errors)
