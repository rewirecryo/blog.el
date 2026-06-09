#!/bin/sh

EMACS=/usr/bin/emacs

TEST_FILE=$1

${EMACS} -batch -l eieio -l ert -l test/load-path.el -l ${TEST_FILE} --exec "(if (functionp 'blog-test-init) (progn (setq was-initialized nil) (blog-test-init) (if (not was-initialized) (progn (message \"(blog-test-init) failed.\") (kill-emacs)))))" -f ert-run-tests-batch --exec "(if (functionp 'blog-test-clean-up) (blog-test-clean-up))"
