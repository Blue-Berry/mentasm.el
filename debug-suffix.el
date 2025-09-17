;;; debug-suffix.el --- Debug suffix matching -*- lexical-binding: t; -*-

(let* ((src-path "/home/liam/playground/nbody/qtree/qtree.ml")
       (target-path "qtree/qtree.ml")
       (test-suffix (concat "/" target-path)))
  
  (message "Testing suffix matching...")
  (message "  src-path: '%s'" src-path)
  (message "  target-path: '%s'" target-path)
  (message "  test-suffix: '%s'" test-suffix)
  (message "  string-suffix-p result: %s" (string-suffix-p test-suffix src-path))
  (message "  file-name-absolute-p: %s" (file-name-absolute-p target-path)))