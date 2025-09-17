;;; debug-file-matching.el --- Debug file path matching -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'mentasm)

(let* ((src-file "/home/liam/playground/nbody/qtree/qtree.ml")
       (local-src-file-name (file-local-name src-file))
       (assembly-file-path "qtree/qtree.ml"))
  
  (message "Testing file path matching...")
  (message "  src-file: %s" src-file)
  (message "  local-src-file-name: %s" local-src-file-name)
  (message "  assembly-file-path: %s" assembly-file-path)
  
  ;; Test the file equality check
  (message "  file-equal-p result: %s" 
           (mentasm--file-equal-p local-src-file-name assembly-file-path)))