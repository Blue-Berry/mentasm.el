;;; debug-file-matching-direct.el --- Debug file path matching directly -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
;; Force reload
(load "mentasm.el" t)

(let* ((src-file "/home/liam/playground/nbody/qtree/qtree.ml")
       (local-src-file-name (file-local-name src-file))
       (assembly-file-path "qtree/qtree.ml"))
  
  (message "Testing file path matching directly...")
  (message "  src-file: %s" src-file)
  (message "  local-src-file-name: %s" local-src-file-name)
  (message "  assembly-file-path: %s" assembly-file-path)
  
  ;; Test each condition separately
  (message "  file-remote-p src: %s" (file-remote-p src-file))
  (message "  file-remote-p asm: %s" (file-remote-p assembly-file-path))
  (message "  file-equal-p: %s" (ignore-errors (file-equal-p src-file assembly-file-path)))
  (message "  file-local-name equal: %s" (equal local-src-file-name (file-local-name assembly-file-path)))
  (message "  file-name-absolute-p asm: %s" (file-name-absolute-p assembly-file-path))
  (message "  string-suffix-p: %s" (string-suffix-p (concat "/" assembly-file-path) src-file))
  
  ;; Test the full function
  (message "  mentasm--file-equal-p result: %s" 
           (mentasm--file-equal-p local-src-file-name assembly-file-path)))