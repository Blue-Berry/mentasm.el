;;; test-debug.el --- Debug mentasm functions -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'mentasm)

;; Simple debug test
(let* ((temp-dir (make-temp-file "mentasm-test" t))
       (dune-project (expand-file-name "dune-project" temp-dir)))
  
  ;; Create dune-project file
  (with-temp-file dune-project
    (insert "(lang dune 3.0)\n"))
  
  (message "Created temp dir: %s" temp-dir)
  (message "Created dune-project: %s" dune-project)
  (message "File exists: %s" (file-exists-p dune-project))
  
  ;; Test from project root
  (let ((default-directory temp-dir))
    (message "Testing from project root...")
    (message "Current default-directory: %s" default-directory)
    (let ((result (mentasm--find-dune-project)))
      (message "Result: %s" result)
      (message "Expected: %s" temp-dir)
      (message "Equal: %s" (string= result temp-dir))))
  
  ;; Test from subdirectory
  (let ((src-dir (expand-file-name "src" temp-dir)))
    (make-directory src-dir)
    (let ((default-directory src-dir))
      (message "Testing from subdirectory...")
      (message "Current default-directory: %s" default-directory)
      (let ((result (mentasm--find-dune-project)))
        (message "Result: %s" result)
        (message "Expected: %s" temp-dir)
        (message "Equal: %s" (string= result temp-dir)))))
  
  ;; Cleanup
  (delete-directory temp-dir t))