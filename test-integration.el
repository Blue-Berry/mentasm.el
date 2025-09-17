;;; test-integration.el --- Integration test -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(load "mentasm.el" t)

;; Test the full integration with a real file
(let ((default-directory "/home/liam/playground/nbody"))
  (message "Testing integration from nbody project...")
  
  ;; Test finding the correct assembly file
  (let* ((src-file "qtree/qtree.ml")
         (project-root (mentasm--find-dune-project))
         (build-dirs (mentasm--find-build-directories project-root))
         (assembly-files (mentasm--find-assembly-files build-dirs))
         (matching-asm (mentasm--match-assembly-file src-file assembly-files)))
    
    (message "Project root: %s" project-root)
    (message "Found %d assembly files" (length assembly-files))
    (message "Matching assembly: %s" matching-asm)
    
    (when matching-asm
      (message "✓ Assembly file discovery working!")
      (message "To use: Open %s in Emacs and run M-x mentasm-compile" 
               (expand-file-name src-file project-root)))))