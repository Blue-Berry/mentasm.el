;;; test-fixed-processing.el --- Test with fixed file matching -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
;; Force reload to get the fix
(load "mentasm.el" t)

;; Test with correct .file and .loc directives
(let* ((src-file "/home/liam/playground/nbody/qtree/qtree.ml")
       (sample-lines '(
         "	.file	1	\"list.ml\""
         "	.file	2	\"qtree/qtree.ml\""
         "	.loc	2	123	50"
         "	movq	%rdi, %rbx"
         "	.loc	2	9	16"
         "	testq	%rdi, %rdi")))
  
  (message "Testing with fixed file mapping...")
  
  ;; Create a temporary buffer for the source file
  (with-temp-buffer
    (setq buffer-file-name src-file)
    (setq major-mode 'tuareg-mode)
    
    ;; Disable directive filtering so we can see all processing
    (setq-local mentasm-filter-directives nil)
    
    ;; Process the assembly lines
    (let ((processed-lines (mentasm--process-asm-lines (current-buffer) sample-lines)))
      (message "Processed %d lines:" (length processed-lines))
      
      ;; Check each line
      (let ((i 0))
        (dolist (line processed-lines)
          (setq i (1+ i))
          (let ((src-line (get-text-property 0 'mentasm-src-line line)))
            (message "  %d: '%s' -> source line %s" i line src-line)))))))