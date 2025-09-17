;;; test-full-processing.el --- Test full assembly processing -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'mentasm)

;; Test with more detailed tracing
(let* ((src-file "/home/liam/playground/nbody/qtree/qtree.ml")
       (sample-lines '(
         "	.loc	1	120	18"
         "	movq	%rdi, %rbx"
         "	.loc	1	123	4" 
         "	testq	%rdi, %rdi")))
  
  (message "Testing detailed assembly processing...")
  
  ;; Create a temporary buffer for the source file
  (with-temp-buffer
    (setq buffer-file-name src-file)
    (setq major-mode 'tuareg-mode)
    
    ;; Check default filter settings
    (message "Default filter settings:")
    (message "  filter-directives: %s" (buffer-local-value 'mentasm-filter-directives (current-buffer)))
    (message "  filter-labels: %s" (buffer-local-value 'mentasm-filter-labels (current-buffer)))
    
    ;; Disable directive filtering to see what happens
    (setq-local mentasm-filter-directives nil)
    (message "Disabled directive filtering")
    
    ;; Process the assembly lines
    (let ((processed-lines (mentasm--process-asm-lines (current-buffer) sample-lines)))
      (message "Processed %d lines:" (length processed-lines))
      
      ;; Check each line
      (let ((i 0))
        (dolist (line processed-lines)
          (setq i (1+ i))
          (let ((src-line (get-text-property 0 'mentasm-src-line line)))
            (message "  %d: '%s' -> source line %s" i line src-line)))))))