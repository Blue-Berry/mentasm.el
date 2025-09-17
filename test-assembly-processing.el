;;; test-assembly-processing.el --- Test assembly processing -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'mentasm)

;; Test processing a few lines of the real assembly file
(let* ((asm-file "/home/liam/playground/nbody/_build/default/qtree/.qtree.objs/native/qtree.s")
       (src-file "/home/liam/playground/nbody/qtree/qtree.ml")
       (sample-lines '(
         "	.loc	1	120	18"
         "	movq	%rdi, %rbx"
         "	.loc	1	123	4" 
         "	testq	%rdi, %rdi"
         "	.loc	2	123	50"
         "	jne	.L100")))
  
  (message "Testing assembly processing...")
  
  ;; Create a temporary buffer for the source file
  (with-temp-buffer
    (setq buffer-file-name src-file)
    (setq major-mode 'tuareg-mode)  ; Just set the major mode directly
    
    (message "Source buffer created with file: %s" buffer-file-name)
    (message "Major mode: %s" major-mode)
    
    ;; Process the assembly lines
    (let ((processed-lines (mentasm--process-asm-lines (current-buffer) sample-lines)))
      (message "Original lines: %d" (length sample-lines))
      (message "Processed lines: %d" (length processed-lines))
      
      ;; Check if any lines have source line properties
      (dolist (line processed-lines)
        (let ((src-line (get-text-property 0 'mentasm-src-line line)))
          (when src-line
            (message "Line '%s' -> source line %s" line src-line)))))))