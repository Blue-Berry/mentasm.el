;;; debug-filters.el --- Debug filter settings -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(load "mentasm.el" t)

;; Check default filter settings and processing
(let ((default-directory "/home/liam/playground/nbody"))
  (with-temp-buffer
    (let ((src-file "/home/liam/playground/nbody/qtree/qtree.ml"))
      (setq buffer-file-name src-file)
      (setq major-mode 'tuareg-mode)
      
      (message "Checking filter settings...")
      (message "  filter-directives (default): %s" mentasm-filter-directives)
      (message "  filter-labels (default): %s" mentasm-filter-labels)  
      (message "  filter-comment-only (default): %s" mentasm-filter-comment-only)
      
      ;; Test with a sample of real assembly lines
      (let ((sample-lines '(
        "camlQtree.fold_left_2611:"
        "	.file	1	\"list.ml\""
        "	.loc	1	120	18"
        "	.cfi_startproc"
        "	leaq	-344(%rsp), %r10"
        "	cmpq	40(%r14), %r10"
        "	jb	.L106"
        "	subq	$24, %rsp"
        "	movq	%rdi, %rdx"
        )))
        
        (message "Testing with default filters...")
        (let ((processed-default (mentasm--process-asm-lines (current-buffer) sample-lines)))
          (message "  Input lines: %d" (length sample-lines))
          (message "  Output lines: %d" (length processed-default))
          (dolist (line processed-default)
            (let ((src-line (get-text-property 0 'mentasm-src-line line)))
              (message "    '%s' -> source %s" line src-line))))
        
        (message "Testing with filters disabled...")
        (setq-local mentasm-filter-directives nil)
        (setq-local mentasm-filter-labels nil)
        (setq-local mentasm-filter-comment-only nil)
        (let ((processed-no-filter (mentasm--process-asm-lines (current-buffer) sample-lines)))
          (message "  Input lines: %d" (length sample-lines))
          (message "  Output lines: %d" (length processed-no-filter))
          (dolist (line processed-no-filter)
            (let ((src-line (get-text-property 0 'mentasm-src-line line)))
              (message "    '%s' -> source %s" line src-line))))
      ))))