;;; debug-qtree-mapping.el --- Debug qtree.ml mapping -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(load "mentasm.el" t)

;; Test with the exact sequence for qtree.ml
(let ((default-directory "/home/liam/playground/nbody"))
  (with-temp-buffer
    (let ((src-file "/home/liam/playground/nbody/qtree/qtree.ml"))
      (setq buffer-file-name src-file)
      (setq major-mode 'tuareg-mode)
      
      ;; Test with the EXACT sequence from the assembly file
      (let ((qtree-lines '(
        "	.file	2	\"qtree/qtree.ml\""
        "	.loc	2	123	50"
        "	movq	8(%rbx), %rdi"
        "	.loc	2	123	43"
        "	movq	(%rbx), %rbx"
        "	movq	%rdx, %rsi"
        )))
        
        (message "Testing qtree.ml line mapping...")
        (setq-local mentasm-filter-directives nil)  ; Keep .loc directives for debugging
        
        (let ((processed (mentasm--process-asm-lines (current-buffer) qtree-lines)))
          (message "  Input lines: %d" (length qtree-lines))
          (message "  Output lines: %d" (length processed))
          (let ((i 0))
            (dolist (line processed)
              (setq i (1+ i))
              (let ((src-line (get-text-property 0 'mentasm-src-line line)))
                (message "    %d: '%s' -> source %s" i line src-line)))))
      ))))