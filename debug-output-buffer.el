;;; debug-output-buffer.el --- Debug output buffer properties -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(load "mentasm.el" t)

;; Test what happens in the actual output buffer
(let ((default-directory "/home/liam/playground/nbody"))
  (with-temp-buffer
    (let ((src-file "/home/liam/playground/nbody/qtree/qtree.ml"))
      (setq buffer-file-name src-file)
      (setq major-mode 'tuareg-mode)
      (insert ";; Sample OCaml content\n")
      
      (message "Running mentasm-compile...")
      (mentasm-compile)
      
      ;; Check the output buffer more carefully
      (let ((output-buf (get-buffer "*mentasm-output*")))
        (when output-buf
          (with-current-buffer output-buf
            (message "Output buffer analysis:")
            (message "  Buffer size: %d" (buffer-size))
            (message "  Major mode: %s" major-mode)
            
            ;; Look for specific patterns that should have properties
            (goto-char (point-min))
            (let ((found-instructions 0)
                  (found-with-props 0))
              
              ;; Search for instruction patterns
              (while (re-search-forward "\\s-+\\(mov\\|lea\\|cmp\\|j[a-z]*\\|sub\\|add\\)" nil t)
                (setq found-instructions (1+ found-instructions))
                (let* ((line-start (line-beginning-position))
                       (line-end (line-end-position))
                       (line-text (buffer-substring-no-properties line-start line-end))
                       (src-line (get-text-property line-start 'mentasm-src-line)))
                  (when src-line
                    (setq found-with-props (1+ found-with-props))
                    (message "  Found: '%s' -> line %s" line-text src-line)))
                
                ;; Stop after checking first 10 instructions
                (when (>= found-instructions 10)
                  (goto-char (point-max))))
              
              (message "  Instructions found: %d" found-instructions)
              (message "  With source properties: %d" found-with-props)
              
              ;; Also check if mentasm-line-mapping exists in the source buffer
              (message "Checking source buffer...")
              (with-temp-buffer
                (setq buffer-file-name src-file)
                (setq major-mode 'tuareg-mode)
                (when (boundp 'mentasm-line-mapping)
                  (message "  mentasm-line-mapping exists: %s" 
                           (if mentasm-line-mapping 
                               (format "hash table with %d entries" (hash-table-count mentasm-line-mapping))
                               "nil")))))))))))