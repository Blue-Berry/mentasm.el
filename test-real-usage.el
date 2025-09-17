;;; test-real-usage.el --- Test real mentasm usage -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(load "mentasm.el" t)

;; Test the actual usage scenario
(let ((default-directory "/home/liam/playground/nbody"))
  (message "Testing real mentasm usage...")
  
  ;; Simulate opening the source file
  (with-temp-buffer
    (let ((src-file "/home/liam/playground/nbody/qtree/qtree.ml"))
      (setq buffer-file-name src-file)
      (setq major-mode 'tuareg-mode)
      (insert ";; Dummy OCaml content\nlet x = 42\n")
      
      (message "Source buffer setup:")
      (message "  buffer-file-name: %s" buffer-file-name)
      (message "  major-mode: %s" major-mode)
      
      ;; Call mentasm-compile
      (message "Calling mentasm-compile...")
      (mentasm-compile)
      
      ;; Check what happened
      (message "After mentasm-compile:")
      (message "  mentasm-mode active: %s" mentasm-mode)
      (message "  mentasm-line-mapping: %s" (boundp 'mentasm-line-mapping))
      (when (boundp 'mentasm-line-mapping)
        (message "  line-mapping hash table size: %s" 
                 (if mentasm-line-mapping (hash-table-count mentasm-line-mapping) "nil")))
      
      ;; Check if output buffer was created
      (let ((output-buf (get-buffer "*mentasm-output*")))
        (message "  output buffer exists: %s" (if output-buf "yes" "no"))
        (when output-buf
          (with-current-buffer output-buf
            (message "  output buffer size: %d chars" (buffer-size))
            (message "  output buffer mode: %s" major-mode)
            (message "  first few lines:")
            (let ((lines (split-string (buffer-string) "\n")))
              (dotimes (i (min 5 (length lines)))
                (message "    %d: %s" (1+ i) (nth i lines)))))))
      
      ;; Test if any line properties exist
      (when (get-buffer "*mentasm-output*")
        (with-current-buffer "*mentasm-output*"
          (goto-char (point-min))
          (let ((found-props nil)
                (line-count 0))
            (while (and (< line-count 20) (not (eobp)))
              (let ((line (buffer-substring-no-properties 
                          (line-beginning-position) 
                          (line-end-position))))
                (when (> (length line) 0)
                  (let ((src-line (get-text-property (line-beginning-position) 'mentasm-src-line)))
                    (when src-line
                      (setq found-props t)
                      (message "  Found property at line %d: '%s' -> source %s" 
                               (1+ line-count) line src-line)))))
              (forward-line 1)
              (setq line-count (1+ line-count)))
            (message "  Found line properties: %s" found-props))))
      
      (current-buffer))))