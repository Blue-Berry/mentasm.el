;;; debug-mapping-carefully.el --- Debug mapping more carefully -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(load "mentasm.el" t)

;; Test mentasm-compile and check the mapping in the right buffer
(let ((default-directory "/home/liam/playground/nbody")
      (src-buffer nil))
  (message "Setting up source buffer...")
  
  ;; Create and keep reference to source buffer
  (setq src-buffer (find-file-noselect "/home/liam/playground/nbody/qtree/qtree.ml"))
  
  (with-current-buffer src-buffer
    (message "In source buffer: %s" (buffer-file-name))
    (setq major-mode 'tuareg-mode)
    
    ;; Run mentasm-compile
    (message "Running mentasm-compile...")
    (mentasm-compile)
    
    ;; Check the mapping in THIS buffer (the source buffer)
    (message "Checking mapping in source buffer...")
    (message "  mentasm-mode active: %s" mentasm-mode)
    (message "  mentasm-line-mapping bound: %s" (boundp 'mentasm-line-mapping))
    (message "  mentasm-line-mapping value: %s" 
             (if (boundp 'mentasm-line-mapping)
                 (if mentasm-line-mapping
                     (format "hash table with %d entries" (hash-table-count mentasm-line-mapping))
                   "nil")
               "unbound"))
    
    ;; If mapping exists, show some entries
    (when (and (boundp 'mentasm-line-mapping) mentasm-line-mapping)
      (message "  Sample mappings:")
      (let ((count 0))
        (maphash (lambda (src-line asm-ranges)
                   (when (< count 5)
                     (message "    source line %s -> assembly lines %s" src-line asm-ranges)
                     (setq count (1+ count))))
                 mentasm-line-mapping)))
    
    ;; Also check if output buffer was created
    (let ((output-buf (get-buffer "*mentasm-output*")))
      (message "  Output buffer exists: %s" (if output-buf "yes" "no"))
      (when output-buf
        (message "  Output buffer size: %d" (buffer-size output-buf))))))