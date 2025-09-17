;;; debug-overlays.el --- Debug overlay system -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(load "mentasm.el" t)

;; Test the overlay system
(let ((default-directory "/home/liam/playground/nbody")
      (src-buffer nil))
  
  ;; Create source buffer  
  (setq src-buffer (find-file-noselect "/home/liam/playground/nbody/qtree/qtree.ml"))
  
  (with-current-buffer src-buffer
    (message "Testing overlay system...")
    
    ;; Run mentasm-compile
    (setq major-mode 'tuareg-mode)
    (mentasm-compile)
    
    ;; Check overlay settings
    (message "Overlay settings:")
    (message "  mentasm-use-overlays: %s" mentasm-use-overlays)
    (message "  buffer-modified-p: %s" (buffer-modified-p))
    (message "  mentasm-mode active: %s" mentasm-mode)
    
    ;; Check output buffer connection
    (let ((output-buffer (get-buffer mentasm-output-buffer)))
      (when output-buffer
        (message "  output buffer src-buffer: %s" 
                 (buffer-local-value 'mentasm-src-buffer output-buffer))
        (message "  current buffer same as src-buffer: %s" 
                 (eq (current-buffer) 
                     (buffer-local-value 'mentasm-src-buffer output-buffer))))
      
      ;; Test overlay update manually
      (message "Testing manual overlay update...")
      (goto-char (point-min))
      (forward-line 10)  ; Go to line 11
      (let ((current-line (line-number-at-pos)))
        (message "  Current line: %s" current-line)
        (message "  Line mapping for line %s: %s" 
                 current-line 
                 (gethash current-line mentasm-line-mapping))
        
        ;; Try to run update-overlays manually
        (mentasm-update-overlays :force t)
        (message "  Manual overlay update completed")
        
        ;; Check if any overlays were created
        (let ((overlays (overlays-in (point-min) (point-max))))
          (message "  Total overlays in buffer: %d" (length overlays))
          (dolist (overlay overlays)
            (when (overlay-get overlay 'mentasm-src)
              (message "    Found mentasm overlay: %s" overlay))))))))