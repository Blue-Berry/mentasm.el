;;; debug-overlays-with-mapping.el --- Debug overlays with actual mappings -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(load "mentasm.el" t)

;; Test the overlay system with lines that have mappings
(let ((default-directory "/home/liam/playground/nbody")
      (src-buffer nil))
  
  ;; Create source buffer  
  (setq src-buffer (find-file-noselect "/home/liam/playground/nbody/qtree/qtree.ml"))
  
  (with-current-buffer src-buffer
    (message "Testing overlays with actual mappings...")
    
    ;; Run mentasm-compile
    (setq major-mode 'tuareg-mode)
    (mentasm-compile)
    
    ;; Find lines that actually have mappings
    (message "Lines with mappings:")
    (let ((mapped-lines nil))
      (maphash (lambda (src-line asm-ranges)
                 (push src-line mapped-lines))
               mentasm-line-mapping)
      (setq mapped-lines (sort mapped-lines #'<))
      
      ;; Show first few mapped lines
      (let ((count 0))
        (dolist (line mapped-lines)
          (when (< count 5)
            (message "  Line %s -> %s" line (gethash line mentasm-line-mapping))
            (setq count (1+ count)))))
      
      ;; Test overlay with the first mapped line
      (when mapped-lines
        (let ((test-line (car mapped-lines)))
          (message "Testing overlay with line %s..." test-line)
          (goto-char (point-min))
          (forward-line (1- test-line))  ; goto that line
          (message "  Moved to line %s (pos %s)" (line-number-at-pos) (point))
          
          ;; Try overlay update
          (mentasm-update-overlays :force t)
          (message "  Overlay update completed")
          
          ;; Check for overlays
          (let ((overlays (overlays-in (point-min) (point-max))))
            (message "  Total overlays: %d" (length overlays))
            (dolist (overlay overlays)
              (when (overlay-get overlay 'mentasm-src)
                (message "    mentasm overlay: start=%s end=%s" 
                         (overlay-start overlay) (overlay-end overlay)))))))
      
      ;; Also check the output buffer for overlays
      (let ((output-buf (get-buffer "*mentasm-output*")))
        (when output-buf
          (with-current-buffer output-buf
            (let ((asm-overlays (overlays-in (point-min) (point-max))))
              (message "  Assembly buffer overlays: %d" (length asm-overlays))
              (dolist (overlay asm-overlays)
                (when (overlay-get overlay 'mentasm-asm)
                  (message "    asm overlay: start=%s end=%s" 
                           (overlay-start overlay) (overlay-end overlay))))))))
      )))