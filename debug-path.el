;; Debug path handling
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(let* ((temp-dir (make-temp-file "test" t))
       (src-dir (expand-file-name "src" temp-dir)))
  (make-directory src-dir)
  
  (message "temp-dir: '%s'" temp-dir)
  (message "src-dir: '%s'" src-dir)
  (message "directory-file-name temp-dir: '%s'" (directory-file-name temp-dir))
  (message "directory-file-name src-dir: '%s'" (directory-file-name src-dir))
  (message "file-name-directory src-dir: '%s'" (file-name-directory src-dir))
  (message "file-name-directory (directory-file-name src-dir): '%s'" 
           (file-name-directory (directory-file-name src-dir)))
  (message "directory-file-name of that: '%s'" 
           (directory-file-name (file-name-directory (directory-file-name src-dir))))
  
  (delete-directory temp-dir t))