;;; test-regex.el --- Test regex matching -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'mentasm)

(let ((test-line "	.loc	1	120	18"))
  (message "Testing line: '%s'" test-line)
  (message "mentasm-source-tag regex: %s" mentasm-source-tag)
  
  (if (string-match mentasm-source-tag test-line)
      (progn
        (message "✓ Regex matches!")
        (message "  Match 1 (file): %s" (match-string 1 test-line))
        (message "  Match 2 (line): %s" (match-string 2 test-line)))
    (message "✗ Regex does not match")))