;;; test-opcode.el --- Test opcode detection -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'mentasm)

(let ((test-lines '("	movq	%rdi, %rbx"
                    "	testq	%rdi, %rdi"
                    "	jne	.L100"
                    "	.loc	1	120	18")))
  
  (dolist (line test-lines)
    (message "Testing line: '%s'" line)
    (message "  has-opcode-p: %s" (mentasm--has-opcode-p line))
    (message "")))