;;; Debug the debug function -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'mentasm)

(let ((messages nil)
      (mentasm-debug t))
  (message "mentasm-debug value: %s" mentasm-debug)
  (message "bound-and-true-p result: %s" (bound-and-true-p mentasm-debug))
  
  ;; Test debug function directly
  (cl-letf (((symbol-function 'message)
             (lambda (format-string &rest args)
               (let ((msg (apply #'format format-string args)))
                 (push msg messages)
                 (message "Captured: %s" msg)))))
    (mentasm--debug "Test message: %s" "hello")
    (message "Messages collected: %s" messages)))