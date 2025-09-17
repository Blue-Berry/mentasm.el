;;; test-mentasm.el --- Tests for mentasm.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple tests for mentasm.el functionality

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'mentasm)

;; Test helper functions
(defun test-mentasm-assert (condition message)
  "Assert CONDITION with MESSAGE."
  (unless condition
    (error "Test failed: %s" message)))

(defun test-mentasm-setup-temp-files ()
  "Set up temporary test files."
  (let ((temp-dir (make-temp-file "mentasm-test" t)))
    (list
     ;; Create a mock dune-project
     (let ((dune-project (expand-file-name "dune-project" temp-dir)))
       (with-temp-file dune-project
         (insert "(lang dune 3.0)\n"))
       dune-project)
     ;; Create a mock _build directory with assembly file
     (let ((build-dir (expand-file-name "_build/default/src" temp-dir)))
       (make-directory build-dir t)
       (let ((asm-file (expand-file-name "test.s" build-dir)))
         (with-temp-file asm-file
           (insert "\t.text\n")
           (insert "\t.globl _start\n")
           (insert "_start:\n")
           (insert "\tmov $60, %rax\n")
           (insert "\tmov $0, %rdi\n")
           (insert "\tsyscall\n"))
         asm-file))
     ;; Create a mock source file
     (let ((src-file (expand-file-name "src/test.ml" temp-dir)))
       (make-directory (file-name-directory src-file) t)
       (with-temp-file src-file
         (insert "let main () = exit 0\n"))
       src-file)
     temp-dir)))

;; Test 1: Test dune project discovery
(defun test-mentasm-find-dune-project ()
  "Test finding dune-project file."
  (let* ((test-files (test-mentasm-setup-temp-files))
         (dune-project (nth 0 test-files))
         (temp-dir (nth 3 test-files)))
    (unwind-protect
        (progn
          ;; Test from project root
          (let ((default-directory temp-dir))
            (test-mentasm-assert
             (string= (mentasm--find-dune-project) temp-dir)
             "Should find dune-project in current directory"))
          
          ;; Test from subdirectory
          (let ((default-directory (expand-file-name "src" temp-dir)))
            (test-mentasm-assert
             (string= (directory-file-name (mentasm--find-dune-project)) 
                      (directory-file-name temp-dir))
             "Should find dune-project by traversing up"))
          
          ;; Test with explicit start-dir
          (test-mentasm-assert
           (string= (directory-file-name (mentasm--find-dune-project (expand-file-name "src" temp-dir))) 
                    (directory-file-name temp-dir))
           "Should find dune-project with explicit start directory"))
      
      ;; Cleanup
      (delete-directory temp-dir t))))

;; Test 2: Test build directory discovery
(defun test-mentasm-find-build-directories ()
  "Test finding _build directories."
  (let* ((test-files (test-mentasm-setup-temp-files))
         (temp-dir (nth 3 test-files)))
    (unwind-protect
        (let ((build-dirs (mentasm--find-build-directories temp-dir)))
          (test-mentasm-assert
           (= (length build-dirs) 1)
           "Should find exactly one _build directory")
          (test-mentasm-assert
           (string= (file-name-nondirectory (car build-dirs)) "_build")
           "Found directory should be named _build"))
      
      ;; Cleanup
      (delete-directory temp-dir t))))

;; Test 3: Test assembly file discovery
(defun test-mentasm-find-assembly-files ()
  "Test finding assembly files in build directories."
  (let* ((test-files (test-mentasm-setup-temp-files))
         (temp-dir (nth 3 test-files)))
    (unwind-protect
        (let* ((build-dirs (mentasm--find-build-directories temp-dir))
               (asm-files (mentasm--find-assembly-files build-dirs)))
          (test-mentasm-assert
           (= (length asm-files) 1)
           "Should find exactly one assembly file")
          (test-mentasm-assert
           (string= (file-name-extension (car asm-files)) "s")
           "Found file should have .s extension"))
      
      ;; Cleanup
      (delete-directory temp-dir t))))

;; Test 4: Test source file component extraction
(defun test-mentasm-get-source-file-components ()
  "Test extracting components from source file paths."
  (let* ((test-file "/home/user/project/src/lib/test.ml")
         (components (mentasm--get-source-file-components test-file))
         (basename (nth 0 components))
         (dirs (nth 1 components)))
    
    (test-mentasm-assert
     (string= basename "test")
     "Should extract basename without extension")
    
    (test-mentasm-assert
     (member "src" dirs)
     "Should include 'src' in directory components")
    
    (test-mentasm-assert
     (member "lib" dirs)
     "Should include 'lib' in directory components")))

;; Test 5: Test assembly file matching
(defun test-mentasm-match-assembly-file ()
  "Test matching source files to assembly files."
  (let* ((test-files (test-mentasm-setup-temp-files))
         (src-file (nth 2 test-files))
         (temp-dir (nth 3 test-files)))
    (unwind-protect
        (let* ((build-dirs (mentasm--find-build-directories temp-dir))
               (asm-files (mentasm--find-assembly-files build-dirs))
               (match (mentasm--match-assembly-file src-file asm-files)))
          (test-mentasm-assert
           match
           "Should find a matching assembly file")
          (test-mentasm-assert
           (string= (file-name-nondirectory match) "test.s")
           "Matched file should have same basename"))
      
      ;; Cleanup
      (delete-directory temp-dir t))))

;; Test 6: Test debug function  
(defun test-mentasm-debug ()
  "Test debug message function."
  ;; Just test that the function doesn't error out
  ;; The debug function relies on global variables which are complex to test
  (test-mentasm-assert
   (condition-case nil
       (progn
         (mentasm--debug "Test message")
         t)
     (error nil))
   "Debug function should not error"))

;; Test runner
(defun run-mentasm-tests ()
  "Run all mentasm tests."
  (interactive)
  (message "Running mentasm tests...")
  
  (condition-case err
      (progn
        (test-mentasm-find-dune-project)
        (message "✓ test-mentasm-find-dune-project passed")
        
        (test-mentasm-find-build-directories)
        (message "✓ test-mentasm-find-build-directories passed")
        
        (test-mentasm-find-assembly-files)
        (message "✓ test-mentasm-find-assembly-files passed")
        
        (test-mentasm-get-source-file-components)
        (message "✓ test-mentasm-get-source-file-components passed")
        
        (test-mentasm-match-assembly-file)
        (message "✓ test-mentasm-match-assembly-file passed")
        
        (test-mentasm-debug)
        (message "✓ test-mentasm-debug passed")
        
        (message "All mentasm tests passed! 🎉"))
    
    (error
     (message "❌ Test failed: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

;; Auto-run tests when loading this file
(when noninteractive
  (run-mentasm-tests))

(provide 'test-mentasm)

;;; test-mentasm.el ends here