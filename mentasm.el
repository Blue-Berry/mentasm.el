;;; mentasm.el --- OCaml assembly viewer -*- lexical-binding: t; -*-

;;; Commentary:

;; Mentasm is a package to provide assembly output for OCaml source files.
;; It finds pre-compiled assembly files in dune build directories.
;;
;; It only supports OCaml.
;;
;; It works by:
;; 1. Finding pre-compiled assembly files in dune _build directories
;; 2. Parsing assembly to create a map from it to the original OCaml source
;; 3. Stripping out unneeded information to only show useful code
;; 4. Providing an interface for highlighting the matched assembly line
;; to the source and vice versa
;;

;;;; Tweakables:

;; RMSBolt is primarily configured with Emacs local variables.  This lets you
;; change compiler and mentasm options simply by editing a local variable block.
;;
;; Notable options:
;; `mentasm-command': determines the prefix of the compilation command to use.
;; `mentasm-default-directory': determines the default-drectory to compile from.
;; `mentasm-disassemble': disassemble from a compiled binary with objdump, if supported.
;; `mentasm-filter-*': Tweak filtering of binary output.
;; `mentasm-asm-format': Choose between intel att, and other syntax if supported.
;; `mentasm-demangle': Demangle the output, if supported.
;;
;; For more advanced configuration (to the point where you can override almost
;; all of RMSbolt yourself), you can set `mentasm-language-descriptor' with a
;; replacement language spec.
;;
;; Please see the readme at https://gitlab.com/jgkamat/mentasm for
;; more information!
;;
;; Tramp support notes:
;; - Compilation occurs on a remote system, via Tramp, when default-directory is
;;   a remote path.  default-directory will be remote when mentasm is invoked on
;;   a buffer backed by a remote path.
;; - Paths used in externally-run command lines must use the local component of a
;;   path so they can run on a remote system.  `mentasm--with-local-files` takes
;;   care of commonly used paths that typically need to be local (`src-filename` and
;;   `output-filename`), though `file-local-name` should be used in other cases.
;; - One temporary directory exists per remote so that intermediate files are
;;   created on the system performing the compilation.
;;
;; Thanks:
;; Inspiration and some assembly parsing logic was adapted from Matt Godbolt's
;; compiler-explorer: https://github.com/mattgodbolt/compiler-explorer and
;; Jonas Konrad's javap: https://github.com/yawkat/javap.

;;; Requires:

(require 'cl-lib)
(eval-when-compile (require 'subr-x))
(require 'map)
(require 'cc-defs)
(require 'compile)
(require 'disass)
(require 'json)


;;; Code:

;;;; Utility Functions

(defun mentasm-split-rm-single (cmd arg &optional pred)
  "Remove ARG from CMD string. PRED is optional predicate function."
  (if pred
      (replace-regexp-in-string (concat "\\b" (regexp-quote arg) "\\S-*\\b") "" cmd)
    (replace-regexp-in-string (concat "\\b" (regexp-quote arg) "\\b") "" cmd)))

(defun mentasm-split-rm-double (cmd arg)
  "Remove ARG and its value from CMD string."
  (replace-regexp-in-string (concat "\\b" (regexp-quote arg) "\\s-+\\S-+\\b") "" cmd))


;;;; File Discovery Functions

(defun mentasm--debug (format-string &rest args)
  "Debug message function for mentasm."
  (when (bound-and-true-p mentasm-debug)
    (apply #'message (concat "mentasm: " format-string) args)))

(defun mentasm--find-dune-project (&optional start-dir)
  "Find the dune-project file by traversing up the directory tree.
START-DIR is the directory to start searching from, defaults to current buffer directory."
  (let ((dir (directory-file-name (or start-dir
                                      (when buffer-file-name
                                        (file-name-directory buffer-file-name))
                                      default-directory))))
    (catch 'found
      (while dir
        (let ((dune-project (expand-file-name "dune-project" dir)))
          (when (file-exists-p dune-project)
            (throw 'found dir)))
        (let ((parent (file-name-directory dir)))
          (setq dir (unless (equal dir parent) (directory-file-name parent)))))
      nil)))

(defun mentasm--find-build-directories (project-root)
  "Find all _build directories under PROJECT-ROOT."
  (let ((build-dirs nil))
    (when (file-directory-p project-root)
      (dolist (item (directory-files project-root t "^[^.]"))
        (cond
         ((and (file-directory-p item)
               (string= (file-name-nondirectory item) "_build"))
          (push item build-dirs))
         ((file-directory-p item)
          (setq build-dirs (append (mentasm--find-build-directories item) build-dirs))))))
    build-dirs))

(defun mentasm--find-assembly-files (build-dirs)
  "Find all .s assembly files in BUILD-DIRS."
  (let ((assembly-files nil))
    (dolist (dir build-dirs)
      (when (file-directory-p dir)
        (let ((files (directory-files-recursively dir "\\.s$")))
          (setq assembly-files (append files assembly-files)))))
    assembly-files))

(defun mentasm--get-source-file-components (source-file)
  "Get components of SOURCE-FILE for matching."
  (let* ((basename (file-name-sans-extension (file-name-nondirectory source-file)))
         (dir-components (split-string 
                         (file-name-directory source-file) 
                         "/" t)))
    (list basename dir-components)))

(defun mentasm--match-assembly-file (source-file assembly-files)
  "Find the assembly file that corresponds to SOURCE-FILE from ASSEMBLY-FILES."
  (let* ((components (mentasm--get-source-file-components source-file))
         (source-basename (nth 0 components))
         (source-dirs (nth 1 components))
         (best-match nil)
         (best-score 0))

    (mentasm--debug "Looking for assembly file matching: %s (basename: %s)" source-file source-basename)

    (dolist (asm-file assembly-files)
      (let* ((asm-basename (file-name-sans-extension (file-name-nondirectory asm-file)))
             (asm-dirs (split-string (file-name-directory asm-file) "/" t))
             (score 0))

        (mentasm--debug "Checking assembly file: %s (basename: %s)" asm-file asm-basename)

        ;; Check if basename matches (direct match)
        (when (string= source-basename asm-basename)
          (setq score (+ score 10))
          (mentasm--debug "Basename match found! Score: %d" score))

        ;; Check for dune executable naming convention: dune__exe__<Name>
        ;; e.g., main.ml -> dune__exe__Main.s
        (when (and (string-prefix-p "dune__exe__" asm-basename)
                   (string= (downcase source-basename)
                            (downcase (substring asm-basename 11))))
          (setq score (+ score 10))
          (mentasm--debug "Dune executable basename match found! Score: %d" score))

        ;; Only continue if we have a basename match
        (when (> score 0)
          ;; Check directory component matches
          (dolist (src-dir source-dirs)
            (when (member src-dir asm-dirs)
              (setq score (+ score 1))
              (mentasm--debug "Directory match: %s, Score now: %d" src-dir score)))

          ;; Update best match if this is better
          (when (> score best-score)
            (setq best-match asm-file
                  best-score score)
            (mentasm--debug "New best match: %s (score: %d)" best-match best-score)))))

    (mentasm--debug "Final match: %s" best-match)
    best-match))

(defun mentasm--source-files-match-p (target-file assembly-file-path)
  "Check if TARGET-FILE matches ASSEMBLY-FILE-PATH.
Handles different path formats like 'qtree/qtree.ml' vs '/full/path/to/qtree.ml'."
  (let ((target-basename (file-name-nondirectory target-file))
        (assembly-basename (file-name-nondirectory assembly-file-path)))
    ;; First check if basenames match
    (when (string= target-basename assembly-basename)
      ;; Also check if the path components are compatible
      (let ((target-components (reverse (split-string (file-name-directory target-file) "/" t)))
            (assembly-components (reverse (split-string (file-name-directory assembly-file-path) "/" t))))
        ;; Check if the last few directory components match
        (catch 'match
          (let ((i 0))
            (while (and (< i (min (length target-components) (length assembly-components)))
                       (< i 3)) ; check up to 3 directory levels
              (unless (string= (nth i target-components) (nth i assembly-components))
                (throw 'match nil))
              (setq i (1+ i))))
          t)))))

;;;; Customize:
(defgroup mentasm nil
  "Customization for mentasm."
  :group 'applications)

(defcustom mentasm-use-overlays t
  "Whether we should use overlays to show matching code."
  :type 'boolean
  :group 'mentasm)
(defcustom mentasm-goto-match t
  "Whether we should goto the match in the other buffer if it is non visible."
  :type 'boolean
  :group 'mentasm)

(defcustom mentasm-mode-lighter " RMSB"
  "Lighter displayed in mode line when function `mentasm-mode' is active."
  :type 'string
  :group 'mentasm)

(defcustom mentasm-large-buffer-size 500
  "Number of lines past which a buffer is considred large."
  :type 'natnum
  :group 'mentasm)

(defcustom mentasm-automatic-recompile t
  "Whether to automatically save and recompile the source buffer.
This setting is automatically disabled on large buffers, set to
`force' to force-enable it.  To only recompile when the buffer is
manually saved, set to `on-save'."
  :group 'mentasm
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On save" on-save)
                 (const :tag "On" t)
                 (const :tag "Always" force)))

;;;;; Buffer Local Tweakables
(defcustom mentasm-disassemble nil
  "Whether we should disassemble an output binary."
  :group 'mentasm
  :type 'boolean
  :safe 'booleanp)

(defcustom mentasm-command nil
  "The base command to run mentasm from."
  :type 'string
  :group 'mentasm
  ;; nil means use default command
  :safe (lambda (v) (or (booleanp v) (stringp v))))

(defcustom mentasm-default-directory nil
  "The default directory to compile from.
This must be an absolute path if set.
Some exporters (such as pony) may not work with this set."
  :type 'string
  ;; nil means use default command
  :safe (lambda (v) (or (booleanp v) (stringp v))))

(define-obsolete-variable-alias 'mentasm-intel-x86
  'mentasm-asm-format "RMSBolt-0.2"
  "Sorry about not providing a proper migration for this variable.
Unfortunately the new options aren't a straightforward mapping.
Most likely what you want:

t -> \"intel\"
nil -> \"att\"
tool defaults -> nil

This means that if you had mentasm-intel-x86 set manually, you
are now getting tool defaults.")

(defcustom mentasm-asm-format "intel"
  "Which output assembly format to use.

The supported values depend highly on the exporter, but typical
values are: intel, att, <nil/t> (for using tool defaults).
Invalid values will be passed onto the disassembly tools, which
may throw errors.

If you are not on x86, you most likely want to set this to nil.

Since this defaults to \"intel\", implementers must support this
being set (at worst falling back to nil if passed \"intel\")."
  :type 'string
  :safe (lambda (v) (or (booleanp v) (stringp v)))
  :group 'mentasm)
(defcustom mentasm-filter-directives t
  "Whether to filter assembly directives."
  :type 'boolean
  :safe 'booleanp
  :group 'mentasm)
(defcustom mentasm-filter-labels t
  "Whether to filter unused labels."
  :type 'boolean
  :safe 'booleanp
  :group 'mentasm)
(defcustom mentasm-filter-comment-only t
  "Whether to filter comment-only lines."
  :type 'boolean
  :safe 'booleanp
  :group 'mentasm)
(defcustom mentasm-ignore-binary-limit nil
  "Whether to ignore the binary limit. Could hang emacs..."
  :type 'boolean
  :safe 'booleanp
  :group 'mentasm)
(defcustom mentasm-demangle t
  "Whether to attempt to demangle the resulting assembly."
  :type 'boolean
  :safe 'booleanp
  :group 'mentasm)
(defcustom mentasm-flag-quirks t
  "Whether to tweak flags to enable as many features as possible.

In most cases, we will try to honor flags in mentasm-command as
much as possible.  However, some features may be disabled with
some odd combinations of flags.  This variable controls
removing/adding flags to handle those cases.

Note that basic flags to ensure basic usage are always modified."
  :type 'boolean
  :safe 'booleanp
  :group 'mentasm)

(defcustom mentasm-after-parse-hook nil
  "Hook after all parsing is done, but before compile command is run.

Exercise caution when setting variables in this hook - doing so
can disrupt mentasm state and cause issues. Variables set here
may not be cleared to default as variables are usually."
  :group 'mentasm
  :type 'hook)

;;;; Faces

(defface mentasm-current-line-face
  '((t (:weight bold :inherit highlight)))
  "Face to fontify the current line for showing matches."
  :group 'mentasm)

;;;; Variables:
(defvar mentasm-output-buffer "*mentasm-output*")
;; whether mentasm-mode is enabled.
(defvar mentasm-mode)

(defvar mentasm-hide-compile t)
(defvar mentasm-binary-asm-limit 10000)
(defvar-local mentasm-line-mapping nil
  "Line mapping hashtable from source lines -> asm lines.")
(defvar-local mentasm-current-line nil
  "Current line for fontifier.")
(defvar-local mentasm--last-point nil
  "Used to detect when the point has moved.")

(defvar mentasm-overlays nil
  "List of overlays to use.")
(defvar mentasm-compile-delay 0.4
  "Time in seconds to delay before recompiling if there is a change.")
(defvar mentasm--automated-compile nil
  "Whether this compile was automated or not.")
(defvar mentasm--shell "bash"
  "Which shell to prefer if available.
Used to work around inconsistencies in alternative shells.")

(defvar mentasm--temp-dir nil
  "Temporary directory to use for compilation and other reasons.

Please DO NOT modify this blindly, as this directory will get
deleted on Emacs exit.")

(defvar mentasm-dir (and load-file-name (file-name-directory load-file-name))
  "The directory which mentasm is installed to.")

(defvar-local mentasm-src-buffer nil)

(defvar-local mentasm--real-src-file nil
  "The real filename that we compiled from.
If this is set, it is probably due to a copy from this file.")
;; FIXME should we be unbinding the list here, or is setting nil good enough.
(defvar-local mentasm--default-variables nil
  "A list of the buffer-local variables we filled in with defaults.
Useful for determining if the user overrode things like `mentasm-command'.

This list of variables will automatically be restored to nil.")

(defvar-local mentasm-objdump-binary "objdump"
  "A binary to use for objdumping when using `mentasm-disassemble'.
Useful if you have multiple objdumpers and want to select between them")

(defvar-local mentasm--assembly-file-watcher nil
  "File watcher for the current assembly file.
Automatically reloads assembly when the .s file changes.")

(defvar-local mentasm--current-assembly-file nil
  "Path to the currently loaded assembly file.
Used to track which file we're watching for changes.")

(defvar mentasm--temp-dirs-hash (make-hash-table :test #'equal)
  "Hash table mapping file path remote components to a 'local' temporary directory.")

;;;; Variable-like funcs
(defun mentasm-output-filename (src-buffer &optional asm)
  "Function for generating an output filename for SRC-BUFFER.

Outputs assembly file if ASM.
This function does NOT quote the return value for use in inferior shells."
  (if (and (not asm)
           (buffer-local-value 'mentasm-disassemble src-buffer))
      (expand-file-name "mentasm.o" mentasm--temp-dir)
    (expand-file-name "mentasm.s" mentasm--temp-dir)))

;;;; Regexes

(defvar mentasm-label-def  (rx bol (group (any ".a-zA-Z_$@")
                                          (0+ (any "a-zA-Z0-9$_@.")))
                               ":"))
(defvar mentasm-defines-global (rx bol (0+ space) ".glob"
                                   (opt "a") "l" (0+ space)
                                   (group (any ".a-zA-Z_")
                                          (0+ (any "a-zA-Z0-9$_.")))))
(defvar mentasm-label-find (rx (any ".a-zA-Z_")
                               (0+
                                (any "a-zA-Z0-9$_."))))
(defvar mentasm-assignment-def (rx bol (0+ space)
                                   (group (any ".a-zA-Z_$")
                                          (1+ (any "a-zA-Z0-9$_.")))
                                   (0+ space) "="))
(defvar mentasm-has-opcode (rx bol (0+ space)
                               (any "a-zA-Z")))

(defvar mentasm-defines-function (rx bol (0+ space) ".type"
                                     (0+ any) "," (0+ space) (any "@%")
                                     "function" eol))
(defvar mentasm-data-defn (rx bol (0+ space) "."
                              (group (or "string" "asciz" "ascii"
                                         (and
                                          (optional (any "1248")) "byte")
                                         "short" "word" "long" "quad" "value" "zero"))))

(defvar mentasm-directive (rx bol (0+ space) "." (0+ any) eol))
(defvar mentasm-endblock (rx "." (or "cfi_endproc" "data" "text" "section")))
(defvar mentasm-comment-only (rx bol (0+ space) (or (and (or (any "#@;") "//"))
                                                    (and "/*" (0+ any) "*/"))
                                 (0+ any) eol))
(defvar mentasm-disass-line (rx bol
                                (group "/" (1+ (not (any ":")))) ":"
                                (group (1+ num))
                                (0+ any)))
(defvar mentasm-disass-label (rx bol (group (1+ (any digit "a-f")))
                                 (1+ space) "<"
                                 (group (1+ (not (any ">")))) ">:" eol))
(defvar mentasm-disass-dest (rx (0+ any) (group (1+ (any digit "a-f")))
                                (1+ space) "<" (group (1+ (not (any ">")))) ">" eol))

(defvar mentasm-disass-opcode (rx bol (0+ space) (group (1+ (any digit "a-f")))
                                  ":" (0+ space)
                                  (group (1+
                                          (repeat 2
                                                  (any digit "a-f"))
                                          (opt " ")))
                                  (0+ space)
                                  (group (0+ any))))
(defvar mentasm-source-file (rx bol (0+ space) ".file" (1+ space)
                                (group (1+ digit)) (1+ space) ?\"
                                (group (1+ (not (any ?\")))) ?\"
                                (opt (1+ space) ?\"
                                     (group (1+ (not (any ?\")))) ?\")
                                (0+ any)))
(defvar mentasm-source-tag (rx bol (0+ space) ".loc" (1+ space)
                               (group (1+ digit)) (1+ space)
                               (group (1+ digit))
                               (0+ any)))
(defvar mentasm-source-stab (rx bol (0+ any) ".stabn" (1+ space)
                                (group (1+ digit)) ",0,"
                                (group (1+ digit)) "," (0+ any)))

;;;; Classes

(cl-defstruct (mentasm-lang
               (:conc-name mentasm-l-))
  (supports-disass
   nil
   :type 'bool
   :documentation "If we support assembly directly. If nil, we must use other methods.")
  (supports-asm
   nil
   :type 'bool
   :documentation "If we support disassembling from binaries. If nil, we must use other methods.")
  (objdumper
   'objdump
   :type 'symbol
   :documentation "The object dumper to use if disassembling binary.")
  (demangling-style
   nil
   :type 'string
   :documentation "The demangling style as interpreted by the objdumper, if applicable.")
  (demangler
   nil
   :type 'string
   :documentation "The command of the demangler to use for this source code.
If nil, don't demangle.")
  (disass-hidden-funcs
   nil
   :type 'string
   :documentation "Functions that are hidden when disassembling.")
  (compile-cmd
   nil
   :type 'string
   :documentation "Default compilation command to use if none is provided.
If provided a function, call that function with the source buffer to determine
the compile command.")
  (default-directory
    nil
    :type 'string
    :documentation "Default directory to run compilation in. By default, use mentasm--temp-dir.
If provided a function, call that function with the source buffer to determine
the default directory.")
  (compile-cmd-function
   nil
   :type 'function
   :documentation "A function which takes in a compile command
(could be the default) and adds needed args to it.")
  (process-asm-custom-fn
   nil
   :type 'function
   :documentation "A custom function used for parsing asm lines
   instead of the default assembly one." )
  (elisp-compile-override
   nil
   :type 'function
   :documentation "A custom function to run instead of running any compilation command.
Generally not useful with the sole exception of the emacs lisp disassembler.
This function is responsible for calling `mentasm--handle-finish-compile'
Please be careful when setting this, as it bypasses most logic and is
generally not useful."))

;;;; Helper Functions
(defun mentasm--convert-file-name-to-system-type (file-name) ;perhaps use `convert-standard-filename'?
  "Convert FILE-NAME to windows format if `system-type' is equal to `cygwin'.

Additional escaping with double quotes included to avoid
backslashes loss in cygwin environment. If not `cygwin' then
bypass the FILE-NAME."
  (if (eq system-type 'cygwin)
      (concat "\"" (cygwin-convert-file-name-to-windows file-name) "\"")
    file-name))

(defmacro mentasm--with-local-files (src-buffer &rest body)
  "Execute BODY with `src-filename' and `output-filename' defined as the local
components of the args taken from SRC-BUFFER.
Return value is quoted for passing to the shell."
  ;; Both src-filename and output-filename are local components so
  ;; they can be used in commands on a remote system.
  `(let ((src-filename
          (mentasm--convert-file-name-to-system-type
           (shell-quote-argument
            (file-local-name
             (buffer-file-name)))))
         (output-filename
          (mentasm--convert-file-name-to-system-type
           (shell-quote-argument
            (file-local-name
             (mentasm-output-filename ,src-buffer))))))
     ,@body))

(defmacro mentasm--set-local (var val)
  "Set unquoted variable VAR to value VAL in current buffer."
  (declare (debug (symbolp form)))
  `(set (make-local-variable ,var) ,val))

(defun mentasm--file-equal-p (src-path target-path)
  "Determine if SRC-PATH and TARGET-PATH refer to the same file.
Checks the existence of the file when the paths are either both local or remote,
otherwise returns whether the paths are equal and does not check whether they
point to a file that exists."
  ;; When possible verify the file's existence, otherwise fall back to comparing
  ;; paths.  The fallback is necessary when the paths provided are the local
  ;; components of remote paths that don't exist on the current system.
  (or (and (equal (file-remote-p src-path)
                  (file-remote-p target-path))
           (file-equal-p src-path target-path))
      (equal (file-local-name src-path)
             (file-local-name target-path))
      ;; OCaml-specific: handle relative paths like "qtree/qtree.ml" 
      ;; matching "/full/path/to/qtree/qtree.ml"
      (and (not (file-name-absolute-p target-path))
           (string-suffix-p (concat "/" target-path) src-path))))

;;;; Language Functions
;;;;; Compile Commands


(cl-defun mentasm--ocaml-compile-cmd (&key src-buffer)
  "Process a compile command for ocaml.

Use SRC-BUFFER as buffer for local variables.

Needed as ocaml cannot output asm to a non-hardcoded file"
  (mentasm--with-local-files
   src-buffer
   (let* ((diss (buffer-local-value 'mentasm-disassemble src-buffer))
          (predicted-asm-filename (shell-quote-argument
                                   (concat (file-name-sans-extension src-filename) ".s")))
          (cmd (buffer-local-value 'mentasm-command src-buffer))
          (cmd (string-join
                (list cmd
                      "-g"
                      (if (buffer-local-value 'mentasm-disassemble src-buffer)
                          ""
                        "-S")
                      src-filename
                      (string-join
                       (cond
                        (diss
                         (list "-o" output-filename))
                        ((equal predicted-asm-filename output-filename)
                         nil)
                        (t
                         (list "&&" "mv"
                               predicted-asm-filename
                               output-filename)))
                       " "))
                " ")))
     cmd)))



;;;;; Hidden Function Definitions


(defvar mentasm--hidden-func-ocaml
  (rx bol
      (or (and "__" (0+ any))
          (and "_" (or "init" "start" "fini"))
          (and (opt "de") "register_tm_clones")
          (and ".plt" (0+ any))
          (and "camlCamlinternalFormat" (0+ any))
          (and (1+ (not (any "@"))) "@plt")
          (and (or "caml_" "camlStd_") (0+ any))
          (and "caml" (or "Pervasives" "List" "Bytes"
                          "String" "Buffer" "Printf"
                          "Char" "Sys")
               "__" (0+ any))
          ;; Ocaml likes to make labels following camlModule__,
          ;; filter out any lowercase
          (and (1+ (1+ lower) (opt (or "64" "32" "8" "16")) (opt "_"))))
      eol))


;;;;; Demangling Functions



;;;; Language Definitions
(defvar mentasm-languages)
(setq
 mentasm-languages
 `(;; OCaml support - works with both tuareg-mode and ocaml-mode
   (tuareg-mode
    . ,(make-mentasm-lang :compile-cmd "ocamlopt"
                          :supports-asm t
                          :supports-disass t
                          :disass-hidden-funcs mentasm--hidden-func-ocaml))
   (ocaml-mode
    . ,(make-mentasm-lang :compile-cmd "ocamlopt"
                          :supports-asm t
                          :supports-disass t
                          :disass-hidden-funcs mentasm--hidden-func-ocaml))
   ))
(make-obsolete-variable 'mentasm-languages
                        'mentasm-language-descriptor "RMSBolt-0.2")

(defvar-local mentasm-language-descriptor nil
  ;; FIXME: Major modes can't set this without calling `make-mentasm-lang',
  ;; so it forces them to require `mentasm', which is a bummer.
  "Description of the language tools of current buffer for use by RMSBolt.
This should be an object of type `mentasm-lang', normally set by the major mode")

;;;; Macros

(defmacro mentasm-with-display-buffer-no-window (&rest body)
  "Run BODY without displaying any window."
  ;; See http://debbugs.gnu.org/13594
  `(let ((display-buffer-overriding-action
          (if mentasm-hide-compile
              (list #'display-buffer-no-window)
            display-buffer-overriding-action)))
     ,@body))

;;;; Functions
;; Functions to parse and lint assembly were lifted almost directly from the compiler-explorer

(defun mentasm-re-seq (regexp string)
  "Get list of all REGEXP match in STRING."
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))

;; Prevent byte-compilation warnings for cl-print-compiled, which is imported
;; from cl-print
(defvar cl-print-compiled)
(defun mentasm--disassemble-file (filename out-buffer)
  "Disassemble an elisp FILENAME into elisp bytecode in OUT-BUFFER.
Lifted from https://emacs.stackexchange.com/questions/35936/disassembly-of-a-bytecode-file"
  (if (not (require 'cl-print nil 'noerror))
      (error "Package cl-print or Emacs 26+ are required for the Emacs disassembler")
    (byte-compile-file filename)
    ;; .el -> .elc
    (setq filename (concat filename "c"))
    (with-temp-buffer
      (insert-file-contents filename)
      (let ((inbuf (current-buffer)))
        (goto-char (point-min))
        (with-current-buffer out-buffer
          (erase-buffer)
          (setq-local cl-print-compiled 'disassemble)
          (condition-case ()
              (cl-loop for expr = (read inbuf)
                       do
                       (pcase expr
                         (`(byte-code ,(pred stringp) ,(pred vectorp) ,(pred natnump))
                          (princ "TOP-LEVEL byte code:\n" (current-buffer))
                          (disassemble-1 expr 0))
                         (_ (cl-prin1 expr (current-buffer))))
                       do (terpri (current-buffer)))
            (end-of-file nil)))))))

;;;;; Filter Functions

;; Filtering functions were more or less lifted from the godbolt compiler explorer to maintain compatiblity.
;; https://github.com/mattgodbolt/compiler-explorer/blob/master/lib/asm.js

(defun mentasm--has-opcode-p (line)
  "Check if LINE has opcodes."
  (save-match-data
    (let* ((match (string-match mentasm-label-def line))
           (line (if match
                     (substring line (match-end 0))
                   line))
           (line (cl-first (split-string line (rx (1+ (any ";#")))))))
      (if (string-match-p mentasm-assignment-def line)
          nil
        (string-match-p mentasm-has-opcode line)))))

(defun mentasm--find-used-labels (src-buffer asm-lines)
  "Find used labels in ASM-LINES generated from SRC-BUFFER."
  (let ((match nil)
        (current-label nil)
        (labels-used (make-hash-table :test #'equal))
        (weak-usages (make-hash-table :test #'equal)))
    (dolist (line asm-lines)
      (setq line (string-trim-left line)
            match (and (string-match mentasm-label-def line)
                       (match-string 1 line)))
      (when match
        (setq current-label match))
      (setq match (and (string-match mentasm-defines-global line)
                       (match-string 1 line)))
      (when match
        (puthash match t labels-used))
      ;; When we have no line or a period started line, skip
      (unless (or (string-empty-p line)
                  (eq (elt line 0) ?.)
                  (not (string-match-p mentasm-label-find line)))
        (if (or (not (buffer-local-value 'mentasm-filter-directives src-buffer))
                (mentasm--has-opcode-p line)
                (string-match-p mentasm-defines-function line))
            ;; Add labels indescriminantly
            (dolist (l (mentasm-re-seq mentasm-label-find line))
              (puthash l t labels-used))
          (when (and current-label
                     (or (string-match-p mentasm-data-defn line)
                         (mentasm--has-opcode-p line)))
            (dolist (l (mentasm-re-seq mentasm-label-find line))
              (cl-pushnew l (gethash current-label weak-usages) :test #'equal))))))

    (let* ((max-label-iter 10)
           (label-iter 0)
           (completed nil))

      (while (and (<= (cl-incf label-iter)
                      max-label-iter)
                  (not completed))
        (let ((to-add nil))
          (maphash
           (lambda (label _v)
             (dolist (now-used (gethash label weak-usages))
               (when (not (gethash now-used labels-used))
                 (cl-pushnew now-used to-add :test #'equal))))
           labels-used)
          (if to-add
              (dolist (l to-add)
                (puthash l t labels-used))
            (setq completed t))))
      labels-used)))

(defun mentasm--user-func-p (src-buffer func)
  "Return t if FUNC is a user function.
Argument SRC-BUFFER source buffer."
  (let* ((lang (with-current-buffer src-buffer
                 (mentasm--get-lang)))
         (regexp (mentasm-l-disass-hidden-funcs lang)))
    (if regexp
        (not (string-match-p regexp func))
      t)))

;; TODO godbolt does not handle disassembly with filter=off, but we should.
(cl-defun mentasm--process-disassembled-lines (src-buffer asm-lines)
  "Process and filter disassembled ASM-LINES from SRC-BUFFER."
  (let* ((src-file-name (or (buffer-local-value 'mentasm--real-src-file src-buffer)
                            (buffer-file-name src-buffer)))
         (local-src-file-name (file-local-name src-file-name))
         (result nil)
         (func nil)
         (source-linum nil)
         (def-dir (or (buffer-local-value 'mentasm-default-directory src-buffer)
                      (and src-file-name
                           (file-name-directory src-file-name)))))
    (dolist (line asm-lines)
      (catch 'continue
        (when (and (> (length result) mentasm-binary-asm-limit)
                   (not (buffer-local-value 'mentasm-ignore-binary-limit src-buffer)))
          (cl-return-from mentasm--process-disassembled-lines
            '("Aborting processing due to exceeding the binary limit.")))
        (when (string-match mentasm-disass-line line)
          ;; Don't add linums from files which we aren't inspecting
          ;; If we get a non-absolute .file path, check to see if we
          ;; have a default dir. If not, treat it like we are in the
          ;; src directory.
          (let ((default-directory def-dir))
            (if (mentasm--file-equal-p local-src-file-name
                              (match-string 1 line))
                (setq source-linum (string-to-number (match-string 2 line)))
              (setq source-linum nil)))
          ;; We are just setting a linum, no data here.
          (throw 'continue t))

        (when (string-match mentasm-disass-label line)
          (setq func (match-string 2 line))
          (when (mentasm--user-func-p src-buffer func)
            (push (concat func ":") result))
          (throw 'continue t))
        (unless (and func
                     (mentasm--user-func-p src-buffer func))
          (throw 'continue t))
        (when (string-match mentasm-disass-opcode line)
          (let ((line (concat (match-string 1 line)
                              "\t" (match-string 3 line))))
            ;; Add line text property if available
            (when source-linum
              (add-text-properties 0 (length line)
                                   `(mentasm-src-line ,source-linum) line))
            (push line result))
          (throw 'continue t))))
    (nreverse result)))

(cl-defun mentasm--process-src-asm-lines (src-buffer asm-lines)
  "Process and filter compiler-generated ASM-LINES from SRC-BUFFER."
  (let* ((used-labels (mentasm--find-used-labels src-buffer asm-lines))
         (src-file-name (or (buffer-local-value 'mentasm--real-src-file src-buffer)
                            (buffer-file-name src-buffer)))
         (local-src-file-name (file-local-name src-file-name))
         (result nil)
         (prev-label nil)
         (source-linum nil)
         (source-file-map (make-hash-table :test #'eq))
         (def-dir (or (buffer-local-value 'mentasm-default-directory src-buffer)
                      (and src-file-name
                           (file-name-directory src-file-name)))))
    (dolist (line asm-lines)
      (let* ((raw-match (or (string-match mentasm-label-def line)
                            (string-match mentasm-assignment-def line)))
             (match (when raw-match
                      (match-string 1 line)))
             (used-label-p (gethash match used-labels)))
        (catch 'continue
          (cond
           ;; Process file name hints
           ((string-match mentasm-source-file line)
            (if (match-string 3 line)
                ;; Clang style match
                (puthash (string-to-number (match-string 1 line))
                         (expand-file-name (match-string 3 line) (match-string 2 line))
                         source-file-map)
              (puthash (string-to-number (match-string 1 line))
                       (match-string 2 line)
                       source-file-map)))
           ;; Process any line number hints
           ((string-match mentasm-source-tag line)
            (if (or (not src-file-name) ;; Skip file match if we don't have a current filename
                    ;; If we get a non-absolute .file path, check to see if we
                    ;; have a default dir. If not, treat it like we are in the
                    ;; src directory.
                    (let ((default-directory def-dir))
                      (mentasm--file-equal-p local-src-file-name
                             (gethash
                              (string-to-number (match-string 1 line))
                              source-file-map
                              ;; Assume we never will compile dev null :P
                              "/dev/null"))))
                (setq source-linum (string-to-number
                                    (match-string 2 line)))
              (setq source-linum nil)))
           ((string-match mentasm-source-stab line)
            (pcase (string-to-number (match-string 1 line))
              ;; http://www.math.utah.edu/docs/info/stabs_11.html
              (68
               (setq source-linum (match-string 2 line)))
              ((or 100 132)
               (setq source-linum nil)))))
          ;; End block, reset prev-label and source
          (when (string-match-p mentasm-endblock line)
            (setq prev-label nil))

          (when (and (buffer-local-value 'mentasm-filter-comment-only src-buffer)
                     (string-match-p mentasm-comment-only line))
            (throw 'continue t))

          ;; continue means we don't add to the ouptut
          (when match
            (if (not used-label-p)
                ;; Unused label
                (when (buffer-local-value 'mentasm-filter-labels src-buffer)
                  (throw 'continue t))
              ;; Real label, set prev-label
              (setq prev-label raw-match)))
          (when (and (buffer-local-value 'mentasm-filter-directives src-buffer)
                     (not match))
            (if  (and (string-match-p mentasm-data-defn line)
                      prev-label)
                ;; data is being used
                nil
              (when (string-match-p mentasm-directive line)
                (throw 'continue t))))
          ;; Add line numbers to mapping
          (when (and source-linum
                     (mentasm--has-opcode-p line))
            (add-text-properties 0 (length line)
                                 `(mentasm-src-line ,source-linum) line))
          ;; Add line
          (push line result))))
    (nreverse result)))



(cl-defun mentasm--process-asm-lines (src-buffer asm-lines)
  "Process and filter a set of ASM-LINES from SRC-BUFFER.

Essentially a switch that chooses which processing function to use."
  (let* ((lang (with-current-buffer src-buffer
                 (mentasm--get-lang)))
         (process-asm-fn (when lang
                           (mentasm-l-process-asm-custom-fn lang))))
    (cond
     (process-asm-fn
      (funcall process-asm-fn src-buffer asm-lines))
     ((buffer-local-value 'mentasm-disassemble src-buffer)
      (mentasm--process-disassembled-lines src-buffer asm-lines))
     (t
      (mentasm--process-src-asm-lines src-buffer asm-lines)))))



;;;;; HANDLERS
(cl-defun mentasm--handle-finish-compile (buffer str &key override-buffer stopped)
  "Finish hook for compilations.
Argument BUFFER compilation buffer.
Argument STR compilation finish status.
Argument OVERRIDE-BUFFER asm src buffer to use instead of reading
   `mentasm-output-filename'.
Argument STOPPED The compilation was stopped to start another compilation."
  (when (not (buffer-live-p buffer))
    (error "Dead buffer passed to compilation-finish-function! RMSBolt cannot continue"))
  (let ((compilation-fail
         (and str
              (not (string-match "^finished" str))))
        (src-default-directory (buffer-local-value 'default-directory buffer))
        (src-buffer (buffer-local-value 'mentasm-src-buffer buffer)))

    (with-current-buffer (get-buffer-create mentasm-output-buffer)
      (setq default-directory src-default-directory)
      ;; Store src buffer value for later linking
      (cond (stopped) ; Do nothing
            ((not compilation-fail)
             (if (and (not override-buffer)
                      (not (file-exists-p (mentasm-output-filename src-buffer t))))
                 (message "Error reading from output file.")
               (let ((lines
                      (mentasm--process-asm-lines
                       src-buffer
                       (or (when override-buffer
                             (with-current-buffer override-buffer
                               (split-string (buffer-string) "\n" nil)))
                           (with-temp-buffer
                             (insert-file-contents (mentasm-output-filename src-buffer t))
                             (split-string (buffer-string) "\n" nil)))))
                     (ht (make-hash-table :test #'eq))
                     (linum 1)
                     (start-match nil)
                     (in-match nil)
                     (output-buffer (current-buffer)))
                 ;; Add lines to hashtable
                 (dolist (line lines)
                   (let ((property
                          (get-text-property
                           0 'mentasm-src-line line)))
                     (cl-tagbody
                      run-conditional
                      (cond
                       ((and in-match (eq in-match property))
                        ;; We are continuing an existing match
                        nil)
                       (in-match
                        ;; We are in a match that has just expired
                        (push (cons start-match (1- linum))
                              (gethash in-match ht))
                        (setq in-match nil
                              start-match nil)
                        (go run-conditional))
                       (property
                        (setq in-match property
                              start-match linum)))))
                   (cl-incf linum))

                 (with-current-buffer src-buffer
                   (setq mentasm-line-mapping ht))

                 ;; Replace buffer contents but save point and scroll
                 (let* ((window (get-buffer-window output-buffer))
                        (old-point (window-point window))
                        (old-window-start (window-start window)))
                   (erase-buffer)
                   (insert (string-join lines "\n"))
                   (when window
                     (set-window-start window old-window-start)
                     (set-window-point window old-point)))
                 (asm-mode)
                 (mentasm-mode 1)
                 (setq mentasm-src-buffer src-buffer)
                 (display-buffer (current-buffer) '(nil (inhibit-same-window . t)))
                 (run-at-time 0 nil #'mentasm-update-overlays))))
            (t ; Compilation failed
             ;; Display compilation buffer
             (if mentasm--automated-compile
                 (display-buffer buffer '(nil (inhibit-same-window . t)))
               ;; If the compilation was directly started by the user,
               ;; select the compilation buffer.
               (pop-to-buffer buffer))
             ;; TODO find a cleaner way to disable overlays.
             (with-current-buffer src-buffer
               (setq mentasm-line-mapping nil))
             (mentasm--remove-overlays)))
      ;; Reset automated recompile
      (setq mentasm--automated-compile nil))
    ;; Clear out default-set variables
    (with-current-buffer src-buffer
      (dolist (var mentasm--default-variables)
        (mentasm--set-local var nil))
      (setq mentasm--default-variables nil))))

;;;;; Parsing Options
(defun mentasm--get-lang ()
  "Helper function to get lang def for LANGUAGE."
  (or (if (symbolp mentasm-language-descriptor)
          (symbol-value mentasm-language-descriptor)
        mentasm-language-descriptor)
      (cdr-safe (or
                 (assoc major-mode mentasm-languages)
                 ;; If the normal major mode cannot be found, try converting
                 ;; from a tree-sitter to a non-tree-sitter mode name
                 ;; https://github.com/renzmann/treesit-auto/blob/d32617b5edb660b8a046053af3b92cf14f9b978e/treesit-auto.el#L89
                 (assoc (thread-last
                          (symbol-name major-mode)
                          (replace-regexp-in-string "ts-mode$" "mode")
                          (intern))
                        mentasm-languages)))))

(defun mentasm--parse-options ()
  "Parse RMS options from file."
  (hack-local-variables)
  (let* ((lang (mentasm--get-lang))
         (src-buffer (current-buffer))
         (cmd mentasm-command)
         (dir mentasm-default-directory)
         (force-disass (not (mentasm-l-supports-asm lang)))
         (force-asm (not (mentasm-l-supports-disass lang))))
    ;; If this is non-nil, most likely we are running two compiles at once.
    ;; This is not exactly ideal, as it causes a race condition.
    (when mentasm--default-variables
      (message "It looks like RMSbolt state wasn't cleaned up properly.
Are you running two compilations at the same time?"))
    (when (and force-disass force-asm)
      (error "No disassemble method found for this langauge, please double check spec"))
    (when force-disass
      (setq-local mentasm-disassemble t))
    (when force-asm
      (setq-local mentasm-disassemble nil))
    (when (not dir)
      (add-to-list 'mentasm--default-variables 'mentasm-default-directory)
      (setq-local mentasm-default-directory
                  (let ((new-dir (mentasm-l-default-directory lang)))
                    (pcase new-dir
                      ((pred functionp) (funcall new-dir src-buffer))
                      (_ new-dir)))))
    (when (not cmd)
      (add-to-list 'mentasm--default-variables 'mentasm-command)
      (setq-local mentasm-command
                  (let ((new-cmd (mentasm-l-compile-cmd lang)))
                    (pcase new-cmd
                      ((pred functionp) (funcall new-cmd src-buffer))
                      (_ new-cmd)))))
    src-buffer))

(defun mentasm--demangle-command (existing-cmd lang src-buffer)
  "Add a demangler routine to EXISTING-CMD with LANG and SRC-BUFFER and return."
  (if-let* ((to-demangle (buffer-local-value 'mentasm-demangle src-buffer))
           (demangler (mentasm-l-demangler lang))
           (demangler-exists (executable-find demangler))
           (output-filename (mentasm--convert-file-name-to-system-type
                             (file-local-name (mentasm-output-filename src-buffer t))))
           (assembly-filename (mentasm--convert-file-name-to-system-type
                               (file-local-name (expand-file-name "tmp.s" mentasm--temp-dir)))))
      (concat existing-cmd " "
              (string-join
               (list "&&" demangler
                     "<" output-filename
                     ">" assembly-filename
                     "&&" "mv"
                     assembly-filename
                     output-filename)
               " "))
    existing-cmd))

;;;;; UI Functions
(defun mentasm--find-and-load-assembly ()
  "Find and load assembly for the current OCaml buffer."
  ;; Current buffer = src-buffer at this point
  (setq mentasm-src-buffer (current-buffer))
  (cond
   ((derived-mode-p 'asm-mode)
    ;; We cannot process asm-mode files
    (message "Cannot process assembly files. Are you sure you are not in the output buffer?"))
   ((not (or (derived-mode-p 'tuareg-mode) (derived-mode-p 'ocaml-mode)))
    (message "Mentasm only supports OCaml files (tuareg-mode or ocaml-mode)"))
   (t
    (mentasm--parse-options)
    (let* ((src-buffer (current-buffer))
           (src-file (buffer-file-name src-buffer)))
      (if (not src-file)
          (message "Buffer must be visiting a file to find assembly")
        (mentasm--debug "Starting assembly file discovery for: %s" src-file)
        (let* ((project-root (mentasm--find-dune-project))
               (build-dirs (when project-root 
                            (mentasm--find-build-directories project-root)))
               (assembly-files (when build-dirs
                                (mentasm--find-assembly-files build-dirs)))
               (matching-asm (when assembly-files
                              (mentasm--match-assembly-file src-file assembly-files))))
          (cond
           ((not project-root)
            (message "No dune-project found. Make sure you're in a dune project directory."))
           ((not build-dirs)
            (message "No _build directories found in project."))
           ((not assembly-files)
            (message "No assembly files (.s) found in build directories. Try building your project first."))
           ((not matching-asm)
            (message "No matching assembly file found for %s" (file-name-nondirectory src-file)))
           (t
            (mentasm--debug "Found matching assembly file: %s" matching-asm)
            (message "Loading assembly from: %s" (file-relative-name matching-asm project-root))
            ;; Enable mentasm-mode in source buffer for highlighting
            (unless mentasm-mode
              (mentasm-mode 1))
            ;; Process the assembly file directly
            (mentasm--load-assembly-file src-buffer matching-asm)))))))))

(defun mentasm--load-assembly-file (src-buffer asm-file-path)
  "Load assembly file for display.
Load ASM-FILE-PATH and display it for SRC-BUFFER."
  (when (file-exists-p asm-file-path)
    (mentasm--display-assembly src-buffer asm-file-path)
    ;; Set up file watching for automatic updates
    (mentasm--setup-assembly-file-watcher src-buffer asm-file-path)))

(defun mentasm--display-assembly (src-buffer asm-file)
  "Display assembly file ASM-FILE for SRC-BUFFER."
  (with-current-buffer src-buffer
    (let ((asm-lines (with-temp-buffer
                       (insert-file-contents asm-file)
                       (split-string (buffer-string) "\n" t))))
      ;; Process and display the assembly
      (mentasm--display-output src-buffer asm-lines))))

(defun mentasm--display-output (src-buffer asm-lines)
  "Display processed ASM-LINES for SRC-BUFFER using existing mechanism."
  (let ((src-default-directory (buffer-local-value 'default-directory src-buffer)))
    (with-current-buffer (get-buffer-create mentasm-output-buffer)
      (setq default-directory src-default-directory)
      ;; Store src buffer value for later linking
      (let* ((lines (mentasm--process-asm-lines src-buffer asm-lines))
             (ht (make-hash-table :test #'eq))
             (linum 1)
             (start-match nil)
             (in-match nil)
             (output-buffer (current-buffer)))
        
        ;; Build line mapping hash table (like the original function)
        (dolist (line lines)
          (let ((property (get-text-property 0 'mentasm-src-line line)))
            (cl-tagbody
             run-conditional
             (cond
              ((and in-match (eq in-match property))
               ;; We are continuing an existing match
               nil)
              (in-match
               ;; We are in a match that has just expired
               (push (cons start-match (1- linum))
                     (gethash in-match ht))
               (setq in-match nil
                     start-match nil)
               (go run-conditional))
              (property
               (setq in-match property
                     start-match linum)))))
          (cl-incf linum))
        
        ;; Handle final match if still in one
        (when in-match
          (push (cons start-match (1- linum))
                (gethash in-match ht)))
        
        ;; Update source buffer mapping
        (with-current-buffer src-buffer
          (setq mentasm-line-mapping ht))
        
        ;; Update display while preserving window position
        (let* ((window (get-buffer-window output-buffer))
               (old-point (when window (window-point window)))
               (old-window-start (when window (window-start window))))
          (erase-buffer)
          (insert (string-join lines "\n"))
          (when window
            (set-window-start window old-window-start)
            (set-window-point window old-point)))
        
        ;; Setup modes and display
        (asm-mode)
        (mentasm-mode 1)
        (setq mentasm-src-buffer src-buffer)
        (display-buffer (current-buffer) '(nil (inhibit-same-window . t)))
        (run-at-time 0 nil #'mentasm-update-overlays)))))

;;;;; File Watching Functions

(defun mentasm--setup-assembly-file-watcher (src-buffer asm-file-path)
  "Set up file watcher for ASM-FILE-PATH to automatically reload when it changes.
SRC-BUFFER is the source buffer that should be updated when assembly changes."
  (with-current-buffer src-buffer
    ;; Clean up any existing watcher first
    (mentasm--cleanup-assembly-file-watcher)
    
    ;; Set up new watcher if file exists
    (when (and asm-file-path (file-exists-p asm-file-path))
      (condition-case err
          (progn
            (setq mentasm--current-assembly-file asm-file-path)
            (setq mentasm--assembly-file-watcher
                  (file-notify-add-watch 
                   asm-file-path
                   '(change)
                   (lambda (event)
                     (mentasm--on-assembly-file-change src-buffer event))))
            (mentasm--debug "Set up file watcher for: %s" asm-file-path))
        (error 
         (mentasm--debug "Failed to set up file watcher: %s" (error-message-string err))
         (setq mentasm--assembly-file-watcher nil
               mentasm--current-assembly-file nil))))))

(defun mentasm--on-assembly-file-change (src-buffer event)
  "Handle assembly file change EVENT for SRC-BUFFER.
Automatically reloads the assembly when the .s file is modified."
  (when (and (buffer-live-p src-buffer)
             (eq (nth 1 event) 'changed))
    (mentasm--debug "Assembly file changed, reloading: %s" (nth 2 event))
    ;; Small delay to ensure file write is complete
    (run-with-timer 0.1 nil
                    (lambda ()
                      (when (buffer-live-p src-buffer)
                        (with-current-buffer src-buffer
                          (let ((asm-file mentasm--current-assembly-file))
                            (when (and asm-file (file-exists-p asm-file))
                              (mentasm--display-assembly src-buffer asm-file)))))))))

(defun mentasm--cleanup-assembly-file-watcher ()
  "Clean up the current assembly file watcher."
  (when mentasm--assembly-file-watcher
    (condition-case err
        (progn
          (file-notify-rm-watch mentasm--assembly-file-watcher)
          (mentasm--debug "Cleaned up file watcher for: %s" mentasm--current-assembly-file))
      (error
       (mentasm--debug "Error cleaning up file watcher: %s" (error-message-string err))))
    (setq mentasm--assembly-file-watcher nil
          mentasm--current-assembly-file nil)))

(defun mentasm--stop-running-compilation ()
  "Cancel a compilation."
  (when-let* ((compilation-buffer (get-buffer "*mentasm-compilation*"))
              (proc (get-buffer-process compilation-buffer)))
    (when (eq (process-status proc) 'run)
      (set-process-sentinel proc nil)
      (interrupt-process proc)
      (mentasm--handle-finish-compile compilation-buffer nil :stopped t)
      ;; Wait a short while for the process to exit cleanly
      (sit-for 0.2)
      (delete-process proc))))

;;;; Keymap
(defvar mentasm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'mentasm)
    map)
  "Keymap for function `mentasm-mode'.")

;;;; Init commands

(defun mentasm--gen-temp ()
  "Get a temporary directory close to the current buffer.  A new directory is
created if needed.  Only one temporary directory exists per host so as to support
compilation of remote files."
  ;; Get this buffer's temporary directory.  Use the buffer's default directory
  ;; if the current buffer isn't backed by a file.
  (let* ((remote-components (file-remote-p (or (buffer-file-name)
                                               default-directory)))
        (temp-directory (gethash remote-components mentasm--temp-dirs-hash)))
    ;; Create a temporary directory if we haven't already for this remote.
    (unless temp-directory
      (puthash remote-components (make-nearby-temp-file "mentasm-" t) mentasm--temp-dirs-hash)
      (setq temp-directory (gethash remote-components mentasm--temp-dirs-hash))
      ;; Make sure this directory is removed when we exit.
      (add-hook 'kill-emacs-hook
                (lambda ()
                  (when (and temp-directory
                             (file-directory-p temp-directory)
                             (string-match "mentasm" (file-name-nondirectory temp-directory)))
                    (delete-directory temp-directory t)))))
    (setq mentasm--temp-dir temp-directory)))

;;;;; Starter Definitions

;; IIUC, this "starter" business is not a necessary part of RMSBolt, but is
;; a way to provide sample files with which users can try out RMSBolt.


;;;; Overlay Commands
(defun mentasm--goto-line (line)
  "Goto a certain LINE."
  (when line
    (let ((cur (line-number-at-pos)))
      (forward-line (- line cur)))))

(defun mentasm--setup-overlay (start end buf)
  "Setup overlay with START and END in BUF."
  (let ((o (make-overlay start end buf)))
    (overlay-put o 'face 'mentasm-current-line-face)
    o))

(cl-defun mentasm--point-visible (point)
  "Check if the current POINT is visible in a window in the current buffer."
  (cl-find-if (lambda (w)
                (<= (window-start w) point (window-end w)))
              (get-buffer-window-list)))

(cl-defun mentasm-update-overlays (&key (force nil))
  "Update overlays to highlight the currently selected source and asm lines.
If FORCE, always scroll overlay, even when one is visible.  FORCE also
scrolls to the first line, instead of the first line of the last block."
  (when mentasm-mode
    (if-let ((should-run mentasm-use-overlays)
             (output-buffer (get-buffer mentasm-output-buffer))
             (src-buffer (buffer-local-value 'mentasm-src-buffer output-buffer))
             (should-run (and (or (eq (current-buffer) src-buffer)
                                  (eq (current-buffer) output-buffer))
                              ;; Don't run on unsaved buffers
                              (not (buffer-modified-p src-buffer))
                              (buffer-local-value 'mentasm-mode src-buffer)))
             (current-line (line-number-at-pos))
             (src-current-line
              (if (eq (current-buffer) src-buffer)
                  current-line
                (get-text-property (point) 'mentasm-src-line)))
             (line-mappings (buffer-local-value 'mentasm-line-mapping src-buffer))
             (asm-regions (gethash src-current-line line-mappings))
             ;; TODO also consider asm
             (src-pts
              (with-current-buffer src-buffer
                (save-excursion
                  (mentasm--goto-line src-current-line)
                  (cl-values (c-point 'bol) (c-point 'bonl))))))
        (let*
            ;; If nil, output-buffer is scrolled instead
            ((scroll-src-buffer-p (not (eq (current-buffer) src-buffer)))
             (line-visible (or (not mentasm-goto-match)
                               (when scroll-src-buffer-p
                                 (with-current-buffer src-buffer
                                   (mentasm--point-visible (cl-first src-pts)))))))
          ;; Remove existing overlays
          (mentasm--remove-overlays)
          (push (mentasm--setup-overlay (cl-first src-pts) (cl-second src-pts) src-buffer)
                mentasm-overlays)
          (with-current-buffer output-buffer
            (let ((saved-pt (point)))
              (save-excursion
                (cl-loop for (start . end) in asm-regions
                         do (let ((start-pt (progn (mentasm--goto-line start)
                                                   (c-point 'bol)))
                                  (end-pt (progn (mentasm--goto-line end)
                                                 (c-point 'bonl))))
                              (when (and (not line-visible)
                                         (not scroll-src-buffer-p))
                                (setq line-visible (or (mentasm--point-visible start-pt)
                                                       (mentasm--point-visible end-pt)
                                                       (< start-pt saved-pt end-pt))))
                              (push (mentasm--setup-overlay start-pt end-pt output-buffer)
                                    mentasm-overlays)))))
            (when (or (not line-visible) force)
              ;; Scroll buffer to first line
              (when-let ((scroll-buffer (if scroll-src-buffer-p
                                            src-buffer
                                          output-buffer))
                         (window (get-buffer-window scroll-buffer))
                         (line-scroll (if scroll-src-buffer-p
                                          src-current-line
                                        (progn
                                          (car-safe
                                           ;; If forcing, pick the last region instead
                                           (if force
                                               (car-safe (last asm-regions))
                                             (cl-first asm-regions)))))))
                (with-selected-window window
                  (mentasm--goto-line line-scroll)
                  ;; If we scrolled, recenter
                  (recenter))))))
      (mentasm--remove-overlays))
    ;; If not in mentasm-mode, don't do anything
    ))

(defun mentasm--remove-overlays ()
  "Clean up overlays, assuming they are no longer needed."
  (mapc #'delete-overlay mentasm-overlays)
  (setq mentasm-overlays nil))

(defun mentasm--post-command-hook ()
  "Update overlays and perform book-keeping post-compile."
  ;; Use (point) instead of (line-number-at-pos) to track movements because
  ;; the former is faster (constant runtime)
  (unless (eq (point) mentasm--last-point)
    (setq mentasm--last-point (point))
    (mentasm-update-overlays)))

(defun mentasm--on-kill-buffer ()
  "Perform cleanup if the user deletes the buffer."
  (when-let (output-buffer (get-buffer mentasm-output-buffer))
    (when (or (eq (current-buffer) output-buffer)
              (eq (current-buffer) (buffer-local-value 'mentasm-src-buffer output-buffer)))
      (mentasm--remove-overlays)
      ;; Clean up file watcher if this is the source buffer
      (when (eq (current-buffer) (buffer-local-value 'mentasm-src-buffer output-buffer))
        (mentasm--cleanup-assembly-file-watcher)))))

(defun mentasm--is-active-src-buffer ()
  "Helper to see if our src buffer is active."
  (when-let (output-buffer (get-buffer mentasm-output-buffer))
    (eq (current-buffer) (buffer-local-value 'mentasm-src-buffer output-buffer))))

(defun mentasm--after-save ()
  "Handle automated compile and other after-safe functions."
  (when (and (mentasm--is-active-src-buffer)
             mentasm-automatic-recompile)
    (setq mentasm--automated-compile t)
    (mentasm--find-and-load-assembly)))

;; Auto-save the src buffer after it has been unchanged for `mentasm-compile-delay' seconds.
;; The buffer is then automatically recompiled via `mentasm--after-save'.
(defvar mentasm--change-timer nil)
(defvar mentasm--buffer-to-auto-save nil)

(defun mentasm--after-change (&rest _)
  "Handle automatic recompile and other functions after edit."
  (when (and (mentasm--is-active-src-buffer)
             mentasm-automatic-recompile
             (not (eq mentasm-automatic-recompile 'on-save)))
    (when mentasm--change-timer
      (cancel-timer mentasm--change-timer))
    (setq mentasm--buffer-to-auto-save (current-buffer)
          mentasm--change-timer (run-with-timer mentasm-compile-delay nil #'mentasm--on-change-timer))))

(defun mentasm--on-change-timer ()
  "Hook to run on automatic recompile timer."
  (setq mentasm--change-timer nil)
  (when (buffer-live-p mentasm--buffer-to-auto-save)
    (with-current-buffer mentasm--buffer-to-auto-save
      (setq mentasm--buffer-to-auto-save nil)
      (when (or (< (line-number-at-pos (point-max)) mentasm-large-buffer-size)
                (eq mentasm-automatic-recompile 'force))
        ;; Clear `before-save-hook' to prevent things like whitespace cleanup
        ;; (e.g., set by spacemacs in `spacemacs-whitespace-cleanup.el`)
        ;; and aggressive indenting from running (this is a hot recompile).
        ;; TODO does anyone want before-save-hook to run on a hot recompile?
        (let ((before-save-hook nil))
          (save-buffer))))))

;;;; Mode Definition:

;;;###autoload
;; TODO handle more modes than c-mode
(define-minor-mode mentasm-mode
  "Toggle `mentasm-mode'.

This mode is enabled in both src and assembly output buffers."
  :global nil
  :lighter mentasm-mode-lighter
  :keymap mentasm-mode-map
  ;; Init
  (cond
   (mentasm-mode
    (setq mentasm--last-point (point))
    (add-hook 'post-command-hook #'mentasm--post-command-hook nil t)
    (add-hook 'kill-buffer-hook #'mentasm--on-kill-buffer nil t)

    (when (and mentasm-automatic-recompile
               ;; Only turn on auto-save in src buffers
               (not (eq (current-buffer) (get-buffer mentasm-output-buffer))))
      (add-hook 'after-save-hook #'mentasm--after-save nil t)
      (when (eq mentasm-automatic-recompile t)
        (add-hook 'after-change-functions #'mentasm--after-change nil t)))

    (mentasm--gen-temp))
   (t ;; Cleanup
    (mentasm--remove-overlays)
    (mentasm--cleanup-assembly-file-watcher)
    (remove-hook 'after-change-functions #'mentasm--after-change t)
    (remove-hook 'after-save-hook #'mentasm--after-save t)
    (remove-hook 'kill-buffer-hook #'mentasm--on-kill-buffer t)
    (remove-hook 'post-command-hook #'mentasm--post-command-hook t))))

;;;###autoload
(defun mentasm ()
  "Find and load assembly for the current OCaml buffer and enable `mentasm-mode'.

Provides code region highlighting and automatic assembly reloading when .s files change."
  (interactive)
  (unless mentasm-mode
    (mentasm-mode))
  (mentasm--find-and-load-assembly))

;;;###autoload
(defun mentasm-select ()
  "Interactively select an assembly file from all .s files in the dune project."
  (interactive)
  (cond
   ((derived-mode-p 'asm-mode)
    (message "Cannot process assembly files. Are you sure you are not in the output buffer?"))
   ((not (or (derived-mode-p 'tuareg-mode) (derived-mode-p 'ocaml-mode)))
    (message "Mentasm only supports OCaml files (tuareg-mode or ocaml-mode)"))
   (t
    (mentasm--parse-options)
    (let* ((src-buffer (current-buffer))
           (project-root (mentasm--find-dune-project)))
      (if (not project-root)
          (message "No dune-project found. Make sure you're in a dune project directory.")
        (let* ((build-dirs (mentasm--find-build-directories project-root))
               (assembly-files (when build-dirs
                                (mentasm--find-assembly-files build-dirs))))
          (cond
           ((not build-dirs)
            (message "No _build directories found in project."))
           ((not assembly-files)
            (message "No assembly files (.s) found in build directories. Try building your project first."))
           (t
            ;; Create readable choices with relative paths
            (let* ((choices (mapcar
                            (lambda (path)
                              (cons (file-relative-name path project-root) path))
                            assembly-files))
                   (selected (completing-read "Select assembly file: "
                                            choices
                                            nil t)))
              (when selected
                (let ((full-path (cdr (assoc selected choices))))
                  (message "Loading assembly from: %s" selected)
                  ;; Enable mentasm-mode in source buffer for highlighting
                  (unless mentasm-mode
                    (mentasm-mode 1))
                  ;; Process the assembly file directly
                  (mentasm--load-assembly-file src-buffer full-path))))))))))))

(provide 'mentasm)

;;; mentasm.el ends here
