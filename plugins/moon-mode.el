;;; moon-mode.el --- Major mode for editing Moonscript files in Emacs

;;; Commentary

;; Provides syntax highlighting, indentation support, imenu support, a
;; menu bar, and a few utility commands.

(require 'font-lock)
(require 'lua-mode)
(eval-when-compile
  (require 'cl))

;; Customizable Variables
(defconst moon-mode-version "0.0.1"
  "the version of `moon-mode'.")

(defgroup moon nil
  "A MoonScript major mode."
  :group 'languages)

(defcustom moon-tab-width tab-width
  "The tab width to use when indenting."
  :type 'integer
  :group 'moon)

(defcustom moon-command "moon"
  "The MoonScript command used for evaluating code."
  :type 'string
  :group 'moon)

(defcustom moon-compiler-command "moonc"
  "The MoonScript command used for compiling code."
  :type 'string
  :group 'moon)

(defcustom moon-comment-start "-- "
  "Default value of `comment-start'."
  :type 'string
  :group 'moon)

(defcustom moon-comment-start-skip "-- "
  "Default value of `comment-start-skip'."
  :type 'string
  :group 'moon)

(defcustom moon-compiled-buffer-name "*moon-compiled*"
  "The name of the scratch buffer used for compiled MoonScript."
  :type 'string
  :group 'moon)

(defcustom moon-compile-jump-to-error t
  "Whether to jump to the first error if compilation fails.
Since the moonscript compiler does not always include a line number in
its error messages, this is not always possible."
  :type 'boolean
  :group 'moon)

(defcustom moon-watch-buffer-name "*moon-watch*"
  "The name of the scratch buffer used when using the --watch flag
with MoonScript."
  :type 'string
  :group 'moon)

(defcustom moon-mode-hook nil
  "Hooks called when Moonscript mode fires up."
  :type 'hook
  :group 'moon)

(defcustom moon-lua-directory ""
  "The directory for compiled Lua files output"
  :type 'string
  :group 'moon)

(defcustom moon-compile-jump-to-error t
  "The directory for compiled Lua files output"
  :type 'bool
  :group 'moon)



(defvar moon-indent-after-keywords-regexp
  (regexp-opt
   '( "if" "unless" "else" "for" "while" "with" "elseif" "class"
      "switch" "when" )
   'words))

(defvar moon-indent-after-operators
  '(?> ?\{ ))


(defun moon-line-wants-indent ()
  "Return t if the current line should be indented relative to the
previous line."
  (interactive)

  (save-excursion
  (let ((indenter-at-bol) (indenter-at-eol))
    ;; go back a line and to the first character
    (forward-line -1)
    (backward-to-indentation 0)

    ;; If the next few characters match one of our magic indenter
    ;; keywords, we want to indent the line we were on originally.
    (when (looking-at moon-indent-after-keywords-regexp)
      (setq indenter-at-bol t))

    ;; If that didn't match, go to the back of the line and check to
    ;; see if the last character matches one of our indenter
    ;; characters.
    (when (not indenter-at-bol)
      (end-of-line)

      ;; Optimized for speed - checks only the last two character.
      (let ((indenters moon-indent-after-operators))
        (while indenters
          (if (and (char-before) (/= (char-before) (car indenters)))
              (setq indenters (cdr indenters))
            (setq indenter-at-eol t)
            (setq indenters nil)))))

    (or indenter-at-bol indenter-at-eol))))

(defun moon-newline-and-indent ()
  "Insert a newline and indent it to the same level as the previous line."
  (interactive)

  ;; Remember the current line indentation level, insert a newline,
  ;; and indent the newline to the same level as the previous line.
  (let ((prev-indent (current-indentation))
        (indent-next nil))
    (delete-horizontal-space t)
    (newline)
    (insert-tab (/ prev-indent moon-tab-width))

    ;; We need to insert an additional tab because the last line was special.
    (when (moon-line-wants-indent)
      (insert-tab)))

  ;; Last line was a comment so this one should probably be,
  ;; too. Makes it easy to write multi-line comments (like the one I'm
  ;; writing right now).
  (when (save-excursion
          (forward-line -1)
          (backward-to-indentation 0)
          (search-forward moon-comment-start (+ (point) 3)))
    (insert "-- ")))

(defun moon-compiled-file-name (&optional filename)
  (let ((working-on-file (expand-file-name (or filename (buffer-file-name)))))
        (unless (string= moon-lua-directory "")
          (setq working-on-file
                (expand-file-name (concat (file-name-directory working-on-file)
                                          moon-lua-directory
                                          (file-name-nondirectory working-on-file)))))
          (concat (file-name-sans-extension working-on-file) ".lua")))

;; (defun moon-compile-file ()
;;   (interactive)
;;   (let ((compiler-output (shell-command-to-string (concat
;;                                                    moon-compiler-command " "
;;                                                    (buffer-file-name)))))
;;     (message compiler-output)
    ;; (if (string= compiler-output "")
    ;;     (message "Compiled and saved %s" (moon-compiled-file-name))
    ;;   (let* ((msg (last (split-string compiler-output "[\n\r]+")))
    ;;          (line (and (string-match "\\[\\([0-9]+\\)\\] >>" msg)
    ;;                     (string-to-number (match-string 1 msg)))))
    ;;     (message msg)
    ;;     (when (and moon-compile-jump-to-error line (> line 0))
    ;;       (goto-char (point-min))
    ;;       (forward-line (1- line)))))
    ;; ))
    

(defvar moon-boolean-regexp
  (regexp-opt
   '( "nil" "true" "false" )
   'symbols))

(defvar moon-keywords-regexp
  (regexp-opt
   '( "and" "break" "class" "do" "else" "elseif" "end" "export" "extends"
      "for" "from" "function" "import" "in" "local" "not" "or" "if"
      "repeat" "return" "then" "until" "while" "with" )
   'symbols))

(defvar moon-string-regexp
  "\"\\([^\\]\\|\\\\.\\)*?\"\\|'\\([^\\]\\|\\\\.\\)*?'")

(defvar moon-function-keywords
  '("->" "=>" "(" ")" "[" "]" "{" "}"))

(defvar moon-function-regexp
  (regexp-opt moon-function-keywords))

(defvar moon-octal-number-regexp
  "\\_<0x[[:xdigit:]]+\\_>")

(defvar moon-table-key-regexp
  "\\_<\\w+:")

(defvar moon-ivar-regexp
  "@\\_<\\w+\\_>")

(defvar moon-assignment-var-regexp
  (concat "\\(\\_<\\w+\\) = "))

(defvar moon-assign-regexp
  "\\(\\(\\w\\|\\.\\|_\\|$\\)+?\s*\\):")

(defvar moon-local-assign-regexp
  "\\(\\(_\\|\\w\\|\\$\\)+\\)\s+=")

(defvar moon-number-regexp
  (mapconcat 'identity '("[0-9]+\\.[0-9]*" "[0-9]*\\.[0-9]+" "[0-9]+") "\\|"))

(defvar moon-font-lock-defaults
  `(
    (,moon-string-regexp . font-lock-string-face)
    (,moon-boolean-regexp . font-lock-constant-face)
    (,moon-assignment-var-regexp . (1 font-lock-variable-name-face))
    (,moon-assign-regexp . font-lock-type-face)
    (,moon-local-assign-regexp (1 font-lock-variable-name-face))
    (,moon-octal-number-regexp . font-lock-constant-face)
    (,moon-number-regexp . font-lock-constant-face)
    (,moon-function-regexp . font-lock-function-name-face)
    (,lua--builtins (1 font-lock-builtin-face) (2 font-lock-builtin-face nil noerror))
    (,moon-keywords-regexp . font-lock-function-name-face)
  ))

(defun moon-indent-line ()
  "Indent current line as MoonScript."
  (interactive)

  (if (= (point) (point-at-bol))
      (insert-tab)
    (save-excursion
      (let ((prev-indent (moon-previous-indent))
            (cur-indent (current-indentation)))
        ;; Shift one column to the left
        (beginning-of-line)
        (insert-tab)

        (when (= (point-at-bol) (point))
          (forward-char moon-tab-width))

        ;; We're too far, remove all indentation.
        (when (> (- (current-indentation) prev-indent) moon-tab-width)
          (backward-to-indentation 0)
          (delete-region (point-at-bol) (point)))))))


(defun moon-previous-indent ()
  "Return the indentation level of the previous non-blank line."
  (save-excursion
    (forward-line -1)
    (if (bobp)
        0
      (progn
        (while (and (looking-at "^[ \t]*$") (not (bobp))) (forward-line -1))
        (current-indentation)))))


(defvar moon-mode-map
  (let ((map (make-sparse-keymap)))
    ;; key bindings
    ;; (define-key map (kbd "A-r") 'coffee-compile-buffer)
    ;; (define-key map (kbd "A-R") 'coffee-compile-region)
    ;; (define-key map (kbd "A-M-r") 'coffee-repl)
    ;; (define-key map [remap comment-dwim] 'coffee-comment-dwim)
    (define-key map [remap newline-and-indent] 'moon-newline-and-indent)
    (define-key map "\C-m" 'moon-newline-and-indent)
    ;; (define-key map "\C-c\C-o\C-s" 'coffee-cos-mode)
    ;; (define-key map "\177" 'coffee-dedent-line-backspace)
    ;; (define-key map (kbd "C-c C-<") 'coffee-indent-shift-left)
    ;; (define-key map (kbd "C-c C->") 'coffee-indent-shift-right)
    map)
  "Keymap for CoffeeScript major mode.")


;;;###autoload
(define-derived-mode moon-mode fundamental-mode
  "Moon"
  "Major mode for editing MoonScript."

  ;; code for syntax highlighting
  (set (make-local-variable 'font-lock-defaults) '(moon-font-lock-defaults))

  (set (make-local-variable 'comment-start) moon-comment-start)
  (set (make-local-variable 'comment-start-skip) moon-comment-start-skip)

  (set (make-local-variable 'compile-command)
       (let ((file (buffer-file-name)))
         (format "cd $(dirname %s) && moonc $(basename %s)" file file)))

  ;; indentation
  (set (make-local-variable 'indent-line-function) 'moon-indent-line)
  (set (make-local-variable 'tab-width) moon-tab-width)
  ;; (set (make-local-variable 'syntax-propertize-function) 'coffee-propertize-function)
  
  ;; single quote strings
  (modify-syntax-entry ?' "\"" moon-mode-syntax-table)

  ;; comments
  (set-syntax-table (copy-syntax-table))
  (modify-syntax-entry ?\- ". 12" moon-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" moon-mode-syntax-table)
  (modify-syntax-entry ?\_ "w" moon-mode-syntax-table)
  (modify-syntax-entry ?+ "." moon-mode-syntax-table)
  (modify-syntax-entry ?- ". 12" moon-mode-syntax-table)
  (modify-syntax-entry ?* "." moon-mode-syntax-table)
  (modify-syntax-entry ?/ "." moon-mode-syntax-table)
  (modify-syntax-entry ?^ "." moon-mode-syntax-table)
  ;; This might be better as punctuation, as for C, but this way you
  ;; can treat table index as symbol.
  (modify-syntax-entry ?. "_" moon-mode-syntax-table) ; e.g. `io.string'
  (modify-syntax-entry ?> "." moon-mode-syntax-table)
  (modify-syntax-entry ?< "." moon-mode-syntax-table)
  (modify-syntax-entry ?= "." moon-mode-syntax-table)
  (modify-syntax-entry ?~ "." moon-mode-syntax-table)
  (modify-syntax-entry ?\n ">" moon-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" moon-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" moon-mode-syntax-table)

  ;; no tabs
  (setq indent-tabs-mode nil))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.moon$" . moon-mode))

(provide 'moon-mode)
