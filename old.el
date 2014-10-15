
;-----------------------------------------------------------------------------

;; ;;; general settings
;; (let ((theme (or (getenv "EMACS_THEME_NAME") "base16-tomorrow")))
;;   (load-theme (intern theme) t))

;; ;; (set-face-attribute  'default nil :font "Liberation Mono")
;; (set-face-foreground 'vertical-border "#fcf6e3")
;; (set-face-background 'vertical-border "#fcf6e3")

;; ;;; abbrev mode
;; (setq default-abbrev-mode t)
;; (setq save-abbrevs nil)
;; (setq abbrev-file-name "~/.emacs.d/abbrev-defs.el")

;; ;;; ack-mode
;; (defalias 'ack 'ack-and-a-half)
;; (defalias 'ack-same 'ack-and-a-half-same)
;; (defalias 'ack-find-file 'ack-and-a-half-find-file)
;; (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
;; (eval-after-load 'ack
;;   '(progn
;;      (evil-make-overriding-map ack-mode-map 'normal t)
;;      (evil-define-key 'normal ack-mode-map
;;        (kbd "RET") 'ack-find-match
;;        "j" 'ack-next-match
;;        "k" 'ack-previous-match
;;        "h" 'ack-previous-file
;;        "l" 'ack-next-file
;;        )))

;; ;; (evil-add-hjkl-bindings speedbar-key-map 'motion
;; ;;   "j" 'speedbar-next
;; ;;   "k" 'speedbar-prev
;; ;;   "i" 'speedbar-item-info
;; ;;   "G" 'evil-goto-line
;; ;;   "gg"'evil-goto-first-line
;; ;;   "r" 'speedbar-refresh
;; ;;   "u" 'speedbar-up-directory
;; ;;   "h" 'speedbar-up-directory
;; ;;   "o" 'speedbar-toggle-line-expansion
;; ;;   "l" 'speedbar-toggle-line-expansion
;; ;;   ";" 'smex
;; ;;   "q" 'kill-buffer-and-window
;; ;;   (kbd "RET") 'speedbar-edit-line)

;; ;;; diminish
;; (eval-after-load "abbrev" '(diminish 'abbrev-mode))
;; (eval-after-load "autopair" '(diminish 'autopair-mode))
;; ;;(eval-after-load "projectile" '(diminish 'projectile-mode))
;; (eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
;; (eval-after-load "helm-mode" '(diminish 'helm-mode))
;; (eval-after-load "auto-complete" '(diminish 'auto-complete-mode))
;; (add-hook 'emacs-lisp-mode-hook (lambda() (setq mode-name "el")))
;; (add-hook 'python-mode-hook (lambda() (setq mode-name "py")))

;; ;;; visual-regex
;; (define-key global-map (kbd "C-c r") 'vr/replace)
;; (define-key global-map (kbd "C-c q") 'vr/query-replace)
;; (define-key global-map (kbd "C-c m") 'vr/mc-mark)

;; ;;; spelling and grammar
;; (when (file-exists-p "~/.local/LanguageTool-2.4.1/languagetool-commandline.jar")
;;   (setq langtool-language-tool-jar  "~/.local/LanguageTool-2.4.1/languagetool-commandline.jar")
;;   (setq langtool-mother-tongue "en")
;;   (setq langtool-disabled-rules
;;         '("WHITESPACE_RULE" "EN_UNPAIRED_BRACKETS" "COMMA_PARENTHESIS_WHITESPACE" "EN_QUOTES")))

;; (when (executable-find "hunspell")
;;   (setq-default flyspell-issue-welcome-flag nil
;;                 ispell-program-name "hunspell"
;;                 ispell-really-hunspell t)
;;   (eval-after-load "flyspell"
;;     '(progn
;;        (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
;;        (define-key flyspell-mouse-map [mouse-3] #'undefined))))

;; ;;; paredit
;; (add-hook 'minibuffer-setup-hook
;;           (lambda () (if (memq this-command '(eval-expression))
;;                          (enable-paredit-mode))))

;; (eval-after-load 'paredit
;;   '(progn
;;      (evil-define-key 'normal paredit-mode-map
;;        "D"  'paredit-kill
;;        "gh" 'paredit-backward
;;        "gk" 'paredit-forward-up
;;        "gj" 'paredit-backward-up
;;        "gl" 'paredit-forward
;;        "x"  'paredit-forward-delete
;;        "X"  'paredit-backward-delete
;;        "))" 'paredit-forward-slurp-sexp
;;        ")}" 'paredit-forward-barf-sexp
;;        "((" 'paredit-backward-slurp-sexp
;;        "({" 'paredit-backward-barf-sexp
;;        "(J" 'paredit-join-sexps
;;        "(R" 'paredit-raise-sexp
;;        "(S" 'paredit-split-sexp
;;        "(s" 'paredit-splice-sexp
;;        "(W" 'paredit-wrap-sexp)))

;; ;;; multiple-cursors (only in emacs mode)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; ;;; org
;; (require 'my-orgcfg)

;; (setq ac-fuzzy-enable t)
;; (setq ac-use-fuzzy t)
;; (setq ac-auto-start 4)
;; (setq ac-auto-show-menu 0.4)
;; (setq ac-quick-help-delay 1.0)
;; ;; (setq ac-auto-show-menu nil)
;; (setq ac-quick-help-delay 1)
;; (setq ac-quick-help-height 50)
;; (setq ac-use-quick-help nil)
;; (setq ac-limit 10)
;; (setq ac-candidate-menu-min 2)

;; (map-add-to-list 'ac-modes 'org-mode 'latex-mode 'scss-mode
;;                  'magit-log-edit-mode 'text-mode 'yaml-mode 'nxml-mode
;;                  'html-mode 'sh-mode 'python-mode 'lisp-mode 'perl-mode
;;                  'cperl-mode 'go-mode 'rust-mode 'yaml-mode 'makefile-mode
;;                  'lua-mode 'js3-mode 'c-mode 'c++-mode)

;; (define-key evil-insert-state-map (kbd "C-X f") 'ac-complete-filename)
;; ;; (define-key ac-complete-mode-map [tab] 'ac-expand)

;; (eval-after-load "auto-complete"
;;   ; todo
;;   '(progn
;;      (add-hook 'css-mode-hook 'ac-css-mode-setup)
;;      (add-hook 'sass-mode-hook 'ac-css-mode-setup)))

;; (setq-default ac-sources '(ac-source-yasnippet
;;                            ac-source-abbrev
;;                            ac-source-dictionary
;;                            ac-source-filename
;;                            ac-source-words-in-same-mode-buffers))

;; ;; (setq-default ac-sources
;; ;;              '(ac-source-yasnippet
;; ;;                ;; ac-source-dictionary
;; ;;                ;; ac-source-words-in-buffer
;; ;;                ;; ac-source-words-in-same-mode-buffers
;; ;;                ;; ac-source-words-in-all-buffer
;; ;;                ))

;; (ac-flyspell-workaround)
;; (setq ac-math-unicode-in-math-p t)
;; (add-hook 'org-mode-hook 'ac-any-mode-setup)
;; ;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; ;;; minimap
;; ;; (setq minimap-always-recenter nil)
;; ;; (setq minimap-display-semantic-overlays nil)
;; ;; (setq minimap-width-fraction 0.1)
;; ;; (setq minimap-window-location '(right))
;; ;; (global-set-key (kbd "<f3>") 'minimap-toggle)
;; (global-set-key (kbd "<f3>") 'sr-speedbar-toggle)

;; ;;; calfw
;; ;; (setq cfw:fchar-junction ?╋
;; ;;       cfw:fchar-vertical-line ?┃
;; ;;       cfw:fchar-horizontal-line ?━
;; ;;       cfw:fchar-left-junction ?┣
;; ;;       cfw:fchar-right-junction ?┫
;; ;;       cfw:fchar-top-junction ?┯
;; ;;       cfw:fchar-top-left-corner ?┏
;; ;;       cfw:fchar-top-right-corner ?┓)

;; ;; (add-hook 'cfw:calendar
;; ;;           '(lambda ()
;; ;;              (load-theme-buffer-local 'adwaita (current-buffer))))


;; ;;; autopair
;; ;; (autopair-global-mode t)
;; ;; (setq autopair-autowrap t)
;; ;; (add-hook 'lisp-mode-hook #'(lambda () (setq autopair-dont-activate t)))
;; ;; (add-hook 'css-mode-hook #'(lambda () (setq autopair-dont-activate t)))
;; ;; (add-hook 'scss-mode-hook #'(lambda () (setq autopair-dont-activate t)))
;; ;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ;;; smartparens
;; (require 'smartparens)
;; ; todo: put in eval-after load
;; (require 'smartparens-latex)
;; (require 'smartparens-html)
;; ;; (require 'smartparens-lua)
;; ;; (require 'smartparens-ruby)

;; ; from graphene
;; (defun gp/sp/pair-on-newline (id action context)
;;   "Put trailing pair on newline and return to point."
;;   (save-excursion
;;     (newline)
;;     (indent-according-to-mode)))

;; (defun gp/sp/pair-on-newline-and-indent (id action context)
;;   "Open a new brace or bracket expression, with relevant newlines and indent. "
;;   (gp/sp/pair-on-newline id action context)
;;   (indent-according-to-mode))

;; (sp-pair "{" nil :post-handlers
;;          '(:add ((lambda (id action context)
;;                    (gp/sp/pair-on-newline-and-indent id action context)) "RET")))

;; (sp-pair "[" nil :post-handlers
;;          '(:add ((lambda (id action context)
;;                    (gp/sp/pair-on-newline-and-indent id action context)) "RET")))

;; (map-add-to-list 'sp--lisp-modes 'hy-mode 'el)
;; (sp-with-modes sp--lisp-modes
;;   (sp-local-pair "'" nil :actions nil)
;;   (sp-local-pair "`" "'" :when '(sp-in-string-p)))

;; (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
;;   (sp-local-pair "$" "$")
;;   (sp-local-pair "\\[" "\\]")
;;   (sp-local-pair "`" "'")
;;   (sp-local-tag "\\b" "\\begin{_}" "\\end{_}"))

;; (sp-with-modes '(python-mode)
;;   (sp-local-pair "\"" nil :unless '(sp-point-after-word-p))
;;   (sp-local-pair "'" nil :unless '(sp-point-after-word-p)))

;; (setq sp-highlight-pair-overlay nil)
;; (setq sp-autoescape-string-quote nil)
;; (setq sp-autoskip-closing-pair "always-end")
;; (setq sp-cancel-autoskip-on-backward-movement nil)

;; ;; html modes
;; (sp-with-modes '(sgml-mode html-mode)
;;   (sp-local-pair "<" ">")
;;   (sp-local-tag  "<" "<_>" "</_>" :transform 'sp-match-sgml-tags))

;; ;; (smartparens-global-strict-mode t)
;; (smartparens-global-mode t)
;; (show-smartparens-global-mode t)
;; ;; (setq sp-autoescape-string-quote t)
;; ;; (setq sp-autoinsert-quote-if-followed-by-closing-pair t)

;; ;;; helm
;; ;;; latex
;; (setq TeX-PDF-mode t)
;; (setq TeX-parse-self t)
;; (setq TeX-PDF-mode t)
;; (setq-default TeX-master nil)
;; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; (setq reftex-plug-into-AUCTeX t)

;; ;;;; keymaps

;; ;;; buffers and windows
;; ;; (define-key evil-visual-state-map ",n" 'comment-or-uncomment-region)
;; ;; (global-set-key (kbd "C-w o") 'switch-window)
