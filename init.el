;; -*- elisp -*

(setq basedir (expand-file-name "~/.emacs.d/"))
;; (setq default-directory "~/source/")

;-----------------------------------------------------------------------------
; setup load-path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/colors"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'image-load-path (concat basedir "images"))
(add-to-list 'custom-theme-load-path (concat basedir "colors"))

(require 'config-defuns)
(require 'my-defuns)

;;;---------------------------------------------------------------------------
;;; Core Config
;;;---------------------------------------------------------------------------

(defconst is-bsd (eq system-type 'berkeley-unix))
(defconst is-linux (eq system-type 'gnu/linux))

;-----------------------------------------------------------------------------
; package management
(setq package-enable-at-startup t)
(setq delete-old-versions nil)

(require 'package)
(require 'cask "~/.cask/cask.el")

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(cask-initialize)
(require 'pallet)

;(setq use-package-verbose t)
(require 'use-package)
(require 'dash)

;-----------------------------------------------------------------------------
; general settings
(fset 'yes-or-no-p 'y-or-n-p)
(setq stack-trace-on-error t)
(setq confirm-nonexistent-file-or-buffer nil)

;-----------------------------------------------------------------------------
; encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;-----------------------------------------------------------------------------
(setq-default enable-recursive-minibuffers nil)
(setq-default redisplay-dont-pause t)
(setq-default confirm-kill-emacs nil)
(setq-default compilation-scroll-output 'first-error)
(setq-default buffers-menu-max-size 30)

;-----------------------------------------------------------------------------
(setq inhibit-startup-message t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-echo-area-message t)

;-----------------------------------------------------------------------------
; scratch buffer
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message nil)

;-----------------------------------------------------------------------------
; backup and autosave
(if (not (file-exists-p "~/.emacs.d/backups"))
    (make-directory "~/.emacs.d/backups" t))

(setq backup-directory-alist `(("." . "~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms
      `((".*", "~/.emacs.d/backups/" t)))

(setq make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      delete-by-moving-to-trash nil
      kept-old-version 6
      kept-new-versions 9)

(setq delete-auto-save-files t
      auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)

(setq create-lockfiles t)

;(setq auto-save-file-name-transforms ...)

;-----------------------------------------------------------------------------
; ui settings
;; (setq scroll-margin 0)
;; (setq scroll-conservatively 100000)
;; (setq scroll-preserve-screen-position 1)

(setq scroll-step 2)
(setq scroll-conservatively 4)
(setq mouse-wheel-follow-mouse t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; (setq-default line-spacing nil)
(setq indicate-empty-lines t)
(setq indicate-buffer-boundaries 'left)
(setq split-height-threshold 110)
(setq enable-recursive-minibuffers nil)

;-----------------------------------------------------------------------------
; theme
(load-theme 'zenburn t)
;; (load-theme 'base16-tomorrow t)

;-----------------------------------------------------------------------------
; font
(add-to-list 'default-frame-alist '(line-spacing . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 0))
(add-to-list 'default-frame-alist '(font .  "DejaVu Sans Mono-10"))
;; (add-to-list 'default-frame-alist '(font .  "Fantasque Sans Mono-12"))
;; (set-face-font 'default  "DejaVu Sans Mono-10")
;; (set-face-font 'variable-pitch  "Liberation Sans-10")
;; (set-face-font 'fixed-pitch "DejaVu Sans Mono-10" )
;; (set-face-font 'default "Fantasque Sans Mono-12")

;-----------------------------------------------------------------------------
; clipboard
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq save-interprogram-paste-before-kill t)
(setq mouse-yank-at-point t)

;-----------------------------------------------------------------------------
; features
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;-----------------------------------------------------------------------------
; ui elements
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'fringe-mode) (fringe-mode '(4 . 6)))

;-----------------------------------------------------------------------------
; ui minor modes
(blink-cursor-mode -1)
(global-hl-line-mode t)
(transient-mark-mode t)
(delete-selection-mode t)

(setq frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name))
                "%b")))

;-----------------------------------------------------------------------------
; line numbers and fringe
;; (global-linum-mode t)
(require 'linum-relative)
(setq linum-format "%3d")

;-----------------------------------------------------------------------------
; smart parenthesis
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)
(set-face-background 'show-paren-match-face (face-background 'default))
(set-face-foreground 'show-paren-match-face "magenta")
(set-face-attribute 'show-paren-match-face nil :weight 'bold)
;; (set-face-background 'show-paren-match-face nil)
;; (set-face-foreground 'show-paren-match-face nil)

;-----------------------------------------------------------------------------
(use-package whitespace
  :bind ("<f12>" . whitespace-mode)
  :config
  (setq whitespace-style '(face tabs spaces trailing lines space-before-tab newline
                                indentation empty space-after-tab space-mark tab-mark newline-mark)))

;-----------------------------------------------------------------------------
; modeline
(column-number-mode t)
(line-number-mode t)
(size-indication-mode 1)

(use-package smart-mode-line
  :init
  (progn
    (setq sml/no-confirm-load-theme t)
    (setq sml/mode-width 'full)
    (setq sml/show-remote nil)
    (setq sml/encoding-format nil)
    (setq sml/vc-mode-show-backend t)
    (sml/setup)
    (sml/apply-theme 'respectful)))

;-----------------------------------------------------------------------------
; preferred applications
(setq browse-url-generic-program "firefox")
(cond (is-bsd   (setq shell-file-name "/usr/local/bin/zsh"))
      (is-linux (setq shell-file-name "/bin/zsh")))

;-----------------------------------------------------------------------------
; hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;---------------------------------------------------------------------------
;;; Editor Config
;;;---------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; word fill
(setq sentence-end-double-space nil)
(setq colon-double-space nil)
;; (setq-default truncate-lines t)

;-----------------------------------------------------------------------------
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent nil)
(setq require-final-newline t)

; url
(setq url-configuration-directory "~/.emacs.d/cache/url")

;-----------------------------------------------------------------------------
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))

;-----------------------------------------------------------------------------
; editing minor modes
(electric-indent-mode -1)

; save position
(use-package saveplace
  :init
  (progn
    (setq-default save-place t)
    (setq save-place-file "~/.emacs.d/cache/saveplace")))

; persistent history
(use-package savehist
  :init
  (progn
    (setq savehist-file "~/.emacs.d/cache/savehist")
    (setq savehist-autosave-interval 60)
    (setq savehist-additional-variables
          '(search ring kill-ring mark-ring search-ring regexp-search-ring extended-command-history))
    (savehist-mode 1)))

(use-package recentf
  :init
  (progn
    (setq recentf-save-file "~/.emacs.d/cache/recentf")
    (setq recentf-auto-cleanup 'never)
    (setq recentf-max-saved-items 500)
    (setq recentf-max-menu-items 40)
    (setq recentf-auto-save-timer (run-with-idle-timer 600 t 'recentf-save-list))
    (setq recentf-exclude
          '("/tmp/" "\\.ido\\.last" "ido.last" "\\.git/config" "\\.git/COMMIT_EDITMSG"
            "cache/recentf" "\\.emacs\\.d/elpa/.*" "\\.emacs\\.d/.cask/.*"  ))
    (add-hook 'kill-emacs-hook #'recentf-cleanup)
    (recentf-mode 1)))

;-----------------------------------------------------------------------------
(add-hook 'text-mode-hook 'enable-hard-wrap)
(add-hook 'prog-mode-hook 'enable-comment-hard-wrap)

;-----------------------------------------------------------------------------
; evil
(use-package evil-leader
  :config
  (progn
    (evil-leader/set-leader ",")
    (setq evil-leader/in-all-states t)
    (global-evil-leader-mode)))

(use-package evil-args
  :init
  (progn
     (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
     (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
     (evil-leader/set-key
       "an" 'evil-forward-arg
       "ap" 'evil-backward-arg
       "a`" 'evil-jump-out-args)))

(use-package evil-jumper
  :init
  (progn
    (setq evil-jumper-auto-center t)
    (setq evil-jumper-file "~/.emacs.d/cache/evil-jumper")
    (setq evil-jumper-auto-save-interval 1800)))

;; (use-package sackspace
;;   :init (sackspace-global-mode 1))

(use-package goto-chg)

(use-package evil
  :config
  (progn
    (evil-mode 1)
    (add-hook! 'find-file-hook (setq evil-shift-width tab-width))

    (setq evil-search-module 'evil-search)
    (setq evil-magic 'very-magic)
    ;(setq evil-esc-delay 0)
    (setq evil-move-cursor-back t)
    (setq evil-cross-lines nil)
    (setq evil-symbol-word-search t)
    ;; (evil-set-toggle-key "<pause>")

    ; look and feel
    (setq evil-normal-state-cursor '("white" box))
    (setq evil-visual-state-cursor '("blue" box))
    (setq evil-god-state-cursor '("orange" box))

    ;; (setq evil-normal-state-tag (propertize "N" 'face '((:background "green" :foreground "black"))))
    ;; (setq evil-emacs-state-tag  (propertize "E" 'face '((:background "orange" :foreground "black"))))
    ;; (setq evil-insert-state-tag (propertize "I" 'face '((:background "red"))))
    ;; (setq evil-motion-state-tag (propertize "M" 'face '((:background "blue"))))
    ;; (setq evil-visual-state-tag (propertize "V" 'face '((:background "grey80" :foreground "black"))))
    ;; (setq evil-operator-state-tag (propertize "O" 'face '((:background "purple"))))

    ; initial mode states
    (evil-set-initial-state 'dired 'emacs)
    (evil-set-initial-state 'magit-branch-manager-mode 'emacs)
    (evil-set-initial-state 'magit-commit-mode 'emacs)
    (evil-set-initial-state 'magit-log-mode 'normal)
    (evil-set-initial-state 'log-view-mode 'emacs)
    (evil-set-initial-state 'ibuffer-mode 'normal)
    (evil-set-initial-state 'ack-mode 'normal)
    (evil-set-initial-state 'inferior-emacs-lisp-mode 'emacs)
    (evil-set-initial-state 'comint-mode 'emacs)
    (evil-set-initial-state 'shell-mode 'emacs)
    (evil-set-initial-state 'term-mode 'emacs)
    (evil-set-initial-state 'artist-mode 'emacs)
    (evil-set-initial-state 'image-mode 'emacs)
    (evil-set-initial-state 'bc-menu-mode 'emacs)
    (evil-set-initial-state 'erc-mode 'normal)
    (evil-set-initial-state 'calc-mode 'emacs)

    (evil-defmap evil-ex-completion-map
      (kbd "C-r") #'evil-ex-paste-from-register ; registers in ex-mode
      (kbd "C-a") 'move-beginning-of-line
      (kbd "<s-left>") 'move-beginning-of-line
      (kbd "<s-right>") 'move-beginning-of-line
      (kbd "<s-backspace>") 'evil-delete-whole-line)

    (evil-defmap evil-normal-state-map
       "+"  'evil-numbers/inc-at-pt
       "-"  'evil-numbers/dec-at-pt
       "[e" 'previous-error
       "]e" 'next-error)

    ;; select paste immediately after paste
    ;; (define-key evil-normal-state-map "p"
    ;;   (lambda ()
    ;;     (interactive)
    ;;     (evil-paste-after)
    ;;     (my/select-previous-evil-paste)))

    ; evil packages
    (use-package evil-exchange
      :init (evil-exchange-install))

    (use-package evil-nerd-commenter
      :config
      (setq evilnc-hotkey-comment-operator "g#"))

    (use-package evil-matchit
      :init (global-evil-matchit-mode 1))

    (use-package evil-surround
      :init
      (progn
        (global-evil-surround-mode 1)

        (add-hooks! '(emacs-lisp-mode-hook lisp-mode-hook)
                    (push '(?` . ("`" . "'")) evil-surround-pairs-alist))

        (add-hooks! '(markdown-mode-hook rst-mode-hook python-mode-hook)
                    (push '(?~ . ("``" . "``")) evil-surround-pairs-alist))

        (setq-default evil-surround-pairs-alist
                      '((?\) . ("(" . ")"))
                        (?\] . ("[" . "]"))
                        (?\} . ("{" . "}"))
                        (?\( . ("( " . " )"))
                        (?\[ . ("[ " . " ]"))
                        (?\{ . ("{ " . " }"))
                        (?< . ("< " . " >"))
                        (?# . ("#{" . "}"))
                        (?p . ("(" . ")"))
                        (?b . ("[" . "]"))
                        (?B . ("{" . "}"))
                        (?> . ("<" . ">"))
                        (?t . evil-surround-read-tag)))))))

;-----------------------------------------------------------------------------
(global-set-key [escape] 'keyboard-escape-quit)
(define-key evil-insert-state-map (kbd "C-g") 'evil-force-normal-state)
;; (define-key evil-normal-state-map [escape] 'keyboard-quit)
;; (define-key evil-visual-state-map [escape] 'keyboard-quit)

(mapc (lambda (map)
        (evil-defmap map [escape] 'my/minibuffer-quit))
      (list minibuffer-local-map
            minibuffer-local-ns-map
            minibuffer-local-completion-map
            minibuffer-local-must-match-map
            minibuffer-local-isearch-map))

(evil-defmap evil-ex-search-keymap [escape] 'evil-ex-search-exit)
(evil-defmap isearch-mode-map [escape] 'isearch-abort)
(evil-defmap help-mode-map [escape] 'kill-buffer-and-window)
(evil-defmap compilation-mode-map [escape] 'kill-buffer-and-window)

;;;---------------------------------------------------------------------------
;;; Minor modes
;;;---------------------------------------------------------------------------

;-----------------------------------------------------------------------------
(use-package window-numbering
  :init (window-numbering-mode 1))

;-----------------------------------------------------------------------------
(use-package winner
  :init (winner-mode 1))

(use-package dedicated
  :bind ("C-x d" . dedicated-mode))

;-----------------------------------------------------------------------------
(use-package uniquify
  :config
  (progn
    ;; (setq uniquify-buffer-name-style 'post-forward)
    (setq uniquify-buffer-name-style 'forward)
    (setq uniquify-separator " • ")
    ;; (setq uniquify-separator "/")
    (setq uniquify-after-kill-buffer-p t)
    (setq uniquify-ignore-buffers-re "^\\*")))

;-----------------------------------------------------------------------------
(use-package volatile-highlights
  :diminish ""
  :config
  (progn
    (volatile-highlights-mode t)
    (vhl/give-advice-to-make-vhl-on-changes evil-paste-after)
    (vhl/give-advice-to-make-vhl-on-changes evil-paste-before)
    (vhl/give-advice-to-make-vhl-on-changes evil-paste-pop)))

;-----------------------------------------------------------------------------
(use-package visual-regexp-steroids)

;-----------------------------------------------------------------------------
;; (use-package auto-complete
;;   :diminish auto-complete-mode
;;   :init
;;   (progn
;;     (require 'auto-complete-config)
;;     (setq ac-auto-start t
;;           ac-auto-show-menu t ; Suggestions box must be invoked manually (see core-keymaps.el)
;;           ac-use-menu-map t   ; Enable ac-menu-map map when menu is open
;;           ac-use-quick-help t ; Don't show tooltips unless invoked (see core-keymaps.el)
;;           ac-use-fuzzy nil
;;           ac-candidate-limit 25)
;;     (setq ac-comphist-file "~/.emacs.d/cache/ac-comphist.dat")
;;     (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
;;     (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
;;     (add-hook 'css-mode-hook 'ac-css-mode-setup)
;;     (add-hook 'shell-script-mode-hook 'ac-add-files)
;;     (global-auto-complete-mode t))
;;   :config
;;   (progn
;;     ;; (add-to-list 'ac-dictionary-files (expand-file-name "global" *ac-dicts-dir))
;;     ;; (add-to-list 'ac-dictionary-directories *ac-dicts-dir)
;;     (add-to-list 'ac-modes 'nxml-mode)
;;     (setq completion-ignore-case t)))

;-----------------------------------------------------------------------------
(use-package company
  :diminish company-mode
  :config
  (progn
    (global-company-mode 1)
    (setq company-idle-delay 0.1
          company-tooltip-limit 30
          company-minimum-prefix-length 2
          company-show-numbers t
          ;; company-require-match nil
          company-echo-delay 1
          company-auto-complete nil
          company-auto-complete-chars)

    ;; (define-key company-mode-map (kbd "C-j") 'company-select-next)
    ;; (define-key company-mode-map (kbd "C-k") 'company-select-previous)
    (setq company-backends '(company-elisp company-nxml company-clang company-cmake company-capf
                          (company-dabbrev-code company-gtags company-etags company-keywords)
                          company-files company-dabbrev))))

;-----------------------------------------------------------------------------
(use-package yasnippet
  :mode (("emacs.+/snippets/" . snippet-mode))
  :diminish yas-minor-mode
  :config
  (progn
    ;; (defadvice evil-force-normal-state (before evil-esc-quit-yasnippet activate)
    ;;   (shut-up (yas-exit-all-snippets)))
    ;; Only load personal snippets
    (setq yas-verbosity 1)
    (setq yas-triggers-in-field t)
    (setq yas-snippet-dirs '("~/.emacs.d/snippets" yas-installed-snippets-dir))
    (setq yas-prompt-functions '(yas-ido-prompt yas-no-prompt))
    (after auto-complete
           (add-hook! 'yas-before-expand-snippet-hook (auto-complete-mode -1))
           (add-hook! 'yas-after-exit-snippet-hook (auto-complete-mode t))
           (defadvice ac-expand (before advice-for-ac-expand activate)
             (when (yas-expand) (ac-stop))))
    (evil-defmap yas-keymap (kbd "DEL") 'my/yas-clear-field)
    (yas-reload-all))
  :init
  (progn
    (defvar yas-minor-mode-map
      (let ((map (make-sparse-keymap)))
        (evil-define-key 'insert map [(tab)] 'yas-expand)
        (evil-define-key 'insert map (kbd "TAB") 'yas-expand)
        map))
    (add-hook 'prog-mode-hook 'yas-minor-mode)
    (add-hook 'snippet-mode-hook 'yas-minor-mode)
    (add-hook 'markdown-mode-hook 'yas-minor-mode)
    (add-hook 'org-mode-hook 'yas-minor-mode)))

(defun my/yas-clear-field (&optional field)
  (interactive)
  (let ((field (or field
                   (and yas--active-field-overlay
                        (overlay-buffer yas--active-field-overlay)
                        (overlay-get yas--active-field-overlay 'yas--field)))))
    (cond ((and field
                (not (yas--field-modified-p field))
                (eq (point) (marker-position (yas--field-start field))))
           (yas--skip-and-clear field))
          (t (delete-char -1)))))

;-----------------------------------------------------------------------------
; flycheck
(use-package flycheck
  :init
  (progn
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
    (setq flycheck-indication-mode 'right-fringe)))


;-----------------------------------------------------------------------------
; tramp
(setq tramp-persistency-file-name "~/emacs/cache/tramp")
(setq tramp-default-method "ssh")
;; (setq tramp-debug-buffer t)
(setq tramp-verbose 2)
;; (setq tramp-password-prompt-regexp ".*[Pp]assword: *$")
;; (setq tramp-shell-prompt-pattern "^[^;$#>]*[;$#>] *")
(setq password-cache-expiry nil)

;-----------------------------------------------------------------------------
; imenu
(set-default 'imenu-auto-rescan t)

;-----------------------------------------------------------------------------
; ido
(use-package ido-ubiquitous)
;; (use-package ido-vertical-mode)
(use-package flx-ido)

(ido-mode 1)
;; (ido-vertical-mode 1)
;; (ido-everywhere 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)

(setq ido-use-faces t
      ido-confirm-unique-completion t
      ido-case-fold t
      ;; ido-auto-merge-work-directories-length -1
      ido-enable-tramp-completion nil
      ido-default-buffer-method 'selected-window
      ;; ido-max-prospects 30
      ido-use-filename-at-point 'guess
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-enable-tramp-completion t
      ido-enable-last-directory-history t)

;; (setq ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido"))
;; (setq ido-work-directory-list '("~/" "~/source/"))
;; (setq ido-case-fold t)
;; (setq ido-save-directory-list-file "~/.emacs.d/cache/ido.last")
;; (setq ido-use-filename-at-point nil)
;; (setq ido-enable-prefix nil)
;; (setq ido-enable-flex-matching t)
;; (setq ido-show-dot-for-dired t)
;; (setq ido-confirm-unique-completion nil)  ; can't make up my mind about this
;; (setq ido-default-buffer-method 'selected-window)
;; (setq confirm-nonexistent-file-or-buffer nil)
;; (setq ido-auto-merge-work-directories-length 0)
;; (setq ido-create-new-buffer 'always)
;; (setq ido-use-virtual-buffers t)
;; (setq ido-handle-duplicate-virtual-buffers 2)
;; (setq ido-max-prospects 10)
;; (setq ido-create-new-buffer 'always)
;; (setq ido-file-extensions-order
;;       '(".org" ".py" ".sh" ".c" ".emacs" ".xml" ".el" ))

;-----------------------------------------------------------------------------
(use-package popwin
  :config
  (progn
    (popwin-mode 1)
    (bind-key "C-c C-p" popwin:keymap)
    (push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
    (push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)))


;-----------------------------------------------------------------------------
(use-package helm
  :diminish helm-mode
  :init
  (progn
    (setq helm-input-idle-delay 0)
    (setq helm-quick-update t)
    (setq helm-idle-delay 0.05)
    (setq helm-buffer-max-length 30)
    (setq helm-buffer-details-flag nil)
    (setq helm-quick-update t)
    (setq helm-candidate-number-limit nil)
    (setq helm-su-or-sudo "sudo")
    (setq helm-allow-skipping-current-buffer nil)
    (setq helm-enable-shortcuts t)
    (setq helm-ff-transformer-show-only-basename nil)

    (setq helm-ff-lynx-style-map nil)
    (setq helm-ff-auto-update-initial-value nil)
    (setq helm-yank-symbol-first t)

    (setq helm-M-x-requires-pattern t)
    (setq helm-M-x-always-save-history t)
    (setq helm-split-window-in-side-p t)

    (setq helm-c-boring-file-regexp
          (rx (or
               ;; directories
               (and "/" (or ".svn" ".git" ".hg" "__pycache__") (or "/" eol))
               ;; files
               (and line-start (or ".#" "."))
               (and (or ".class" ".la" ".o" "~" ".pyc" ".pyo" ".elc") eol))))

    (setq helm-c-boring-buffer-regexp
          (rx (or
               (and line-start " ")
               "*helm"
               "*ac-mode-"
               "Map_Sym.txt"
               "*Ibuffer*"
               "*Help*"
               "*Pp Eval Output*"
               "*Completions*"
               "*Customize")))
    (helm-mode 1))

    :config
    (progn
      (define-key helm-map (kbd "<f11>") 'my/helm-fullscreen)
      (define-key helm-map (kbd "C-j") 'helm-next-line)
      (define-key helm-map (kbd "C-k") 'helm-previous-line)))


;;;---------------------------------------------------------------------------
;;; Major modes
;;;---------------------------------------------------------------------------

;-----------------------------------------------------------------------------
(use-package smex
  :config
  (progn
    (smex-initialize)
    (define-key evil-normal-state-map (kbd ";") 'smex)
    (define-key evil-visual-state-map (kbd ";") 'smex)
    (global-set-key (kbd "M-x") 'smex)
    (global-set-key (kbd "M-X") 'smex-major-mode-commands)
    (setq smex-save-file  "~/.emacs.d/cache/smex-items")
    (setq smex-history-length 20)

    (defun smex-update-after-load (ignore)
      (when (boundp 'smex-cache) (smex-update)))
    (add-hook 'after-load-functions 'smex-update-after-load)))

;-----------------------------------------------------------------------------
; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; (setq ediff-split-window-function 'split-window-vertically)

;-----------------------------------------------------------------------------
;; (use-package browse-kill-ring)
;; (global-set-key (kbd "C-c p")  'helm-show-kill-ring)

;-----------------------------------------------------------------------------
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C-+" . er/contract-region)))

;-----------------------------------------------------------------------------
(use-package ace-jump-mode
  :init
  (progn
    (setq ace-jump-mode-gray-background t)
    (define-key evil-normal-state-map (kbd "SPC") 'evil-ace-jump-word-mode)))

;-----------------------------------------------------------------------------
(use-package auto-highlight-symbol
  :commands auto-highlight-symbol-mode
  :config
  (progn
    (custom-set-variables
     '(ahs-case-fold-search nil)
     '(ahs-default-range 'ahs-range-whole-buffer)
     '(ahs-idle-interval 0.25)
     '(ahs-inhibit-face-list nil))
    (evil-leader/set-key
      "se" 'ahs-edit-mode
      "sb" 'spacemacs/goto-last-searched-ahs-symbol
      "sn" (lambda () (interactive) (eval '(progn (ahs-highlight-now) (ahs-forward)) nil))
      "sN" (lambda () (interactive) (eval '(progn (ahs-highlight-now) (ahs-backward)) nil))
      "srb" (lambda () (interactive) (eval '(ahs-change-range 'ahs-range-whole-buffer) nil))
      "srd" (lambda () (interactive) (eval '(ahs-change-range 'ahs-range-display) nil))
      "srf" (lambda () (interactive) (eval '(ahs-change-range 'ahs-range-beginning-of-defun) nil))
      "sR" (lambda () (interactive) (eval '(ahs-change-range ahs-default-range) nil))
      "th" 'auto-highlight-symbol-mode)))

;-----------------------------------------------------------------------------
(use-package mark-multiple
  :init
  (progn
    (define-key evil-visual-state-map "]" 'mark-next-like-this)
    (define-key evil-visual-state-map "[" 'mark-previous-like-this)
    (define-key evil-visual-state-map "m" 'mark-more-like-this)))

;-----------------------------------------------------------------------------
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (progn
    (setq undo-tree-auto-save t)
    (setq undo-tree-history-directory-alist
          '((".*" . "~/.emacs.d/cache/undo-tree")))
    (global-undo-tree-mode)))

;-----------------------------------------------------------------------------
(use-package smartparens
  :config
  (progn
    (smartparens-global-mode t)

    (setq sp-highlight-pair-overlay nil
          sp-navigate-close-if-unbalanced t)

    (sp-pair "(" ")"   :wrap "M-(")
    (sp-pair "{" "}"   :wrap "M-{")
    (sp-pair "[" "]"   :wrap "M-[")
    (sp-pair "\"" "\"" :wrap "M-\"")
    (sp-pair "${" "}"  :wrap "M-$")

    (sp-with-modes '(emacs-lisp-mode
                     inferior-emacs-lisp-mode
                     lisp-interaction-mode
                     lisp-mode)
      (sp-local-pair "'" nil :actions nil)
      (sp-local-pair "`" "'" :when '(sp-in-string-p) :actions '(insert wrap)))

    (sp-with-modes '(rst-mode)
      (sp-local-pair "'" nil :actions nil))

    (sp-with-modes '(text-mode)
      (sp-local-pair "'" nil :actions nil)
      (sp-local-pair "`" "'" :actions '(insert wrap)))))

;-----------------------------------------------------------------------------
(eval-after-load 'artist-mode
  '(progn
     (define-key artist-mode-map [down-mouse-3] 'artist-mouse-choose-operation)))

;-----------------------------------------------------------------------------
; multiple-cursors (only in emacs mode)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(add-hook 'multiple-cursors-mode-enabled-hook  (lambda () (smartparens-mode -1)))
(add-hook 'multiple-cursors-mode-disabled-hook (lambda () (smartparens-mode t)))

(defvar my-mc-evil-previous-state nil)

; switch to emacs mode when using multiple-cursors
(defun my-mc-evil-switch-to-emacs-state ()
  (when (and (bound-and-true-p evil-mode)
             (not (eq evil-state 'emacs)))
    (setq my-mc-evil-previous-state evil-state)
    (evil-emacs-state)))

(defun my-mc-evil-back-to-previous-state ()
  (when my-mc-evil-previous-state
    (unwind-protect
        (case my-mc-evil-previous-state
          ((normal visual insert) (evil-force-normal-state))
          (t (message "Don't know how to handle previous state: %S"
                      my-mc-evil-previous-state)))
      (setq my-mc-evil-previous-state nil))))

(add-hook 'multiple-cursors-mode-enabled-hook  'my-mc-evil-switch-to-emacs-state)
(add-hook 'multiple-cursors-mode-disabled-hook 'my-mc-evil-back-to-previous-state)

;-----------------------------------------------------------------------------
; calendar config
(setq calendar-week-start-day 1)
(setq calendar-date-style 'european)
(setq calendar-mark-holidays-flag t)

(setq calendar-time-display-form
      '(24-hours ":" minutes (and time-zone (concat " (" time-zone ")"))))

(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'font-lock-warning-face))

(setq calendar-intermonth-header
      (propertize "Wk" 'font-lock-face 'font-lock-keyword-face))

;-----------------------------------------------------------------------------
(use-package projectile
  :diminish "Pj"
  :init
  (progn
    (projectile-global-mode)
    (setq projectile-indexing-method 'alien
          projectile-sort-order 'recentf
          projectile-cache-file "~/.emacs.d/cache/projectile.cache"
          projectile-known-projects-file "~/.emacs.d/cache/projectile-bookmarks.eld"
          projectile-enable-caching t)
    (add-to-list 'projectile-globally-ignored-directories "elpa")
    (add-to-list 'projectile-globally-ignored-directories ".cask")
    (add-to-list 'projectile-globally-ignored-directories ".cache")))

;-----------------------------------------------------------------------------
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init
  (progn
    (define-key evil-normal-state-map ",b" 'ibuffer)
    (setq ibuffer-show-empty-filter-groups nil)
    (setq ibuffer-expert nil)
    (setq ibuffer-default-sorting-mode '(recency))
    (setq ibuffer-use-other-window nil))
  :config
  (progn
    (use-package ibuffer-vc)
    (define-ibuffer-column size-human
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
       ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
       (t (format "%8d" (buffer-size)))))

    (setq ibuffer-formats
          '((mark modified read-only vc-status-mini " "
                  (name 18 18 :left :elide) " "
                  (size 9 -1 :right) " "
                  (mode 16 16 :left :elide) " "
                  (vc-status 16 16 :left) " "
                  filename-and-process)))

    (evil-make-overriding-map ibuffer-mode-map 'normal t)
    (evil-define-key 'normal ibuffer-mode-map
      "j"   'evil-next-line
      "q"   'kill-buffer-and-window
      "k"   'evil-previous-line
      "RET" 'ibuffer-visit-buffer
      "J"   'ibuffer-jump-to-buffer
      "G"   'evil-goto-line
      "/"   'evil-search-forward
      "gg"  'evil-goto-first-line)

    (setq ibuffer-saved-filter-groups
          `(("default"
             ("Config" (or (filename . ".emacs.d")
                           (filename . ,(expand-file-name "~/.config"))
                           (filename . ,(expand-file-name "~/.dotfiles")))))
            ("Org"    (or (mode . org-mode)
                          (mode . org-mode-agenda)
                          (filename . "OrgMode")))
            ("Magit"  (or (name . "\\*magit-.*\\*")
                          (mode . magit-mode)))
            ("Dired"  (mode . dired-mode))
            ("ERC"    (mode . erc-mode))
            ("Emacs"  (or (name . "^\\*scratch\\*$")
                          (name . "^\\*Compile-Log\\*$")
                          (name . "^\\*Messages\\*$")))
            ("Help"   (or (name . "\*Help\*")
                          (name . "\*Apropos\*")
                          (name . "\*info\*")))))

    ;; Switching to ibuffer puts the cursor on the most recent buffer
    (defadvice ibuffer (around ibuffer-point-to-most-recent) ()
      "Open ibuffer with cursor pointed to most recent buffer name"
      (let ((recent-buffer-name (buffer-name)))
        ad-do-it
        (ibuffer-jump-to-buffer recent-buffer-name)))
    (ad-activate 'ibuffer)

    (require 'ibuffer-projectile)
    (add-hook! 'ibuffer-mode-hook
               (progn
                 (ibuffer-tramp-set-filter-groups-by-tramp-connection)
                 (ibuffer-tramp-set-filter-groups-by-tramp-connection)
                 (ibuffer-vc-set-filter-groups-by-vc-root)
                 (ibuffer-do-sort-by-alphabetic)
                 ;; (ibuffer-switch-to-saved-filter-groups "default")
                 (ibuffer-auto-mode 1)))))

;-----------------------------------------------------------------------------
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)))

;-----------------------------------------------------------------------------
; dired
(setq-default dired-listing-switches (if is-bsd "-alh" "-alhv"))
;; (setq dired-recursive-deletes 'always)
;; (setq dired-recursive-copies 'always)
(setq dired-dwim-target t)
(setq dired-auto-revert-buffer t)
;;(toggle-diredp-find-file-reuse-dir 1)
(setq diredp-hide-details-initially-flag nil)
(setq diredp-hide-details-propagate-flag nil)
(put 'dired-find-alternate-file 'disabled nil)

;-----------------------------------------------------------------------------
(use-package rainbow-mode
  :defer t
  :init
  (progn
    (add-hook 'scss-mode 'rainbow-mode)
    (add-hook 'css-mode-hook 'rainbow-mode)
    (add-hook 'html-mode-hook 'rainbow-mode)
    (add-hook 'sass-mode-hook 'rainbow-mode)
    (add-hook 'scss-mode-hook 'rainbow-mode)))

(require 'langtool)
(setq langtool-language-tool-jar "/usr/share/java/languagetool/languagetool-commandline.jar")

;-----------------------------------------------------------------------------
(use-package gitconfig-mode
  :mode (("/\\.?git/?config\\'" . gitconfig-mode)
         ("/\\.gitmodules\\'" . gitconfig-mode))
  :config
  (add-hook 'gitconfig-mode-hook 'flyspell-mode))

(use-package gitignore-mode
  :mode (("/\\.gitignore\\'" . gitignore-mode)
         ("/\\.git/info/exclude\\'" . gitignore-mode)
         ("/git/ignore\\'" . gitignore-mode)))

;; (setq magit-save-some-buffers nil)
(use-package magit
  :commands (magit-log
             magit-commit
             magit-status
             magit-blame-mode)
  :bind ("<f11>" . magit-status)
  :config
  (progn
    (setq magit-commit-signoff nil
          magit-auto-revert-mode nil
          magit-process-popup-time 5
          magit-save-some-buffers nil
          magit-set-upstream-on-push t
          magit-diff-refine-hunk t
          magit-auto-revert-mode-lighter nil
          ;; magit-completing-read-function 'magit-ido-completing-read
          magit-completing-read-function 'magit-builtin-completing-read)

    (add-hook 'magit-log-edit-mode-hook 'flyspell-mode)
    ;; (add-hook 'magit-log-edit-mode-hook 'auto-capitalize-mode)
    ;; (add-hook 'magit-log-edit-mode-hook 'artbollocks-mode)

    (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
    ;; (add-hook 'git-commit-mode-hook 'auto-capitalize-mode)
    ;; (add-hook 'git-commit-mode-hook 'artbollocks-mode)
    (add-hook! 'git-commit-mode-hook (toggle-save-place 0))

    (evil-add-hjkl-bindings magit-status-mode-map 'emacs
      "K" 'magit-discard-item
      "l" 'magit-key-mode-popup-logging
      "j" 'magit-goto-next-section
      "k" 'magit-goto-previous-section
      "u" 'magit-unstage-item
      "s" 'magit-stage-item
      "h" 'magit-toggle-diff-refine-hunk)))

;-----------------------------------------------------------------------------
(use-package zeal-at-point
  :commands (zeal-at-point zeal-at-point-with-docset)
  :config
  (progn
    (add-to-list 'zeal-at-point-mode-alist '(sh-mode . "bash"))
    (add-to-list 'zeal-at-point-mode-alist '(python-mode . "py3"))))

;;;---------------------------------------------------------------------------
;;; Programming modes
;;;---------------------------------------------------------------------------

;-----------------------------------------------------------------------------
(use-package go-mode
  :mode "\\.go\\'"
  :interpreter "go")
  ;; :init
  ;; (require 'go-autocomplete))

(use-package cider
  :config
  (progn
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
    (setq cider-popup-stacktraces nil)
    (setq cider-repl-popup-stacktraces nil)
    (setq cider-repl-pop-to-buffer-on-connect t)
    (setq cider-repl-use-clojure-font-lock t)
    (setq nrepl-hide-special-buffers t)
    (setq cider-repl-pop-to-buffer-on-connect nil)
    (setq cider-stacktrace-fill-column 80)
    (setq nrepl-buffer-name-show-port t)
    (setq cider-repl-result-prefix ";; => ")
    (setq cider-repl-history-size 1000)
    (setq cider-repl-history-file "~/.emacs.d/cache/cider_repl_history")))

;-----------------------------------------------------------------------------
(use-package yaml-mode :defer t
  :config
  (add-hook 'yaml-mode-hook 'enable-tab-width-2))

;-----------------------------------------------------------------------------
(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\.jshintrc\\'" . json-mode)))

;-----------------------------------------------------------------------------
(use-package emmet-mode
  :defer t
  :config
  (setq emmet-move-cursor-between-quotes t)
  :init
  (progn
    (add-hook 'scss-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'html-mode-hook 'emmet-mode)
    (add-hook 'haml-mode-hook 'emmet-mode)
    (add-hook 'nxml-mode-hook 'emmet-mode)))

;-----------------------------------------------------------------------------
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)))

;-----------------------------------------------------------------------------
(use-package scss-mode
  :mode "\\.scss\\'"
  :config
  (progn
    (setq scss-compile-at-save nil)
    (setq scss-compile-at-save nil)
    (setq css-indent-offset 2)
    (add-hook 'scss-mode-hook 'ac-css-mode-setup)))

(use-package cc-mode
  :config
  (progn
    (setq-default c-basic-offset 4)
    (setq-default c-default-style "linux")
    (setq-default c-hungry-delete-key t)
    (setq-default c-tab-always-indent nil)
    (setq-default c-auto-newline nil)
    (setq comment-multi-line t)
    (add-hook! 'c-mode-hook (setq comment-start "//" comment-end ""))))

;-----------------------------------------------------------------------------
(use-package web-mode
  :mode (("\\.\\(p\\)?htm\\(l\\)?\\'" . web-mode)
         ("\\.tpl\\(\\.php\\)?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("wp-content/themes/.+/.+\\.php\\'" . web-mode))
  :config
  (progn
    (setq web-mode-ac-sources-alist
          '(("css" . (ac-source-css-property))))
    (setq web-mode-markup-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-style-padding 2
          web-mode-script-padding 2
          web-mode-block-padding 2)
    (add-hook 'web-mode-hook 'enable-tab-width-2)))

;-----------------------------------------------------------------------------
(use-package virtualenvwrapper
  :config
  (setq venv-location "~/.virtualenvs/"))

;-----------------------------------------------------------------------------
(use-package python-environment
  :init
  (progn
    (setq python-environment-directory "~/.emacs.d/cache/python-environments")))

;-----------------------------------------------------------------------------
; pip install rope jedi flake8 importmagic
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  (setq python-indent-offset 4)
  :config
  (progn
    ;; (use-package jedi)
    ;; (unless (file-directory-p "~/.emacs.d/cache/python-environments/default/")
    ;;   (jedi:install-server))

    ;-------------------------------------------------------------------------
    (use-package anaconda-mode
     :init
     (progn
       (add-hook 'python-mode-hook 'anaconda-mode)
       (add-hook 'python-mode-hook 'eldoc-mode)))

    (setq python-shell-interpreter "python")
    (setq python-fill-docstring-style 'django)

    ;; (setq-default python-skeleton-autoinsert nil)
    ;; (setq py-shell-name "ipython")
    ;; (setq py-which-bufname "IPython")
    ;; (setq py-python-command-args '("--colors=linux"))
    ;; (setq ropemacs-enable-autoimport t)
    ;; (modify-syntax-entry ?_ "w" python-mode-syntax-table)

    ;; (add-hook 'python-mode-hook 'set-newline-and-indent)
    ;; (evil-defmap python-mode-map (kbd "DEL") nil)

    (add-hook! 'python-mode-hook (progn
               (add-to-list 'company-backends 'company-anaconda)
               (setq my-switch-to-repl-func 'python-shell-switch-to-shell
                     my-send-region-to-repl-func 'python-shell-send-region
                     my-run-code-interpreter "python")))))

(setq ropemacs-codeassist-maxfixes 5
      ropemacs-global-prefix "C-c C-p"
      ropemacs-guess-project t
      ropemacs-enable-autoimport t
      ropemacs-completing-read-function 'completing-read)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")

;-----------------------------------------------------------------------------
(use-package ruby
  :mode (("\\.rake\\'" . ruby-mode)
         ("Rakefile\\'" . ruby-mode)
         ("Vagrantfile\'" . ruby-mode)
         ("\\.erb\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("Guardfile\\'" . ruby-mode)))

;-----------------------------------------------------------------------------
(use-package sh-script
  :config
  (progn
    (modify-syntax-entry ?_ "w" sh-mode-syntax-table)
    (setq-default sh-basic-offset 2
                  sh-indentation 2
                  sh-intent-for-case-label 0
                  sh-intent-for-case-alt '+)))

;-----------------------------------------------------------------------------
(use-package perl-mode
  :mode ("\\.pl\\'" . perl-mode)
  :init (defalias 'perl-mode 'cperl-mode)
  :config
  (progn
    ;; (use-package perl-find-library)
    (setq cperl-invalid-face nil
          cperl-close-paren-offset -4
          cperl-continued-statement-offset 0
          cperl-indent-level 4
          cperl-indent-parens-as-block t)))

;-----------------------------------------------------------------------------
(use-package gyp
  :mode (("\\.gyp\\'" . gyp-mode)
         ("\\.gypi\\'" . gyp-mode)))

;;;---------------------------------------------------------------------------
;;; File associations
;;;---------------------------------------------------------------------------
(associate-mode "\\.sls\\'"  yaml-mode)
(associate-mode "\\.zsh\\'"  sh-mode)
(associate-mode "\\(?:\\.gitconfig\\|\\.gitmodules\\|config\\)\'" conf-mode)
(associate-mode "\\.html\\'" web-mode)
(associate-mode "\\.ledger\\'" ledger-mode)
(associate-mode "\\.spec\\.tpl$" rpm-spec-mode)
(associate-mode "\\.t$" perl-mode)
(associate-mode "nginx.conf$" nginx-mode)
(associate-mode "Makefile\\.PL$" perl-mode)
(associate-mode "Makefile\\(\\..*\\)?" makefile-mode)
(associate-mode "CMakeLists\\.txt\\'" cmake-mode)
(associate-mode "SConscript\\|SConstruct" python-mode)
(associate-mode "\\.cmake\\'" cmake-mode)
(associate-mode "\\.vcf$" sensitive-minor-mode)
(associate-mode "\\.gpg$" sensitive-minor-mode)

;;;---------------------------------------------------------------------------
;;; Program associations
;;;---------------------------------------------------------------------------
(use-package openwith
  :config
  (progn
    (add-to-list 'mm-inhibit-file-name-handlers 'openwith-file-handler)
    (setq openwith-associations
          (list
           (list (openwith-make-extension-regexp '("pdf")) "zathura" '(file))))
    (openwith-mode t)))

;;;---------------------------------------------------------------------------
;;; Org mode
;;;---------------------------------------------------------------------------
(use-package org
  :mode ("\\.org$" . org-mode)
  :defer t
  :init
  (progn
    (setq org-return-follows-link t)
    (setq org-log-done 'time)
    (setq org-link-abbrev-alist
          '(("attach" . org-attach-expand-link)
            ("gmap" . "http://maps.google.com/maps?q=%s")
            ("omap" . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")))

    (evil-leader/set-key-for-mode 'org-mode
      "oc" 'org-capture
      "od" 'org-deadline
      "oe" 'org-export-dispatch
      "oi" 'org-clock-in
      "oo" 'org-clock-out
      "om" 'org-ctrl-c-ctrl-c
      "or" 'org-refile
      "os" 'org-schedule
      "oa" 'org-agenda
      "oA" 'org-archive-subtree
      "oC" 'evil-org-recompute-clocks
      "ol" 'evil-org-open-links
      "ot" 'org-show-todo-tre))
  :config
  (progn
    (evil-define-key 'normal evil-org-mode-map (kbd "RET") 'org-return)
    (eval-after-load "org-agenda"
      '(progn
         (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
         (define-key org-agenda-mode-map "k" 'org-agenda-previous-line)))
    (use-package evil-org)
    (use-package ox-reveal
      :config
      (setq org-reveal-root "file:///home/gv/downloads/reveal.js-3.1.0/"))
    (add-hook 'org-mode-hook 'evil-org-mode)))

(setq org-startup-indented t)
(setq org-startup-folded 0)
(setq org-cycle-separator-lines 1)

(setq org-src-fontify-natively t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-completion-use-ido t)
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-edit-timestamp-down-means-later t)

;-----------------------------------------------------------------------------
; default locations
(setq org-default-refile-file "~/org/refile.org")
(setq org-id-locations-file "~/.emacs.d/cache/org-id-locations")
(setq org-directory "~/org")
(setq org-default-notes-file "~/org/notes.org")
(setq org-mobile-directory "~/org/mobile-org")
(setq org-mobile-inbox-for-pull "~/org/from-mobile.org")
(setq org-agenda-files '("~/org/notes.org"
                         "~/org/projects.org"
                         "~/org/refile.org"))

;-----------------------------------------------------------------------------
; agenda configs
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-ndays 14)
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-show-all-dates t)
(setq org-agenda-include-diary nil)
;; (setq org-agenda-skip-deadline-if-done t)
;; (setq org-agenda-skip-scheduled-if-done t)
;; (setq org-remember-store-without-prompt t)

;-----------------------------------------------------------------------------
; capture
(setq org-tag-alist
      '(("@work" . ?b)
        ("@home" . ?h)
        ("@errands" . ?e)
        ("@reading" . ?r)
        ("projects" . ?q)))

;-----------------------------------------------------------------------------
; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (plantuml . t)
   (dot .t )
   (ledger . t)
   (python . t)))

;-----------------------------------------------------------------------------
(setq org-export-htmlize-output-type 'css)

;;;---------------------------------------------------------------------------
;;; Mail
;;;---------------------------------------------------------------------------

;;;---------------------------------------------------------------------------
;;; Keymaps
;;;---------------------------------------------------------------------------
(global-set-key [down-mouse-3] 'x-menu-bar-open)
(global-set-key (kbd "<menu>") 'x-menu-bar-open)

(define-key evil-normal-state-map "gp" 'my/select-previous-evil-paste)
(define-key evil-normal-state-map (kbd ";") 'smex)
(define-key evil-visual-state-map (kbd ";") 'smex)
(global-set-key (kbd "M-x") 'smex)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-:") 'eval-expression)
(global-set-key (kbd "C-j") "5j")
(global-set-key (kbd "C-k") "5k")
(global-set-key (kbd "RET") 'newline-and-indent)
;; (global-set-key (kbd "C-w o") 'switch-window)

; evil-nerd-comment
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-c l") 'evilnc-comment-or-uncomment-to-the-line)
(global-set-key (kbd "C-c c") 'evilnc-copy-and-comment-lines)
(global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)

(global-set-key (kbd "C-h ,") 'helm-apropos)
(global-set-key (kbd "C-h .") 'helm-info-emacs)
(global-set-key (kbd "C-c w") 'whitespace-cleanup)

; my/commands
(global-set-key (kbd "C-c n") 'my/show-and-copy-file-name)
(global-set-key (kbd "C-c u") 'untabify)
(global-set-key (kbd "C-c t") 'tabify)

(evil-leader/set-key
 "w" 'save-buffer
 "k" 'kill-this-buffer
 "d" 'kill-buffer-and-window
 "q" 'kill-buffer-and-window
 "h" 'ff-find-other-file
 "f" 'helm-find-files
 "l" 'helm-mini
 "m" 'helm-recentf
 "u" 'undo-tree-visualize
 "i" 'helm-semantic-or-imenu
 "r" 'dired-jump
 "y" 'helm-show-kill-ring
 "p" 'helm-projectile
 "vr" 'vr/replace
 "vq" 'vr/query-replace
 "sc" (lambda () (interactive)(switch-to-buffer "*scratch*"))
 "sw" 'helm-swoop
 "er" 'evil-show-registers
 ;; "l" 'ido-switch-buffer
 ;; "f" 'ido-find-file

 ; magit
 "gs" 'magit-status
 "gl" 'magit-log
 "gL" 'magit-log-buffer-file
 "gb" 'magit-blame-mode

 ; evil-nerd-comment
 "ci" 'evilnc-comment-or-uncomment-lines
 "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
 "cc" 'evilnc-copy-and-comment-lines
 "cp" 'evilnc-comment-or-uncomment-paragraphs
 "cr" 'comment-or-uncomment-region
 "cv" 'evilnc-toggle-invert-comment-line-by-line
 "\\" 'evilnc-comment-operator)

(global-set-key (kbd "<C-mouse-5>") 'zoom-frm-out)
(global-set-key (kbd "<C-mouse-4>") 'zoom-frm-in)

;-----------------------------------------------------------------------------
; toggle between relative, absolute and no line numbering
(global-set-key (kbd "C-<f5>") (my/toggle-linum-keycb "%3d"))
(global-set-key (kbd "C-S-<f5>") (my/toggle-linum-keycb 'linum-relative))

;;;---------------------------------------------------------------------------
(when (file-readable-p "~/.emacs.d/local.el")
    (load "~/.emacs.d/local.el"))

(if (file-readable-p "~/.emacs.d/custom.el")
    (progn
      (setq custom-file "~/.emacs.d/custom.el")
      (load custom-file)))
