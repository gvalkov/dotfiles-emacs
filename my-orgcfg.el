;;; org-mode
(add-hook 'org-mode-hook 'turn-on-font-lock)
;; (add-hook 'org-mode-hook 'turn-on-org-cdlatex)

(setq org-log-done 'time)
(setq org-log-into-drawer t)
;; (setq org-log-state-notes-insert-after-drawers nil)

(setq org-completion-use-ido t)
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-edit-timestamp-down-means-later t)

(setq org-id-locations-file "~/.emacs.d/cache/org-id-locations")
(setq org-default-notes-file "~/org/notes.org")
(setq org-pygment-path (if (bsd-p) "/usr/local/bin/pygmentize" "/usr/bin/pygmentize"))

(setq org-directory "~/org")
(setq org-mobile-directory "~/org/mobile-org")
(setq org-mobile-inbox-for-pull "~/org/from-mobile.org")

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(define-key evil-normal-state-map (kbd ",r") 'org-capture)
(define-key global-map (kbd "<f6>" ) 'org-capture)

(setq org-agenda-start-on-weekday nil)
(setq org-agenda-ndays 14)
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-show-all-dates t)
(setq org-agenda-files '("~/org/notes.org"
                         "~/org/projects.org"
                         "~/org/refile.org"))
(setq org-agenda-include-diary nil)
;; (setq org-agenda-skip-deadline-if-done t)
;; (setq org-agenda-skip-scheduled-if-done t)
;; (setq org-remember-store-without-prompt t)

(setq org-deadline-warning-days 30)

(setq org-fast-tag-selection-single-key 'expert)
(setq org-tags-column 80)

(setq org-src-fontify-natively t)
(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (:foreground "PaleGreen"
                 :weight normal
                 :strike-through t))))
 '(org-headline-done
            ((((class color) (min-colors 16) (background dark))
               (:foreground "LightSalmon" :strike-through t)))))


(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   ))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "pygment")))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(setq org-link-abbrev-alist '(("attach" . org-attach-expand-link)))

(setq org-capture-templates
      '(
        ("t" "todo" entry (file+headline org-default-refile-file "INBOX")
         "*** TODO %?\n %i\n")

        ("n" "note" entry (file+datetree org-default-refile-file)
         "* %^{Description} %^g %? Added: %U")

        ("j" "journal" entry (file+datetree "~/org/journal.org")
         "** %^{Heading}")
        ))

(setq org-default-refile-file "~/org/refile.org")
(setq org-refile-targets (quote (("projects.org" :maxlevel . 1)
                                 ("notes.org" :level . 2))))


(setq org-export-htmlize-output-type 'css)

(eval-after-load "org"
  '(add-to-list 'org-modules 'org-habit))

(defun my-org-mode-hook ()
  (local-set-key (kbd "A-b") 'org-text-bold)
  (local-set-key (kbd "A-i") 'org-text-italics)
  (local-set-key (kbd "A-=") 'org-text-code))

(add-hook 'org-mode-hook 'turn-on-flyspell 'append)
(add-hook 'org-mode-hook 'my-org-mode-hook)

(setq org-startup-indented t)
(setq org-startup-folded 0)
(setq org-cycle-separator-lines 1)
;; (setq org-reverse-note-order nil)
;; (setq org-id-method 'uuid)

(defun evil-org-insert-heading ()
  (interactive)
  ;; (org-insert-heading-respect-content)
  (org-insert-heading)
  (evil-insert-state))


(evil-define-key 'normal org-mode-map
  (kbd "RET") 'org-open-at-point
  (kbd "<tab>") 'org-cycle
  (kbd "t") 'org-todo
  (kbd "M-L") 'org-metaright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "M-j") 'org-shiftleft
  (kbd "M-k") 'org-shiftright
  (kbd "C-o") 'evil-org-insert-heading
  (kbd "C-j") 'outline-next-visible-heading
  (kbd "C-k") 'outline-previous-visible-heading
  (kbd "-")   'org-cycle-list-bullet
  "za" 'org-cycle
  "zA" 'org-shifttab
  "zm" 'hide-body
  "zr" 'show-all
  "zo" 'show-subtree
  "zO" 'show-all
  "zc" 'hide-subtree
  "zC" 'hide-all)

(evil-define-key 'normal orgstruct-mode-map
  (kbd "RET") 'org-open-at-point
  (kbd "M-L") 'org-metaright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "M-j") 'org-shiftleft
  (kbd "M-k") 'org-shiftright
  (kbd "C-o") 'evil-org-insert-heading
  (kbd "C-j") 'outline-next-visible-heading
  (kbd "C-k") 'outline-previous-visible-heading
  (kbd "-")   'org-cycle-list-bullet
  "za" 'org-cycle
  "zA" 'org-shifttab
  "zm" 'hide-body
  "zr" 'show-all
  "zo" 'show-subtree
  "zO" 'show-all
  "zc" 'hide-subtree
  "zC" 'hide-all)

(evil-define-key 'insert org-mode-map
  (kbd "C-o") 'evil-org-insert-heading
  (kbd "M-j") 'org-shiftleft
  (kbd "M-k") 'org-shiftright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "M-L") 'org-metaright)

(evil-define-key 'insert orgstruct-mode-map
  (kbd "C-o") 'evil-org-insert-heading
  (kbd "M-j") 'org-shiftleft
  (kbd "M-k") 'org-shiftright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "M-L") 'org-metaright)

(evil-define-key 'visual org-mode-map
  (kbd "C-j") 'org-forward-same-level
  (kbd "C-k") 'org-backward-same-level)

(defun org-text-wrapper (txt &optional endtxt)
  "Wraps the region with the text passed in as an argument."
  (if (use-region-p)
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (goto-char (point-min))
        (insert txt)
        (goto-char (point-max))
        (if endtxt
            (insert endtxt)
          (insert txt)))
    (if (looking-at "[A-z]")
        (save-excursion
          (if (not (looking-back "[     ]"))
              (backward-word))
          (progn
            (mark-word)
            (org-text-wrapper txt endtxt)))
      (progn
        (insert txt)
        (let ((spot (point)))
          (insert txt)
          (goto-char spot))))))

(defun org-text-bold () "Wraps the region with asterisks."
  (interactive)
  (org-text-wrapper "*"))

(defun org-text-italics () "Wraps the region with slashes."
  (interactive)
  (org-text-wrapper "/"))

(defun org-text-code () "Wraps the region with equal signs."
  (interactive)
  (org-text-wrapper "="))

(defun my-open-org-calendar ()
  "Open an org schedule calendar in the new buffer."
  (interactive)
  (let* ((source1 (cfw:org-create-source))
         (my-buffer (get-buffer-create cfw:calendar-buffer-name))
         ;; (frame-background-mode "light")
         (cp (cfw:create-calendar-component-buffer
              :view 'month
              :buffer my-buffer
              :contents-sources (list source1)
              :custom-map cfw:org-schedule-map
              :sorter 'cfw:org-schedule-sorter)))
    (load-theme-buffer-local 'sanityinc-solarized-light my-buffer)
    (switch-to-buffer (cfw:cp-get-buffer cp))))

(provide 'my-orgcfg)
