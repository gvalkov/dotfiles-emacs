;;; org-mode
(add-hook 'org-mode-hook 'my/org-mode-hook)
(defun my/org-mode-hook ()
  (local-set-key (kbd "A-b") 'org-text-bold)
  (local-set-key (kbd "A-i") 'org-text-italics)
  (local-set-key (kbd "A-=") 'org-text-code)
  (set-newline-and-indent)
  (turn-on-flyspell))

;-----------------------------------------------------------------------------
(define-key evil-normal-state-map (kbd ",r") 'org-capture)
(define-key global-map (kbd "<f6>" ) 'org-capture)


(setq org-deadline-warning-days 30)

(setq org-fast-tag-selection-single-key 'expert)
(setq org-tags-column 80)
(setq org-link-abbrev-alist
      '(("attach" . org-attach-expand-link)
        ("gmap" . "http://maps.google.com/maps?q=%s")
        ("omap" . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")))

;-----------------------------------------------------------------------------

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
  (kbd "M-j") 'org-shiftleft
  (kbd "M-k") 'org-shiftright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "M-L") 'org-metaright)

(evil-define-key 'insert orgstruct-mode-map
  (kbd "M-j") 'org-shiftleft
  (kbd "M-k") 'org-shiftright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "M-L") 'org-metaright)

(evil-define-key 'normal orgstruct-mode-map
  (kbd "RET") 'org-open-at-point
  (kbd "M-L") 'org-metaright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "M-j") 'org-shiftleft
  (kbd "M-k") 'org-shiftright
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
