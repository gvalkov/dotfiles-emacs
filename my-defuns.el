(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; tags
(defun build-ctags ()
  (interactive)
  (message "building ctags")
  (let ((root (eproject-root)))
        (shell-command (concat "ctags -e -R --extra=+fq --exclude=db --exclude=test --exclude=.git --exclude=public -f " root "TAGS " root)))
    (visit-project-tags)
    (message "tags build successfully"))

(defun visit-project-tags ()
  (interactive)
  (let ((tags-file (concat (eproject-root) "TAGS")))
    (visit-tags-table tags-file)
    (message (concat "Loaded " tags-file))))

(defun reload-emacs-config ()
  (interactive)
  (let ((init-el  "~/.emacs.d/init.el"))
    (and (get-file-buffer init-el)
         (save-buffer (get-file-buffer init-el)))
    (load-file init-el)))

(defun map-add-to-list (dest &rest args)
  (mapc '(lambda (arg) (add-to-list dest arg))
        args))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p
         (format "Really delete '%s'?" (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "New name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defun my-org-calendar-buffer ()
  "Open an org schedule calendar in the new buffer."
  (interactive)
  (let* ((source1 (cfw:org-create-source))
         (my-buffer (get-buffer-create cfw:calendar-buffer-name))
         ;; (frame-background-mode "light")
         (cp (cfw:create-calendar-component-buffer
              :view 'day
              :buffer my-buffer
              :contents-sources (list source1)
              :custom-map cfw:org-schedule-map
              :sorter 'cfw:org-schedule-sorter)))
    (load-theme-buffer-local 'sanityinc-solarized-light my-buffer)
    (cfw:cp-get-buffer cp)))

(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command. If no region is selected
  and current line is not blank and we are not at the end of the
  line,then comment current line.  Replaces default behaviour of
  comment-dwim, when it inserts comment at the end of the line. If we
  are commenting a full line, we also yank its content to the 'c'
  register."

  (interactive "*P")
  (comment-normalize-vars)

  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (unless (comment-only-p beg end) (copy-to-register ?1 beg end))

    (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
        (comment-or-uncomment-region beg end)
      (comment-dwim arg))))

(defun set-newline-and-indent ()
  "Map the return key with `newline-and-indent'"
  (local-set-key (kbd "RET") 'newline-and-indent))

; https://gist.github.com/nibrahim/640311
(defun sort-lines-by-length (b e)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region b e)
      (let ((items (sort
                    (split-string
                     (buffer-substring (point-min) (point-max)) "[\n]")
                    (lambda(x y) (< (length x) (length y))))))
        (delete-region (point-min) (point-max))
        (save-excursion
          (point-min)
          (insert (apply 'concat (map 'list (lambda (x) (format "%s\n" x)) items))))))))

(defun align= (b e)
  (interactive "r")
  (align-regexp b e "\\(\\s-*\\)[=|:]" 1 1))

(defun align: (b e)
  (interactive "r")
  (align-regexp b e ":\\(\\s-*\\)" 1 1 nil))

(defun align-lstrip-untabify: (b e)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region b e)
      (untabify (point-min) (point-max))
      (goto-char (point-min))
      (while (re-search-forward " +:" nil t)
        (replace-match ": "))
      (align-regexp (point-min) (point-max) "\\(\\s-+\\)")
      (goto-char (point-min))
      )))

(defun untabify-trailing (b e)
  (interactive "r")
  (save-excursion
    (goto-char b)
    (while (< (point) e)
      (beginning-of-line)
      (skip-chars-forward " 	")
      (untabify (point) (line-end-position))
      (forward-line 1))))

(defun reverse-words (b e)
  "Reverse the order of words in region."
  (interactive "*r")
  (apply
   'insert
   (reverse
    (split-string
     (delete-and-extract-region b e) "\\b"))))

(defun my/select-previous-evil-paste ()
  (interactive)
  (evil-goto-mark ?\[)
  (evil-visual-char)
  (evil-goto-mark ?\]))

(defun my/newline-and-indent ()
  "Newline and indent; if in a comment, auto-comment and properly
indent the next line."
  (interactive)
  (cond ((sp-point-in-string)
         (evil-ret))
        ((evil-in-comment-p)
         (indent-new-comment-line))
        (t
         (evil-ret-and-indent))))

(defun my/minibuffer-quit ()
  "Abort recursive edit. In Delete Selection mode, if the mark is
active, just deactivate it; then it takes a second \\[keyboard-quit]
to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun toggle-linum-keycb (fmt)
  (lexical-let ((format fmt))
    (lambda ()
      (interactive)
      (if linum-mode
          (if (eq linum-format format)
              (command-execute 'linum-mode)
            (setq linum-format format))
        (progn
          (setq linum-format format)
          (command-execute 'linum-mode))))))

(defun my/newline-and-indent ()
  "Newline and indent; if in a comment, auto-comment and properly
indent the next line."
  (interactive)
  (cond ((sp-point-in-string)
         (evil-ret))
        ((evil-in-comment-p)
         (indent-new-comment-line))
        (t
         (evil-ret-and-indent))))

;-----------------------------------------------------------------------------
; The following have been 'borrowed' from the excellent steckemacs config
(defun my/show-and-copy-file-name ()
  "Show and copy the full path name."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))

(defun my/url-insert-file-contents (url)
  "Prompt for URL and insert file contents at point."
  (interactive "sURL: ")
  (url-insert-file-contents url))

(provide 'my-defuns)
