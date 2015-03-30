;;; makefile-shell-backslash.el --- Smart ";/" or "/" for shell-heavy makefiles

;; Copyright (C) 2014 Georgi Valkov

;; Author: Georgi Valkov <georgi.t.valkov@gmail.com>
;; Version: 1.0
;; Keywords: tools unix convenience make
;; Created: May 2014
;; URL: https://github.com/gvalkov/makefile-shell-backslash/

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;;; Code:

(defconst makefile-rx-semicolon-backslash
  (rx (optional (group ";"))
      (zero-or-more whitespace)
      (group "\\")
      (zero-or-more whitespace)
      line-end)
  "Regex that matches a semicolon followed by a backslash - (;?)\s*(\\)\s*$")

(defconst makefile-rx-shell-backslash-only
  (regexp-opt '("do" "then" "else" "done"))
  "Keywords that should be followed by '\' instead of ';\'.")

(defun makefile-delete-semicolon-backslash ()
  "Remove '\' or ';\' from the end of a line."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward makefile-rx-semicolon-backslash (line-end-position) t)
        (replace-match ""))
    (delete-trailing-whitespace (line-beginning-position) (line-end-position))))

(defun makefile-append-semicolon-backslash (column &optional preserve)
  "Append and indent a ';\' to given column."
  (save-excursion
    (beginning-of-line)
    (let ((replace-with (if preserve "\\1\\2" ";\\\\"))
          (replace-len 2))
      (if (re-search-forward makefile-rx-semicolon-backslash (line-end-position) t)
          (progn
            (replace-match replace-with)
            (setq replace-len (length (match-substitute-replacement replace-with))))
        (end-of-line)
        (insert ";\\"))

    (end-of-line)
    (forward-char (- 0 replace-len))
    (delete-horizontal-space)
    (let ((col (if (looking-at ";\\\\") column (+ column 1)))
          (min (if (and (boundp 'makefile-backslash-align) makefile-backslash-align)
                   nil 1)))
      (indent-to col min)))))

(defun makefile-append-shell-semicolon-backslash (column)
  "Append and indent a ';\' or '\' to given column."
  (makefile-delete-semicolon-backslash)
  (save-excursion
    (end-of-line)
    (if (save-excursion
          (search-backward-regexp
           makefile-rx-shell-backslash-only
           (line-beginning-position) t))
        (insert "  \\")
      (insert "  ;\\")))
  (makefile-append-semicolon-backslash column t))

(defun makefile-calculate-alignment-column (b e)
  "Calculate suitable alignment column - stolen from makefile-mode."
  (let ((column 40))
    (save-excursion
      (goto-char b)
      (while (< (point) e)
        (end-of-line)
        (skip-chars-backward " 	;\\\\")
        (setq column (max column (+ 1 (current-column))))
        (forward-line 1)

        ;; Adjust upward to a tab column, if that doesn't push
        ;; past the margin.
        (if (> (% column tab-width) 0)
            (let ((adjusted (* (/ (+ column tab-width -1) tab-width)
                               tab-width)))
              (if (< adjusted (window-width))
                  (setq column adjusted))))))
    column))

;;###autoload
(defun makefile-shell-aligned-semicolon-backslash-region (b e)
  "Append and indent a '\' or ';\' to each line of region."
  (interactive "r")
  (save-excursion
    (let ((endmark (copy-marker e))
          (column (makefile-calculate-alignment-column b e)))
      (goto-char b)
      (while (and (< (point) endmark)
                  (save-excursion
                    (forward-line 1)
                    (< (point) endmark)))
        (makefile-append-shell-semicolon-backslash column)
        (forward-line 1))
      (move-marker endmark nil))))

;;###autoload
(defun makefile-shell-semicolon-backslash-region (b e)
  "Append '\' or ';\' to each line in region."
  (interactive "r")
  (save-excursion
    (let ((endmark (copy-marker e)))
      (goto-char b)
      (while (and (< (point) endmark)
                  (save-excursion
                    (forward-line 1)
                    (< (point) endmark)))
        (end-of-line)
        (makefile-delete-semicolon-backslash)
        (makefile-append-shell-semicolon-backslash (+ (current-column) 2))
        (forward-line 1))
      (move-marker endmark nil))))

;;###autoload
(defun makefile-delete-semicolon-backslash-region (b e)
  "Remove '\' and ';\' from each line in region."
  (interactive "r")
  (save-excursion
    (let ((endmark (copy-marker e)))
      (goto-char b)
      (while (< (point) endmark)
        (makefile-delete-semicolon-backslash)
        (end-of-line)
        (delete-horizontal-space)
        (forward-line 1)))))

(provide 'makefile-shell-backslash)
