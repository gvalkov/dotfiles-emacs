(require 'speedbar)
(require 'ezimage)
(require 'sb-image)

(speedbar-add-supported-extension ".py")

;; (setq speedbar-frame-parameters
;;       '((minibuffer)
;;         (width          . 35)
;;         (border-width   . 0)
;;         (menu-bar-lines . 0)
;;         (background-mode . "light")
;;         ;; (background-color . "#E6E6E6")
;;         ;; (foreground-color . "#000000")
;;         (unsplittable   . t)))
;;         ;; (font           . "-*-Consolas-normal-r-*-*-11-*-*-*-c-*-iso8859-1")))

(setq speedbar-hide-button-brackets-flag t)
(setq speedbar-show-unknown-files t)
(setq speedbar-smart-directory-expand-flag t)
(setq speedbar-directory-button-trim-method 'trim)
(setq speedbar-use-images nil)
(setq speedbar-indentation-width 2)
(setq speedbar-use-imenu-flag t)
(setq speedbar-file-unshown-regexp "flycheck-.*")
(setq sr-speedbar-width 40)
(setq sr-speedbar-width-x 40)
(setq sr-speedbar-auto-refresh nil)
(setq sr-speedbar-skip-other-window-p t)
(setq sr-speedbar-right-side nil)



(defezimage sbimage-directory
  ((:type png :file "speedbar/dir.png" :ascent center))
  "Image used for empty directories.")

(defezimage sbimage-directory-plus
  ((:type png :file "speedbar/dir-plus.png" :ascent center))
  "Image used for closed directories with stuff in them.")

(defezimage sbimage-directory-minus
  ((:type png :file "speedbar/dir-minus.png" :ascent center))
  "Image used for open directories with stuff in them.")

(defezimage sbimage-page-plus
  ((:type png :file "speedbar/page-plus.png" :ascent center))
  "Image used for closed files with stuff in them.")

(defezimage sbimage-page-minus
  ((:type png :file "speedbar/page-minus.png" :ascent center))
  "Image used for open files with stuff in them.")

(defezimage sbimage-page
  ((:type png :file "speedbar/page.png" :ascent center))
  "Image used for files with nothing interesting in it.")

(defezimage sbimage-tag
  ((:type png :file "speedbar/tag.png" :ascent center))
  "Image used for tags.")

(defezimage sbimage-tag-plus
  ((:type png :file "speedbar/tag-plus.png" :ascent center))
  "Image used for closed tag groups.")

(defezimage sbimage-tag-minus
  ((:type png :file "speedbar/tag-minus.png" :ascent center))
  "Image used for open tags.")

(defezimage sbimage-tag-gt
  ((:type png :file "speedbar/tag-gt.png" :ascent center))
  "Image used for closed tags (with twist arrow).")

(defezimage sbimage-tag-v
  ((:type png :file "speedbar/tag-v.png" :ascent center))
  "Image used for open tags (with twist arrow).")

(defezimage sbimage-tag-type
  ((:type png :file "speedbar/tag-type.png" :ascent center))
  "Image used for tags that represent a data type.")

(defezimage sbimage-box-plus
  ((:type png :file "speedbar/box-plus.png" :ascent center))
  "Image of a closed box.")

(defezimage sbimage-box-minus
  ((:type png :file "speedbar/box-minus.png" :ascent center))
  "Image of an open box.")

(defezimage sbimage-mail
  ((:type png :file "speedbar/mail.png" :ascent center))
  "Image of an envelope.")

(defezimage sbimage-checkout
  ((:type png :file "speedbar/checkmark.png" :ascent center))
  "Image representing a checkmark.  For files checked out of a VC.")

(defezimage sbimage-object
  ((:type png :file "speedbar/bits.png" :ascent center))
  "Image representing bits (an object file.)")

(defezimage sbimage-object-out-of-date
  ((:type png :file "speedbar/bitsbang.png" :ascent center))
  "Image representing bits with a ! in it.  (An out of data object file.)")

(defezimage sbimage-label
  ((:type png :file "speedbar/label.png" :ascent center))
  "Image used for label prefix.")

(defezimage sbimage-lock
  ((:type png :file "speedbar/lock.png" :ascent center))
  "Image of a lock.  Used for Read Only, or private.")

(defezimage sbimage-unlock
  ((:type png :file "speedbar/unlock.png" :ascent center))
  "Image of an unlocked lock.")

(defezimage sbimage-key
  ((:type png :file "speedbar/key.png" :ascent center))
  "Image of a key.")

(defezimage sbimage-document-tag
  ((:type png :file "speedbar/doc.png" :ascent center))
  "Image used to indicate documentation available.")

(defezimage sbimage-document-plus
  ((:type png :file "speedbar/doc-plus.png" :ascent center))
  "Image used to indicate closed documentation.")

(defezimage sbimage-document-minus
  ((:type png :file "speedbar/doc-minus.png" :ascent center))
  "Image used to indicate open documentation.")

(defezimage sbimage-info-tag
  ((:type png :file "speedbar/info.png" :ascent center))
  "Image used to indicate more information available.")

(setq speedbar-expand-image-button-alist
      '(("<+>" . sbimage-directory-plus)
        ("<->" . sbimage-directory-minus)
        ("< >" . sbimage-directory)
        ("[+]" . sbimage-page-plus)
        ("[-]" . sbimage-page-minus)
        ("[?]" . sbimage-page)
        ("[ ]" . sbimage-page)
        ("{+}" . sbimage-box-plus)
        ("{-}" . sbimage-box-minus)
        ("<M>" . sbimage-mail)
        ("<d>" . sbimage-document-tag)
        ("<i>" . sbimage-info-tag)
        (" =>" . sbimage-tag)
        (" +>" . sbimage-tag-gt)
        (" ->" . sbimage-tag-v)
        (">"   . sbimage-tag)
        ("@"   . sbimage-tag-type)
        ("  @" . sbimage-tag-type)
        ("*"   . sbimage-checkout)
        ("#"   . sbimage-object)
        ("!"   . sbimage-object-out-of-date)
        ("//"  . sbimage-label)
        ("%"   . sbimage-lock)
        ))

(provide 'my-speedbar)
