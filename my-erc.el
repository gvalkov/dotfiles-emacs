;;; erc
;; (setq etc-autojoin-channels-alist
;;       '(("freenode.net" "#python")))

; logging
;; (setq erc-interpret-mirc-color t)
;; (setq erc-log-channels-directory "/tmp/emacs-erc.log")
;; (setq erc-log-insert-log-on-open t)
;; (setq erc-log-mode t)
;; (setq erc-spelling-mode t)
;; (setq erc-log-write-after-send t)
;; (setq erc-fill-column 120)
;; (add-hook 'erc-mode-hook (lambda () (auto-fill-mode 0)))

;; (setq erc-kill-buffer-on-part t)
;; (setq erc-kill-server-buffer-on-quit t)
;; (setq erc-hide-list '("JOIN" "PART" "QUIT"))

;;; (setq tls-program
;;;       '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof -cert /home/gv/private/gv.pem"
;;;         "gnutls-cli --priority secure256 --x509certfile /home/gv/private/gv.pem -p %p %h"
;;;         ))

;; (setq erc-auto-query 'window-noselect)

;; (defun start-irc ()
;;   (interactive)
;;   (erc-tls :server "irc.freenode.net" :port 7000
;;            :nick "gv" :full-name "gv"))

;; (eval-after-load 'erc
;;   '(progn
;;      (evil-make-overriding-map erc-mode-map 'normal t)
;;      (evil-make-overriding-map erc-mode-map 'insert t)
;;      (evil-define-key 'normal erc-mode-map
;;        (kbd "<up>")   'erc-previous-command
;;        (kbd "<down>") 'erc-next-command
;;        )

;;      (evil-define-key 'insert erc-mode-map
;;        (kbd "<up>")   'erc-previous-command
;;        (kbd "<down>") 'erc-next-command
;;        )))

;; ;; (add-hook 'erc-mode-hook #'(lambda () (setq autopair-dont-activate t)))
