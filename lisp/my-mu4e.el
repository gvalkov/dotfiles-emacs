;; ;; (eval-after-load 'mu4e
;; ;;   '(progn
;; ;;      (evil-make-overriding-map mu4e-headers-mode-map 'normal t)
;; ;;      (evil-define-key 'normal mu4e-headers-mode-map
;; ;;        (kbd "RET") 'mu4e-headers-view-message
;; ;;        (kbd "<tab>") 'mu4e-headers-view-message
;; ;;        [mouse-2] 'mu4e-headers-view-message
;; ;;        "o"    'mu4e-headers-view-message
;; ;;        "j"    'evil-next-line
;; ;;        "k"    'evil-previous-line
;; ;;        "G"    'evil-goto-line
;; ;;        "gg"   'evil-goto-first-line
;; ;;        "\C-j" 'mu4e-view-headers-next
;; ;;        "\C-k" 'mu4e-view-headers-prev)

;; ;;      (evil-make-overriding-map mu4e-view-mode-map 'normal t)
;; ;;      (evil-define-key 'normal mu4e-view-mode-map
;; ;;        "j" 'evil-next-line
;; ;;        "\C-j" 'mu4e-view-headers-next
;; ;;        "\C-k" 'mu4e-view-headers-prev
;; ;;        )))
