(use-package multiple-cursors
  :config
  (setq mc/always-run-for-all t)
  :bind
  (
   ("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this) ;ctrl + shift + >
   ("C-<" . mc/mark-previous-like-this)
   ("C-M->" . mc/skip-to-next-like-this)
   ("C-M-<" . mc/skip-to-previous-like-this)
   ("C-c C->" . mc/mark-all-dwim)
   ("C-c C-<" . mc/mark-all-like-this)
  ))


(use-package expand-region
  :bind
  (
   ("M-'" . er/expand-region)
   ("M-\"" . er/contract-region)
   ))

(global-set-key (kbd "M-i") 'imenu)


;; (use-package avy
;;   :ensure t
;;   :bind ("M-s" . avy-goto-char-timer)
;;   :config
;;   (setq avy-timeout-seconds 1)
;;   )

(use-package flash
  :defer nil
  :straight
  (flash :host github
         :repo "Prgebish/flash"
         :branch "main")
  :custom
  (flash-multi-window t)
  (flash-rainbow t)
  (flash-label-position 'overlay)
  :config
  (require 'flash-isearch)
  (flash-isearch-mode 1)  
  )

(use-package move-text
  :bind
  ("M-p" . move-text-up)
  ("M-n" . move-text-down)
  )

(provide 'zt-editing)
