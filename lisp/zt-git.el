(use-package git-link
  :config
  (global-set-key (kbd "C-c g l") 'git-link)
  )

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package git-gutter
  :defer nil
  :config
  (setq git-gutter:modified-sign "|")
  (setq git-gutter:added-sign "|")
  (setq git-gutter:deleted-sign "|")
  (global-git-gutter-mode t)
  )

;;emacs solo buffer gutter?


(provide 'zt-git)
