;;; │ SMERGE
(use-package smerge-mode
  :ensure nil
  :straight nil  
  :bind (:map smerge-mode-map
              ("C-c C-s C-u" . smerge-keep-upper)
              ("C-c C-s C-l" . smerge-keep-lower)
              ("C-c C-s C-n" . smerge-next)
              ("C-c C-s C-p" . smerge-prev)))

;;; │ DIFF
(use-package diff-mode
  :ensure nil
  :straight nil  
  :defer t
  :bind (:map diff-mode-map
              ("M-o" . other-window))
  :config
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  (setq diff-font-lock-syntax 'hunk-also)
  (setq diff-font-lock-prettify nil))

;;; │ EDIFF
(use-package ediff
  :ensure nil
  :straight nil
  :commands (ediff-buffers ediff-files ediff-buffers3 ediff-files3)
  :init
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-show-clashes-only t))


(provide 'zt-diff)
