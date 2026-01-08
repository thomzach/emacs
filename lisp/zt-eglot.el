;; ;;; │ EGLOT
;; (use-package eglot
;;   :ensure nil
;;   :staright nil
;;   :custom
;;   (eglot-autoshutdown t)
;;   (eglot-events-buffer-size 0)
;;   (eglot-events-buffer-config '(:size 0 :format full))
;;   (eglot-prefer-plaintext nil)
;;   (jsonrpc-event-hook nil)
;;   (eglot-code-action-indications nil) ;; EMACS-31 -- annoying as hell
;;   :init
;;   (fset #'jsonrpc--log-event #'ignore)

;;   (setq-default eglot-workspace-configuration (quote
;;                                                (:gopls (:hints (:parameterNames t)))))

;;   (defun emacs-solo/eglot-setup ()
;;     "Setup eglot mode with specific exclusions."
;;     (unless (eq major-mode 'emacs-lisp-mode)
;;       (eglot-ensure)))

;;   (add-hook 'prog-mode-hook #'emacs-solo/eglot-setup)

;;   (with-eval-after-load 'eglot
;;     (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp")))

;;   :bind (:map
;;          eglot-mode-map
;;          ("C-c l a" . eglot-code-actions)
;;          ("C-c l o" . eglot-code-action-organize-imports)
;;          ("C-c l r" . eglot-rename)
;;          ("C-c l i" . eglot-inlay-hints-mode)
;;          ("C-c l f" . eglot-format)))

;; ;;; │ FLYMAKE
;; (use-package flymake
;;   :ensure nil
;;   :straight nil
;;   :defer t
;;   :hook (prog-mode-hook . flymake-mode)
;;   :bind (:map flymake-mode-map
;;               ("M-8" . flymake-goto-next-error)
;;               ("M-7" . flymake-goto-prev-error)
;;               ("C-c ! n" . flymake-goto-next-error)
;;               ("C-c ! p" . flymake-goto-prev-error)
;;               ("C-c ! l" . flymake-show-buffer-diagnostics)
;;               ("C-c ! t" . toggle-flymake-diagnostics-at-eol))
;;   :custom
;;   (flymake-show-diagnostics-at-end-of-line nil)
;;   ;; (flymake-show-diagnostics-at-end-of-line 'short)
;;   (flymake-indicator-type 'margins)
;;   (flymake-margin-indicators-string
;;    `((error "!" compilation-error)      ;; Alternatives: », E, W, i, !, ?, ⚠️)
;;      (warning "?" compilation-warning)
;;      (note "i" compilation-info)))
;;   :config
;;   ;; Define the toggle function
;;   (defun toggle-flymake-diagnostics-at-eol ()
;;     "Toggle the display of Flymake diagnostics at the end of the line
;; and restart Flymake to apply the changes."
;;     (interactive)
;;     (setq flymake-show-diagnostics-at-end-of-line
;;           (not flymake-show-diagnostics-at-end-of-line))
;;     (flymake-mode -1) ;; Disable Flymake
;;     (flymake-mode 1)  ;; Re-enable Flymake
;;     (message "Flymake diagnostics at end of line: %s"
;;              (if flymake-show-diagnostics-at-end-of-line
;;                  "Enabled" "Disabled"))))
(use-package eglot
  :hook
  (c-ts-mode . eglot-ensure)
  )


(provide 'zt-eglot)
