;; (defun my/project-compilation-buffer-name-function (name-of-mode)
;;   (format "*compilation: %s*" (project-name (project-current))))

;; ;; Customize compilation buffer names using the project name
;; (setq project-compilation-buffer-name-function #'my/project-compilation-buffer-name-function)

;; (defun my/project-switch-to-buffer (&optional all-buffers)
;;   (interactive "P")
;;   (if (or (not (project-current))
;;           all-buffers)
;;       (call-interactively #'switch-to-buffer)
;;     (call-interactively #'project-switch-to-buffer)))

;; ;; Use our custom buffer switching function
;; (keymap-global-set "C-x b" #'my/project-switch-to-buffer)

;; ;; Allow all keymap keys to be used when switching project
;; (setq project-switch-use-entire-map t)

;; ;; ;; Returns the parent directory containing a .project.el file, if any,
;; ;; ;; to override the standard project.el detection logic when needed.
;; ;; (defun zkj-project-override (dir)
;; ;;   (let ((override (locate-dominating-file dir ".project.el")))
;; ;;     (if override
;; ;;       (cons 'vc override)
;; ;;       nil)))

;; ;; (add-hook 'project-find-functions #'zkj-project-override)

;; ;; (use-package project
;; ;;   ;; Cannot use :hook because 'project-find-functions does not end in -hook
;; ;;   ;; Cannot use :init (must use :config) because otherwise
;; ;;   ;; project-find-functions is not yet initialized.
;; ;;   :config
;; ;;   (add-hook 'project-find-functions #'zkj-project-override))


(use-package projectile
  :diminish projectile-mode
  :bind-keymap
  ("C-x p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (eq system-type 'windows-nt)
    (when (file-directory-p "c:/work")
      (setq projectile-project-search-path '("c:/work")))
    (setq projectile-switch-project-action #'projectile-dired)
    )
  (when (eq system-type 'gnu/linux)
    (when (file-directory-p "~/code")
      (setq projectile-project-search-path '("~/code")))
    (setq projectile-switch-project-action #'projectile-dired)
    )
  :config
 (projectile-mode)
 (setq projectile-per-project-compilation-buffer t)
 )



(provide 'zt-projects)
