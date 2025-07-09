(defun my/project-compilation-buffer-name-function (name-of-mode)
  (format "*compilation: %s*" (project-name (project-current))))

;; Customize compilation buffer names using the project name
(setq project-compilation-buffer-name-function #'my/project-compilation-buffer-name-function)

(defun my/project-switch-to-buffer (&optional all-buffers)
  (interactive "P")
  (if (or (not (project-current))
          all-buffers)
      (call-interactively #'switch-to-buffer)
    (call-interactively #'project-switch-to-buffer)))

;; Use our custom buffer switching function
(keymap-global-set "C-x b" #'my/project-switch-to-buffer)

;; Allow all keymap keys to be used when switching project
(setq project-switch-use-entire-map t)

(provide 'zt-projects)
