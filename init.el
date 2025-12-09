;; Initialize package sources
(use-package emacs
  :ensure nil
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  (setq cursor-type 'bar)
  (setq tab-always-indent 'complete)
  :bind
  (("M-o" . other-window)
   ("M-O" . other-frame)
   ("C-," . duplicate-dwim)
   ("<f5>" . compile)
   ;; ("M-g r" . recentf)
   ;; ("M-s g" . grep)
   ;; ("M-s f" . find-name-dired)
   ;; ("C-x C-b" . ibuffer)
   ;; ("RET" . newline-and-indent)
   ;; ("C-z" . nil)
   ("C-x C-z" . nil)
   ("C-x C-k RET" . nil)
   ("C-M-." . rg-dwim-project-dir)
   )
  :custom
  (completion-ignore-case t)
  (delete-by-moving-to-trash t)
  (ispell-dictionary "en_US")
  (create-lockfiles nil)   ; No backup files
  (make-backup-files nil)  ; No backup files
  (backup-inhibited t)     ; No backup files
  (xref-search-program 'ripgrep)
  (grep-command "rg -nS --no-heading ")
  :config
  (toggle-frame-maximized)
  (select-frame-set-input-focus (selected-frame))
  (savehist-mode 1)
  (save-place-mode 1)
  (winner-mode)
  ;; Save manual customizations to other file than init.el
  (setq custom-file (locate-user-emacs-file "custom-vars.el")))

  (load custom-file 'noerror 'nomessage)

;;; COMPILATION
(use-package compile
  :ensure nil
  :hook
  (;; Not ideal, but I do not want this poluting the modeline
   (compilation-start . (lambda () (setq compilation-in-progress nil))))
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  (ansi-color-for-compilation-mode t)
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)
(setq dired-dwim-target t)


(fset 'yes-or-no-p 'y-or-n-p)

(setq comp-async-report-warnings-errors nil)

;; (set-face-attribute 'default nil :font "Blex Mono Nerd Font" :height 200)
;; (set-face-attribute 'default nil :font "Iosevka NF" :height 200)
;; (set-face-attribute 'default nil :font "Iosevka NF")

(defvar zt/default-font "Iosevka NF-20"
  "My preferred default font for graphical Emacs frames.")

(when (display-graphic-p)
  (set-frame-font zt/default-font nil t))

(defun zt/set-default-font (frame)
  (with-selected-frame frame
    (when (display-graphic-p)
      (set-frame-font zt/default-font nil t))))

(add-hook 'after-make-frame-functions #'zt/set-default-font)


(use-package no-littering)
;; (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
(setq make-backup-files nil)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))


(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package recentf
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 1000
        recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-mode t)
  :diminish nil)


(set-default-coding-systems 'utf-8)

;; (use-package dashboard
;;   :config
;;   (dashboard-setup-startup-hook)
;;   )

(setq treesit-font-lock-level 4)

(use-package ripgrep)

(blink-cursor-mode -1)
(setq scroll-margin 5)
(setq scroll-conservatively 100)

;; (use-package dumb-jump
;;   :config
;;   (setq dumb-jump-force-searcher 'rg)
;;   (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;;   (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
;;   )

(add-to-list 'auto-mode-alist '("\\.dsc" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.inf" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.dec" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.dsc" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.fdf" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.vfr" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.hfr" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.robot" . python-mode))

(add-to-list 'auto-mode-alist '("\\.bin\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.dts\\'" . devicetree-ts-mode))
(add-to-list 'auto-mode-alist '("\\.dtsi\\'" . devicetree-ts-mode))
(add-to-list 'auto-mode-alist '("\\.overlay\\'" . devicetree-ts-mode))

(add-to-list 'auto-mode-alist '("\\.defconfig\\'" . kconfig-mode))
(add-to-list 'auto-mode-alist '("\\.board\\'" . kconfig-mode))

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))

(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-ts-mode))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


(setq tramp-default-method "sshx")
;; Dired stuff

;; Open dired folders in same buffer
(put 'dired-find-alternate-file 'disabled nil)
;; Sort Dired buffers
(setq dired-listing-switches "-agho --group-directories-first")
;; Copy and move files netween dired buffers
(setq dired-dwim-target t)
;;trash instead of delete
(setq delete-by-moving-to-trash t)
;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(when (eq system-type 'windows-nt)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "C:/Program Files/Google/Chrome/Application/chrome.exe")
  )
(when (eq system-type 'gnu/linux)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program (if (string-match-p "Windows" (getenv "PATH"))
                                       "/mnt/c/Program Files/Mozilla Firefox/firefox.exe"
                                     "thorium-browser"))
  )
(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'ibuffer-mode-hook
          #'(lambda ()
              (ibuffer-auto-mode 1)))


(use-package async)

;; (when (eq system-type 'windows-nt)
;;   (setq org-agenda-files '("H:/zthomas/private/org"))
;;   )
;; (when (eq system-type 'gnu/linux)
;;   (setq org-agenda-files '("/mnt/nas/org"))
;;   )
(setq org-agenda-files '("/mnt/nas/org/agenda/"))

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("vterm" (mode . vterm-mode))
               ("shells" (mode . shell-mode))
               ("dired" (mode . dired-mode))
               ("org" (mode . org-mode))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ("pdfs" (name . "\\.pdf"))
               ))))

(require 'ibuf-ext)

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

;; (modify-syntax-entry ?_ "w")

;; ;; Define the whitespace style.
;; (setq-default whitespace-style
;;               '(face spaces empty tabs newline trailing space-mark tab-mark newline-mark))
;; ;; Whitespace color corrections.
;; (require 'color)
;; (let* ((ws-lighten 30) ;; Amount in percentage to lighten up black.
;;        (ws-color (color-lighten-name "#000000" ws-lighten)))
;;   (custom-set-faces
;;    `(whitespace-newline                ((t (:foreground ,ws-color))))
;;    `(whitespace-missing-newline-at-eof ((t (:foreground ,ws-color))))
;;    `(whitespace-space                  ((t (:foreground ,ws-color))))
;;    `(whitespace-space-after-tab        ((t (:foreground ,ws-color))))
;;    `(whitespace-space-before-tab       ((t (:foreground ,ws-color))))
;;    `(whitespace-tab                    ((t (:foreground ,ws-color))))
;;    `(whitespace-trailing               ((t (:foreground ,ws-color))))))
;; ;; Make these characters represent whitespace.
;; (setq-default whitespace-display-mappings
;;               '(
;;                 ;; space -> · else .
;;                 (space-mark 32 [183] [46])
;;                 ;; new line -> ¬ else $
;;                 (newline-mark ?\n [172 ?\n] [36 ?\n])
;;                 ;; carriage return (Windows) -> ¶ else #
;;                 (newline-mark ?\r [182] [35])
;;                 ;; tabs -> » else >
;;                 (tab-mark ?\t [187 ?\t] [62 ?\t])))


(use-package yasnippet
  :ensure t
  :hook ((text-mode
          prog-mode
          conf-mode
          snippet-mode) . yas-minor-mode-on)
  :init
  (setq yas-snippet-dir "~/.emacs.d/snippets"))

(use-package yasnippet-snippets)

(setq c-set-offset 2)
(setq-default tab-width 4)

(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent 'complete)

(use-package capf-autosuggest
  :config
  (add-hook 'comint-mode-hook #'capf-autosuggest-mode)
  (add-hook 'eshell-mode-hook #'capf-autosuggest-mode)
  )

(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil)

(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  )

(windmove-default-keybindings 'shift)
(setq framemove-hook-into-windmove t)

(use-package rg
  :config
  (rg-enable-default-bindings)
)

;; (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))

(use-package envrc
  :config
  (envrc-global-mode)
  )

;; (use-package exec-path-from-shell
;;   :config
;;   (when (daemonp)
;;   (exec-path-from-shell-initialize)))
;; (add-to-list 'exec-path "~/.cargo/bin")

;;; │ EMACS-SOLO-EXEC-PATH-FROM-SHELL
;;
;;  Loads users default shell PATH settings into Emacs. Usefull
;;  when calling Emacs directly from GUI systems.
;;
(use-package emacs-solo-exec-path-from-shell
  :ensure nil
  :straight nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/set-exec-path-from-shell-PATH ()
    "Set up Emacs' `exec-path' and PATH environment the same as the user's shell.
This works with bash, zsh, or fish)."
    (interactive)
    (let* ((shell (getenv "SHELL"))
           (shell-name (file-name-nondirectory shell))
           (command
            (cond
             ((string= shell-name "fish")
              "fish -c 'string join : $PATH'")
             ((string= shell-name "zsh")
              "zsh -i -c 'printenv PATH'")
             ((string= shell-name "bash")
              "bash --login -c 'echo $PATH'")
             (t nil))))
      (if (not command)
          (message "emacs-solo: Unsupported shell: %s" shell-name)
        (let ((path-from-shell
               (replace-regexp-in-string
                "[ \t\n]*$" ""
                (shell-command-to-string command))))
          (when (and path-from-shell (not (string= path-from-shell "")))
            (setenv "PATH" path-from-shell)
            (setq exec-path (split-string path-from-shell path-separator))
            (message ">>> emacs-solo: PATH loaded from %s" shell-name))))))

  (add-hook 'after-init-hook #'emacs-solo/set-exec-path-from-shell-PATH))


;; (require 'ansi-color)
;; (defun my/ansi-colorize-buffer ()
;;   (let ((buffer-read-only nil))
;;     (ansi-color-apply-on-region (point-min) (point-max))))
;; (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

(setq dired-kill-when-opening-new-dired-buffer t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(use-package devicetree-ts-mode)

(setq gnus-select-method
      '(nntp "news.gwene.org"))

(use-package direnv
 :config
 (direnv-mode))


(require 'zt-themes)
(require 'zt-icons)
(require 'zt-lsp)
(require 'zt-treesit)
(require 'zt-org)
(require 'zt-minibuffer)
(require 'zt-completion)
(require 'zt-term)
(require 'zt-dired)
(require 'zt-editing)
(require 'zt-git)
(require 'zt-custom-commands)
(require 'zt-display-buffer-alist)
;; (require 'zt-ai)
(require 'zt-utils)
(require 'zt-projects)
(require 'zt-notes)
(require 'zt-window)
(require 'zt-highlight)


;; LOOK into jinx emacs packages
