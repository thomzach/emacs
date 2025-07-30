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
   ("M-j" . duplicate-dwim)
   ("<f5>" . compile)
   ;; ("M-g r" . recentf)
   ;; ("M-s g" . grep)
   ;; ("M-s f" . find-name-dired)
   ;; ("C-x C-b" . ibuffer)
   ;; ("RET" . newline-and-indent)
   ;; ("C-z" . nil)
   ("C-x C-z" . nil)
   ("C-x C-k RET" . nil)
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
  )

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
(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

(fset 'yes-or-no-p 'y-or-n-p)

(setq comp-async-report-warnings-errors nil)

;; (set-face-attribute 'default nil :font "Blex Mono Nerd Font" :height 200)
(set-face-attribute 'default nil :font "Iosevka NF" :height 230)

(use-package no-littering)
;; (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
(setq make-backup-files nil)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))

(use-package undo-fu)


(use-package treemacs-icons-dired
  :config (treemacs-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode))


(use-package recentf
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 1000
        recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-mode t)
  :diminish nil)


(set-default-coding-systems 'utf-8)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  )

;; (setq treesit-language-source-alist
;;       '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;         (cmake "https://github.com/uyha/tree-sitter-cmake")
;;         (css "https://github.com/tree-sitter/tree-sitter-css")
;;         (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;         (go "https://github.com/tree-sitter/tree-sitter-go")
;;         (html "https://github.com/tree-sitter/tree-sitter-html")
;;         (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;         (json "https://github.com/tree-sitter/tree-sitter-json")
;;         (make "https://github.com/alemuller/tree-sitter-make")
;;         (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;         (python "https://github.com/tree-sitter/tree-sitter-python")
;;         (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;         (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;         (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;         (yaml "https://github.com/ikatyang/tree-sitter-yaml")
;;         (c "https://github.com/tree-sitter/tree-sitter-c")
;;         (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;         (rust "https://github.com/tree-sitter/tree-sitter-rust")
;;         ))

;; (setq major-mode-remap-alist
;;       '((yaml-mode . yaml-ts-mode)
;;         (bash-mode . bash-ts-mode)
;;         (js2-mode . js-ts-mode)
;;         (typescript-mode . typescript-ts-mode)
;;         (json-mode . json-ts-mode)
;;         (css-mode . css-ts-mode)
;;         (python-mode . python-ts-mode)
;;         (c-mode . c-ts-mode)
;;         (c++-mode . c++-ts-mode)
;;         (c-or-c++-mode . c-or-c++-ts-mode)
;;         ))
(setq treesit-font-lock-level 4)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package ripgrep)
(use-package hl-todo
  :ensure t
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode)))
(global-hl-todo-mode)



(setq browse-url-browser-function 'eww-browse-url
      shr-use-colors nil
      shr-bullet "• "
      shr-folding-mode t
      eww-search-prefix "https://duckduckgo.com/html?q="
      url-privacy-level '(email agent cookies lastloc))
(use-package pdf-tools
  :config
  (pdf-tools-install)
  (add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)
  ;;(add-hook 'pdf-view-mode-hook (lambda () (blink-cursor-mode -1)))
  (setq-default pdf-view-display-size 'fit-page)
  )

(blink-cursor-mode -1)
(setq scroll-margin 5)
(setq scroll-conservatively 100)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

(advice-add 'ggtags-prev-mark :override
	        (lambda () (pop-tag-mark)))

(use-package dumb-jump
  :config
  (setq dumb-jump-force-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  )

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
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-ts-mode))


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

(use-package nix-mode
  :mode "\\.nix\\'")



(use-package savehist
  :init
  (savehist-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     orderless-flex
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  )



;; A few more useful configurations...
(defun zt-toggle-window-dedication ()
  "Toggles window dedication in the selected window."
  (interactive)
  (set-window-dedicated-p (selected-window)
                          (not (window-dedicated-p (selected-window)))))

(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

(fset 'perl-mode 'cperl-mode)
(setq cperl-invalid-face nil)

;; (modify-syntax-entry ?_ "w")

;; Define the whitespace style.
(setq-default whitespace-style
              '(face spaces empty tabs newline trailing space-mark tab-mark newline-mark))
;; Whitespace color corrections.
(require 'color)
(let* ((ws-lighten 30) ;; Amount in percentage to lighten up black.
       (ws-color (color-lighten-name "#000000" ws-lighten)))
  (custom-set-faces
   `(whitespace-newline                ((t (:foreground ,ws-color))))
   `(whitespace-missing-newline-at-eof ((t (:foreground ,ws-color))))
   `(whitespace-space                  ((t (:foreground ,ws-color))))
   `(whitespace-space-after-tab        ((t (:foreground ,ws-color))))
   `(whitespace-space-before-tab       ((t (:foreground ,ws-color))))
   `(whitespace-tab                    ((t (:foreground ,ws-color))))
   `(whitespace-trailing               ((t (:foreground ,ws-color))))))
;; Make these characters represent whitespace.
(setq-default whitespace-display-mappings
              '(
                ;; space -> · else .
                (space-mark 32 [183] [46])
                ;; new line -> ¬ else $
                (newline-mark ?\n [172 ?\n] [36 ?\n])
                ;; carriage return (Windows) -> ¶ else #
                (newline-mark ?\r [182] [35])
                ;; tabs -> » else >
                (tab-mark ?\t [187 ?\t] [62 ?\t])))

;; (use-package undo-tree
;;   :ensure t
;;   :init
;;   (global-undo-tree-mode))

(use-package yasnippet
  :ensure t
  :hook ((text-mode
          prog-mode
          conf-mode
          snippet-mode) . yas-minor-mode-on)
  :init
  (setq yas-snippet-dir "~/.emacs.d/snippets"))

(use-package yasnippet-snippets)

(use-package beacon
  :config
  (beacon-mode 0)
  )

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

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(windmove-default-keybindings 'shift)
(setq framemove-hook-into-windmove t)

(use-package rg
  :config
  (rg-enable-default-bindings)
)

;; Sample executable configuration
;; (use-package plantuml-mode
;;   :init
;;   (setq plantuml-executable-path "/usr/bin/plantuml")
;;   (setq plantuml-default-exec-mode 'executable)
;;   ;; Enable plantuml-mode for PlantUML files
;;   (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
;;   (add-to-list
;;   'org-src-lang-modes '("plantuml" . plantuml))
;;   (setq org-plantuml-exec-mode 'plantuml)
;;   )


(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'global)
  (aw-keys '(?t ?s ?n ?e ?r ?i ?a ?k o))
  (aw-minibuffer-flags t)
  )
(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))

(use-package envrc
  :config
  (envrc-global-mode)
  )

(use-package exec-path-from-shell
  :config
  (when (daemonp)
  (exec-path-from-shell-initialize)))
(add-to-list 'exec-path "~/.cargo/bin")

(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)


(setq dired-kill-when-opening-new-dired-buffer t)


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'zt-themes)
(require 'zt-lsp)
(require 'zt-org)
(require 'zt-minibuffer)
(require 'zt-completion)
(require 'zt-term)
(require 'zt-editing)
(require 'zt-git)
(require 'zt-custom-commands)
(require 'zt-display-buffer-alist)
(require 'zt-ai)
(require 'zt-utils)
(require 'zt-projects)

(use-package direnv
 :config
 (direnv-mode))

