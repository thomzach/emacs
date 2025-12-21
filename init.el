;;; EMACS
(use-package emacs
  :ensure nil
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  (setq cursor-type 'bar)
  (setq tab-always-indent 'complete)

  (when (>= emacs-major-version 31)
    (tty-tip-mode nil))   ;; EMACS-31
  (tooltip-mode nil)

  (select-frame-set-input-focus (selected-frame))
  (blink-cursor-mode 0)
  (global-auto-revert-mode 1)
  (recentf-mode 1)
  (repeat-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (winner-mode)
  (xterm-mouse-mode 1)
  (file-name-shadow-mode 1) ; allows us to type a new path without having to delete the current one
  (with-current-buffer (get-buffer-create "*scratch*")
    (insert (format ";;
;;   Loading time : %s
;;   Packages     : %s
;;
"
                    (emacs-init-time)
                    (number-to-string (length package-activated-list)))))

  (message (emacs-init-time))
  
  :bind
  (("M-o" . other-window)
   ("C-," . duplicate-dwim)
   ("C-x C-b" . ibuffer)
   ("<f5>" . compile)
   ;; ("M-O" . other-frame)
   ;; ("M-g r" . recentf)
   ;; ("M-s g" . grep)
   ;; ("M-s f" . find-name-dired)
   ;; ("RET" . newline-and-indent)
   ;; ("C-z" . nil)
   ("C-x C-z" . nil)
   ("C-x C-k RET" . nil)
   ("C-M-z" . delete-pair)
   ("C-M-." . rg-dwim-project-dir)
   )
  :custom
  (treesit-font-lock-level 4)
  (completion-ignore-case t)
  (delete-by-moving-to-trash t)
  (ispell-dictionary "en_US")
  (xref-search-program 'ripgrep)
  (grep-command "rg -nS --no-heading ")
  (global-auto-revert-non-file-buffers t)
  (comp-async-report-warnings-errors nil)
  (windmove-default-keybindings 'shift)
  (framemove-hook-into-windmove t)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (ad-redefinition-action 'accept)
  (auto-save-default t)
  (bookmark-file (expand-file-name "cache/bookmarks" user-emacs-directory))
  (column-number-mode t)
  (line-number-mode t)
  (line-spacing nil)
  (completions-detailed t)
  (doc-view-resolution 200)
  (delete-pair-blink-delay 0)
  (display-line-numbers-width 4)
  (display-line-numbers-widen t)
  (display-fill-column-indicator-warning nil) ; EMACS-31
  (delete-selection-mode 1)
  (enable-recursive minibuffers t)
  (find-ls-option '("-exec ls -ldh {} +" . "-ldh"))  ; find-dired results with human readable sizes
  (frame-resize-pixelwise t)
  (global-auto-revert-non-file-buffers t)
  (global-goto-address-mode t)                            ;     C-c RET on URLs open in default browser
  (browse-url-secondary-browser-function 'eww-browse-url) ; C-u C-c RET on URLs open in EWW
  (help-window-select t)
  (history-length 300)
  (inhibit-startup-message t)
  (initial-scratch-message "")
  (ibuffer-human-readable-size t) ; EMACS-31
  (ispell-dictionary "en_US")
  (kill-do-not-save-duplicates t)
  (kill-region-dwim 'emacs-word)  ; EMACS-31
  (create-lockfiles nil)   ; No lock files
  (make-backup-files nil)  ; No backup files
  (multisession-directory (expand-file-name "cache/multisession/" user-emacs-directory))
  (native-comp-async-on-battery-power nil)  ; No compilations when on battery EMACS-31
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-use-momentum nil)
  (project-list-file (expand-file-name "cache/projects" user-emacs-directory))
  (ring-bell-function 'ignore)
  (read-answer-short t)
  (recentf-max-saved-items 300) ; default is 20
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))
  (recentf-save-file (expand-file-name "cache/recentf" user-emacs-directory))
  (register-use-preview t)
  (remote-file-name-inhibit-delete-by-moving-to-trash t)
  (remote-file-name-inhibit-auto-save t)
  (remote-file-name-inhibit-locks t)
  (remote-file-name-inhibit-auto-save-visited t)
  (tramp-copy-size-limit (* 2 1024 1024)) ;; 2MB
  (tramp-use-scp-direct-remote-copying t)
  (tramp-verbose 2)
  (resize-mini-windows 'grow-only)
  (scroll-conservatively 8)
  (scroll-margin 5)
  (savehist-save-minibuffer-history t)    ; t is default
  (savehist-additional-variables
   '(kill-ring                            ; clipboard
     register-alist                       ; macros
     mark-ring global-mark-ring           ; marks
     search-ring regexp-search-ring))     ; searches
  (savehist-file (expand-file-name "cache/history" user-emacs-directory))
  (save-place-file (expand-file-name "cache/saveplace" user-emacs-directory))
  (save-place-limit 600)
  (set-mark-command-repeat-pop t) ; So we can use C-u C-SPC C-SPC C-SPC... instead of C-u C-SPC C-u C-SPC...
  (split-width-threshold 170)     ; So vertical splits are preferred
  (split-height-threshold nil)
  (shr-use-colors nil)
  (switch-to-buffer-obey-display-actions t)
  (tab-always-indent 'complete)
  (tab-width 4)
  (transient-history-file (expand-file-name "cache/transient/history.el" user-emacs-directory))
  (transient-levels-file (expand-file-name "cache/transient/levels.el" user-emacs-directory))
  (transient-values-file (expand-file-name "cache/transient/values.el" user-emacs-directory))
  (treesit-font-lock-level 4)
  (treesit-auto-install-grammar t) ; EMACS-31
  (treesit-enabled-modes t)        ; EMACS-31
  (truncate-lines t)
  (undo-limit (* 13 160000))
  (undo-strong-limit (* 13 240000))
  (undo-outer-limit (* 13 24000000))
  (url-configuration-directory (expand-file-name "cache/url/" user-emacs-directory))
  (use-dialog-box nil)
  (use-file-dialog nil)
  (use-package-hook-name-suffix nil)
  (use-short-answers t)
  (visible-bell nil)
  (window-combination-resize t)
  (window-resize-pixelwise nil)
  (xref-search-program 'ripgrep)
  (zone-all-frames t)            ; EMACS-31
  (zone-all-windows-in-frame t)  ; EMACS-31
  (zone-programs '[zone-pgm-rat-race])
  (grep-command "rg -nS --no-heading ")
  (grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".jj" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  :config
  ;; (toggle-frame-maximized)
  ;; (select-frame-set-input-focus (selected-frame))
  (savehist-mode 1)
  (save-place-mode 1)
  (winner-mode)
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; Save manual customizations to other file than init.el
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))

  (load custom-file 'noerror 'nomessage)

  ;; Make C-x 5 o repeatable
  (defvar-keymap frame-repeat-map
    :repeat t
    "o" #'other-frame
    "n" #'make-frame
    "d" #'delete-frame)
  (put 'other-frame 'repeat-map 'frame-repeat-map)

  ;; ;; Setup preferred fonts when present on System
  ;; (defun emacs-solo/setup-font ()
  ;;   (let* ((emacs-solo-have-default-font (find-font (font-spec :family emacs-solo-preferred-font-name)))
  ;;          (size (nth (if (eq system-type 'darwin) 0 1)
  ;;                     emacs-solo-preferred-font-sizes)))
  ;;     (set-face-attribute 'default nil
  ;;                         :family (when emacs-solo-have-default-font
  ;;                                   emacs-solo-preferred-font-name)
  ;;                         :height size)

  ;;     ;; macOS specific fine-tuning
  ;;     (when (and (eq system-type 'darwin) emacs-solo-have-default-font)
  ;;       ;; Glyphs for powerline/icons
  ;;       (set-fontset-font t '(#xe0b0 . #xe0bF) (font-spec :family emacs-solo-preferred-font-name))
  ;;       ;; Emojis
  ;;       (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'append)
  ;;       (add-to-list 'face-font-rescale-alist '("Apple Color Emoji" . 0.8)))))

  ;; ;; Load Preferred Font Setup
  ;; (when emacs-solo-enable-preferred-font
  ;;   (emacs-solo/setup-font))

  
  ;; We want auto-save, but no #file# cluterring, so everything goes under our config cache/
  (make-directory (expand-file-name "cache/auto-saves/" user-emacs-directory) t)
  (setq auto-save-list-file-prefix (expand-file-name "cache/auto-saves/sessions/" user-emacs-directory)
        auto-save-file-name-transforms `((".*" ,(expand-file-name "cache/auto-saves/" user-emacs-directory) t)))

  ;; TERMs should use the entire window space
  (defun emacs-solo/disable-global-scrolling-in-ansi-term ()
    "Disable global scrolling behavior in ansi-term buffers."
    (setq-local scroll-conservatively 101)
    (setq-local scroll-margin 0)
    (setq-local scroll-step 0))
  (add-hook 'term-mode-hook #'emacs-solo/disable-global-scrolling-in-ansi-term)

  ;; TRAMP specific HACKs
  ;; See https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)

  (with-eval-after-load 'tramp
    (with-eval-after-load 'compile
      (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

  (setopt tramp-persistency-file-name (expand-file-name "cache/tramp" user-emacs-directory))

  ;; Set line-number-mode with relative numbering
  (setq display-line-numbers-type 'relative)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode)

  ;; A Protesilaos life savier HACK
  ;; Add option "d" to whenever using C-x s or C-x C-c, allowing a quick preview
  ;; of the diff (if you choose `d') of what you're asked to save.
  (add-to-list 'save-some-buffers-action-alist
               (list "d"
                     (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
                     "show diff between the buffer and its file"))
  
  ;; On Terminal: changes the vertical separator to a full vertical line
  ;;              and truncation symbol to a right arrow
  (set-display-table-slot standard-display-table 'vertical-border ?\u2502)
  (set-display-table-slot standard-display-table 'truncation ?\u2192)

  ;; Ibuffer filters
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("org"     (or
                       (mode . org-mode)
                       (name . "^\\*Org Src")
                       (name . "^\\*Org Agenda\\*$")))
           ("tramp"   (name . "^\\*tramp.*"))
           ("emacs"   (or
                       (name . "^\\*scratch\\*$")
                       (name . "^\\*Messages\\*$")
                       (name . "^\\*Warnings\\*$")
                       (name . "^\\*Shell Command Output\\*$")
                       (name . "^\\*Async-native-compile-log\\*$")))
           ("ediff"   (name . "^\\*[Ee]diff.*"))
           ("vc"      (name . "^\\*vc-.*"))
           ("dired"   (mode . dired-mode))
           ("terminal" (or
                        (mode . term-mode)
                        (mode . shell-mode)
                        (mode . eshell-mode)))
           ("help"    (or
                       (name . "^\\*Help\\*$")
                       (name . "^\\*info\\*$")))
           ("news"    (name . "^\\*Newsticker.*"))
           ("gnus"    (or
                       (mode . message-mode)
                       (mode . gnus-group-mode)
                       (mode . gnus-summary-mode)
                       (mode . gnus-article-mode)
                       (name . "^\\*Group\\*")
                       (name . "^\\*Summary\\*")
                       (name . "^\\*Article\\*")
                       (name . "^\\*BBDB\\*")))
           ("chat"    (or
                       (mode . rcirc-mode)
                       (mode . erc-mode)
                       (name . "^\\*rcirc.*")
                       (name . "^\\*ERC.*"))))))

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))
  (setq ibuffer-show-empty-filter-groups nil) ; don't show empty groups

  ;; So eshell git commands open an instance of THIS config of Emacs
  (setenv "GIT_EDITOR" (format "emacs --init-dir=%s " (shell-quote-argument user-emacs-directory)))
  (setenv "JJ_EDITOR" (format "emacs --init-dir=%s " (shell-quote-argument user-emacs-directory)))
  (setenv "EDITOR" (format "emacs --init-dir=%s " (shell-quote-argument user-emacs-directory)))
  (setenv "PAGER" "cat")
  ;; So rebase from eshell opens with a bit of syntax highlight
  (add-to-list 'auto-mode-alist '("/git-rebase-todo\\'" . conf-mode))
  
  ;; Makes any xref buffer "exportable" to a grep buffer with "E" so you can edit it with "e".
  (defun emacs-solo/xref-to-grep-compilation ()
    "Export the current Xref results to a grep-like buffer (Emacs 30+)."
    (interactive)
    (unless (derived-mode-p 'xref--xref-buffer-mode)
      (user-error "Not in an Xref buffer"))

    (let* ((items (and (boundp 'xref--fetcher)
                       (funcall xref--fetcher)))
           (buf-name "*xref→grep*")
           (grep-buf (get-buffer-create buf-name)))
      (unless items
        (user-error "No xref items found"))

      (with-current-buffer grep-buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "-*- mode: grep; default-directory: %S -*-\n\n"
                          default-directory))
          (dolist (item items)
            (let* ((loc (xref-item-location item))
                   (file (xref-file-location-file loc))
                   (line (xref-file-location-line loc))
                   (summary (xref-item-summary item)))
              (insert (format "%s:%d:%s\n" file line summary)))))
        (grep-mode))
      (pop-to-buffer grep-buf)))
  (with-eval-after-load 'xref
    (define-key xref--xref-buffer-mode-map (kbd "E")
                #'emacs-solo/xref-to-grep-compilation))

  ;; Runs 'private.el' after Emacs inits
  (add-hook 'after-init-hook
            (lambda ()
              (let ((private-file (expand-file-name "private.el" user-emacs-directory)))
                (when (file-exists-p private-file)
                  (load private-file)))))
  
) ;; EMACS

;;; COMPILATION
(use-package compile
  :ensure nil
  :straight nil
  :hook
  (;; Not ideal, but I do not want this poluting the modeline
   (compilation-start . (lambda () (setq compilation-in-progress nil))))
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  (ansi-color-for-compilation-mode t)
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

;; Keeps .elc files up to date with .el
(use-package auto-compile
  :config (auto-compile-on-load-mode))

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

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; (use-package recentf
;;   :config
;;   (setq recentf-auto-cleanup 'never
;;         recentf-max-saved-items 1000
;;         recentf-save-file (concat user-emacs-directory ".recentf"))
;;   (recentf-mode t)
;;   :diminish nil)

(set-default-coding-systems 'utf-8)

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
;; (add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c-ts-mode))

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

(use-package async)

;; (when (eq system-type 'windows-nt)
;;   (setq org-agenda-files '("H:/zthomas/private/org"))
;;   )
;; (when (eq system-type 'gnu/linux)
;;   (setq org-agenda-files '("/mnt/nas/org"))
;;   )
(setq org-agenda-files '("/mnt/nas/org/agenda/"))


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

;; (use-package virtualenvwrapper
;;   :ensure t
;;   :config
;;   (venv-initialize-interactive-shells)
;;   (venv-initialize-eshell)
;;   )

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


;;; │ ELECTRIC-PAIR
;; Adds closing brackets
(use-package electric-pair
  :ensure nil
  :straight nil
  :defer
  :hook (after-init-hook . electric-pair-mode))

;;; │ PAREN
;; Shows matching parthesis
(use-package paren
  :ensure nil
  :straight nil
  :hook (after-init-hook . show-paren-mode)
  :custom
  (show-paren-delay 0)
  (show-paren-style 'mixed)
  (show-paren-context-when-offscreen t)) ;; show matches within window splits

;;; │ TIME
(use-package time
  :ensure nil
  :straight nil
  :hook (after-init-hook . display-time-mode) ;; If we'd like to see it on the mode-line
  :custom
  (world-clock-time-format "%A %d %B %r %Z")
  (world-clock-sort-order "%FT%T") ; EMACS-31
  (display-time-day-and-date t)
  (display-time-default-load-average nil)
  (display-time-mail-string "")
  (zoneinfo-style-world-list                ; use `M-x worldclock RET' to see it
   '(("America/Chicago" "Chicago"))))



(setq gnus-select-method
      '(nntp "news.gwene.org"))

(use-package direnv
 :config
 (direnv-mode))

(use-package envrc
  :config
  (envrc-global-mode))

(use-package rg
  :config
  (rg-enable-default-bindings))

(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)
(use-package capf-autosuggest
  :config
  (add-hook 'comint-mode-hook #'capf-autosuggest-mode)
  (add-hook 'eshell-mode-hook #'capf-autosuggest-mode))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
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
(require 'zt-vc)
(require 'zt-diff)
(require 'zt-programming)

;; (require 'zt-eshell)
;; (require 'zt-eglot) ;; zt-lsp has eglot currently

;; LOOK into jinx emacs packages
(use-package multi-magit
  :straight
  (multi-magit :host github
               :repo "luismbo/multi-magit"
               :branch "master"))
