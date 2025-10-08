;; (use-package kaolin-themes
;;  :config
;;   (load-theme 'kaolin-temple t)
;;  (kaolin-treemacs-theme))

(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'frappe)
  )

;; (use-package spaceway-theme
;;   :ensure nil
;;   :load-path "lisp/spaceway/"
;;   :config
;;   (global-hl-line-mode t)
;;   (set-frame-parameter nil 'cursor-color "#dc322f")
;;   (add-to-list 'default-frame-alist '(cursor-color . "#dc322f"))

;; (when my/my-system
;;   (set-frame-parameter nil 'alpha-background 85)
;;   (add-to-list 'default-frame-alist '(alpha-background . 85)))

;; (load-theme 'spaceway t)
;; (setenv "SCHEME" "dark")
;; )
(use-package spacemacs-theme
  :defer t
  ;; (load-theme 'spacemacs-dark t)
  )


(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(column-number-mode)
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(dolist (mode '(
		        term-mode-hook
		        pdf-view-mode-hook
		        eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "SlateBlue1"))))
   '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "chartreuse4"))))
   '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "medium orchid"))))
   '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "HotPink1"))))
   '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "SystemHilight"))))
   '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "gray55"))))
   '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "firebrick1"))))
   '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "chartreuse2"))))
   '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "purple3"))))
   ))

(use-package all-the-icons)

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))



(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; (use-package modus-themes
;;   :init
;;   (require-theme 'modus-themes)
;;   :custom
;;   (modus-themes-italic-constructs t)
;;   (modus-themes-bold-constructs nil)
;;   (modus-themes-mixed-fonts t)
;;   (modus-themes-variable-pitch-ui t)
;;   (modus-themes-custom-auto-reload t)
;;   (modus-themes-disable-other-themes t)
;;   (modus-themes-prompts '(italic bold))
;;   (modus-themes-completions
;;    '((matches . (extrabold))
;;      (selection . (semibold italic text-also))))

;;   (modus-themes-org-blocks 'gray-background)

;;   (modus-themes-headings
;;    '((1 . (variable-pitch 1.5))
;;      (2 . (1.3))
;;      (agenda-date . (1.3))
;;      (agenda-structure . (variable-pitch light 1.8))
;;      (t . (1.1))))


;;   (modus-vivendi-palette-overrides
;;    '(

;;      (bg-main     "#000000")
;;      (bg-dim      "#111111")
;;      (bg-active   "#222222")
;;      (bg-inactive "#333333")

;;      (fg-main     "#ffffff")
;;      (fg-dim      )

;;      (cursor      "#00ffff")
;;      (warning     "#fafad2")

;;      (bg-completion "#2e8b57")
;;      (bg-region     bg-active)
;;      (bg-tab-bar        bg-main)
;;      (bg-tab-current    bg-active)
;;      (bg-tab-other      bg-dim)
;;      (fringe unspecified)
;;      (bg-mode-line-active bg-dim)
;;      (border-mode-line-active unspecified)
;;      (bg-line-number-active  bg-main)
;;      (bg-line-number-inactive  bg-main)
;;      ))

;;   :config
;;   ;; (load-theme 'modus-vivendi t)
;;   )

;;; ┌──────────────────── THEMES
;;; │ Catppuccin Mocha Based Theme (hacked Modus)
;;
;; This tries to follow: https://github.com/catppuccin/catppuccin/blob/main/docs/style-guide.md
;; With the colors from: https://github.com/catppuccin/catppuccin/ (Mocha)
(use-package modus-themes
  :ensure nil
  :defer t
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts nil)
  (modus-themes-prompts '(bold intense))
  (modus-themes-common-palette-overrides
   `((accent-0 "#89b4fa")
     (accent-1 "#89dceb")
     (bg-active bg-main)
     (bg-added "#364144")
     (bg-added-refine "#4A5457")
     (bg-changed "#3e4b6c")
     (bg-changed-refine "#515D7B")
     (bg-completion "#45475a")
     (bg-completion-match-0 "#1e1e2e")
     (bg-completion-match-1 "#1e1e2e")
     (bg-completion-match-2 "#1e1e2e")
     (bg-completion-match-3 "#1e1e2e")
     (bg-hl-line "#2a2b3d")
     (bg-hover-secondary "#585b70")
     (bg-line-number-active unspecified)
     (bg-line-number-inactive "#1e1e2e")
     (bg-main "#1e1e2e")
     (bg-mark-delete "#443245")
     (bg-mark-select "#3e4b6c")
     (bg-mode-line-active "#181825")
     (bg-mode-line-inactive "#181825")
     (bg-prominent-err "#443245")
     (bg-prompt unspecified)
     (bg-prose-block-contents "#313244")
     (bg-prose-block-delimiter bg-prose-block-contents)
     (bg-region "#585b70")
     (bg-removed "#443245")
     (bg-removed-refine "#574658")
     (bg-tab-bar      "#1e1e2e")
     (bg-tab-current  bg-main)
     (bg-tab-other    "#1e1e2e")
     (border-mode-line-active nil)
     (border-mode-line-inactive nil)
     (builtin "#89b4fa")
     (comment "#9399b2")
     (constant  "#f38ba8")
     (cursor  "#f5e0dc")
     (date-weekday "#89b4fa")
     (date-weekend "#fab387")
     (docstring "#a6adc8")
     (err     "#f38ba8")
     (fg-active fg-main)
     (fg-completion "#cdd6f4")
     (fg-completion-match-0 "#89b4fa")
     (fg-completion-match-1 "#f38ba8")
     (fg-completion-match-2 "#a6e3a1")
     (fg-completion-match-3 "#fab387")
     (fg-heading-0 "#f38ba8")
     (fg-heading-1 "#fab387")
     (fg-heading-2 "#f9e2af")
     (fg-heading-3 "#a6e3a1")
     (fg-heading-4 "#74c7ec")
     (fg-line-number-active "#b4befe")
     (fg-line-number-inactive "#7f849c")
     (fg-link  "#89b4fa")
     (fg-main "#cdd6f4")
     (fg-mark-delete "#f38ba8")
     (fg-mark-select "#89b4fa")
     (fg-mode-line-active "#bac2de")
     (fg-mode-line-inactive "#585b70")
     (fg-prominent-err "#f38ba8")
     (fg-prompt "#cba6f7")
     (fg-prose-block-delimiter "#9399b2")
     (fg-prose-verbatim "#a6e3a1")
     (fg-region "#cdd6f4")
     (fnname    "#89b4fa")
     (fringe "#1e1e2e")
     (identifier "#cba6f7")
     (info    "#94e2d5")
     (keyword   "#cba6f7")
     (keyword "#cba6f7")
     (name "#89b4fa")
     (number "#fab387")
     (property "#89b4fa")
     (string "#a6e3a1")
     (type      "#f9e2af")
     (variable  "#fab387")
     (warning "#f9e2af")))
  :config
  (modus-themes-with-colors
    (custom-set-faces
     `(change-log-acknowledgment ((,c :foreground "#b4befe")))
     `(change-log-date ((,c :foreground "#a6e3a1")))
     `(change-log-name ((,c :foreground "#fab387")))
     `(diff-context ((,c :foreground "#89b4fa")))
     `(diff-file-header ((,c :foreground "#f5c2e7")))
     `(diff-header ((,c :foreground "#89b4fa")))
     `(diff-hunk-header ((,c :foreground "#fab387")))
     `(gnus-button ((,c :foreground "#8aadf4")))
     `(gnus-group-mail-3 ((,c :foreground "#8aadf4")))
     `(gnus-group-mail-3-empty ((,c :foreground "#8aadf4")))
     `(gnus-header-content ((,c :foreground "#7dc4e4")))
     `(gnus-header-from ((,c :foreground "#cba6f7")))
     `(gnus-header-name ((,c :foreground "#a6e3a1")))
     `(gnus-header-subject ((,c :foreground "#8aadf4")))
     `(log-view-message ((,c :foreground "#b4befe")))
     `(match ((,c :background "#3e5768" :foreground "#cdd6f5")))
     `(modus-themes-search-current ((,c :background "#f38ba8" :foreground "#11111b" ))) ;; :foreground "#cdd6f4" -- Catppuccin default, not that visible...
     `(modus-themes-search-lazy ((,c :background "#3e5768" :foreground "#cdd6f5")))     ;; :foreground "#cdd6f4" :background "#94e2d5" -- Catppuccin default, not that visible...
     `(newsticker-extra-face ((,c :foreground "#9399b2" :height 0.8 :slant italic)))
     `(newsticker-feed-face ((,c :foreground "#f38ba8" :height 1.2 :weight bold)))
     `(newsticker-treeview-face ((,c :foreground "#cdd6f4")))
     `(newsticker-treeview-selection-face ((,c :background "#3e5768" :foreground "#cdd6f5")))
     `(tab-bar ((,c :background "#1e1e2e" :foreground "#bac2de")))
     `(tab-bar-tab ((,c :background "#1e1e2e" :underline t)))
     `(tab-bar-tab-group-current ((,c :background "#1e1e2e" :foreground "#bac2de" :underline t)))
     `(tab-bar-tab-group-inactive ((,c :background "#1e1e2e" :foreground "#9399b2"))))
     `(tab-bar-tab-inactive ((,c :background "#1e1e2e" :foreground "#a6adc8")))
     `(vc-dir-file ((,c :foreground "#89b4fa")))
     `(vc-dir-header-value ((,c :foreground "#b4befe"))))
  :init
  (load-theme 'modus-vivendi t))



(defun zt/apply-theme-for-frame (&optional frame)
  "Apply GUI vs TTY theme for FRAME (or current frame)."
  (with-selected-frame (or frame (selected-frame))
    ;; Always start from a clean slate so earlier themes can't bleed through:
    (mapc #'disable-theme custom-enabled-themes)
    (if (display-graphic-p)
        (progn
          ;; ---- GUI THEME ----
          ;; Pick ONE of these; comment the other out.
          ;; (load-theme 'modus-vivendi t)
          ;; If you prefer catppuccin in GUI instead:
          (load-theme 'catppuccin t)
          (when (boundp 'catppuccin-flavor) (catppuccin-reload))
          )
      ;; ---- TERMINAL THEME ----
      (load-theme 'wombat t))))

;; Apply immediately for the current frame:
(zt/apply-theme-for-frame)

;; Also apply for any frames created later (useful for emacsclient/daemon):
(add-hook 'after-make-frame-functions #'zt/apply-theme-for-frame)
(add-hook 'tty-setup-hook (lambda () (zt/apply-theme-for-frame (selected-frame))))



(provide 'zt-themes)
