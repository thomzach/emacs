(defcustom emacs-solo-use-custom-theme 'catppuccin
  "Select which `emacs-solo` customization theme to use.

- nil: Disable custom theme
- 'catppuccin: Use customizations for Catppuccin
- 'crafters: Use customizations for the Crafters theme

IMPORTANT NOTE: If you disable this or choose another theme, also check
`emacs-solo-avoid-flash-options` to ensure compatibility."
  :type '(choice
          (const :tag "Disabled" nil)
          (const :tag "Catppuccin" catppuccin)
          (const :tag "Crafters" crafters)
          (const :tag "Matrix" matrix))
  :group 'emacs-solo)

(defcustom emacs-solo-enable-rainbown-delimiters t
  "Enable `emacs-solo-enable-rainbown-delimiters'."
  :type 'boolean
  :group 'emacs-solo)


(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimiters-mode)
;;   :config
;;   (custom-set-faces
;;    '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "SlateBlue1"))))
;;    '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "chartreuse4"))))
;;    '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "medium orchid"))))
;;    '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "HotPink1"))))
;;    '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "SystemHilight"))))
;;    '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "gray55"))))
;;    '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "firebrick1"))))
;;    '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "chartreuse2"))))
;;    '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "purple3"))))
;;    ))

;;; │ EMACS-SOLO-RAINBOW-DELIMITERS
;;
;;  Colorizes matching delimiters
;;
;;  FIXME: Make it play nice with treesitter modes
;;
(use-package emacs-solo-rainbow-delimiters
  :if emacs-solo-enable-rainbown-delimiters
  :ensure nil
  :straight nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/rainbow-delimiters ()
    "Apply simple rainbow coloring to parentheses, brackets, and braces in the current buffer.
Opening and closing delimiters will have matching colors."
    (interactive)
    (let ((colors '(font-lock-function-name-face
                    font-lock-builtin-face
                    font-lock-type-face
                    font-lock-keyword-face
                    font-lock-variable-name-face
                    font-lock-constant-face
                    font-lock-string-face)))
      (font-lock-add-keywords
       nil
       `((,(rx (or "(" ")" "[" "]" "{" "}"))
          (0 (let* ((char (char-after (match-beginning 0)))
                    (depth (save-excursion
                             ;; Move to the correct position based on opening/closing delimiter
                             (if (member char '(?\) ?\] ?\}))
                                 (progn
                                   (backward-char) ;; Move to the opening delimiter
                                   (car (syntax-ppss)))
                               (car (syntax-ppss)))))
                    (face (nth (mod depth ,(length colors)) ',colors)))
               (list 'face face)))))))
    (font-lock-flush)
    (font-lock-ensure))

  (add-hook 'prog-mode-hook #'emacs-solo/rainbow-delimiters))


;; (use-package all-the-icons)

;; (use-package all-the-icons-completion
;;   :after (marginalia all-the-icons)
;;   :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
;;   :init
;;   (all-the-icons-completion-mode))



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

;; (defun zt/apply-theme-for-frame (&optional frame)
;;   "Apply GUI vs TTY theme for FRAME (or current frame)."
;;   (with-selected-frame (or frame (selected-frame))
;;     ;; Always start from a clean slate so earlier themes can't bleed through:
;;     (mapc #'disable-theme custom-enabled-themes)
;;     (if (display-graphic-p)
;;         (progn
;;           ;; ---- GUI THEME ----
;;           ;; Pick ONE of these; comment the other out.
;;           (load-theme 'modus-vivendi t)
;;           ;; If you prefer catppuccin in GUI instead:
;;           ;; (load-theme 'catppuccin t)
;;           ;; (when (boundp 'catppuccin-flavor) (catppuccin-reload))
;;           )
;;       ;; ---- TERMINAL THEME ----
;;       (load-theme 'wombat t))))

;; ;; Apply immediately for the current frame:
;; (zt/apply-theme-for-frame)

;; ;; Also apply for any frames created later (useful for emacsclient/daemon):
;; (add-hook 'after-make-frame-functions #'zt/apply-theme-for-frame)
;; (add-hook 'tty-setup-hook (lambda () (zt/apply-theme-for-frame (selected-frame))))


;;; ┌──────────────────── THEMES
;;; │ Cattpuccin Mocha Based Theme (hacked Modus)
;;
;; This tries to follow: https://github.com/catppuccin/catppuccin/blob/main/docs/style-guide.md
;; With the colors from: https://github.com/catppuccin/catppuccin/ (Mocha)
(use-package modus-themes
  :if (eq emacs-solo-use-custom-theme 'catppuccin)
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


;;; │ #SystemCrafters  Based Theme (hacked Modus)
(use-package modus-themes
  :if (eq emacs-solo-use-custom-theme 'crafters)
  :ensure nil
  :defer t
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts nil)
  (modus-themes-prompts '(bold intense))
  (modus-themes-common-palette-overrides
   `((accent-0 "#a1bfff")
     (accent-1 "#79a8ff")
     (bg-active bg-main)
     (bg-added "#2A3B2E")
     (bg-added-refine "#384c3f")
     (bg-changed "#3C435E")
     (bg-changed-refine "#4F5875")
     (bg-completion "#2f447f")
     (bg-completion-match-0 bg-main)
     (bg-completion-match-1 bg-main)
     (bg-completion-match-2 bg-main)
     (bg-completion-match-3 bg-main)
     (bg-hl-line "#30344a")
     (bg-hover-secondary "#676E95")
     (bg-line-number-active unspecified)
     (bg-line-number-inactive "#292D3E")
     (bg-main "#292D3E")
     (bg-mark-delete "#4d2d2d")
     (bg-mark-select "#3C435E")
     (bg-mode-line-active "#232635")
     (bg-mode-line-inactive "#282c3d")
     (bg-prominent-err "#4d2d2d")
     (bg-prompt unspecified)
     (bg-prose-block-contents "#232635")
     (bg-prose-block-delimiter bg-prose-block-contents)
     (bg-region "#3C435E")
     (bg-removed "#4d2d2d")
     (bg-removed-refine "#603939")
     (bg-tab-bar      "#292D3E")
     (bg-tab-current  bg-main)
     (bg-tab-other    "#292D3E")
     (border-mode-line-active nil)
     (border-mode-line-inactive nil)
     (builtin "#82aaff")
     (comment "#676E95")
     (constant  "#f78c6c")
     (cursor  "#EEFFFF")
     (date-weekday "#82aaff")
     (date-weekend "#f78c6c")
     (docstring "#8d92af")
     (err     "#ff5370")
     (fg-active fg-main)
     (fg-completion "white")
     (fg-completion-match-0 "#82aaff")
     (fg-completion-match-1 "#ff5370")
     (fg-completion-match-2 "#c3e88d")
     (fg-completion-match-3 "#f78c6c")
     (fg-heading-0 "#82aaff")
     (fg-heading-1 "#82aaff")
     (fg-heading-2 "#c792ea")
     (fg-heading-3 "#bb80b3")
     (fg-heading-4 "#a1bfff")
     (fg-line-number-active fg-main)
     (fg-line-number-inactive "gray50")
     (fg-link  "#82aaff")
     (fg-main "#EEFFFF")
     (fg-mark-delete "#ff5370")
     (fg-mark-select "#82aaff")
     (fg-mode-line-active "#A6Accd")
     (fg-mode-line-inactive "#676E95")
     (fg-prominent-err "#ff5370")
     (fg-prompt "#c792ea")
     (fg-prose-block-delimiter "#676E95")
     (fg-prose-verbatim "#c3e88d")
     (fg-region "white")
     (fnname    "#82aaff")
     (fringe "#292D3E")
     (identifier "#c792ea")
     (info    "#89DDFF")
     (keyword   "#89DDFF")
     (name "#82aaff")
     (number "#f78c6c")
     (property "#82aaff")
     (string "#c3e88d")
     (type      "#c792ea")
     (variable  "#c792ea")
     (warning "#ffcb6b")))
  :config
  (modus-themes-with-colors
    (custom-set-faces
     `(change-log-acknowledgment ((,c :foreground "#a1bfff")))
     `(change-log-date ((,c :foreground "#c3e88d")))
     `(change-log-name ((,c :foreground "#f78c6c")))
     `(diff-context ((,c :foreground "#82aaff")))
     `(diff-file-header ((,c :foreground "#bb80b3")))
     `(diff-header ((,c :foreground "#82aaff")))
     `(diff-hunk-header ((,c :foreground "#f78c6c")))
     `(gnus-button ((,c :foreground "#82aaff")))
     `(gnus-group-mail-3 ((,c :foreground "#82aaff")))
     `(gnus-group-mail-3-empty ((,c :foreground "#82aaff")))
     `(gnus-header-content ((,c :foreground "#89DDFF")))
     `(gnus-header-from ((,c :foreground "#c792ea")))
     `(gnus-header-name ((,c :foreground "#c3e88d")))
     `(gnus-header-subject ((,c :foreground "#82aaff")))
     `(log-view-message ((,c :foreground "#a1bfff")))
     `(match ((,c :background "#3C435E" :foreground "#EEFFFF")))
     `(modus-themes-search-current ((,c :background "#ff5370" :foreground "#292D3E" )))
     `(modus-themes-search-lazy ((,c :background "#3C435E" :foreground "#EEFFFF")))
     `(newsticker-extra-face ((,c :foreground "#8d92af" :height 0.8 :slant italic)))
     `(newsticker-feed-face ((,c :foreground "#ff5370" :height 1.2 :weight bold)))
     `(newsticker-treeview-face ((,c :foreground "#EEFFFF")))
     `(newsticker-treeview-selection-face ((,c :background "#3C435E" :foreground "#EEFFFF")))
     `(tab-bar ((,c :background "#292D3E" :foreground "#A6Accd")))
     `(tab-bar-tab ((,c :background "#292D3E" :underline t)))
     `(tab-bar-tab-group-current ((,c :background "#292D3E" :foreground "#A6Accd" :underline t)))
     `(tab-bar-tab-group-inactive ((,c :background "#292D3E" :foreground "#777")))
     `(tab-bar-tab-inactive ((,c :background "#292D3E" :foreground "#676E95")))
     `(vc-dir-file ((,c :foreground "#82aaff")))
     `(vc-dir-header-value ((,c :foreground "#a1bfff")))))
  :init
  (load-theme 'modus-vivendi-tinted t))


;;; │ Matrix Based Theme (hacked Modus)
(use-package modus-themes
  :if (eq emacs-solo-use-custom-theme 'matrix)
  :ensure nil
  :defer t
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts nil)
  (modus-themes-prompts '(bold intense))

  ;; MATRIX COLOR SCHEME OVERRIDES
  (modus-themes-common-palette-overrides
   `(
     ;; accents → bright greens
     (accent-0 "#00FF41")      ; malachite
     (accent-1 "#008F11")      ; islamic green

     ;; backgrounds
     (bg-active bg-main)
     (bg-added "#003B00")              ; dark green
     (bg-added-refine "#005A00")       ; slightly brighter
     (bg-changed "#004800")
     (bg-changed-refine "#006600")
     (bg-completion "#0D0208")         ; vampire black
     (bg-completion-match-0 "#0D0208")
     (bg-completion-match-1 "#0D0208")
     (bg-completion-match-2 "#0D0208")
     (bg-completion-match-3 "#0D0208")
     (bg-hl-line "#002200")
     (bg-hover-secondary "#003B00")
     (bg-line-number-active unspecified)
     (bg-line-number-inactive "#0D0208")
     (bg-main "#0D0208")
     (bg-mark-delete "#190A10")
     (bg-mark-select "#003B00")
     (bg-mode-line-active "#001900")
     (bg-mode-line-inactive "#001900")
     (bg-prominent-err "#190A10")
     (bg-prompt unspecified)
     (bg-prose-block-contents "#001600")
     (bg-prose-block-delimiter bg-prose-block-contents)
     (bg-region "#003B00")
     (bg-removed "#190A10")
     (bg-removed-refine "#2B1520")
     (bg-tab-bar      "#0D0208")
     (bg-tab-current  bg-main)
     (bg-tab-other    "#0D0208")

     ;; borders
     (border-mode-line-active nil)
     (border-mode-line-inactive nil)

     ;; foreground semantic groups
     (builtin "#00FF41")
     (comment "#005A00")          ; dim green
     (constant  "#00FF41")
     (cursor  "#00FF41")
     (date-weekday "#00FF41")
     (date-weekend "#008F11")
     (docstring "#00C738")
     (err     "#00FF71")     ;; red is NOT in Matrix palette—using red for contrast
     (fg-active fg-main)
     (fg-completion "#00FF41")
     (fg-completion-match-0 "#00FF41")
     (fg-completion-match-1 "#00FF71")   ;; keeping semantic separation
     (fg-completion-match-2 "#00C738")
     (fg-completion-match-3 "#008F11")

     ;; headings mapped from bright → dark green gradient
     (fg-heading-0 "#00FF41")
     (fg-heading-1 "#00C738")
     (fg-heading-2 "#00A52A")
     (fg-heading-3 "#008F11")
     (fg-heading-4 "#005A00")

     (fg-line-number-active "#00FF41")
     (fg-line-number-inactive "#006600")
     (fg-link  "#00FF41")
     (fg-main "#00FF41")
     (fg-mark-delete "#00FF71")
     (fg-mark-select "#00FF41")
     (fg-mode-line-active "#00C738")
     (fg-mode-line-inactive "#005A00")
     (fg-prominent-err "#00FF71")
     (fg-prompt "#00FF41")
     (fg-prose-block-delimiter "#006600")
     (fg-prose-verbatim "#00C738")
     (fg-region "#00FF41")
     (fnname    "#00FF41")
     (fringe "#0D0208")
     (identifier "#00C738")
     (info    "#00FF41")
     (keyword   "#00C738")
     (keyword "#00C738")
     (name "#00FF41")
     (number "#008F11")
     (property "#00FF41")
     (string "#00C738")
     (type      "#00A52A")
     (variable  "#008F11")
     (warning "#00A52A")))
  :config
  (modus-themes-with-colors
    (custom-set-faces
     `(change-log-acknowledgment ((,c :foreground "#00C738")))
     `(change-log-date ((,c :foreground "#008F11")))
     `(change-log-name ((,c :foreground "#00A52A")))
     `(diff-context ((,c :foreground "#00FF41")))
     `(diff-file-header ((,c :foreground "#00C738")))
     `(diff-header ((,c :foreground "#00FF41")))
     `(diff-hunk-header ((,c :foreground "#008F11")))

     `(flymake-warning ((,c :foreground "#00A52A"
                      :underline (:color "#00A52A" :style wave))))
     `(flymake-note ((,c :foreground "#00FF41"
                      :underline (:color "#00FF41" :style wave))))
     `(link ((,c :foreground "#00FF41"
                      :underline (:color "#00FF41" :style line))))

     ;; GNUS
     `(gnus-button ((,c :foreground "#00FF41")))
     `(gnus-group-mail-3 ((,c :foreground "#00FF41")))
     `(gnus-group-mail-3-empty ((,c :foreground "#00FF41")))
     `(gnus-header-content ((,c :foreground "#00C738")))
     `(gnus-header-from ((,c :foreground "#008F11")))
     `(gnus-header-name ((,c :foreground "#00C738")))
     `(gnus-header-subject ((,c :foreground "#00FF41")))

     `(log-view-message ((,c :foreground "#00C738")))
     `(match ((,c :background "#003B00" :foreground "#00FF41")))

     `(modus-themes-search-current ((,c :background "#00FF41" :foreground "#0D0208")))
     `(modus-themes-search-lazy ((,c :background "#003B00" :foreground "#00FF41")))

     ;; Newsticker
     `(newsticker-extra-face ((,c :foreground "#005A00" :height 0.8 :slant italic)))
     `(newsticker-feed-face ((,c :foreground "#00A52A" :height 1.2 :weight bold)))
     `(newsticker-treeview-face ((,c :foreground "#00FF41")))
     `(newsticker-treeview-selection-face ((,c :background "#003B00" :foreground "#00FF41")))

     ;; Tabs
     `(tab-bar ((,c :background "#0D0208" :foreground "#00C738")))
     `(tab-bar-tab ((,c :background "#0D0208" :underline t)))
     `(tab-bar-tab-group-current ((,c :background "#0D0208" :foreground "#00C738" :underline t)))
     `(tab-bar-tab-group-inactive ((,c :background "#0D0208" :foreground "#005A00"))))

    `(tab-bar-tab-inactive ((,c :background "#0D0208" :foreground "#008F11")))
    `(vc-dir-file ((,c :foreground "#00FF41")))
    `(vc-dir-header-value ((,c :foreground "#00C738"))))
  :init
  (load-theme 'modus-vivendi t)
  )

;;
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


(provide 'zt-themes)
