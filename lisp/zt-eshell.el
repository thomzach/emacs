;;; │ ESHELL
(use-package eshell
  :ensure nil
  :straight nil
  :bind
  (("C-c e" . eshell))
  :defer t
  :config
  (setq eshell-history-size 100000)
  (setq eshell-hist-ignoredups t)


  ;; MAKE ALL INSTANCES OF ESHELL SHARE/MERGE ITS COMMAND HISTORY
  ;;
  (defun emacs-solo/eshell--collect-all-history ()
    "Return a list of all eshell history entries from all buffers and disk."
    (let ((history-from-buffers
           (cl-loop for buf in (buffer-list)
                    when (with-current-buffer buf (derived-mode-p 'eshell-mode))
                    append (with-current-buffer buf
                             (when (boundp 'eshell-history-ring)
                               (ring-elements eshell-history-ring)))))
          (history-from-file
           (when (file-exists-p eshell-history-file-name)
             (with-temp-buffer
               (insert-file-contents eshell-history-file-name)
               (split-string (buffer-string) "\n" t)))))
      (seq-uniq (append history-from-buffers history-from-file))))

  (defun emacs-solo/eshell--save-merged-history ()
    "Save all eshell buffer histories merged into `eshell-history-file-name`."
    (let ((all-history (emacs-solo/eshell--collect-all-history)))
      (with-temp-file eshell-history-file-name
        (insert (mapconcat #'identity all-history "\n")))))

  (add-hook 'kill-emacs-hook #'emacs-solo/eshell--save-merged-history)

  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell-read-history)))


  ;; CUSTOM WELCOME BANNER
  ;;
  (setopt eshell-banner-message
          (concat
           (propertize "   Welcome to the Emacs Solo Shell  \n\n" 'face '(:weight bold :foreground "#f9e2af"))
           (propertize " C-c t" 'face '(:foreground "#89b4fa" :weight bold)) " - toggles between prompts (full / minimum)\n"
           (propertize " C-c T" 'face '(:foreground "#89b4fa" :weight bold)) " - toggles between full prompts (lighter / heavier)\n"
           (propertize " C-c l" 'face '(:foreground "#89b4fa" :weight bold)) " - searches history\n"
           (propertize " C-l  " 'face '(:foreground "#89b4fa" :weight bold)) " - clears scrolling\n\n"))


  ;; DISABLE SCROLLING CONSERVATIVELY ON ESHELL
  ;;
  (defun emacs-solo/reset-scrolling-vars-for-term ()
    "Locally reset scrolling behavior in term-like buffers."
    (setq-local scroll-conservatively 0)
    (setq-local scroll-margin 0))
  (add-hook 'eshell-mode-hook #'emacs-solo/reset-scrolling-vars-for-term)


  ;; MAKES C-c l GIVE AN ICOMPLETE LIKE SEARCH TO HISTORY COMMANDS
  ;;
  (defun emacs-solo/eshell-pick-history ()
    "Show a unified and unique Eshell history from all open sessions and the history file.
Pre-fills the minibuffer with current Eshell input (from prompt to point)."
    (interactive)
    (unless (derived-mode-p 'eshell-mode)
      (user-error "This command must be called from an Eshell buffer"))
    (let* (;; Safely get current input from prompt to point
           (bol (save-excursion (eshell-bol) (point)))
           (eol (point))
           (current-input (buffer-substring-no-properties bol eol))

           ;; Path to Eshell history file
           (history-file (expand-file-name eshell-history-file-name
                                           eshell-directory-name))

           ;; Read from history file
           (history-from-file
            (when (file-exists-p history-file)
              (with-temp-buffer
                (insert-file-contents-literally history-file)
                (split-string (buffer-string) "\n" t))))

           ;; Read from in-memory Eshell buffers
           (history-from-rings
            (cl-loop for buf in (buffer-list)
                     when (with-current-buffer buf (derived-mode-p 'eshell-mode))
                     append (with-current-buffer buf
                              (when (bound-and-true-p eshell-history-ring)
                                (ring-elements eshell-history-ring)))))

           ;; Deduplicate and sort
           (all-history (reverse
                         (seq-uniq
                          (seq-filter (lambda (s) (and s (not (string-empty-p s))))
                                      (append history-from-rings history-from-file)))))

           ;; Prompt user with current input as initial suggestion
           (selection (completing-read "Eshell History: " all-history
                                       nil t current-input)))

      (when selection
        ;; Replace current input with selected history entry
        (delete-region bol eol)
        (insert selection))))


  ;; GIVES SYNTAX HIGHLIGHTING TO CAT
  ;;
  (defun eshell/cat-with-syntax-highlighting (filename)
    "Like cat(1) but with syntax highlighting.
  Stole from aweshell"
    (let ((existing-buffer (get-file-buffer filename))
          (buffer (find-file-noselect filename)))
      (eshell-print
       (with-current-buffer buffer
         (if (fboundp 'font-lock-ensure)
             (font-lock-ensure)
           (with-no-warnings
             (font-lock-fontify-buffer)))
         (let ((contents (buffer-string)))
           (remove-text-properties 0 (length contents) '(read-only nil) contents)
           contents)))
      (unless existing-buffer
        (kill-buffer buffer))
      nil))
  (advice-add 'eshell/cat :override #'eshell/cat-with-syntax-highlighting)


  ;; LOCAL ESHELL BINDINGS
  ;;
  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c l") #'emacs-solo/eshell-pick-history)
              (local-set-key (kbd "C-c t") #'emacs-solo/toggle-eshell-prompt)
              (local-set-key (kbd "C-c T") #'emacs-solo/toggle-eshell-prompt-resource-intensive)
              (local-set-key (kbd "C-l")
                             (lambda ()
                               (interactive)
                               (eshell/clear 1)))))


  ;; CUSTOM ESHELL PROMPT
  ;;
  (require 'vc)
  (require 'vc-git)

  (defvar emacs-solo/eshell-full-prompt t
    "When non-nil, show the full Eshell prompt. When nil, show minimal prompt.

The minimal version shows only the `emacs-solo/eshell-lambda-symbol', like:
 𝛌

The full version shows something like:

 🟢 0 🧙 user  💻 hostname  🕒 23:03:12  📁 ~/Projects/emacs-solo 
  main 

There is also `emacs-solo/eshell-full-prompt-resource-intensive' which will
print some extra `expensive' information, like conflicts, remote status, and
more, like:

 🟢 0 🧙 user  💻 hostname  🕒 23:03:12  📁 ~/Projects/emacs-solo 
  main ✏️2 ✨1 ")

  (defvar emacs-solo/eshell-full-prompt-resource-intensive nil
    "When non-nil, and emacs-solo/eshell-full-prompt t. Also show slower operations.
Check `emacs-solo/eshell-full-prompt' for more info.")

  (defvar emacs-solo/eshell-lambda-symbol "  λ "
    "Symbol used for the minimal Eshell prompt.")

  (defun emacs-solo/toggle-eshell-prompt ()
    "Toggle between full and minimal Eshell prompt."
    (interactive)
    (setq emacs-solo/eshell-full-prompt (not emacs-solo/eshell-full-prompt))
    (message "Eshell prompt: %s"
             (if emacs-solo/eshell-full-prompt "full" "minimal"))
    (when (derived-mode-p 'eshell-mode)
      (eshell-reset)))

  (defun emacs-solo/toggle-eshell-prompt-resource-intensive ()
    "Toggle between full and minimal Eshell prompt."
    (interactive)
    (setq emacs-solo/eshell-full-prompt-resource-intensive
          (not emacs-solo/eshell-full-prompt-resource-intensive))
    (message "Eshell prompt: %s"
             (if emacs-solo/eshell-full-prompt-resource-intensive "lighter" "heavier"))
    (when (derived-mode-p 'eshell-mode)
      (eshell-reset)))

  (defun enabled-icons-p ()
    "Return 'emoji, 'nerd or nil depending on what is in `emacs-solo-enabled-icons`."
    (cond
     ((memq 'nerd emacs-solo-enabled-icons) 'nerd)
     ((memq 'eshell emacs-solo-enabled-icons) 'emoji)
     (t nil)))

  (unless (eq emacs-solo-use-custom-theme 'catppuccin)
    (defvar eshell-solo/color-bg-dark "#212234")
    (defvar eshell-solo/color-bg-mid "#45475A")
    (defvar eshell-solo/color-fg-user "#89b4fa")
    (defvar eshell-solo/color-fg-host "#b4befe")
    (defvar eshell-solo/color-fg-dir "#A6E3A1")
    (defvar eshell-solo/color-fg-git "#F9E2AF"))

  (when (eq emacs-solo-use-custom-theme 'catppuccin)
    (defvar eshell-solo/color-bg-dark "#363a4f")
    (defvar eshell-solo/color-bg-mid  "#494d64")
    (defvar eshell-solo/color-fg-user "#89b4fa")
    (defvar eshell-solo/color-fg-host "#b4befe")
    (defvar eshell-solo/color-fg-dir  "#a6e3a1")
    (defvar eshell-solo/color-fg-git  "#f9e2af"))

  ;; No icons
  (when (not (enabled-icons-p))
    (defvar emacs-solo/eshell-icons
      '((arrow-left        . "")
        (arrow-right       . "")
        (success           . "1")
        (failure           . "0")
        (user-local        . "")
        (user-remote       . "")
        (host-local        . "")
        (host-remote       . "")
        (time              . "")
        (folder            . "")
        (branch            . " Git:")
        (modified          . "M")
        (untracked         . "U")
        (conflict          . "X")
        (git-merge         . "M")
        (git-ahead         . "A")
        (git-behind        . "B"))
      "Alist of all icons used in the Eshell prompt (no icons)."))


  ;; Emoji icons
  (when (eq (enabled-icons-p) 'emoji)
    (defvar emacs-solo/eshell-icons
      '((arrow-left        . "")
        (arrow-right       . "")
        (success           . "🟢")
        (failure           . "🔴")
        (user-local        . "🧙")
        (user-remote       . "👽")
        (host-local        . "💻")
        (host-remote       . "🌐")
        (time              . "🕒")
        (folder            . "📁")
        (branch            . "")
        (modified          . "✏️")
        (untracked         . "✨")
        (conflict          . "⚔️")
        (git-merge         . "🔀")
        (git-ahead         . "⬆️")
        (git-behind        . "⬇️"))
      "Alist of all icons used in the Eshell prompt (emoji)."))


  ;; Nerd Font icons
  (when (eq (enabled-icons-p) 'nerd)
    (defvar emacs-solo/eshell-icons
      '((arrow-left        . "")
        (arrow-right       . "")
        (success           . "")
        (failure           . "")
        (user-local        . "")
        (user-remote       . "")
        (host-local        . "")
        (host-remote       . "")
        (time              . "")
        (folder            . "")
        (branch            . "")
        (modified          . " ")
        (untracked         . " ")
        (conflict          . " ")
        (git-merge         . " ")
        (git-ahead         . " ")
        (git-behind        . " "))
      "Alist of all icons used in the Eshell prompt (nerd font)."))


  ;; --- Git info caching ---
  (defvar emacs-solo/git-cache nil)
  (defvar emacs-solo/git-cache-dir nil)
  (defvar emacs-solo/git-cache-time 0)

  (defun emacs-solo/git-info ()
    "Return cached Git info, with debug messages."
    (let ((root (ignore-errors (vc-git-root default-directory)))
          (now (float-time)))
      (if (or (not root)
              (not (numberp emacs-solo/git-cache-time))
              (not emacs-solo/git-cache)
              (not (equal root emacs-solo/git-cache-dir))
              (> (- now (or emacs-solo/git-cache-time 0)) 5))
          (progn
            (setq emacs-solo/git-cache-time now
                  emacs-solo/git-cache-dir root)
            (setq emacs-solo/git-cache
                  (when root
                    (let* ((out (shell-command-to-string "git status --porcelain=v2 --branch 2>/dev/null"))
                           (lines (split-string out "\n" t))
                           (ahead 0)
                           (behind 0)
                           (modified 0)
                           (untracked 0)
                           (conflicts 0)
                           (branch nil))

                      (dolist (l lines)
                        (cond
                         ((string-match "^#? *branch\\.head \\(.+\\)" l)
                          (setq branch (match-string 1 l)))
                         ((string-match "^#? *branch\\.ab \\+\\([0-9]+\\) -\\([0-9]+\\)" l)
                          (setq ahead (string-to-number (match-string 1 l)))
                          (setq behind (string-to-number (match-string 2 l))))
                         ((string-match "^1 " l) (cl-incf modified))
                         ((string-match "^\\?\\?" l) (cl-incf untracked))
                         ((string-match "^u " l) (cl-incf conflicts))))

                      (list :branch (or branch "HEAD")
                            :ahead ahead
                            :behind behind
                            :modified modified
                            :untracked untracked
                            :conflicts conflicts)))))
        (progn
          emacs-solo/git-cache))))


  (setopt eshell-prompt-function
          (lambda ()
            (if emacs-solo/eshell-full-prompt
                ;; Full-blown prompt
                (concat
                 (propertize
                  (assoc-default 'arrow-left emacs-solo/eshell-icons) 'face `(:foreground ,eshell-solo/color-bg-dark))

                 (propertize
                  (if (> eshell-last-command-status 0)
                      (concat " " (assoc-default 'failure emacs-solo/eshell-icons)  " ")
                    (concat " " (assoc-default 'success emacs-solo/eshell-icons)  " "))
                  'face `(:background ,eshell-solo/color-bg-dark))

                 (propertize (concat (number-to-string eshell-last-command-status) " ")
                             'face `(:background ,eshell-solo/color-bg-dark))

                 (propertize (assoc-default 'arrow-right emacs-solo/eshell-icons)
                             'face `(:foreground ,eshell-solo/color-bg-dark :background ,eshell-solo/color-bg-mid))

                 (propertize (let ((remote-user (file-remote-p default-directory 'user))
                                   (is-remote (file-remote-p default-directory)))
                               (concat
                                (if is-remote
                                    (concat (assoc-default 'user-remote emacs-solo/eshell-icons)  " ")
                                  (concat (assoc-default 'user-local emacs-solo/eshell-icons)  " "))
                                (or remote-user (user-login-name))
                                " "))
                             'face `(:foreground ,eshell-solo/color-fg-user
                                                 :background ,eshell-solo/color-bg-mid))

                 (propertize (assoc-default 'arrow-right emacs-solo/eshell-icons) 'face
                             `(:foreground ,eshell-solo/color-bg-mid :background ,eshell-solo/color-bg-dark))

                 (let ((remote-host (file-remote-p default-directory 'host))
                       (is-remote (file-remote-p default-directory)))
                   (propertize (concat (if is-remote
                                           (concat " " (assoc-default 'host-remote emacs-solo/eshell-icons)  " ")
                                         (concat " " (assoc-default 'host-local emacs-solo/eshell-icons)  " "))
                                       (or remote-host (system-name)) " ")
                               'face `(:background ,eshell-solo/color-bg-dark  :foreground ,eshell-solo/color-fg-host)))

                 (propertize (assoc-default 'arrow-right emacs-solo/eshell-icons) 'face
                             `(:foreground ,eshell-solo/color-bg-dark :background ,eshell-solo/color-bg-mid))

                 (propertize (concat " " (assoc-default 'time emacs-solo/eshell-icons)  " "
                                     (format-time-string "%H:%M:%S" (current-time)) " ")
                             'face `(:foreground ,eshell-solo/color-fg-user :background ,eshell-solo/color-bg-mid))

                 (propertize (assoc-default 'arrow-right emacs-solo/eshell-icons)
                             'face `(:foreground ,eshell-solo/color-bg-mid :background ,eshell-solo/color-bg-dark))

                 (propertize (concat " " (assoc-default 'folder emacs-solo/eshell-icons)  " "
                                     (if (>= (length (eshell/pwd)) 40)
                                         (concat "…" (car (last (butlast (split-string (eshell/pwd) "/") 0))))
                                       (abbreviate-file-name (eshell/pwd))) " ")
                             'face `(:background ,eshell-solo/color-bg-dark :foreground ,eshell-solo/color-fg-dir))

                 (propertize (concat (assoc-default 'arrow-right emacs-solo/eshell-icons) "\n")
                             'face `(:foreground ,eshell-solo/color-bg-dark))

                 (when-let* ((info (emacs-solo/git-info))
                             (branch (plist-get info :branch)))
                   (concat
                    (propertize (assoc-default 'arrow-left emacs-solo/eshell-icons)
                                'face `(:foreground ,eshell-solo/color-bg-dark))
                    (propertize
                     (concat
                      (concat " " (assoc-default 'branch emacs-solo/eshell-icons) " " branch " ")
                      (when emacs-solo/eshell-full-prompt-resource-intensive
                        (let ((ahead (plist-get info :ahead))
                              (behind (plist-get info :behind))
                              (modified (plist-get info :modified))
                              (untracked (plist-get info :untracked))
                              (conflicts (plist-get info :conflicts)))
                          (concat
                           (when (> ahead 0)
                             (format (concat " " (assoc-default 'git-ahead emacs-solo/eshell-icons) "%d") ahead))
                           (when (> behind 0)
                             (format (concat " " (assoc-default 'git-behind emacs-solo/eshell-icons) "%d") behind))
                           (when (and (> ahead 0) (> behind 0))
                             (concat " " (assoc-default 'git-merge emacs-solo/eshell-icons)))
                           (when (> modified 0)
                             (format (concat " " (assoc-default 'modified emacs-solo/eshell-icons) "%d") modified))
                           (when (> untracked 0)
                             (format (concat " " (assoc-default 'untracked emacs-solo/eshell-icons) "%d") untracked))
                           (when (> conflicts 0)
                             (format (concat " " (assoc-default 'conflict emacs-solo/eshell-icons) "%d") conflicts))
                           " "))))
                     'face `(:background ,eshell-solo/color-bg-dark :foreground ,eshell-solo/color-fg-git))
                    (propertize (concat (assoc-default 'arrow-right emacs-solo/eshell-icons) "\n")
                                'face `(:foreground ,eshell-solo/color-bg-dark))))

                 (propertize emacs-solo/eshell-lambda-symbol 'face font-lock-keyword-face))

              ;; Minimal prompt
              (propertize emacs-solo/eshell-lambda-symbol 'face font-lock-keyword-face))))


  (setq eshell-prompt-regexp emacs-solo/eshell-lambda-symbol)


  ;; SET TERM ENV SO MOST PROGRAMS WON'T COMPLAIN
  ;;
  (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))


  (setq eshell-visual-subcommands
        '(("podman" "run" "exec" "attach" "top" "logs" "stats" "compose")
          ("docker" "run" "exec" "attach" "top" "logs" "stats" "compose")
          ("jj" "resolve" "squash" "split")))

  (setq eshell-visual-commands
        '("vi" "screen" "top"  "htop" "btm" "less" "more" "lynx" "ncftp" "pine" "tin" "trn"
          "elm" "irssi" "nmtui-connect" "nethack" "vim" "alsamixer" "nvim" "w3m" "psql"
          "lazygit" "lazydocker" "ncmpcpp" "newsbeuter" "nethack" "mutt" "neomutt" "tmux"
          "jqp")))

(provide 'zt-eshell)
