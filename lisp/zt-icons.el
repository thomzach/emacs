(defcustom emacs-solo-enabled-icons
  '(dired eshell ibuffer)
  "List of Emacs Solo icon features that are enabled."
  :type '(set :tag "Enabled Emacs Solo icon features"
              (const :tag "Dired Icons" dired)
              (const :tag "Eshell Icons" eshell)
              (const :tag "Ibuffer Icons" ibuffer)
              (const :tag "Nerd Font Icons" nerd))
  :group 'emacs-solo)

;;; │ EMACS-SOLO-FILE-ICONS
;;
;;  Here we set the icons to be used by other `emacs-solo' features,
;;  like `emacs-solo-dired-icons' and `emacs-solo-eshell-icons'
(use-package emacs-solo-file-icons
  :if emacs-solo-enabled-icons
  :straight nil
  :ensure nil
  :no-require t
  :defer t
  :init
  (let ((emoji-icons
         '(("el" . "📜")       ("rb" . "💎")       ("js" . "⚙️")      ("ts" . "⚙️")
           ("json" . "🗂️")     ("md" . "📝")       ("txt" . "📝")     ("html" . "🌐")
           ("css" . "🎨")      ("scss" . "🎨")     ("png" . "🖼️")     ("jpg" . "🖼️")
           ("jpeg" . "🖼️")     ("gif" . "🖼️")      ("svg" . "🖼️")     ("pdf" . "📄")
           ("zip" . "📦")      ("tar" . "📦")      ("gz" . "📦")      ("bz2" . "📦")
           ("7z" . "📦")       ("org" . "🦄")      ("sh" . "💻")      ("c" . "🅲")
           ("h" . "📘")        ("cpp" . "🅲")      ("hpp" . "📘")     ("py" . "🐍")
           ("java" . "☕")    ("go" . "🌍")       ("rs" . "💨")      ("php" . "🐘")
           ("pl" . "🐍")       ("lua" . "🎮")      ("ps1" . "🔧")     ("exe" . "⚡")
           ("dll" . "🔌")      ("bat" . "⚡")     ("yaml" . "⚙️")    ("toml" . "⚙️")
           ("ini" . "⚙️")      ("csv" . "📊")      ("xls" . "📊")     ("xlsx" . "📊")
           ("sql" . "🗄️")      ("log" . "📝")      ("apk" . "📱")     ("dmg" . "💻")
           ("iso" . "💿")      ("torrent" . "🧲")  ("bak" . "🗃️")     ("tmp" . "⚠️")
           ("desktop" . "🖥️")  ("md5" . "🔐")      ("sha256" . "🔐")  ("pem" . "🔐")
           ("sqlite" . "🗄️")   ("db" . "🗄️")       ("gpg" . "🔐")     ("hash" . "#️⃣")
           ("mp3" . "🎶")      ("wav" . "🎶")      ("flac" . "🎶" )  ("mail" . "📧")
           ("ogg" . "🎶")      ("m4a" . "🎶")      ("mp4" . "🎬")     ("avi" . "🎬")
           ("mov" . "🎬")      ("mkv" . "🎬")      ("webm" . "🎬")    ("flv" . "🎬")
           ("ico" . "🖼️")      ("ttf" . "🔠")      ("otf" . "🔠")     ("eot" . "🔠")
           ("woff" . "🔠")     ("woff2" . "🔠")    ("epub" . "📚")    ("mobi" . "📚")
           ("azw3" . "📚")     ("fb2" . "📚")      ("chm" . "📚")     ("tex" . "📚")
           ("bib" . "📚")      ("apk" . "📱")      ("rar" . "📦")     ("xz" . "📦")
           ("zst" . "📦")      ("tar.xz" . "📦")   ("tar.zst" . "📦") ("tar.gz" . "📦")
           ("tgz" . "📦")      ("bz2" . "📦")      ("mpg" . "🎬")     ("webp" . "🖼️")
           ("flv" . "🎬")      ("3gp" . "🎬")      ("ogv" . "🎬")     ("srt" . "🔠")
           ("vtt" . "🔠")      ("cue" . "📀")      ("terminal" . "💻") ("info" . "ℹ️")
           ("direddir" . "📁") ("diredfile" . "📄") ("wranch" . "🔧") ("news" . "📰")))
        (nerd-icons
         '(("el" . "")       ("rb" . "")       ("js" . "")      ("ts" . "")
           ("json" . "")     ("md" . "")       ("txt" . "")     ("html" . "")
           ("css" . "")      ("scss" . "")     ("png" . "")     ("jpg" . "")
           ("jpeg" . "")     ("gif" . "")      ("svg" . "")     ("pdf" . "")
           ("zip" . "")      ("tar" . "")      ("gz" . "")      ("bz2" . "")
           ("7z" . "")       ("org" . "")      ("sh" . "")      ("c" . "")
           ("h" . "")        ("cpp" . "")      ("hpp" . "")     ("py" . "")
           ("java" . "")    ("go" . "")       ("rs" . "")      ("php" . "")
           ("pl" . "")       ("lua" . "")      ("ps1" . "")     ("exe" . "")
           ("dll" . "")      ("bat" . "")     ("yaml" . "")    ("toml" . "")
           ("ini" . "")      ("csv" . "")      ("xls" . "")     ("xlsx" . "")
           ("sql" . "")      ("log" . "")      ("apk" . "")     ("dmg" . "")
           ("iso" . "")      ("torrent" . "")  ("bak" . "")     ("tmp" . "")
           ("desktop" . "")  ("md5" . "")      ("sha256" . "")  ("pem" . "")
           ("sqlite" . "")   ("db" . "")       ("gpg" . "")     ("hash" . "")
           ("mp3" . "")      ("wav" . "")      ("flac" . "" )   ("mail" . "")
           ("ogg" . "")      ("m4a" . "")      ("mp4" . "")     ("avi" . "")
           ("mov" . "")      ("mkv" . "")      ("webm" . "")    ("flv" . "")
           ("ico" . "")      ("ttf" . "")      ("otf" . "")     ("eot" . "")
           ("woff" . "")     ("woff2" . "")    ("epub" . "")    ("mobi" . "")
           ("azw3" . "")     ("fb2" . "")      ("chm" . "")     ("tex" . "")
           ("bib" . "")      ("rar" . "")     ("xz" . "")
           ("zst" . "")      ("tar.xz" . "")   ("tar.zst" . "") ("tar.gz" . "")
           ("tgz" . "")      ("bz2" . "")      ("mpg" . "")     ("webp" . "")
           ("flv" . "")      ("3gp" . "")      ("ogv" . "")     ("srt" . "")
           ("vtt" . "")      ("cue" . "")      ("terminal" . "") ("info" . "ℹ")
           ("direddir" . "") ("diredfile" . "") ("wranch" . "") ("news" . ""))))

    (defvar emacs-solo/file-icons
      (cond
       ;; If nerd icons are enabled, use them.
       ((memq 'nerd emacs-solo-enabled-icons)
        nerd-icons)
       ;; If on kitty terminal AND not using nerd icons, use blank icons
       ;; to prevent emoji rendering issues.
       ((string= (getenv "TERM") "xterm-kitty")
        (mapcar (lambda (p) (cons (car p) "")) emoji-icons))
       ;; Otherwise, use the default emoji icons.
       (t
        emoji-icons))
      "Icons for specific file extensions in Dired and Eshell.")))


;;; │ EMACS-SOLO-DIRED-ICONS
;;
(use-package emacs-solo-dired-icons
  :if (memq 'dired emacs-solo-enabled-icons)
  :straight nil  
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-solo/dired-icons-icon-for-file (file)
    (if (file-directory-p file)
        (assoc-default "direddir" emacs-solo/file-icons)
      (let* ((ext (file-name-extension file))
             (icon (and ext (assoc-default (downcase ext) emacs-solo/file-icons))))
        (or icon (assoc-default "diredfile" emacs-solo/file-icons)))))

  (defun emacs-solo/dired-icons-icons-regexp ()
    "Return a regexp that matches any icon we use."
    (let ((icons (mapcar #'cdr emacs-solo/file-icons)))
      (concat "^\\(" (regexp-opt (cons "📁" icons)) "\\) ")))

  (defun emacs-solo/dired-icons-add-icons ()
    "Add icons and suffixes as overlays to filenames in Dired buffer."
    (when (derived-mode-p 'dired-mode)
      (let ((inhibit-read-only t)
            (icon-regex (emacs-solo/dired-icons-icons-regexp)))
        (remove-overlays (point-min) (point-max) 'emacs-solo-dired-icon-overlay t)

        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (condition-case nil
                (when-let* ((file (dired-get-filename nil t)))
                  (dired-move-to-filename)
                  (let* ((beg (point))
                         (end (line-end-position))
                         (icon (emacs-solo/dired-icons-icon-for-file file))
                         (suffix
                          (cond
                           ((file-directory-p file)
                            (propertize "/" 'face 'dired-directory))
                           ((file-executable-p file)
                            (propertize "*" 'face '(:foreground "#79a8ff")))
                           (t ""))))
                    ;; Add icon before filename
                    (let ((ov1 (make-overlay beg beg)))
                      (overlay-put ov1 'before-string (concat icon " "))
                      (overlay-put ov1 'emacs-solo-dired-icon-overlay t))
                    ;; Add styled suffix after filename
                    (let ((ov2 (make-overlay end end)))
                      (overlay-put ov2 'after-string suffix)
                      (overlay-put ov2 'emacs-solo-dired-icon-overlay t))))
              (error nil))
            (forward-line 1))))))

  (add-hook 'dired-after-readin-hook #'emacs-solo/dired-icons-add-icons))


;;; │ EMACS-SOLO-IBUFFER-ICONS
;;
(use-package emacs-solo-ibuffer-icons
  :if (memq 'ibuffer emacs-solo-enabled-icons)
  :ensure nil
  :straight nil  
  :no-require t
  :defer t
  :init
  (defun emacs-solo/ibuffer-icon-for-buffer (buf)
    "Return an icon for BUF: file-extension emoji if visiting a file,
otherwise mode-based emoji."
    (with-current-buffer buf
      (if-let* ((file (buffer-file-name)))
          ;; File-based icons
          (let* ((ext (file-name-extension file))
                 (icon (and ext (assoc-default (downcase ext) emacs-solo/file-icons))))
            (or icon (assoc-default "diredfile" emacs-solo/file-icons)))
        ;; Mode-based icons for non-file buffers
        (cond
         ((derived-mode-p 'dired-mode)  (assoc-default "direddir" emacs-solo/file-icons))
         ((derived-mode-p 'eshell-mode) (assoc-default "terminal" emacs-solo/file-icons))
         ((derived-mode-p 'org-mode)    (assoc-default "terminal" emacs-solo/file-icons))
         ((derived-mode-p 'shell-mode)  (assoc-default "terminal" emacs-solo/file-icons))
         ((derived-mode-p 'term-mode)   (assoc-default "terminal" emacs-solo/file-icons))
         ((derived-mode-p 'help-mode)   (assoc-default "info" emacs-solo/file-icons))
         ((derived-mode-p 'erc-mode)    (assoc-default "hash" emacs-solo/file-icons))
         ((derived-mode-p 'rcirc-mode)  (assoc-default "hash" emacs-solo/file-icons))
         ((derived-mode-p 'gnus-mode)   (assoc-default "mail" emacs-solo/file-icons))
         ((derived-mode-p 'newsticker-treeview-mode)   (assoc-default "news" emacs-solo/file-icons))
         (t                             (assoc-default "wranch" emacs-solo/file-icons))))))

  (define-ibuffer-column icon
    (:name " ")
    (emacs-solo/ibuffer-icon-for-buffer buffer))

  ;; Update ibuffer formats
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (icon 2 2 :left) " "
                (name 30 30 :left :elide) " "
                (size 9 -1 :right) " "
                (mode 16 16 :left :elide) " "
                filename-and-process))))


;;; │ EMACS-SOLO-ESHELL-ICONS
;;
;; Inspired by: https://www.reddit.com/r/emacs/comments/xboh0y/how_to_put_icons_into_eshell_ls/
(use-package emacs-solo-eshell-icons
  :if (memq 'eshell emacs-solo-enabled-icons)
  :ensure nil
  :straight nil  
  :no-require t
  :defer t
  :init
  (defun emacs-solo/eshell-icons (file)
    "Return a cons of propertized display string and file metadata.
FILE is a list (NAME IS-DIR EXECUTABLE ...), like from `eshell/ls`.
The full list is like:
(FILENAME IS-DIR SIZE OWNER GROUP MOD-TIME ACCESS-TIME CHANGE-TIME
SIZE-LONG PERMS HARDLINKS INODE DEVICE).
"
    (let* ((filename (car file))
           (is-dir (eq (cadr file) t))
           (perms (nth 9 file))
           (is-exec (and perms (string-match-p "x" perms)))
           (ext (and (not is-dir) (file-name-extension filename)))
           (icon (if is-dir
                     (cdr (assoc "direddir" emacs-solo/file-icons))
                   (or (cdr (assoc ext emacs-solo/file-icons))
                       (cdr (assoc "diredfile" emacs-solo/file-icons)))))
           (suffix (cond
                    (is-dir "/")
                    (is-exec "*")
                    (t "")))
           (display-text (propertize
                          (concat icon " " filename suffix)
                          'file-name filename
                          'mouse-face 'highlight
                          'help-echo (concat "Open " filename)
                          'keymap eshell-ls-file-keymap)))
      (cons display-text (cdr file))))


  (defvar eshell-ls-file-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "RET") #'eshell-ls-find-file)
      (define-key map (kbd "<return>") #'eshell-ls-find-file)
      (define-key map [mouse-1] #'eshell-ls-find-file)
      (define-key map (kbd "D") #'eshell-ls-delete-file)
      map)
    "Keymap active on Eshell file entries.")

  (defun eshell-ls-file-at-point ()
    "Get the full path of the Eshell listing at point."
    (get-text-property (point) 'file-name))

  (defun eshell-ls-find-file ()
    "Open the Eshell listing at point."
    (interactive)
    (find-file (eshell-ls-file-at-point)))

  (defun eshell-ls-delete-file ()
    "Delete the Eshell listing at point."
    (interactive)
    (let ((file (eshell-ls-file-at-point)))
      (when (yes-or-no-p (format "Delete file %s?" file))
        (delete-file file 'trash))))

  (advice-remove 'eshell-ls-decorated-name #'emacs-solo/eshell-icons)
  (advice-add #'eshell-ls-annotate :filter-return #'emacs-solo/eshell-icons))



(provide 'zt-icons)
