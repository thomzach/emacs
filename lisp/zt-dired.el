(defcustom emacs-solo-enable-dired-gutter t
  "Enable `emacs-solo-enable-dired-gutter'."
  :type 'boolean
  :group 'emacs-solo)


;;; │ EMACS-SOLO-DIRED-GUTTER
;;
(use-package emacs-solo-dired-gutter
  :if emacs-solo-enable-dired-gutter
  :straight nil
  :ensure nil
  :no-require t
  :defer t
  :init
  (setq emacs-solo-dired-gutter-enabled t)

  (defvar emacs-solo/dired-git-status-overlays nil
    "List of active overlays in Dired for Git status.")

  (defun emacs-solo/dired--git-status-face (code)
    "Return a cons cell (STATUS . FACE) for a given Git porcelain CODE."
    (let* ((git-status-untracked "??")
           (git-status-modified " M")
           (git-status-modified-alt "M ")
           (git-status-deleted "D ")
           (git-status-added "A ")
           (git-status-renamed "R ")
           (git-status-copied "C ")
           (git-status-ignored "!!")
           (status (cond
                    ((string-match-p "\\?\\?" code) git-status-untracked)
                    ((string-match-p "^ M" code) git-status-modified)
                    ((string-match-p "^M " code) git-status-modified-alt)
                    ((string-match-p "^D" code) git-status-deleted)
                    ((string-match-p "^A" code) git-status-added)
                    ((string-match-p "^R" code) git-status-renamed)
                    ((string-match-p "^C" code) git-status-copied)
                    ((string-match-p "\\!\\!" code) git-status-ignored)
                    (t "  ")))
           (face (cond
                  ((string= status git-status-ignored) 'shadow)
                  ((string= status git-status-untracked) 'warning)
                  ((string= status git-status-modified) 'font-lock-function-name-face)
                  ((string= status git-status-modified-alt) 'font-lock-function-name-face)
                  ((string= status git-status-deleted) 'error)
                  ((string= status git-status-added) 'success)
                  (t 'font-lock-keyword-face))))
      (cons status face)))

  (defun emacs-solo/dired-git-status-overlay ()
    "Overlay Git status indicators on the first column in Dired."
    (interactive)
    (require 'vc-git)
    (let ((git-root (ignore-errors (vc-git-root default-directory))))
      (when (and git-root
                 (not (file-remote-p default-directory))
                 emacs-solo-dired-gutter-enabled)
        (setq git-root (expand-file-name git-root))
        (let* ((git-status (vc-git--run-command-string nil "status" "--porcelain" "--ignored" "--untracked-files=normal"))
               (status-map (make-hash-table :test 'equal)))
          (mapc #'delete-overlay emacs-solo/dired-git-status-overlays)
          (setq emacs-solo/dired-git-status-overlays nil)

          (dolist (line (split-string git-status "\n" t))
            (when (string-match "^\\(..\\) \\(.+\\)$" line)
              (let* ((code (match-string 1 line))
                     (file (match-string 2 line))
                     (fullpath (expand-file-name file git-root))
                     (status-face (emacs-solo/dired--git-status-face code)))
                (puthash fullpath status-face status-map))))

          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (let* ((file (ignore-errors (expand-file-name (dired-get-filename nil t)))))
                (when file
                  (setq file (if (file-directory-p file) (concat file "/") file))
                  (let* ((status-face (gethash file status-map (cons "  " 'font-lock-keyword-face)))
                         (status (car status-face))
                         (face (cdr status-face))
                         (status-str (propertize (format " %s " status) 'face face))
                         (ov (make-overlay (line-beginning-position) (1+ (line-beginning-position)))))
                    (overlay-put ov 'before-string status-str)
                    (push ov emacs-solo/dired-git-status-overlays))))
              (forward-line 1)))))))

  (add-hook 'dired-after-readin-hook #'emacs-solo/dired-git-status-overlay))

;;; │ DIRED TODO
(use-package dired
  :ensure nil
  :straight nil
  :bind
  ;; (("M-i" . emacs-solo/window-dired-vc-root-left))
  :custom
  (dired-dwim-target t)
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open")
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open")
     (".*" "xdg-open" "open")))
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-alh --group-directories-first")
  (dired-omit-files "^\\.")                                ; with dired-omit-mode (C-x M-o)
  (dired-hide-details-hide-absolute-location t)            ; EMACS-31
  (image-dired-dir (expand-file-name "cache/image-dired" user-emacs-directory))
  :init
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1))) ;; Turning this ON also sets the C-x M-o binding.

  (defun emacs-solo/dired-rsync-copy (dest)
    "Copy marked files in Dired to DEST using rsync async, with real-time processing of output."
    (interactive
     (list (expand-file-name (read-file-name "rsync to: "
                                             (dired-dwim-target-directory)))))
    (let* ((files (dired-get-marked-files nil current-prefix-arg))
           (dest-original dest)
           (dest-rsync
            (if (file-remote-p dest)
                (let* ((vec (tramp-dissect-file-name dest))
                       (user (tramp-file-name-user vec))
                       (host (tramp-file-name-host vec))
                       (path (tramp-file-name-localname vec)))
                  (concat (if user (concat user "@") "")
                          host
                          ":"
                          path))
              dest))
           (files-rsync
            (mapcar
             (lambda (f)
               (if (file-remote-p f)
                   (let ((vec (tramp-dissect-file-name f)))
                     (let ((user (tramp-file-name-user vec))
                           (host (tramp-file-name-host vec))
                           (path (tramp-file-name-localname vec)))
                       (concat (if user (concat user "@") "")
                               host
                               ":"
                               path)))
                 f))
             files))
           (command (append '("rsync" "-hPur") files-rsync (list dest-rsync)))
           (buffer (get-buffer-create "*rsync*")))

      (message "[rsync] original dest: %s" dest-original)
      (message "[rsync] converted dest: %s" dest-rsync)
      (message "[rsync] source files: %s" files-rsync)
      (message "[rsync] command: %s" (string-join command " "))

      (with-current-buffer buffer
        (erase-buffer)
        (insert "Running rsync...\n"))

      (defun rsync-process-filter (proc string)
        (with-current-buffer (process-buffer proc)
          (goto-char (point-max))
          (insert string)
          (goto-char (point-max))
          (while (re-search-backward "\r" nil t)
            (replace-match "\n" nil nil))))

      (make-process
       :name "dired-rsync"
       :buffer buffer
       :command command
       :filter #'rsync-process-filter
       :sentinel
       (lambda (_proc event)
         (when (string-match-p "finished" event)
           (with-current-buffer buffer
             (goto-char (point-max))
             (insert "\n* rsync done *\n"))
           (dired-revert)))
       :stderr buffer)

      (display-buffer buffer)
      (message "rsync started...")))


  (defun emacs-solo/window-dired-vc-root-left (&optional directory-path)
    "Creates *Dired-Side* like an IDE side explorer"
    (interactive)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)

    (let ((dir (if directory-path
                   (dired-noselect directory-path)
                 (if (eq (vc-root-dir) nil)
                     (dired-noselect default-directory)
                   (dired-noselect (vc-root-dir))))))

      (display-buffer-in-side-window
       dir `((side . left)
             (slot . 0)
             (window-width . 30)
             (window-parameters . ((no-other-window . t)
                                   (no-delete-other-windows . t)
                                   (mode-line-format . (" "
                                                        "%b"))))))
      (with-current-buffer dir
        (let ((window (get-buffer-window dir)))
          (when window
            (select-window window)
            (rename-buffer "*Dired-Side*")
            )))))

  (defun emacs-solo/window-dired-open-directory ()
    "Open the current directory in *Dired-Side* side window."
    (interactive)
    (emacs-solo/window-dired-vc-root-left (dired-get-file-for-visit)))

  (defun emacs-solo/window-dired-open-directory-back ()
    "Open the parent directory in *Dired-Side* side window and refresh it."
    (interactive)
    (emacs-solo/window-dired-vc-root-left "../")
    (when (get-buffer "*Dired-Side*")
      (with-current-buffer "*Dired-Side*"
        (revert-buffer t t))))

  (defun emacs-solo/dired-run-async-on-marked-files (command)
    "Run COMMAND asynchronously on marked files in Dired.
Ex: mpv file1 file2 file3 file4..."
    (interactive "sCommand: ")
    (let ((files (dired-get-marked-files)))
      (start-process-shell-command command nil (format "%s %s" command (mapconcat 'shell-quote-argument files " ")))))


  (eval-after-load 'dired
    '(progn
       ;; Users should navigate with p/n, enter new directories with =, go back with q,
       ;; quit with several q's, only use - to access stuff up on the tree from inicial
       ;; directory.
       (define-key dired-mode-map (kbd "=") 'emacs-solo/window-dired-open-directory)
       (define-key dired-mode-map (kbd "-") 'emacs-solo/window-dired-open-directory-back)
       (define-key dired-mode-map (kbd "#") 'emacs-solo/dired-run-async-on-marked-files)

       ;; A better "BACK" keybiding
       (define-key dired-mode-map (kbd "b") 'dired-up-directory))))


;;; │ WDIRED
(use-package wdired
  :ensure nil
  :commands (wdired-change-to-wdired-mode)
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))


(provide 'zt-dired)
