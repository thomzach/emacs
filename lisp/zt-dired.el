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

(provide 'zt-dired)
