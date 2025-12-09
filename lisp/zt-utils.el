(defvar ediff-do-hexl-diff nil
  "variable used to store trigger for doing diff in hexl-mode")
(defadvice ediff-files-internal (around ediff-files-internal-for-binary-files activate)
  "catch the condition when the binary files differ

the reason for catching the error out here (when re-thrown from the inner advice)
is to let the stack continue to unwind before we start the new diff
otherwise some code in the middle of the stack expects some output that
isn't there and triggers an error"
  (let ((file-A (ad-get-arg 0))
        (file-B (ad-get-arg 1))
        ediff-do-hexl-diff)
    (condition-case err
        (progn
          ad-do-it)
      (error
       (if ediff-do-hexl-diff 
           (let ((buf-A (find-file-noselect file-A))
                 (buf-B (find-file-noselect file-B)))
             (with-current-buffer buf-A
               (hexl-mode 1))
             (with-current-buffer buf-B
               (hexl-mode 1))
             (ediff-buffers buf-A buf-B))
         (error (error-message-string err)))))))

(defadvice ediff-setup-diff-regions (around ediff-setup-diff-regions-for-binary-files activate)
  "when binary files differ, set the variable "
  (condition-case err
      (progn
        ad-do-it)
    (error
     (setq ediff-do-hexl-diff
           (and (string-match-p "^Errors in diff output.  Diff output is in.*"
                                (error-message-string err))
                (string-match-p "^\\(Binary \\)?[fF]iles .* and .* differ"
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))
                (y-or-n-p "The binary files differ, look at the differences in hexl-mode? ")))
     (error (error-message-string err)))))

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)


(when (and (eq system-type 'gnu/linux)
           (with-temp-buffer
             (insert-file-contents "/proc/version")
             (goto-char (point-min))
             (re-search-forward "Microsoft" nil t)))
  (setq interprogram-cut-function
        (lambda (text &optional push)
          (let ((process-connection-type nil))
            (let ((proc (start-process "win32yank" "*Messages*" "win32yank.exe" "-i")))
              (process-send-string proc text)
              (process-send-eof proc)))))
  
  (setq interprogram-paste-function
        (lambda ()
          (shell-command-to-string "win32yank.exe -o"))))


(defgroup zt/web-search nil
  "Search the web for an editable query based on word/region at point."
  :group 'convenience)

(defcustom zt/web-search-engine "https://duckduckgo.com/?q=%s"
  "Search URL template. %s will be replaced with the URL-encoded query."
  :type 'string
  :group 'zt/web-search)

(defvar zt/web-search-history nil
  "Minibuffer history for `zt/web-search-editable'.")

(defun zt/web--initial-query ()
  "Return region text or word/symbol at point, or empty string."
  (cond
   ((use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end)))
   ((thing-at-point 'symbol t))
   ((thing-at-point 'word t))
   (t "")))

(defun zt/web-search-word (&optional choose-engine)
  "Prompt with the word/region at point, let you edit, then open a web search.
With CHOOSE-ENGINE (prefix arg), prompt for a search engine template first."
  (interactive "P")
  (when choose-engine
    (setq zt/web-search-engine
          (read-string "Search engine (use %s for query): "
                       zt/web-search-engine)))
  (let* ((init (string-trim (or (zt/web--initial-query) "")))
         (query (read-from-minibuffer "Search web for: " init nil nil
                                      'zt/web-search-history)))
    (unless (string-empty-p (string-trim query))
      (browse-url (format zt/web-search-engine
                          (url-hexify-string query))))))

;;; │ WEBJUMP
(use-package webjump
  :defer t
  :ensure nil
  :straight nil
  :bind ("C-x /" . webjump)
  :custom
  (webjump-sites
   '(("DuckDuckGo" . [simple-query "www.duckduckgo.com" "www.duckduckgo.com/?q=" ""])
     ("Google" . [simple-query "www.google.com" "www.google.com/search?q=" ""])
     ("YouTube" . [simple-query "www.youtube.com/feed/subscriptions" "www.youtube.com/results?search_query=" ""])
     ("ChatGPT" . [simple-query "https://chatgpt.com" "https://chatgpt.com/?q=" ""]))))

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


(provide 'zt-utils)


