(defun zt-select-window (window)
  (select-window window))

;; (setq display-buffer-alist
;;       '(

;;         ;; The added space is for didactic purposes

;;         ;; Each entry in this list has this anatomy:

;;         ;; ( BUFFER-MATCHING-RULE
;;         ;;   LIST-OF-DISPLAY-BUFFER-FUNCTIONS
;;         ;;   OPTIONAL-PARAMETERS)

;;         ;; Match a buffer whose name is "*Occur*".  We have to escape
;;         ;; the asterisks to match them literally and not as a special
;;         ;; regular expression character.
;;         ("\\*Occur\\*"
;;          ;; If a buffer with the matching major-mode exists in some
;;          ;; window, then use that one.  Otherwise, display the buffer
;;          ;; below the current window.
;;          (display-buffer-reuse-mode-window display-buffer-below-selected)
;;          ;; Then we have the parameters...
;;          (dedicated . t)
;;          (window-height . fit-window-to-buffer)
;;          (body-function . zt-select-window))
;;      ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\|Bookmark List\\|Occur\\|eldoc\\)\\*"
;;       (display-buffer-in-side-window)
;;       (window-height . 0.25)
;;       (side . bottom)
;;       (slot . 0))
;;      ("\\*\\([Hh]elp\\)\\*"
;;       (display-buffer-in-side-window)
;;       (window-width . 75)
;;       (side . right)
;;       (slot . 0))
;;      ("\\*\\(Ibuffer\\)\\*"
;;       (display-buffer-in-side-window)
;;       (window-width . 100)
;;       (side . right)
;;       (slot . 1))
;;      ("\\*\\(Flymake diagnostics\\|xref\\|Completions\\)"
;;       (display-buffer-in-side-window)
;;       (window-height . 0.25)
;;       (side . bottom)
;;       (slot . 1))
;;      ("\\*\\(grep\\|find\\)\\*"
;;       (display-buffer-in-side-window)
;;       (window-height . 0.25)
;;       (side . bottom)
;;       (slot . 2))
;;      ("\\*compilation\\*"
;;       (display-buffer-reuse-mode-window
;;        display-buffer-below-selected)
;;       (window-height . 0.25))
;;      ))

;; If you want `switch-to-buffer' and related to respect those rules
;; (I personally do not want this, because if I am switching to a
;; specific buffer in the current window, I probably have a good
;; reason for it):
;; (setq switch-to-buffer-obey-display-actions t)

;; If you are in a window that is dedicated to its buffer and try to
;; `switch-to-buffer' there, tell Emacs to pop a new window instead of
;; using the current one:
(setq switch-to-buffer-in-dedicated-window 'pop)

;; Other relevant variables which control when Emacs splits the frame
;; vertically or horizontally, with some sample values (do `M-x
;; describe-variable' and search for those variables to learn more
;; about them):
;; (setq split-height-threshold 80)
;; (setq split-width-threshold 125)

;; Evaluate these to get to the relevant entries in the manual (NOTE
;; that this is advanced stuff):
;; (info "(elisp) Displaying Buffers")
;; (info "(elisp) Buffer Display Action Functions")
;; (info "(elisp) Buffer Display Action Alists")
;; (info "(elisp) Window Parameters")

;;; │ WINDOW
(use-package window
  :ensure nil
  :straight nil
  :custom
  (display-buffer-alist
   '(("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\|Bookmark List\\|Occur\\|eldoc\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))
     ("\\*\\([Hh]elp\\)\\*"
      (display-buffer-in-side-window)
      (window-width . 75)
      (side . right)
      (slot . 0))
     ("\\*\\(Ibuffer\\)\\*"
      (display-buffer-in-side-window)
      (window-width . 100)
      (side . right)
      (slot . 1))
     ("\\*\\(Flymake diagnostics\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 2))
     ("\\*\\(grep\\|xref\\|find\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))
     ("\\*\\(M3U Playlist\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 3))
     ("\\*compilation\\*"
      (display-buffer-reuse-mode-window
       display-buffer-below-selected)
      (window-height . 0.5))
     )))



(provide 'zt-display-buffer-alist)
