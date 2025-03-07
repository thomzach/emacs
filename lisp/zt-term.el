(defun zt-serial-term ()
  "Prompt for a serial device and open a terminal with 115200 baud rate.
If the terminal buffer already exists, switch to it instead of creating a new one."
  (interactive)
  (let* ((default-device "/dev/ttyACM0")
         (serial-device (read-string (format "Enter serial device (%s): " default-device)
                                     nil nil default-device))
         (buffer-name (format "%s" serial-device)))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (serial-term serial-device 115200)
      (rename-buffer buffer-name t))))

(use-package vterm)
(use-package multi-vterm)

(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

(eat-eshell-mode)
(setq eshell-visual-commands '())

(provide 'zt-term)
