
(setq org-directory "~/org/")

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(eval-after-load "org"
  '(require 'ox-md nil t))


(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)))

(setq org-confirm-babel-evaluate nil)


(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; ;; Improve org mode looks
;; (setq-default org-startup-indented t
;;               org-pretty-entities t
;;               org-use-sub-superscripts "{}"
;;               org-hide-emphasis-markers t
;;               org-startup-with-inline-images t
;;               org-image-actual-width '(300))

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

(use-package org-modern
  :hook
  (org-mode . global-org-modern-mode)
  ;; :custom
  ;; (org-modern-keyword nil)
  ;; (org-modern-checkbox nil)
  ;; (org-modern-table nil)
  )

(use-package org-download)
(setq org-startup-with-inline-images t)

(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "DONE" )))
(when (eq system-type 'windows-nt)
  (setq org-capture-templates
	    '(("t" "Todo" entry (file+headline "H:/zthomas/private/org/GTD.org" "Tasks")
	       "* TODO %?\n  %i\n")
	      ("j" "Journal" entry (file+datetree "h:/zthomas/private/org/journal.org")
	       "* %?\nEntered on %U\n  %i\n  %a")))
  )
(when (eq system-type 'gnu/linux)
  (setq org-capture-templates
	    '(("t" "Todo" entry (file+headline "/mnt/nas/org/GTD.org" "Tasks")
	       "* TODO %?\n  %i\n")
	      ("j" "Journal" entry (file+datetree "/mnt/nas/org/journal.org")
	       "* %?\nEntered on %U\n  %i\n  %a")))
  )
(setq shr-max-image-proportion 0.8)
(setq org-agenda-span 21)

(setq org-deadline-warning-days 21)


(require 'ob-eshell)
(require 'ob-octave)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (octave . t)
   (latex . t)
   (plantuml . t)
   ))

(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


(provide 'zt-org)
