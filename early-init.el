;;; early-init.el --- Emacs-Solo (no external packages) Configuration  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Early init configuration for Emacs-Solo
;;
;;; Code:

(setq package-enable-at-startup nil)

(defcustom emacs-solo-avoid-flash-options
  '((enabled . t)
    (background . "#1e1e2e") ;; Catppuccin "#1e1e2e" or Crafters "#292D3E"
    (foreground . "#1e1e2e")
    (reset-background . "#1e1e2e")
    (reset-foreground . "#cdd6f4")) ;; Catppuccin "#cdd6f4" or Crafters "#EEFFFF"
  "Options to avoid flash of light on Emacs startup.
- `enabled`: Whether to apply the workaround.
- `background`, `foreground`: Initial colors to use.
- `reset-background`, `reset-foreground`: Optional explicit colors to restore after startup.

NOTE: The default values here presented are set for the default
`emacs-solo' custom theme.  If you'd like to turn this ON with another
theme, change the background/foreground variables.

If reset values are nil, nothing is reset."
  :type '(alist :key-type symbol :value-type (choice (const nil) string))
  :group 'emacs-solo)

;; HACK: avoid being flashbanged
(defun emacs-solo/avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs, based on `emacs-solo-avoid-flash-options`."
  (when (alist-get 'enabled emacs-solo-avoid-flash-options)
    (setq mode-line-format nil)
    (set-face-attribute 'default nil
                        :background (alist-get 'background emacs-solo-avoid-flash-options)
                        :foreground (alist-get 'foreground emacs-solo-avoid-flash-options))))

(defun emacs-solo/reset-default-colors ()
  "Reset any explicitly defined reset values in `emacs-solo-avoid-flash-options`."
  (when (alist-get 'enabled emacs-solo-avoid-flash-options)
    (let ((bg (alist-get 'reset-background emacs-solo-avoid-flash-options))
          (fg (alist-get 'reset-foreground emacs-solo-avoid-flash-options)))
      (when bg
        (set-face-attribute 'default nil :background bg))
      (when fg
        (set-face-attribute 'default nil :foreground fg)))))

(emacs-solo/avoid-initial-flash-of-light)
(add-hook 'after-init-hook #'emacs-solo/reset-default-colors)

;; ;; Always start Emacs and new frames maximized
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Single VC backend inscreases booting speed
(setq vc-handled-backends '(Git))

;;; -------------------- PERFORMANCE & HACKS
;; HACK: inscrease startup speed

;; Delay garbage collection while Emacs is booting
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Schedule garbage collection sensible defaults for after booting
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.1)))

;; Single VC backend inscreases booting speed
(setq vc-handled-backends '(Git))

;; Do not native compile if on battery power
(setopt native-comp-async-on-battery-power nil) ; EMACS-31


;; Better Window Management handling
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("Emacs"))

(setq inhibit-compacting-font-caches t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(setq native-comp-speed 3)
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq native-comp-async-report-warnings-errors nil)


(setq inhibit-compacting-font-caches t)

;; Disables unused UI Elements
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode -1))

;; Avoid raising the *Messages* buffer if anything is still without
;; lexical bindings
(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))

;;;;; PACKAGE 

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)
;; (straight-use-package 'org)
(setq straight-use-package-by-default t)


(use-package project
  :straight (:type built-in))

(use-package eglot
  :straight (:type built-in))

(setq use-package-always-defer t)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         '("nongnu" . "https://elpa.nongnu.org/nongnu/")
			             ("melpa-stable" . "https://stable.melpa.org/packages/")
			             ))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; (defun my/disable-gui-things (frame)
;;   (when (display-graphic-p frame)
;;     (with-selected-frame frame
;;       (scroll-bar-mode -1)
;;       (tool-bar-mode -1)
;;       (menu-bar-mode -1))))

;; (add-hook 'after-make-frame-functions #'my/disable-gui-things)

(provide 'early-init)
;;; early-init.el ends here
