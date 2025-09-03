;;; early-init.el --- Emacs-Solo (no external packages) Configuration  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Early init configuration for Emacs-Solo
;;
;;; Code:

;; Startup hacks
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      vc-handled-backends '(Git))

;; Hack to avoid being flashbanged
(defun emacs-solo/avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs."
  (setq mode-line-format nil)
  ;; These colors should match your selected theme for maximum effect
  ;; Note that for catppuccin whenever we create a new frame or open it on terminal
  ;; it is necessary to reload the theme.
  (set-face-attribute 'default nil :background "#292D3E" :foreground "#292D3E")
  )

;; (emacs-solo/avoid-initial-flash-of-light)

;; Better Window Management handling
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("Emacs"))

(setq inhibit-compacting-font-caches t)

;; Disables unused UI Elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(setq native-comp-speed 3)
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq native-comp-async-report-warnings-errors nil)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar
(global-auto-revert-mode 1)






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
(straight-use-package 'org)
(setq straight-use-package-by-default t)


(use-package project
  :straight (:type built-in))

(use-package eglot
  :straight (:type built-in))


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


(provide 'early-init)
;;; early-init.el ends here
