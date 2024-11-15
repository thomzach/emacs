;;; boon-colemak.el --- An Ergonomic Command Mode  -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:
(use-package boon)
;; (require 'boon)

(define-key boon-select-map "j"  'boon-select-outside-quotes)
(define-key boon-select-map "l"  'boon-select-word)
(define-key boon-select-map "u"  'boon-select-word) ;; 'rf' is easier to type than 'rw'
(define-key boon-select-map "y"  'boon-select-paragraph)
(define-key boon-select-map ";"  'boon-select-org-table-cell)

(define-key boon-select-map "b"  'boon-select-borders) ;; Around
(define-key boon-select-map "n"  'boon-select-justline) ;; Row
(define-key boon-select-map "e"  '("symbol" . boon-select-wim)) ;; symbol
(define-key boon-select-map "E"  'boon-select-sentence)
(define-key boon-select-map "i"  'boon-select-with-spaces)
(define-key boon-select-map "v"  'boon-select-org-tree)
(define-key boon-select-map "d"  'boon-select-document)

(define-key boon-select-map "k"  'boon-select-content) ;; inZide
(define-key boon-select-map "h"  'boon-select-outside-pairs) ;; eXpression
(define-key boon-select-map "C"  'boon-select-comment)
(define-key boon-select-map ","  'boon-select-inside-pairs) ;; Contents
(define-key boon-select-map "/"  'boon-select-block)

(define-key boon-select-map "d"  '("blanks" . boon-select-blanks))

(define-key boon-command-map "k" '("bacK to marK" . boon-switch-mark)) ; bacK to marK
(define-key boon-command-map "K" 'xref-pop-marker-stack)

(define-key boon-command-map "j"  '("jump" . xref-find-definitions))
(define-key boon-command-map "J"  'xref-find-references)
(define-key boon-command-map "u"  'boon-forward-search-map)
(define-key boon-command-map "y"  'boon-backward-search-map)
(define-key boon-command-map "U"  'boon-qsearch-next)
(define-key boon-command-map "Y"  'boon-qsearch-previous)
(define-key boon-command-map "l"  'occur)
(define-key boon-command-map "L"  'kmacro-end-or-call-macro)
(define-key boon-command-map ";"  'boon-quote-character)
(define-key boon-command-map "n"  'boon-take-region)
(define-key boon-command-map "N"  'boon-treasure-region)
(define-key boon-command-map "e"  'boon-splice)
(define-key boon-command-map "E"  'yank-pop)
(define-key boon-command-map "o"  'boon-enclose)

(define-key boon-command-map "i"  'boon-substitute-region)
(define-key boon-command-map "I"  'kmacro-start-macro)
(define-key boon-command-map ","  '("?" . boon-beginning-of-expression))
(define-key boon-command-map "."  '("?" . boon-end-of-expression))

(define-key boon-command-map "<"  'beginning-of-buffer)
(define-key boon-command-map ">"  'end-of-buffer)
(define-key boon-command-map "h"  'boon-set-insert-like-state)
(define-key boon-command-map "?"  'boon-qsearch-previous-at-point)
(define-key boon-command-map "/"  'boon-qsearch-next-at-point)
(define-key boon-command-map "m"  '("hop" . avy-goto-word-1))
(define-key boon-command-map "M"  'avy-goto-char)

;; Special keys

;; LEFT HAND

;; Top row
(define-key boon-moves-map "q" 'beginning-of-line)
(define-key boon-moves-map "w" 'previous-line)
(define-key boon-moves-map "f" 'next-line)
(define-key boon-moves-map "p" 'end-of-line)

(define-key boon-moves-map "W"  'backward-paragraph)
(define-key boon-moves-map "F"  'forward-paragraph)

;; (define-key boon-moves-map "P" 'kmacro-end-or-call-macro) ; Play

;; home row
(define-key boon-moves-map "a" 'boon-smarter-backward)

(define-key boon-moves-map "r" 'backward-char)
(define-key boon-moves-map "s" 'forward-char)

(define-key boon-moves-map "R" 'boon-smarter-upward)
(define-key boon-moves-map "S" 'boon-smarter-downward)

(define-key boon-moves-map "t" 'boon-smarter-forward)

(define-key boon-moves-map "g" '("goto" . boon-goto-map))

;; d
;; (define-key boon-command-map "d" 'boon-set-insert-like-state)
;; (define-key boon-command-map "i" 'boon-set-insert-like-state)

;; Bottom row
;; z
(define-key boon-command-map "z" '("repeat" . boon-repeat-command))
;; x
(define-key boon-command-map "x" 'boon-x-map)
(define-key boon-command-map "X" 'boon-hl-regexp)
;; c
(define-key boon-command-map "c" 'boon-c-god)
(define-key boon-command-map "C"  'boon-exchange)
;; v
(define-key boon-command-map (kbd "C-v") 'boon-open-line-and-insert)
;; (define-key boon-command-map "V" 'boon-open-next-line-and-insert)
(define-key boon-command-map "v" 'boon-replace-by-character)

;; b
(define-key boon-command-map "B" 'boon-copy-to-register)
(define-key boon-command-map "b" '("bank" . insert-register))

;; RIGHT HAND: movement and marking commands.

;; Most of the moves are in boon-moves-map. Yet some moves do not work
;; as selectors, so they are put in the boon-command-map instead.
(define-key boon-command-map (kbd "C-u") 'scroll-down-line)
(define-key boon-command-map (kbd "C-y") 'scroll-up-line)

(define-key indent-rigidly-map "r" 'indent-rigidly-right)
(define-key indent-rigidly-map "s" 'indent-rigidly-left)

(provide 'zt-boon)
;;; boon-colemak.el ends here

(boon-mode)
