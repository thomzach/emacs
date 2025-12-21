
(use-package c-ts-mode
  :ensure nil
  :straight nil
  :mode "\\.c\\'"
  :config
  (add-to-list 'treesit-language-source-alist '(c "https://github.com/tree-sitter/tree-sitter-c.git" "master" "src")))


(provide 'zt-programming)
