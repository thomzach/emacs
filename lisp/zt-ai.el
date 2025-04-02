
(use-package aider
  :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
  :config
  ;; For claude-3-5-sonnet
  (setq aider-args '("--model" "ollama_chat/deepseek-r1:7b"))
  (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434")
  ;; Or chatgpt model
  ;; (setq aider-args '("--model" "o3-mini"))
  ;; (setenv "OPENAI_API_KEY" <your-openai-api-key>)
  ;; Or use your personal config file
  ;; (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))
  ;; ;;
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu))


(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :bind (:map global-map
            ("C-c z C-y" . copilot-chat-yank)
            ("C-c z M-y" . copilot-chat-yank-pop)
            ("C-c z C-M-y" . (lambda () (interactive) (copilot-chat-yank-pop -1))))
  )

(provide 'zt-ai)
