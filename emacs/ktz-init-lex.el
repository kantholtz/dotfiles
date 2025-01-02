(defun ktz--init-lex ()
  (use-package lsp-ltex
    :config
    (defun ktz--lex-ltex-hook ()
      (require 'lsp-ltex)
      (lsp))  ; or lsp-deferred

    :hook
    (org-mode . ktz--lex-ltex-hook)
    (markdown-mode . ktz--lex-ltex-hook)
    :init (setq lsp-ltex-version "16.0.0")) ;; still required?

  (use-package gptel
    :custom
    (gptel-api-key ktz-lex-openai-api-key)
    (gptel-model 'gpt-4o-mini)
    :config
    (global-set-key (kbd "C-c q") 'gptel-send)))


(defun ktz-init-lex ()
  "Initialize prose related config manually"
  (interactive)
  (ktz--init-lex))

(provide 'ktz-init-lex)

