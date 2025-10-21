(defun ktz--init-lex ()
  ;; (when ktz-lex-ltex-ls-path
  ;;   (use-package eglot-ltex
  ;;     :config
  ;;     (defun ktz--lex-ltex-hook ()
  ;;       (require 'eglot-ltex)
  ;;       (eglot-ensure))

  ;;     :hook (text-mode . ktz--lex-ltex-hook)
  ;;     :init (setq
  ;;            eglot-ltex-server-path ktz-lex-ltex-ls-path
  ;;            eglot-ltex-communication-channel 'stdio)))

  (use-package gptel
    :config
    (when ktz-lex-openai-api-key
      (setq gptel-api-key ktz-lex-openai-api-key)
      (setq gptel-model 'gpt-4.1))
    (global-set-key (kbd "C-c q") 'gptel-send)))


(defun ktz-init-lex ()
  "Initialize prose related config manually"
  (interactive)
  (ktz--init-lex))

(provide 'ktz-init-lex)

