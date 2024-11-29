(defun ktz--init-lex ()
  (when (and ktz-lex-languagetool-host ktz-lex-languagetool-port)
    (use-package langtool
      :config (setq
               langtool-http-server-host ktz-lex-languagetool-host
               langtool-http-server-port ktz-lex-languagetool-port)))

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

