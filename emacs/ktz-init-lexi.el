(defun ktz--init-lexi ()
  (when (and ktz-languagetool-host ktz-languagetool-port)
    (use-package langtool
      :config (setq
               langtool-http-server-host ktz-languagetool-host
               langtool-http-server-port ktz-languagetool-port)))

  (use-package gptel
    :custom
    (gptel-api-key ktz-openai-api-key)
    (gptel-model "gpt-4o-mini")
    :config
    (global-set-key (kbd "C-c q") 'gptel-send)))

(defun ktz-init-lexi ()
  "Initialize prose related config manually"
  (interactive)
  (ktz--init-lexi))

(provide 'ktz-init-lexi)

