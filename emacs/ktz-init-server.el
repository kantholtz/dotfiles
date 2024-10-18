(defun ktz--init-server ()
  (use-package yaml-mode)
  (use-package fish-mode)
  (use-package nginx-mode)
  (use-package apache-mode)
  (use-package markdown-toc)
  (use-package dockerfile-mode)
  (use-package markdown-mode
    :hook (markdown-mode . visual-line-mode)))

(defun ktz-init-server ()
  "Initialize org related config manually"
  (interactive)
  (ktz--init-server))

(provide 'ktz-init-server)
