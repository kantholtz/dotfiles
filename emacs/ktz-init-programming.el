;;; ktz-init-programming.el --- Programming initialization.


(defun ktz--init-programming-python-hook ()
  (lsp)
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))


(defun ktz--init-programming ()
  "Setup programming configuration"


  ;; PYTHON

  ;; set up conda
  (when ktz-conda-dir
    (use-package conda
      :straight t
      :custom
      (conda-anaconda-home ktz-conda-dir)
      :config
      (conda-env-initialize-interactive-shells)
      (conda-env-initialize-eshell)
      (conda-env-activate ktz-conda-env)))

  (use-package ein :straight t)
  (use-package numpydoc :straight t)

  ;; LSP
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/

  (use-package flycheck :straight t)

  (use-package helm-lsp
    :straight t
    :commands helm-lsp-workspace-symbol)

  ;; set up lsp
  (use-package lsp-mode
    :straight t
    :commands (lsp lsp-deferred)

    :init
    (setq lsp-keymap-prefix "C-c l")

    :hook
    ((python-mode . ktz--init-programming-python-hook))

    :config
    (lsp-register-custom-settings
     '(("pyls.plugins.flake8.enabled" t t)
       ("pyls.plugins.pyls_black.enabled" t t)))
    (lsp-enable-which-key-integration t)
    (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
    ;; see below @ use-package flycheck
    (lsp-pylsp))

  (use-package company
    :straight t
    :after lsp-mode
    :hook (lsp-mode . company-mode))

  (use-package company-box
    :straight t
    :hook (company-mode . company-box-mode))

  ;; FRONTEND

  ;; https://azzamsa.com/n/vue-emacs/
  (use-package vue-mode :straight t)

  (use-package prettier-js
    :straight t
    :hook (js-mode web-mode typescript-mode vue-mode))

  (use-package typescript-mode
    :straight t
    :config
    (setq js-indent-level 2)
    (setq typescript-indent-level 2))

  (add-hook 'after-init-hook #'global-prettier-mode))


(provide 'ktz-init-programming)
