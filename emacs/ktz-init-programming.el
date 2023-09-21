;;; ktz-init-programming.el --- Programming initialization.


(defun ktz--init-programming ()
  "Setup programming configuration"
  (ktz-log "prog" "initializing configuration")

  ;; python ----------------------------------------

  (when ktz-conda-dir
    (ktz-log "prog" "configuring conda")
    (use-package conda
      :custom
      (conda-anaconda-home ktz-conda-dir)
      :init
      (conda-env-autoactivate-mode t)
      (message "conda-env-autoactivate-mode t")
      :config
      (conda-env-activate ktz-conda-env)
      :hook
      (conda-postactivate . lsp)
      ))

  (use-package python
    :config
    (defun ktz--python-hooks ()
      (lsp-format-buffer)
      (lsp-organize-imports))
    :hook (before-save . ktz--python-hooks))

  (use-package ein)
  (use-package numpydoc)

  ;; go ---------------------------------------------

  (use-package go-mode
    :config
    (defun ktz--go-mode-hook ()
      (when (eq major-mode 'go-mode)
        (lsp-format-buffer)
        (lsp-organize-imports)))

    :hook
    ((go-mode . lsp-deferred)
     (before-save . ktz--go-mode-hook)))

  ;; frontend ----------------------------------------

  (use-package emmet-mode)
  (use-package jinja2-mode
    :init (emmet-mode)
    :mode "\\.html\\'")


  ;; searches upwards from cwd for node_modules/.bin
  (use-package add-node-modules-path
    :hook (js-mode . add-node-modules-path))

  (use-package prettier-js
    :after (add-node-modules-path)
    :hook ((js-mode web-mode typescript-mode vue-mode) . prettier-js-mode))

  (use-package typescript-mode
    :config
    (setq js-indent-level 2)
    (setq typescript-indent-level 2))

  (add-hook 'after-init-hook #'global-prettier-mode)

  ;; lsp ---------------------------------------------

  (use-package lsp-mode
    :config
    ;; following the performance tips of lsp-doctor
    (setq lsp-keymap-prefix "C-c l"
          lsp-headerline-breadcrumb-icons-enable nil
          read-process-output-max (* 1024 1024)
          gc-cons-threshold (* 1024 1024 100))  ;; ~10mb

    (defun ktz--lsp-mode-python-hook ()
      (if (and
             (boundp 'conda-env-current-name)
             (not (string= "base" conda-env-current-name)))
          (lsp)
        (message "activate a conda environment first!")))

    :hook
    (c-mode . lsp) ;; using `clangd`
    (python-mode . ktz--lsp-mode-python-hook)
    (lsp-mode . lsp-enable-which-key-integration)

    :commands lsp)

  (use-package lsp-ui
    :commands lsp-ui-mode
    :config
    (setq lsp-ui-doc-show-with-cursor nil
          lsp-ui-doc-include-signature t
          lsp-ui-doc-border (face-foreground 'default)))

  (use-package dap-mode)

  (use-package which-key
    :config (which-key-mode))

  (use-package company-lsp
    :config (push 'company-lsp company-backends))

  (use-package rainbow-mode
    :config
    (setq rainbow-x-colors nil))  ;; do not color names such as "red"

  ) ;; /ktz--init-programming

(provide 'ktz-init-programming)
