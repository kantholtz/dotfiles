;;; ktz-init-programming.el --- Programming initialization.


(defun ktz--init-programming ()
  "Setup programming configuration"
  (ktz-log "prog" "initializing configuration")

  (use-package dap-mode)

  (use-package company
    :hook (prog-mode . company-mode))

  (use-package which-key
    :config (which-key-mode))

  ;; python (eglot/poetry/pyright)

  ;; will find out about poetry
  (use-package pet
    :hook (python-mode . pet-mode))

  ;; requires 'isort' to be installed as python module
  (use-package isortify
    :hook (python-mode . isortify-mode))

  ;; requires 'black' to be installed as python module
  (use-package blacken
    :hook (python-mode . blacken-mode))

  ;; enable lsp support for python
  (use-package eglot
    :hook (python-mode . eglot-ensure))

    ;; :bind (:map eglot-mode-map
    ;;             ("C-c r" . eglot-rename)
    ;;             ("C-c h" . eldoc)
    ;;             ("C-c f" . eglot-format)
    ;;             ("C-c F" . eglot-format-buffer))

  (use-package ein)
  (use-package numpydoc)

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

  ;; (use-package lsp-mode
  ;;   :config
  ;;   ;; following the performance tips of lsp-doctor
  ;;   (setq lsp-keymap-prefix "C-c l"
  ;;         lsp-headerline-breadcrumb-icons-enable nil
  ;;         read-process-output-max (* 1024 1024)
  ;;         gc-cons-threshold (* 1024 1024 100))  ;; ~10mb

  ;;   (defun ktz--lsp-mode-python-hook ()
  ;;     (if (and
  ;;            (boundp 'conda-env-current-name)
  ;;            (not (string= "base" conda-env-current-name)))
  ;;         (lsp)
  ;;       (message "activate a conda environment first!")))

  ;;   :hook
  ;;   (c-mode . lsp) ;; using `clangd`
  ;;   (python-mode . ktz--lsp-mode-python-hook)
  ;;   (lsp-mode . lsp-enable-which-key-integration)

  ;;   :commands lsp)

  ;; (use-package lsp-ui
  ;;   :commands lsp-ui-mode
  ;;   :config
  ;;   (setq lsp-ui-doc-show-with-cursor nil
  ;;         lsp-ui-doc-include-signature t
  ;;         lsp-ui-doc-border (face-foreground 'default)))

  ;; eglot
  

  (use-package rainbow-mode
    :config
    (setq rainbow-x-colors nil))  ;; do not color names such as "red"

  ) ;; /ktz--init-programming

(provide 'ktz-init-programming)
