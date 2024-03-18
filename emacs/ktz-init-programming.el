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
      ;; :hook
      ;; (conda-postactivate . lsp)
      ))

  ;; will find out about poetry
  (use-package pet
    :hook (python-mode . pet-mode))

  ;; IDE features

  (use-package isortify
    ;; requires 'isort' to be installed as python module
    :hook (python-mode . isortify-mode))

  (use-package apheleia
    :config
    (setf (alist-get 'isort apheleia-formatters)
          '("isort" "--stdout" "-"))
    (setf (alist-get 'python-mode apheleia-mode-alist)
          '(isort black)))

  ;; (use-package blacken
  ;;   ;; requires 'black' to be installed as python module
  ;;   :after python
  ;;   :hook (python-mode . blacken-mode))

  ;; enable lsp support for python
  (use-package eglot
    :hook (python-mode . eglot-ensure)
    :config
    (setq eldoc-idle-delay 0.3)
    (setq company-idle-delay 0.3)
    (setq flymake-no-changes-timeout 0.2)

    ;; TODO figure out how to create a nested keymap (?)
    ;; <C-c e r> <C-c e l> etc.
    :bind (:map eglot-mode-map
                ("C-c C-e r" . eglot-rename)))
    ;;             ("C-c h" . eldoc)
    ;;             ("C-c f" . eglot-format)
    ;;             ("C-c F" . eglot-format-buffer))

  (use-package breadcrumb
    :config (breadcrumb-mode))

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

  ;; misc ----------------------------------------

  (use-package rainbow-mode
    :config
    (setq rainbow-x-colors nil))  ;; do not color names such as "red"

  ) ;; /ktz--init-programming

(provide 'ktz-init-programming)
