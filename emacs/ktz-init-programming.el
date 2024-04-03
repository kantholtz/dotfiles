;;; ktz-init-programming.el --- Programming initialization.

(defun ktz--init-programming ()
  "Setup programming configuration"
  (ktz-log "prog" "initializing configuration")

  (use-package dap-mode)

  (use-package company
    :hook (prog-mode . company-mode))

  (use-package which-key
    :config (which-key-mode))

  ;; python
  ;;   stack: conda → poetry → pyright → eglot

  (when ktz-conda-dir
    (use-package conda
      :custom (conda-anaconda-home ktz-conda-dir)
      :init (conda-env-autoactivate-mode t)
      :config (conda-env-activate ktz-conda-env)))

  ;; will find out about poetry
  (use-package pet
    :config
    (add-hook 'python-base-mode-hook 'pet-mode -10))

  ;; IDE features

  (use-package isortify
    ;; requires 'isort' to be installed as python module
    :hook (python-mode . isortify-mode))

  (use-package apheleia
    :config
    (setf (alist-get 'isort apheleia-formatters)
          '("isort" "--stdout" "-"))
    (setf (alist-get 'python-mode apheleia-mode-alist)
          '(isort black))

    (apheleia-global-mode +1))

  ;; enable lsp support
  (use-package eglot
    :hook (python-mode . eglot-ensure)

    :config
    (setq eldoc-idle-delay 0.3)
    (setq company-idle-delay 0.3)
    (setq flymake-no-changes-timeout 0.2)

    ;; TODO this nesting does not work if god-mode is active

    (defvar-keymap ktz-eglot-find-map
      :doc "Traversal and search"
      "d" #'eglot-find-declaration
      "i" #'eglot-find-implementation
      "t" #'eglot-find-typeDefinition)

    (defvar-keymap ktz-eglot-refactor-map
      :doc "Refactor code"
      "r" #'eglot-rename
      "f" #'eglot-format
      "F" #'eglot-format-buffer)

    (defvar-keymap ktz-eglot-control-map
      :doc "Control commands"
      "s" #'eglot-shutdown
      "S" #'eglot-shutdown-all)

    (defvar-keymap ktz-eglot-map
      :doc "Eglot keybindings"
      "f" `("find" . ,ktz-eglot-find-map)
      "r" `("refactor" . ,ktz-eglot-refactor-map)
      "c" `("control" . ,ktz-eglot-control-map))

    :bind-keymap ("C-c e" . ktz-eglot-map))

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
