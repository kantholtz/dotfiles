;;; ktz-init-ide.el --- Programming initialization.

(defun ktz--init-ide ()
  "Setup programming configuration"
  (ktz-log "prog" "initializing configuration")

  (use-package which-key
    :config (which-key-mode))

  ;; python
  ;;   stack: pyenv → poetry → pyright → eglot

  (use-package pyenv-mode)
  (use-package poetry)

  ;; will find out about poetry
  ;; (use-package pet
  ;;   :config
  ;;   (add-hook 'python-base-mode-hook 'pet-mode -10))

  ;; IDE features

  ;; (use-package isortify
  ;;   ;; requires 'isort' to be installed as python module
  ;;   :hook (python-mode . isortify-mode))

  ;; provides autoformatting
  (use-package apheleia
    :config
    (setf (alist-get 'isort apheleia-formatters)
          '("isort" "--stdout" "-"))
    (setf (alist-get 'python-mode apheleia-mode-alist)
          '(isort black))

    (apheleia-global-mode +1))

  ;; boost performance
  ;; (straight-use-package
  ;;  '(eglot-booster :type git :host github :repo "https://github.com/jdtsmith/eglot-booster")
  ;;  :after eglot
  ;;  :config (eglot-booster-mode))

  ;; enable lsp support
  (use-package eglot
    :hook (python-mode . eglot-ensure)

    :config
    ;; (setq eldoc-idle-delay 0.3)
    ;; (setq company-idle-delay 0.3)
    ;; (setq flymake-no-changes-timeout 0.2)

    ;; TODO this nesting does not work if god-mode is active

    (defvar-keymap ktz-eglot-find-map
      :doc "Traversal and search"
      "r" #'xref-find-references
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

  (use-package breadcrumb)
  ;; :config
  ;; (breadcrumb-mode))

  (use-package ein
    :config (setq ein:output-area-inlined-images t))

  (use-package numpydoc)

  ;; frontend ----------------------------------------

  (use-package emmet-mode)
  (use-package jinja2-mode
    :init (emmet-mode)
    :mode "\\.html\\'")

  (when (executable-find "nvm")
    (use-package nvm
      :straight (:host github :repo "rejeep/nvm.el")
      :config (nvm-use "22")))

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
  ) ;; /ktz--init-ide


(defun ktz-init-ide ()
  "Initialize org related config manually"
  (interactive)
  (ktz--init-ide))


(provide 'ktz-init-ide)
