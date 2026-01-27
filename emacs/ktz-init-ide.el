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
  (use-package pet
    :config
    (add-hook 'python-base-mode-hook 'pet-mode -10))

  (use-package eldoc-box)
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


  ;; enable lsp support
  (use-package eglot
    :hook (python-mode . eglot-ensure)

    :config
    ;; not required if `pet` works as intendet
    ;; (add-to-list 'eglot-server-programs
    ;;              '(python-mode . ("uv" "run" "pyright-langserver" "--stdio")))

    ;; (setq eldoc-idle-delay 0.3)
    ;; (setq company-idle-delay 0.3)
    ;; (setq flymake-no-changes-timeout 0.2)
    )

  (use-package breadcrumb)
  ;; :config
  ;; (breadcrumb-mode))

  ;; seems abandoned unfortunately
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

  ;; ktz-menu ------------------------------------


  (transient-define-prefix ktz-menu--ide ()
    "IDE-related Commands"
    ["IDE Features"
     ["eglot"
      ("efr" "find references" xref-find-references)
      ("efd" "find declaration" eglot-find-declaration)
      ("efi" "find implementation" eglot-find-implementation)
      ("eft" "find type definition" eglot-find-typeDefinition)
      ("err" "rename" eglot-rename)
      ("erf" "format" eglot-format)
      ("erF" "format buffer" eglot-format-buffer)
      ("ecs" "control: shutdown" eglot-shutdown)
      ("ecS" "control: shutdown all" eglot-shutdown-all)]
     ["(f)lymake"
      ("fd" "diagnostics (project)" flymake-show-project-diagnostics)
      ("fn" "next error" flymake-goto-next-error)]
     ["general"
      ("i" "imenu" imenu)]])

  ) ;; /ktz--init-ide


(defun ktz-init-ide ()
  "Initialize org related config manually"
  (interactive)
  (ktz--init-ide))


(provide 'ktz-init-ide)
