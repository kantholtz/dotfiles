;;; ktz-init-programming.el --- Programming initialization.


;; (defun ktz--init-programming-python-hook ()
;;   (lsp)
;;   (add-hook 'before-save-hook #'lsp-format-buffer nil t))


(defun ktz--init-programming ()
  "Setup programming configuration"
  (message "[ktz] initializing programming configuration")

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

  (use-package python
    :hook
    ((python-mode . lsp-deferred)
     (before-save . lsp-format-buffer)))

  (use-package ein :straight t)
  (use-package numpydoc :straight t)

  ;; GO
  (use-package go-mode
    :straight t
    :hook
    ((go-mode . lsp-deferred)
     (before-save . lsp-format-buffer)
     (before-save . lsp-organize-imports)))

  ;; LSP
  ;; how to turn off specific features:
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (use-package lsp-mode
    :straight t
    :commands (lsp lsp-deferred)

    :init
    (setq lsp-keymap-prefix "C-c l")

    ;; :hook
    ;; ((python-mode . ktz--init-programming-python-hook))

    :config
    (message "KTZ: executing lsp-mode:config")

    ;; TODO this just does not work at all
    ;; (at least for multi-hop connections)
    ;; >> Command "pylsp" is not present on the path. <<
    ;; it does not give a shit even when $PATH,
    ;; exec-path and tramp-remote-path are adjusted
    ;; -> falling back to sshfs for now

    ;; (as per yyoncho: put in :config)
    ;; https://github.com/emacs-lsp/lsp-mode/issues/1436
    ;; https://emacs-lsp.github.io/lsp-mode/page/remote/#sample-configuration
    ;; (when ktz-conda-paths
    ;;   (dolist (conda-path ktz-conda-paths)
    ;;     (let ((path (concat conda-path "/bin")))
    ;;       (add-to-list 'tramp-remote-path path)
    ;;       (add-to-list 'exec-path path)
    ;;     )))

    ;; (lsp-register-client
    ;;  (make-lsp-client
    ;;   :new-connection (lsp-tramp-connection "pylsp")
    ;;   :major-modes '(python-mode)
    ;;   :remote? t
    ;;   :server-id 'pyls-remote))

    ;; custom settings
    (lsp-register-custom-settings
     '(("pyls.plugins.flake8.enabled" t t)
       ("pyls.plugins.pyls_black.enabled" t t)))
    (lsp-enable-which-key-integration t)
    (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)

    ;; see below @ use-package flycheck
    ;; (lsp-pylsp)
    )

  (use-package flycheck
    :straight t)

  (use-package helm-lsp
    :straight t
    :commands helm-lsp-workspace-symbol)

  (use-package company
    :straight t
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
