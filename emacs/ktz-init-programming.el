;;; ktz-init-programming.el --- Programming initialization.


(defun ktz--init-programming ()
  "Setup programming configuration"
  (ktz-log "prog" "initializing configuration")

  (use-package dap-mode
    :init
    ;; to use with c-mode lsp: run M-x dap-cpptools-setup
    ;; (gdb must be in the $PATH)
    (require 'dap-cpptools))

  ;; PYTHON --------------------

  ;; when using sshfs for a remote machine:
  ;;   - set up dedicated local conda environment
  ;;   - install all dependencies from mounted drive
  ;;   - activate env (conda-env-activate)
  ;;   - open project using lsp

  ;; set up conda
  (when ktz-conda-dir
    (ktz-log "prog" "configuring conda")
    (use-package conda
      :custom
      (conda-anaconda-home ktz-conda-dir)
      :config
      (conda-env-initialize-interactive-shells)
      (conda-env-initialize-eshell)
      (conda-env-activate ktz-conda-env)
      (conda-env-autoactivate-mode)))

  (use-package python
    :hook
    (before-save . lsp-format-buffer))

  ;;   ((python-mode
  ;;     . lsp-deferred)
  ;;    ;; (before-save
  ;;    ;;  . lsp-format-buffer)
  ;;    ;; (lsp-after-initialize
  ;;    ;;  . (lambda ()
  ;;    ;;      (flycheck-select-checker 'python-flake8)))
  ;;    ))

  (use-package ein)
  (use-package numpydoc)

  ;; GO --------------------

  (use-package go-mode
    :config
    (defun ktz--go-mode-hook ()
      (when (eq major-mode 'go-mode)
        (lsp-format-buffer)
        (lsp-organize-imports)))

    :hook
    ((go-mode . lsp-deferred)
     (before-save . ktz--go-mode-hook)))

  ;; FRONTEND

  (use-package emmet-mode)
  (use-package jinja2-mode
    :init (emmet-mode)
    :mode "\\.html\\'")


  ;; https://azzamsa.com/n/vue-emacs/
  (use-package vue-mode)

  (use-package prettier-js
    :hook (js-mode web-mode typescript-mode vue-mode))

  (use-package typescript-mode
    :config
    (setq js-indent-level 2)
    (setq typescript-indent-level 2))

  (add-hook 'after-init-hook #'global-prettier-mode)

  (use-package lsp-mode
    :init
    ;; (setq lsp-keymap-prefix "C-c l")
    (require 'dap-cpptools)
    (setq lsp-keymap-prefix "C-c l")
    ;;(setq lsp-headerline-breadcrumb-icons-enable nil)
    (setq lsp-headerline-breadcrumb-enable nil)

    :config
    ;; following the performance tips of lsp-doctor
    (when (boundp 'read-process-output-max)
      (setq-local read-process-output-max (* 1024 1024)))
    (setq gc-cons-threshold (* 1024 1024 100))  ;; ~10mb

    ;; TRAMP
    (when ktz-conda-paths
      (ktz-log "prog" "setting remote conda paths")
      (dolist (path ktz-conda-paths)
        (let ((exec-path (concat path "/bin")))
          (ktz-log "prog" (format "  >> adding %s" exec-path))
          ;; TODO understand connection-local variables
          ;;   - https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32090
          (push exec-path tramp-remote-path)))

      ;; it is working hard-coded but not with a variable???
      (push "~/Complex/opt/conda/envs/lsp/bin" tramp-remote-path)
      ;; https://stackoverflow.com/questions/26630640/tramp-ignores-tramp-remote-path
      (add-to-list 'tramp-remote-path 'tramp-own-remote-path)) ;; /when ktz-conda-paths

    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-tramp-connection "pylsp")
      :major-modes '(python-mode)
      :remote? t
      :server-id 'pylsp-remote))

    :hook
    (c-mode . lsp) ;; using `clangd`
    (python-mode . lsp)
    (lsp-mode . lsp-enable-which-key-integration)
    ;; (flycheck-mode . (lambda () (flycheck-select-checker 'python-flake8)))

    :commands lsp)


  ;; thanks https://github.com/daviwil/emacs-from-scratch
  (use-package company
    :after lsp-mode
    :hook (lsp-mode . company-mode)
    :bind (:map company-active-map
                ("<tab>" . company-complete-selection))
    (:map lsp-mode-map
          ("<tab>" . company-indent-or-complete-common))
    :custom
    (company-minimum-prefix-length 1)
    (company-idle-delay 0.0))

  (use-package company-box
    :hook (company-mode . company-box-mode))


  ;; informations: M-x flycheck-verify-setup
  (use-package flycheck :after lsp-mode)

  ;; pyright --------------------

  ;; (use-package lsp-pyright

  ;;   :init
  ;;   (setq lsp-keymap-prefix "C-c l")

  ;;   :config
  ;;   ;; following the performance tips of lsp-doctor
  ;;   (when (boundp 'read-process-output-max)
  ;;     (setq-local read-process-output-max (* 1024 1024)))
  ;;   (setq gc-cons-threshold (* 1024 1024 100))  ;; ~10mb

  ;;   :hook
  ;;   (python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred))))

  ;; legacy

  ;; LSP
  ;; python: using pylsp (_not_ palantir/pyls)
  ;; customize-group lsp-pylsp
  ;; documentation: https://emacs-lsp.github.io/lsp-mode/page/lsp-pylsp/
  ;; (use-package lsp-mode
  ;;   :commands (lsp lsp-deferred)

  ;;   :init
  ;;   (setq lsp-keymap-prefix "C-c l")

  ;;   :config
  ;;   ;; following the performance tips of lsp-doctor
  ;;   (when (boundp 'read-process-output-max)
  ;;     (setq-local read-process-output-max (* 1024 1024)))
  ;;   (setq gc-cons-threshold (* 1024 1024 100))  ;; ~10mb

  ;;   ;; (lsp-enable-which-key-integration t)
  ;;   )

  ) ;; /ktz--init-programming

(provide 'ktz-init-programming)
