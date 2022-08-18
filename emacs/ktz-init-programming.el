;;; ktz-init-programming.el --- Programming initialization.


;; (defun ktz--init-programming-python-hook ()
;;   (lsp)
;;   (add-hook 'before-save-hook #'lsp-format-buffer nil t))


(defun ktz--init-programming ()
  "Setup programming configuration"
  (message "[ktz] initializing programming configuration")

  ;; PYTHON --------------------

  ;; when using sshfs for a remote machine:
  ;;   - set up dedicated local conda environment
  ;;   - install all dependencies from mounted drive
  ;;   - activate env (conda-env-activate)
  ;;   - open project using lsp

  ;; set up conda
  (when ktz-conda-dir
    (use-package conda
      :custom
      (conda-anaconda-home ktz-conda-dir)
      :config
      (conda-env-initialize-interactive-shells)
      (conda-env-initialize-eshell)
      (conda-env-activate ktz-conda-env)))

  ;; (use-package python
  ;;   :hook
  ;;   (before-save . lsp-format-buffer))

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
    :hook
    ((go-mode . lsp-deferred)
     (before-save . lsp-format-buffer)
     (before-save . lsp-organize-imports)))

  ;; FRONTEND

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
    (setq lsp-keymap-prefix "C-c l")

    :config
    ;; following the performance tips of lsp-doctor
    (when (boundp 'read-process-output-max)
      (setq-local read-process-output-max (* 1024 1024)))
    (setq gc-cons-threshold (* 1024 1024 100))  ;; ~10mb

    ;; --------------------
    ;; register remote conda paths for tramp
    (message (format ">> ktz-conda-paths %s" ktz-conda-paths))
    (when ktz-conda-paths
      (message "  >> setting remote conda paths")

      ;; (dolist (path ktz-conda-paths)
      ;;   (let ((exec-path (concat path "/bin")))
      ;;     (message (format "  >> adding %s" exec-path))

      ;;     ;; TODO understand connection-local variables
      ;;     ;;   - https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32090
      ;;     (push exec-path tramp-remote-path)
      ;;     (message (format "  << %s" tramp-remote-path))))

      ;; it is working hard-coded but not with a variable???
      (push "/home/staffsi/hamann/Complex/opt/conda/envs/lsp/bin" tramp-remote-path)

      ;; https://stackoverflow.com/questions/26630640/tramp-ignores-tramp-remote-path
      (add-to-list 'tramp-remote-path 'tramp-own-remote-path)) ;; /when ktz-conda-paths

    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-tramp-connection "pylsp")
      :major-modes '(python-mode)
      :remote? t
      :server-id 'pylsp-remote))
    ;; --------------------

    :hook
    (python-mode . lsp)
    (lsp-mode    . lsp-enable-which-key-integration)

    :commands lsp

    ) ;; /use-package lsp-mode

  ;; lsp over tramp and python


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

  (use-package helm-lsp
    :after lsp-mode
    :commands helm-lsp-workspace-symbol)


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
  ;;   ;; (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))
  ;;   )

  ) ;; /ktz--init-programming

(provide 'ktz-init-programming)
