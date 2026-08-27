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

  ;; didnt work?
  ;; (use-package uv-mode
  ;;   :hook (python-mode . uv-mode-auto-activate-hook))

  ;; this solves the problem of html fragments in eldoc buffers
  ;; thanks: https://emacs.stackexchange.com/questions/80740/how-to-correctly-format-nbsp-in-eldoc-using-eglot

  (defvar ktz-ide--eldoc-html-patterns
    '(("&nbsp;" " ")
      ("&lt;" "<")
      ("&gt;" ">")
      ("&amp;" "&")
      ("&quot;" "\"")
      ("&apos;" "'"))
    "List of (PATTERN . REPLACEMENT) to replace in eldoc output.")

  (defun ktz-ide--string-replace-all (patterns in-string)
    "Replace all cars from PATTERNS in IN-STRING with their pair."
    (mapc (lambda (pattern-pair)
            (setq in-string
                  (string-replace (car pattern-pair) (cadr pattern-pair) in-string)))
          patterns)
    in-string)

  (defun ktz-ide--eldoc-preprocess (orig-fun &rest args)
    "Preprocess the docs to be displayed by eldoc to replace HTML escapes."
    (let ((doc (car args)))
      ;; The first argument is a list of (STRING :KEY VALUE ...) entries
      ;; we replace the text in each such string
      ;; see docstring of `eldoc-display-functions'
      (when (listp doc)
        (setq doc (mapcar
                   (lambda (doc) (cons
                                  (ktz-ide--string-replace-all ktz-ide--eldoc-html-patterns (car doc))
                                  (cdr doc)))
                   doc)))
      (apply orig-fun (cons doc (cdr args)))))

  (advice-add 'eldoc-display-in-buffer :around #'ktz-ide--eldoc-preprocess)

  ;; Provides Autoformatting
  (use-package apheleia
    :config

    :config
    ;; for debugging
    ;; (setq apheleia-log-only-errors nil)

    ;; replace default (black) to use ruff for sorting import and formatting.
    (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))

    (apheleia-global-mode +1))


  ;; enable lsp support
  (use-package eglot
    :hook (python-mode . eglot-ensure)

    :custom
    (eglot-workspace-configuration
     '(:pyright (:disableTaggedHints t)
                :basedpyright (:disableTaggedHints t)))

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
  ;; (use-package ein
  ;;   :config (setq ein:output-area-inlined-images t))

  (use-package jupyter
    :defer t)
  ;; :init
  ;; ;; optional: image rendering
  ;; (setq jupyter-repl-echo-input nil))

  (use-package drepl)

  (use-package code-cells
    :bind (:map code-cells-mode-map
                ("M-p" . code-cells-backward-cell)
                ("M-n" . code-cells-forward-cell)
                ("C-c C-c" . code-cells-eval)
                ([remap jupyter-eval-line-or-region] . code-cells-eval)))
  ;; :mode ("\\.ipynb\\'" . code-cells-convert-ipynb)
  ;; :config
  ;; ;; Custom wrapper functions to bridge code-cells with emacs-jupyter
  ;; (with-eval-after-load 'code-cells
  ;;   (defun my/code-cells-eval-jupyter ()
  ;;     "Send current cell to the active Jupyter REPL (EIN C-c C-c equivalent)."
  ;;     (interactive)
  ;;     (let ((cell (code-cells-bounds)))
  ;;       (jupyter-eval-region (car cell) (cdr cell))))

  ;;   (defun my/code-cells-eval-and-step ()
  ;;     "Execute current cell and jump to next block (EIN C-c C-v equivalent)."
  ;;     (interactive)
  ;;     (my/code-cells-eval-jupyter)
  ;;     (code-cells-forward-cell))

  ;;   (defun my/code-cells-insert-above ()
  ;;     "Insert a cell boundary marker above current point (EIN C-c C-a equivalent)."
  ;;     (interactive)
  ;;     (code-cells-backward-cell)
  ;;     (open-line 1)
  ;;     (insert "# %%")
  ;;     (forward-line 1))

  ;;   (defun my/code-cells-insert-below ()
  ;;     "Insert a cell boundary marker below current point (EIN C-c C-b equivalent)."
  ;;     (interactive)
  ;;     (code-cells-forward-cell)
  ;;     (open-line 1)
  ;;     (insert "# %%")
  ;;     (forward-line 1)))

  ;; :bind (:map code-cells-mode-map
  ;;             ;; --- Evaluation Bindings (EIN Style) ---
  ;;             ("C-c C-c" . my/code-cells-eval-jupyter)  ; Run cell in-place
  ;;             ("C-c C-v" . my/code-cells-eval-and-step) ; Run cell and move next
  ;;             ("<shift>-<return>" . my/code-cells-eval-and-step)

  ;;             ;; --- Navigation Bindings (EIN Style) ---
  ;;             ("M-p"     . code-cells-backward-cell)    ; Prev cell block
  ;;             ("M-n"     . code-cells-forward-cell)     ; Next cell block

  ;;             ;; --- Modification Bindings (EIN Style) ---
  ;;             ("C-c C-a" . my/code-cells-insert-above)  ; Insert cell boundary above
  ;;             ("C-c C-b" . my/code-cells-insert-below)  ; Insert cell boundary below
  ;;             ("C-c C-k" . code-cells-kill)             ; Delete/Kill current cell text

  ;;             ;; --- Structural Manipulation (EIN Style) ---
  ;;             ("C-c C-s" . code-cells-split)            ; Split cell at point
  ;;             ("C-c M-j" . code-cells-merge)))          ; Merge current cell with next



  (use-package numpydoc)

  ;; frontend ----------------------------------------

  (use-package emmet-mode)

  (use-package web-mode
    :ensure t
    :mode (("\\.html\\'" . web-mode)
           ("\\.jinja2\\'" . web-mode)
           ("\\.j2\\'" . web-mode))
    :config
    (setq web-mode-engines-alist
          '(("jinja" . "\\.jinja2\\'")
            ("jinja" . "\\.j2\\'"))))

  ;; (use-package jinja2-mode
  ;;   :init (emmet-mode)
  ;;   :mode "\\.html\\'")

  (use-package nvm
    :straight (:host github :repo "rejeep/nvm.el"))

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

  (autoload 'eglot-find-declaration "eglot" nil t)
  (autoload 'eglot-find-implementation "eglot" nil t)
  (autoload 'eglot-find-typeDefinition "eglot" nil t)
  (autoload 'eglot-rename "eglot" nil t)
  (autoload 'eglot-format "eglot" nil t)
  (autoload 'eglot-format-buffer "eglot" nil t)
  (autoload 'eglot-shutdown "eglot" nil t)
  (autoload 'eglot-shutdown-all "eglot" nil t)

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
     ["project"
      ("ps" "switch" project-switch-project)
      ("pf" "find file" project-find-file)
      ("pd" "find directory" project-find-dir)
      ("pr" "query replace (regex)" project-query-replace-regexp)
      ("pk" "kill buffers" project-kill-buffers)]
     ["flymake"
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
