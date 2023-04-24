;;; ktz-init-minimal.el --- Minimal initialization.


;; move lines up and down
;; https://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun ktz--move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun ktz--move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))


;; vertice/orderless/consult
(defun ktz--init-minimal-voc ()

  ;; vertical completion ui
  (use-package vertico :init (vertico-mode))

  ;; remember which completions are selected frequently
  (use-package savehist :init (savehist-mode))

  ;; space separated patterns
  (use-package orderless
    :custom
    (completion-styles '(orderless basic))
    (completion-category-overrides ''(file (styles basic partial-completion))))

  ;; annotations in the minibuffer
  (use-package marginalia :init (marginalia-mode))

  ;; relevant actions to use on a target determined by the context
  (use-package embark
    :bind
    (("C-." . embark-act)         ;; pick some comfortable binding
     ("C-;" . embark-dwim)        ;; good alternative: M-.
     ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

    :init
    ;; Optionally replace the key help with a completing-read interface
    (setq prefix-help-command #'embark-prefix-help-command)

    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list
     'display-buffer-alist
     '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
       nil
       (window-parameters (mode-line-format . none)))))
  )


(defun ktz--prog-mode-hooks ()
  (setq-default show-trailing-whitespace t)
  (display-line-numbers-mode))


(defun ktz--init-minimal ()
  "Setup minimal configuration"
  (ktz-log "min" "initializing configuration")

  ;; lay your weary pinky to rest
  (use-package god-mode
    :init
    (god-mode)
    :config
    (global-set-key (kbd "<escape>") #'god-mode-all)
    (setq god-exempt-major-modes nil)
    (setq god-exempt-predicates nil))

  (use-package magit
    :config
    (global-set-key (kbd "C-x g") 'magit-status))

  (use-package yasnippet
    :config
    (straight-use-package '(yasnippet-snippets :type git :host github :repo "AndreaCrotti/yasnippet-snippets"))
    (yas-global-mode t)
    (global-set-key (kbd "C-c j") 'yas-expand))

  (use-package multiple-cursors
    :config
    (global-set-key (kbd "C-x n") 'mc/mark-next-like-this))

  (use-package blank-mode)
  (use-package which-key
    :config (which-key-mode))

    ;; server
  (use-package yaml-mode)
  (use-package fish-mode)
  (use-package nginx-mode)
  (use-package apache-mode)
  (use-package markdown-mode)

  (auto-fill-mode t)
  (column-number-mode t)
  (show-paren-mode t)

  (global-set-key (kbd "M-<up>") 'ktz--move-line-up)
  (global-set-key (kbd "M-<down>") 'ktz--move-line-down)

  (ktz--init-minimal-voc)

  ;; hooks
  (add-hook 'prog-mode-hook 'ktz--prog-mode-hooks))


(provide 'ktz-init-minimal)

