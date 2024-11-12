;;; ktz-init-minimal.el --- Minimal initialization.

;; always only ask for y or n
(fset 'yes-or-no-p 'y-or-n-p)

(setq ;; misc
 ;; now pls tell me how to deactivate this on windows aswell
 visible-bell nil
 ;; no message in scratch buffer
 initial-scratch-message nil
 ;; follow symlinks to scm'ed files
 vc-follow-symlinks t
 ;; revert dired and others
 global-auto-revert-non-file-buffers t
 ;; silence compiler warnings
 native-comp-async-report-warnings-errors nil
 ;; wir sind hier in deutschland
 calendar-week-start-day 1
 ;; gather backup files in a central directory
 backup-directory-alist '(("." . (concat user-emacs-directory "backups")))
 ;; better to rely on the most widely distributed one
 explicit-shell-file-name "/bin/bash"
 ;; also use /bin/sh for tramp connections
 tramp-default-method "sshx")

(setq-default
 indent-tabs-mode nil
 tab-width 2
 truncate-lines t)


;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; ---

(defun ktz--init-config ()
  (add-to-list
   'load-path
   (concat ktz-root-dir "/lib")))


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


(defun ktz--init-minimal ()
  "Setup minimal configuration"
  (ktz-log "min" "initializing configuration")

  ;; lay your weary pinky to rest
  (use-package god-mode
    :config
    (global-set-key (kbd "<escape>") #'god-mode-all)
    (when ktz-god-default
      (god-mode)))

  (use-package pulsar
    :config
    (setq pulsar-pulse t
          pulsar-delay 0.025
          pulsar-iterations 10
          pulsar-face 'pulsar-green)
    (pulsar-global-mode 1))

  ;; (use-package yasnippet
  ;;   :config
  ;;   (straight-use-package
  ;;    '(yasnippet-snippets
  ;;      :type git :host github :repo "AndreaCrotti/yasnippet-snippets"))
  ;;   (yas-global-mode t)
  ;;   (global-set-key (kbd "C-c j") 'yas-expand))

  (use-package multiple-cursors
    :config
    (global-set-key (kbd "C-x n") 'mc/mark-next-like-this))

  (use-package blank-mode)
  (use-package which-key
    :config (which-key-mode))

  (use-package rg
    :config
    (rg-enable-menu))

  (use-package magit
    :init
    (setq magit-last-seen-setup-instructions "1.4.0")
    :bind (("C-x g" . magit-status)
           ("<f6>" . magit-status)))

  (use-package markdown-mode
    :hook (markdown-mode . visual-line-mode))

  ;; this spawns a shell (a sh subshell in case of fish)
  ;; and reads the exported environment variables
  ;; and sets exec-path accordingly
  (use-package exec-path-from-shell
    :config
    (when (memq window-system '(mac ns x pgtk))
      (exec-path-from-shell-initialize)
      (ktz-log "min" "initialized exec-path-from-shell")))

  ;; finance
  (use-package beancount
    :mode ("\\.ledger\\'" . beancount-mode)
    :bind (:map beancount-mode-map
                ("M-p" . beancount-goto-previous-transaction)
                ("M-n" . beancount-goto-next-transaction)))

  (auto-fill-mode t)
  (column-number-mode t)
  (show-paren-mode t)

  ;; --

  (global-set-key (kbd "M-<up>") 'ktz--move-line-up)
  (global-set-key (kbd "M-<down>") 'ktz--move-line-down)

  (global-set-key (kbd "<f5>") #'bookmark-jump)
  (global-set-key (kbd "C-<f5>") #'bookmark-jump)

  (ktz--init-minimal-voc)

  ;; hooks
  (defun ktz--prog-mode-hooks ()
    (setq-default show-trailing-whitespace t)
    (display-line-numbers-mode))

  (add-hook 'prog-mode-hook 'ktz--prog-mode-hooks))


(provide 'ktz-init-minimal)
