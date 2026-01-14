;;; ktz-init-min.el --- Minimal initialization.
;;
;;; Code:

;;;; General configuration setup

;; always only ask for y or n
(fset 'yes-or-no-p 'y-or-n-p)

(setq
 ;; Always ask before quitting
 confirm-kill-emacs 'yes-or-no-p
 ;; Now pls tell me how to deactivate this on windows aswell
 visible-bell nil
 ;; No message in scratch buffer
 initial-scratch-message nil
 ;; Follow symlinks to scm'ed files
 vc-follow-symlinks t
 ;; Revert dired and others
 global-auto-revert-non-file-buffers t
 ;; Silence compiler warnings
 native-comp-async-report-warnings-errors nil
 ;; Wir sind hier in Deutschland
 calendar-week-start-day 1
 ;; Gather backup files in a central directory
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
 ;; Better to rely on the most widely distributed one
 explicit-shell-file-name "/bin/bash"
 ;; Also use /bin/sh for tramp connections
 tramp-default-method "sshx"
 ;; Show dictionaries before other files in dired
 dired-listing-switches "-alhv --group-directories-first")

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


;;;; Key-bindings

(global-set-key (kbd "M-<up>") 'ktz--move-line-up)
(global-set-key (kbd "M-<down>") 'ktz--move-line-down)

(global-set-key (kbd "<f5>") #'bookmark-jump)
(global-set-key (kbd "C-<f5>") #'bookmark-jump)

;; to comfortably insert a single space in god-mode
(global-set-key
 (kbd "C-q")
 (lambda () (interactive)
   (save-excursion (insert " "))))


;;;; KTZ setup


;;; Vertico + Orderless + Consult
(defun ktz--init-min-voc ()

  ;; Vertico provides a performant and minimalistic vertical
  ;; completion UI based on the default completion system.
  (use-package vertico :init (vertico-mode))

  ;; Alternative to company with fuzzy search
  ;;   - currently disabled because of problems with eglot
  ;;   - there are tipps: https://github.com/minad/corfu/wiki#configuring-corfu-for-eglot
  ;;
  ;; (use-package corfu
  ;;   :custom
  ;;   (corfu-auto t)          ;; Enable auto completion
  ;;   ;; (corfu-separator ?_) ;; Set to orderless separator, if not using space
  ;;   :bind
  ;;   (:map corfu-map
  ;;         ("C-n" . corfu-next)
  ;;         ("C-p" . corfu-previous))
  ;;   :init
  ;;   (global-corfu-mode))

  ;; Many editors (e.g. Vim) have the feature of saving minibuffer
  ;; history to an external file after exit. This package provides the
  ;; same feature in Emacs.
  (use-package savehist :init (savehist-mode))

  ;; This package provides an orderless completion style that divides
  ;; the pattern into space-separated components, and matches
  ;; candidates that match all of the components in any order.
  (use-package orderless
    :custom
    (completion-styles '(orderless basic))
    (completion-category-overrides ''(file (styles basic partial-completion)))
    ;; disable defaults, use our settings
    (completion-category-defaults nil)
    ;; Emacs 31: partial-completion behaves like substring
    (completion-pcm-leading-wildcard t))

  ;; Consult provides search and navigation commands based on the
  ;; Emacs completion function completing-read.
  ;; TODO https://github.com/minad/consult?tab=readme-ov-file#use-package-example
  ;; (use-package consult ...)

  ;; This package provides marginalia-mode which adds marginalia to
  ;; the minibuffer completions.
  (use-package marginalia :init (marginalia-mode))

  ;; Relevant actions to use on a target determined by the context
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
       (window-parameters (mode-line-format . none))))))


;;; Outline + IMenu
;; thanks prot
(defun ktz--init-min-outline ()

  ;; Outline mode is a major mode derived from Text mode, which is
  ;; specialized for editing outlines. It provides commands to
  ;; navigate between entries in the outline structure, and commands
  ;; to make parts of a buffer temporarily invisible, so that the
  ;; outline structure may be more easily viewed.
  (use-package outline
    :config
    ;; Enable visibility-cycling commands on headings in ‘outline-minor-mode’.
    (setq outline-minor-mode-cycle t)
    ;; Non-nil means to leave an unhidden blank line before headings.
    (setq outline-blank-line t))


  ;; This package teaches outline-minor-mode to highlight section
  ;; headings, without also highlighting top-level s-expressions.
  (use-package outline-minor-faces
    :after outline
    :hook (outline-minor-mode . outline-minor-faces-mode))

  ;; This package provides commands for cycling the visibility of
  ;; outline sections and code blocks. These commands are intended to
  ;; be bound in outline-minor-mode-map and do most of the work using
  ;; functions provided by the outline package.
  ;; (use-package bicycle
  ;;   :after outline)

  ;; The Foldout package extends Outline mode and Outline minor mode
  ;; with folding commands. The idea of folding is that you zoom in on
  ;; a nested portion of the outline, while hiding its relatives at
  ;; higher levels.
  ;; (use-package foldout)
  )


;;; Initialization
(defun ktz--init-min ()
  "Setup minimal configuration"
  (ktz-log "min" "initializing configuration")

  ;;;; Globally used modes

  ;; buffer-move is for lazy people wanting to swap buffers without
  ;; typing C-x b on each window.
  (use-package buffer-move
    :bind (("C-c <up>"    . #'buf-move-up)
           ("C-c <down>"  . #'buf-move-down)
           ("C-c <left>"  . #'buf-move-left)
           ("C-c <right>" . #'buf-move-right)))

  ;; This is a global minor mode for entering Emacs commands without
  ;; modifier keys. It's similar to Vim's separation of command mode
  ;; and insert mode.
  (use-package god-mode
    :bind
    ("<escape>" . god-mode-all)

    :hook
    (god-mode-disabled . (lambda () (setq cursor-type 'box)))
    (god-mode-enabled . (lambda () (setq cursor-type '(hbar . 5))))

    :config
    (when ktz-god-default (god-mode))

    :bind (:map god-local-mode-map
                ("[" . backward-word)
                ("]" . forward-word)
                ("{" . backward-paragraph)
                ("}" . forward-paragraph)))

  ;; Popups with completion
  (use-package company
    :ensure t
    :hook (
           (prog-mode . company-mode)
           (LaTeX-mode . company-mode)))
  ;; :bind (:map company-active-map
  ;;             ("<return>" . nil)
  ;;             ("RET" . nil)
  ;;             ("C-<return>" . company-complete-selection)
  ;;             ([tab] . company-complete-selection)
  ;;             ("TAB" . company-complete-selection)))

  (use-package company-box
    :ensure t
    :hook (company-mode . company-box-mode))

  ;; This is a small Emacs package that temporarily highlights the
  ;; current line after a given function is invoked.
  (use-package pulsar
    :config
    (setq pulsar-pulse t
          pulsar-delay 0.025
          pulsar-iterations 20
          pulsar-face 'pulsar-green)
    (pulsar-global-mode 1))

  ;; Jinx is a fast JIT spell-checker for Emacs. Jinx highlights
  ;; misspelled words in the visible portion of the buffer.
  (use-package jinx
    :config
    (setq jinx-languages "en_GB-ize de_DE")
    ;; (dolist (hook '(text-mode-hook conf-mode-hook))
    ;;   (add-hook hook #'jinx-mode))
    :bind (("C-c w" . jinx-correct))
    :hook (LaTeX-mode . jinx-mode))

  ;; YASnippet is a template system for Emacs. It allows you to type
  ;; an abbreviation and automatically expand it into function
  ;; templates.
  (use-package yasnippet
    :config
    (straight-use-package
     '(yasnippet-snippets
       :type git :host github :repo "AndreaCrotti/yasnippet-snippets"))
    (yas-global-mode t)
    (global-set-key (kbd "C-c j") 'yas-expand))

  (use-package multiple-cursors
    :config
    (global-set-key (kbd "C-c m") 'mc/mark-next-like-this))

  ;; Use ripgrep in Emacs. Ripgrep is a replacement for both grep
  ;; like (search one file) and ag like (search many files) tools.
  (use-package rg
    :config
    (rg-enable-menu))

  ;; Magit is a complete text-based user interface to Git.
  (use-package magit
    :init
    (setq magit-last-seen-setup-instructions "1.4.0")
    :bind (("C-x g" . magit-status)))

  ;; This spawns a shell (a sh subshell in case of fish)
  ;; and reads the exported environment variables
  ;; and sets exec-path accordingly
  (use-package exec-path-from-shell
    :config
    (when (memq window-system '(mac ns x pgtk))
      (exec-path-from-shell-initialize)
      (ktz-log "min" "initialized exec-path-from-shell")))

  (ktz--init-min-voc)
  (ktz--init-min-outline)

  ;;;; Builtin globally enabled minor modes

  (use-package emacs
    :custom
    ;; disable Ispell completion function.
    ;; try `cape-dict' as an alternative.
    (text-mode-ispell-word-completion nil)

    :config
    ;; Automatically breaks lines to fit within the window width
    (auto-fill-mode t)
    ;; Displays the column number in the mode line
    (column-number-mode t)
    ;; Highlights matching parentheses
    (show-paren-mode t))

  ;; Displays available key bindings
  (use-package which-key
    :config (which-key-mode t))
  ;;;; Hooks

  (use-package markdown-mode
    :hook (markdown-mode . visual-line-mode))

  ;; Modes to enable for prog-modes
  (defun ktz--prog-mode-hooks ()
    (setq-default show-trailing-whitespace t)
    (display-line-numbers-mode))

  (add-hook 'prog-mode-hook 'ktz--prog-mode-hooks))


(provide 'ktz-init-min)
