;; include package repos
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	'("melpa" .
	  "http://melpa.milkbox.net/packages/") t)
(package-initialize)


;; for the sandbox
(load-theme 'manoj-dark t)
(set-face-background 'mode-line "black")
(set-face-background 'mode-line-buffer-id "black")
(set-face-background 'mode-line-inactive "black")


;; generally desired modes and configurations
(show-paren-mode t)
(ido-mode t)
(column-marker-1 80)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)


;; for faster reactions
(fset 'yes-or-no-p 'y-or-n-p)

;; no annoying beeps
(setq visible-bell t)

;; linum-mode config
(setq linum-format "%4d \u2502 ")
(global-linum-mode t)

;; magit config
(global-set-key (kbd "C-x g") 'magit-status)

;; aggregate all backups in one place
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(put 'dired-find-alternate-file 'disabled nil)
