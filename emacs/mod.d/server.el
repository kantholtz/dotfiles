;;
;; loose config
;;
(auto-fill-mode t)
(column-number-mode t)
(show-paren-mode t)

(unless (boundp 'nvrn-ido-disabled)
  (ido-mode t)
  (ido-vertical-mode t))

(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell nil)

(setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq magit-last-seen-setup-instructions "1.4.0")

(global-set-key (kbd "M-n") 'forward-list)
(global-set-key (kbd "M-p") 'backward-list)

;;
;;   package config
;;
;; linum-mode config
(setq linum-format "%4d | ")
(global-linum-mode t)

;; magit config
(global-set-key (kbd "C-x g") 'magit-status)

;; multiple cursors config
(require 'multiple-cursors)
(global-set-key (kbd "C-x n") 'mc/mark-next-like-this)

;; aggregate all backups in one place
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))