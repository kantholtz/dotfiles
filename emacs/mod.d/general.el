;;
;; loose config
;;
(auto-fill-mode t)
(column-number-mode t)
(show-paren-mode t)
(ido-mode t)
(ido-vertical-mode t)

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

;; yasnippet config
(require 'yasnippet)
(yas-global-mode t)


;; company mode config
(add-hook 'after-init-hook 'global-company-mode)

;; auto complete
;;(require 'auto-complete-config)
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;;(ac-config-default)
;;
;;(ac-set-trigger-key "TAB")
;;(ac-set-trigger-key "<tab>")


;; ispell config
(setq ispell-program-name "/usr/local/bin/ispell")

;; aggregate all backups in one place
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; use gfm-mode for readme files
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
