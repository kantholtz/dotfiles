
;; include package repos
(require 'package)
;; (add-to-list 'package-archives 
;;     '("marmalade" .
;;       "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	'("melpa" .
	  "http://melpa.milkbox.net/packages/") t)
(package-initialize)


;; for the iTerm2 theme "mona lisa"
(load-theme 'dreadworks t)

;; generally desired modes and configurations
(column-number-mode t)
(show-paren-mode t)
(ido-mode t)

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

;; multiple cursors config
(require 'multiple-cursors)
(global-set-key (kbd "C-x n") 'mc/mark-next-like-this)

;; aggregate all backups in one place
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))


;; c programming
(add-to-list 'load-path "~/.emacs.d/cc-mode/")

(setq c-basic-offset 4)

(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)
