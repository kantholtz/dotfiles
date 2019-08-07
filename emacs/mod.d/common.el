(setq-default line-spacing 2)  ;; reset this upon next pull!

(auto-fill-mode t)
(column-number-mode t)
(show-paren-mode t)
(ido-mode t)
(ido-vertical-mode t)
(hl-line-mode t)

(require 'helm-config)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell nil)
(setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default truncate-lines t)
(setq magit-last-seen-setup-instructions "1.4.0")

;; (global-set-key (kbd "M-n") 'forward-list)
;; (global-set-key (kbd "M-p") 'backward-list)

(setq inhibit-splash-screen t)

(global-set-key (kbd "C-x g") 'magit-status)

(require 'multiple-cursors)
(global-set-key (kbd "C-x n") 'mc/mark-next-like-this)

;; aggregate all backup files in one place
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; speedbar setup
(setq speedbar-use-images nil)
(setq speedbar-show-unknown-files t)
(add-hook 'speedbar-mode-hook '(lambda () (hl-line-mode 1)))

;; doom theme specific
(when (display-graphic-p)
    (require 'doom-themes)

  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; org mode
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'hl-line-mode)
