;; default modes

(auto-fill-mode t)
(column-number-mode t)
(show-paren-mode t)
(ido-mode t)
(ido-vertical-mode t)


;; general configuration

(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell nil)
(setq inhibit-splash-screen t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default truncate-lines t)
(setq magit-last-seen-setup-instructions "1.4.0")


;; package specific configuration

(require 'helm-config)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

;; org mode
(setq calendar-week-start-day 1)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'hl-line-mode)

;; edit-server
(when (require 'edit-server nil :noerror)
  (setq edit-server-new-frame nil)  ;; do not open a new window
  (edit-server-start))
(add-hook 'edit-server-start-hook 'markdown-mode)


;; mutliple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-x n") 'mc/mark-next-like-this)

;; others
(setq tramp-default-method "sshx")
(global-set-key (kbd "C-x g") 'magit-status)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)


;; when using a gui

(when (display-graphic-p)
  (require 'doom-themes)

  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; not nice in terminal mode
  (global-hl-line-mode t))
