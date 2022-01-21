;; -*- coding: utf-8 -*-

;; DEFAULT MODES
;; --------------------
(auto-fill-mode t)
(column-number-mode t)
(show-paren-mode t)


;; GENERAL
;; --------------------
(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell nil)
(setq inhibit-splash-screen t)
(setq calendar-week-start-day 1)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq explicit-shell-file-name "/bin/bash")
(setq tramp-default-method "sshx")
(setq magit-last-seen-setup-instructions "1.4.0")

(setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default truncate-lines t)

;; disable line numbers for some modes
;; (dolist (mode
;;          '(org-mode-hook
;;            term-mode-hook
;;            shell-mode-hook))
;;   (add-hook mode (lambda () display-line-numbers 0)))
;; disable auto-fill-mode from some modes
;; (dolist (mode
;;          '(latex-mode-hook
;;            org-mode-hook))
;;   mode
;;   (lambda ()
;;     (message "foo")))
  ;; (add-hook mode '(auto-fill-mode 0))
  ;; (add-hook mode 'visual-line-mode)
  ;; (add-hook
  ;;  mode
  ;;  (lambda ()
  ;;    ((interactive)
  ;;     (auto-fill-mode 0)
  ;;     (visual-line-mode 1))))
;; (add-hook 'latex-mode-hook 'visual-line-mode)
;; (add-hook 'latex-mode-hook '(auto-fill-mode 0))

;; move lines up and down
(defun ktz/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun ktz/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-<up>") 'ktz/move-line-up)
(global-set-key (kbd "M-<down>") 'ktz/move-line-down)


;; HELM
;; --------------------
(require 'helm-config)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

;; ORG
;; --------------------

;; this must not be part of org-roam :config
;; because it would not be initialized when
;; the splash screen is shown
(if (boundp 'ktz/org-dir)
  (setq org-agenda-files '("~/Complex/psi/Roam/"))
  (setq org-agenda-start-with-log-mode t))

(when (display-graphic-p)
  (setq org-ellipsis " ▼")
  (require 'org-bullets)
  (setq org-bullets-bullet-list '("●" "●" "○" "○" "▫"))
  (add-hook 'org-mode-hook 'org-bullets-mode))

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(setq org-agenda-window-setup 'current-window)   ;; do not destroy current splits

(if (boundp 'ktz/org-dir)
    (use-package org-roam
      :custom
      (org-roam-directory ktz/org-dir)
      (org-roam-capture-templates
       '(("d" "default" plain
          (file "~/Complex/psi/Roam/_templates/default.org")
          :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "")
          :unnarrowed t)))
      :init
      (setq org-roam-v2-ack t)
      :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n i" . org-roam-node-insert)
             ("M-." . org-open-at-point)
             ("M-," . org-mark-ring-goto)
             :map org-mode-map
             ("C-M-i"    . completion-at-point))
      :config
      (org-roam-setup)
      (setq org-log-done 'time)
      ))


;; see https://github.com/alphapapa/org-super-agenda#examples
(let ((org-super-agenda-groups
       '(;; Each group has an implicit boolean OR operator between its selectors.
         (:name "Today"  ; Optionally specify section name
                :time-grid t  ; Items that appear on the time grid
                :todo "NEXT")  ; Items that have this TODO keyword
         (:name "Important"
                ;; Single arguments given alone
                :tag "bills"
                :priority "A")
         )))
  (org-agenda nil "a"))


;; EDIT SERVER
;; --------------------
(when (require 'edit-server nil :noerror)
  (setq edit-server-new-frame nil)  ;; do not open a new window
  (edit-server-start))
(add-hook 'edit-server-start-hook 'markdown-mode)

;; MULTIPLE CURSORS
;; --------------------
(require 'multiple-cursors)
(global-set-key (kbd "C-x n") 'mc/mark-next-like-this)

;; YASNIPPETS
;; --------------------
(require 'yasnippet)
(yas-global-mode t)
(global-set-key (kbd "C-c j") 'yas-expand)

;; MISC
;; --------------------
(global-set-key (kbd "C-x g") 'magit-status)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(add-hook 'after-init-hook 'global-company-mode)


;; PYTHON

;; - elpy and anaconda-mode do the same thing
;; - anaconda-mode seems easier and more lighweight
;; - flycheck is more powerful than the built-in flymake

(require 'python)
(require 'company)
(eval-after-load "company"
 '(add-to-list 'company-backends 'company-anaconda))
(setq anaconda-mode-localhost-address "localhost")

(dolist (mode
  '(anaconda-mode
    anaconda-eldoc-mode
    blacken-mode
    flycheck-mode))
  (add-hook 'python-mode-hook mode))

;; (add-hook 'python-mode-hook 'anaconda-mode)
;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;; (add-hook 'python-mode-hook 'blacken-mode)
;; (add-hook 'python-mode-hook 'flycheck-mode)


;; VUE
;; https://azzamsa.com/n/vue-emacs/

;; (require 'lsp-mode)
;; (require 'prettier)
(add-hook 'after-init-hook #'global-prettier-mode)

(setq js-indent-level 2)
(setq typescript-indent-level 2)

;; (add-hook 'vue-mode-hook #'lsp)
(dolist
    (hook
     '(js-mode-hook
       typescript-mode-hook
       vue-mode-hook))
  (add-hook hook 'prettier-js-mode))
