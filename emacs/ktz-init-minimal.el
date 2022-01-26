;;; ktz-init-minimal.el --- Minimal initialization.


(defvar ktz--pkgs-minimal
  '(
    helm
    magit
    yasnippet
    multiple-cursors

    ;; server
    yaml-mode
    fish-mode
    nginx-mode
    apache-mode
    markdown-mode))


(defun ktz--set-trailing-whitespace ()
  (setq-default show-trailing-whitespace t))


(defun ktz--init-terminal-overwrites ()
  "visual adjustments in terminal mode"
  (set-face-attribute 'helm-selection nil :foreground 'black)
  )


;; move lines up and down
; ;https://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
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


(defun ktz--init-minimal ()
  "Setup minimal configuration"
  (dolist (pkg ktz--pkgs-minimal)
    (straight-use-package pkg))

  (unless (display-graphic-p)
    (ktz--init-terminal-overwrites))

  (auto-fill-mode t)
  (column-number-mode t)
  (show-paren-mode t)

  (global-set-key (kbd "C-x g") 'magit-status)

  (global-set-key (kbd "M-<up>") 'ktz/move-line-up)
  (global-set-key (kbd "M-<down>") 'ktz/move-line-down)

  (require 'helm-config)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (helm-mode 1)

  (require 'multiple-cursors)
  (global-set-key (kbd "C-x n") 'mc/mark-next-like-this)

  (require 'yasnippet)
  (yas-global-mode t)
  (global-set-key (kbd "C-c j") 'yas-expand)

  ;; hooks
  (add-hook 'prog-mode-hook 'ktz--set-trailing-whitespace)
  ;; (add-hook 'text-mode-hook 'ktz--set-trailing-whitespace)
  (add-hook 'after-init-hook 'global-company-mode)

  t)


(provide 'ktz-init-minimal)

