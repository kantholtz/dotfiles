;;; ktz-init-minimal.el --- Minimal initialization.


(defun ktz--set-trailing-whitespace ()
  (setq-default show-trailing-whitespace t))


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


(defun ktz--init-minimal-helm ()
  (require 'helm-config)

  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)

  (helm-mode 1)

  ;; terminal colors don't work well with helm defaults
  (unless (display-graphic-p)
    (set-face-attribute
     'helm-selection nil
     :foreground "black" :background "white")
    (set-face-attribute
     'helm-ff-directory nil
     :foreground "white" :background "black"))

  t)


(defun ktz--prog-mode-hooks ()
  (ktz--set-trailing-whitespace)
  (linum-mode)
  (setq linum-format "%4d ")

  t)


(defun ktz--init-minimal ()
  "Setup minimal configuration"
  (message "[ktz] initializing minimal configuration")

  (use-package helm)
  (use-package magit)
  (use-package yasnippet)
  (use-package which-key)
  (use-package multiple-cursors)

    ;; server
  (use-package yaml-mode)
  (use-package fish-mode)
  (use-package nginx-mode)
  (use-package apache-mode)
  (use-package markdown-mode)

  (auto-fill-mode t)
  (column-number-mode t)
  (show-paren-mode t)

  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "M-<up>") 'ktz--move-line-up)
  (global-set-key (kbd "M-<down>") 'ktz--move-line-down)

  (ktz--init-minimal-helm)

  (require 'multiple-cursors)
  (global-set-key (kbd "C-x n") 'mc/mark-next-like-this)

  (require 'yasnippet)
  (yas-global-mode t)
  (global-set-key (kbd "C-c j") 'yas-expand)

  (use-package which-key
    :config (which-key-mode))

  ;; hooks
  (add-hook 'prog-mode-hook 'ktz--prog-mode-hooks)
  t)


(provide 'ktz-init-minimal)

