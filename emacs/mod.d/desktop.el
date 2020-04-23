
;; nlinum
(when (display-graphic-p)
  (global-linum-mode 1))

;; yasnippets
(require 'yasnippet)
(yas-global-mode t)

;; company mode
(add-hook 'after-init-hook 'global-company-mode)

;; use gfm-mode for readme files
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
