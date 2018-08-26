
;; nlinum
(when (display-graphic-p)
  (global-nlinum-mode t))

;; yasnippets
(require 'yasnippet)
(yas-global-mode t)

;; company mode
(add-hook 'after-init-hook 'global-company-mode)

;; use gfm-mode for readme files
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
