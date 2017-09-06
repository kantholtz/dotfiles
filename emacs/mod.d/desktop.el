
;; nlinum
(global-nlinum-mode t)

;; ispell
(setq ispell-program-name "/usr/local/bin/ispell")

;; yasnippets
(require 'yasnippet)
(yas-global-mode t)

;; company mode
(add-hook 'after-init-hook 'global-company-mode)

;; use gfm-mode for readme files
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
