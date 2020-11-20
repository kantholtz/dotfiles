
;; NON-TERMINAL THINGS
;; --------------------
(when (display-graphic-p)
  (require 'doom-themes)

  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; show line numbers
  (global-linum-mode 1)

  ;; not nice in terminal mode
  (global-hl-line-mode t)

)

;; company mode
(add-hook 'after-init-hook 'global-company-mode)

;; use gfm-mode for readme files
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
