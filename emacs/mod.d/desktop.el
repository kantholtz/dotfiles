;; -*- coding: utf-8 -*-

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

  ;; add some space
  (fringe-mode 10)

  ;; dynamically set theme based on environment vars
  (if (getenv "KTZ_LIGHT")
    (load-theme 'doom-flatwhite t)
    (load-theme 'doom-city-lights t))

)

;; company mode
(add-hook 'after-init-hook 'global-company-mode)

;; use gfm-mode for readme files
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; docview should auto-refresh
(add-hook 'doc-view-mode-hook 'auto-revert-mode)


;; LATEX
;; --------------------

(defun ktz/latex-mode-visual-fill ()
  (setq visual-fill-column-width 80
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(add-hook 'LaTeX-mode-hook 'ktz/latex-mode-visual-fill)
