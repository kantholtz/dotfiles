;;; ktz-init-desktop.el --- Desktop initialization.

;; --


(defun ktz--init-desktop-theme-set-light ()

  ;; use M-x list-faces-display
  ;; or M-x describe-face (with cursor)

  "Overwriting nano-theme-light.el."
  (setq frame-background-mode    'light)
  (setq nano-color-foreground "#37474f") ;; blue grey 800
  (setq nano-color-background "#fafafa") ;; grey 50
  (setq nano-color-highlight  "#b0bec5") ;; blue grey 200
  (setq nano-color-critical   "#c2185b") ;; pink 700
  (setq nano-color-salient    "#4527a0") ;; deep purple 800
  (setq nano-color-strong     "#000000") ;; black
  (setq nano-color-popout     "#00695c") ;; teal 800
  (setq nano-color-subtle     "#eceff1") ;; blue grey 100
  (setq nano-color-faded      "#90a4ae") ;; blue grey 300

  ;; adding some of my own
  ;; (later: use defcustom)
  (defvar nano-color-warning    "#bf360c")  ;; deep orange 900

  (defvar nano-color-critical-light "#fce4ec") ;; pink 50
  (defvar nano-color-warning-light  "#fbe9e7") ;; deep orange 50
  (defvar nano-color-popout-light   "#e0f2f1") ;; teal 50
  t)


(defun ktz--init-desktop-overwrite-faces ()
  (let ;; overwrite (mostly 'light) :weight face attributes
      ((weight-alist
        '((medium
           . (nano-face-default
              nano-face-faded
              nano-face-header-default))
          (semi-bold
           . (nano-face-salient
              nano-face-tag-default
              nano-face-tag-strong
              nano-face-tag-salient
              nano-face-tag-popout
              nano-face-tag-faded
              nano-face-tag-critical))
          (bold
           . (nano-face-strong)))))

    (dolist (pair weight-alist)
      (dolist (face (cdr pair))
        (set-face-attribute face nil :weight (car pair)))))

  (set-face-attribute
   'nano-face-critical nil
   :foreground nano-color-critical
   :background nano-color-background)

  (set-face-attribute
   'line-number nil :foreground "pink")

  t)


(defun ktz--init-desktop-theme ()

  ;; lsp

  (set-face-attribute
   'header-line nil :background nano-color-subtle)

  (add-hook
   'lsp-after-initialize-hook
   (lambda ()

     (set-face-attribute
      'lsp-headerline-breadcrumb-path-face nil
      :weight 'regular)

     (set-face-attribute
      'lsp-headerline-breadcrumb-path-error-face nil
      :underline nil
      :foreground nano-color-critical)

     (set-face-attribute
      'lsp-headerline-breadcrumb-path-warning-face nil
      :underline nil
      :foreground nano-color-warning)

     (set-face-attribute
      'lsp-headerline-breadcrumb-path-hint-face nil
      :underline nil
      :foreground nano-color-salient)

     (set-face-attribute
      'lsp-headerline-breadcrumb-path-info-face nil
      :underline nil
      :foreground nano-color-salient)

     (customize-set-variable
      'lsp-headerline-breadcrumb-segments
      '(path-up-to-project file))))

  ;; company

  ;; does not work (?)
  ;; (use-package company
  ;;   :custom-face
  ;;   `(company-tooltip ((t (:background ,nano-color-background)))))

  (add-hook
   'company-mode-hook
   (lambda ()

     (set-face-attribute
      'company-tooltip nil
      :background nano-color-background)

     (set-face-attribute
      'company-tooltip-selection nil
      :foreground nano-color-foreground
      :background nano-color-background)

     (set-face-attribute
      'company-tooltip-common-selection nil
      :foreground nano-color-salient
      :background nano-color-background)

     (set-face-attribute
      'company-tooltip-scrollbar-thumb nil
      :background nano-color-background)

     (set-face-attribute
      'company-tooltip-scrollbar-track nil
      :background nano-color-background)))


  ;; flycheck
  (require 'flycheck)

  (set-face-attribute
   'flycheck-info nil
   :underline nil
   :background nano-color-popout-light)

  (set-face-attribute
   'flycheck-warning nil
   :underline nil
   :background nano-color-warning-light)

  (set-face-attribute
   'flycheck-error nil
   :underline nil
   :background nano-color-critical-light)

  ;; other

  (set-face-attribute
   'show-paren-match nil
   :foreground nano-color-critical
   :weight 'bold)

  t)


(defun ktz--init-desktop-nano ()
  ;; gonna absorb and adapt nano one by one
  ;; the order of all these expressions is very important...

  ;; attributes must be set before requiring nano
  ;; (setq nano-font-family-monospaced (face-attribute 'default :family))
  (setq nano-font-size ktz-font-size)
  (setq nano-font-family-monospaced ktz-font-monospace)
  (setq nano-font-family-proportional ktz-font-proportional)

  ;; basic layout customization (window divider)
  (require 'nano-layout)

  ;; lots of defcustom with nil initialization
  (require 'nano-faces)

  ;; overwrite colors
  (ktz--init-desktop-theme-set-light)

  ;; sets many attributes, e.g. font weights
  (nano-faces)
  (ktz--init-desktop-overwrite-faces)

  ;; assigns the nano-faces (set-face, set-face-attribute)
  (require 'nano-theme)
  (nano-theme)
  (ktz--init-desktop-theme)

  ;; (nano-theme) invokes (nano-theme--basics) which
  ;; overwrites :weight for 'default...
  (set-face-attribute 'default nil :weight 'regular)

  ;; setting up the modeline
  ;; (require 'nano-modeline)
  (require 'ktz-theme-modeline))


(defun ktz--init-desktop-graphic-p ()
  (message "[ktz] initializing theme (desktop-graphics-p)")

  (ktz--init-desktop-nano)

  ;; spell checks
  (setq ispell-program-name (executable-find "hunspell") ispell-dictionary "en_GB")
  (defun flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word"
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word))

  t)


(defun ktz--init-desktop ()
  "Setup desktop configuration - includes Roam, LaTex etc."
  (message "[ktz] initializing desktop configuration")

  ;; (use-package auctex)
  (use-package pdf-tools)

  ;; lay your weary pinky to rest
  (require 'control-lock)
  (control-lock-keys)
  (global-set-key (kbd "C-`") 'control-lock-enable)

  ;; non-terminal mode only
  (when (display-graphic-p)

    ;; cannot use (use-package nano-emacs ...)
    ;; https://github.com/rougier/nano-emacs/issues/43
    (straight-use-package
     '(nano-emacs :host github :repo "rougier/nano-emacs"))
    (ktz--init-desktop-graphic-p)

    ;; disable window clutter
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)))


(provide 'ktz-init-desktop)
