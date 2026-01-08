;;; ktz-theme-el --- Overwriting faces
;;
;;; Commentary:
;;   - depends heavily on Protesilaos' modus themes
;;
;;; Code:
(require 'ktz-colors)
(require 'ktz-theme-light)
(require 'ktz-theme-dark)

(defun ktz--theme-config ()
  ;; ESSENTIAL to make the underline move to the bottom of the box:
  ;; TODO move to mode-line overrides
  x-underline-at-descent-line t

  modus-themes-italic-constructs t
  modus-themes-bold-constructs t
  modus-themes-mixed-fonts t
  modus-themes-variable-pitch-ui t
  modus-themes-disable-other-themes t

  ;; org mode related

  modus-themes-headings
  '((agenda-date . (light 1.1))
    (agenda-structure . (light 1.4))
    (1 . (variable-pitch semibold 1.3))
    (t . (variable-pitch normal  1.1))))


;;;; Face overwrite (hooked)

(defun ktz--theme-custom-faces ()
  ;; follows modus-themes-to-toggle
  ;; do not use custom-set-faces but set-face-attribute;
  ;; the former saves to .emacs
  (if (eq ktz-theme-current 'modus-vivendi)
      (setq ktz-theme-current 'modus-operandi)
    (setq ktz-theme-current 'modus-vivendi))

  ;; more involved groups
  (when (fboundp 'ktz-modeline-set-faces)
    (ktz-modeline-set-faces))

  (when (fboundp 'ktz-headerline-set-faces)
    (ktz-headerline-set-faces))

  ;; add some space between windows
  ;; and adjust line height
  (when (display-graphic-p)
    (setq window-divider-default-places 'bottom-only
          window-divider-default-bottom-width 15)
    (window-divider-mode)
    (setq-default line-spacing .2)
    (modus-themes-with-colors
      (set-face-attribute 'window-divider nil :foreground bg-main)))

  ;; change cursor based on god-mode state
  ;;   - todo: this hook is called many times per toggle for whatever reason...
  (defun ktz--theme-god-hook ()
    ;; (ktz-log "ktz--theme-god-hook" (format "%s" ktz-theme-current))
    (if (or (and (boundp god-local-mode) god-local-mode) buffer-read-only)
        (setq cursor-type '(hbar . 5))
      (setq cursor-type 'box)))


    ;; highlighting and search
    (set-face-attribute 'highlight nil
                        :foreground green :background bg-green-subtle)
    (set-face-attribute 'lazy-highlight nil
                        :foreground yellow :background bg-yellow-nuanced)
    (set-face-attribute 'isearch nil
                        :foreground green :background bg-green-nuanced)
    (set-face-attribute 'query-replace nil
                        :foreground red :background bg-red-nuanced)

    ;;; emacs builtin
    ;; interaction elements
    (set-face-attribute 'widget-field nil
                        :background bg-active)
    ;;; other packages
    ;; org
    (set-face-attribute
     'org-level-1 nil
     :box `(:line-width (-1 . 1) :color ,bg-main))

    (set-face-attribute
     'org-level-2 nil
     :box `(:line-width (-1 . 1) :color ,bg-main)
     :weight 'semi-bold)

    (set-face-attribute 'org-tag nil
                        :foreground cyan-cooler :weight 'normal)
    (set-face-attribute 'org-checkbox nil
                        :foreground fg-alt)
    (set-face-attribute 'org-priority nil
                        :foreground fg-dim)

    (set-face-attribute 'org-upcoming-distant-deadline nil
                        :foreground fg-dim)
    (set-face-attribute 'org-upcoming-deadline nil
                        :weight 'normal)
    (set-face-attribute 'org-imminent-deadline nil
                        :weight 'normal :foreground magenta)

    ;; org-cite
    (set-face-attribute 'org-cite-key nil
                        :foreground green)

    ;; jinx
    (when (facep 'jinx-misspelled)
      (set-face-attribute 'jinx-misspelled nil
                          :underline nil :foreground red :background bg-red-nuanced))

    ;; pulsar
    (when (facep 'pulsar-green)
      (set-face-attribute 'pulsar-green nil
                          :foreground green :background bg-green-intense))
    ;; languagetool
    (when (facep 'langtool-errline)
      (set-face-attribute 'langtool-errline nil
                          :inherit 'error)))


;;;; Initialization

(defun ktz--init-theme ()

  (when (display-graphic-p)
    ;; disable window clutter
    (menu-bar-mode 0)
    (tool-bar-mode 0)
    (scroll-bar-mode 0))

  (use-package modus-themes
    ;; :straight (modus-themes-fork
    ;;            :host github
    ;;            :repo "protesilaos/modus-themes")

    :config
    ;; load theme specific colour palettes
    (ktz--init-theme-light)
    (ktz--init-theme-dark)
    (ktz--theme-config)

    ;; first in the list determines default theme
    ;;   vivendi: dark
    ;;   operandi: light
    (setq modus-themes-to-toggle
          (if ktz-theme-default ;; t is dark, nil is light
              '(modus-vivendi modus-operandi)
            '(modus-operandi modus-vivendi)))

    (ktz-log "theme" (format "dark default: %s, current %s"
                             ktz-theme-default
                             modus-themes-to-toggle))

    (defvar ktz-theme-current (car modus-themes-to-toggle))

    (keymap-global-set "<f6>" #'modus-themes-toggle)
    (keymap-global-set "C-<f6>" #'modus-themes-toggle)

    (add-hook
     'modus-themes-after-load-theme-hook
     #'ktz--theme-custom-faces)

    (add-hook 'god-mode-enabled-hook #'ktz--theme-god-hook)
    (add-hook 'god-mode-disabled-hook #'ktz--theme-god-hook)

    (dolist (theme modus-themes-to-toggle)
      (load-theme theme :no-confirm))

    (modus-themes-toggle)))

(defun ktz-load-theme ()
  (interactive)
  (ktz--init-theme))

(provide 'ktz-theme)
;;; ktz-theme.el ends here
