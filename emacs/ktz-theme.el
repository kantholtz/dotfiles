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

(defun ktz--theme-modus-faces ()
  "Adjust modus-themes-* faces.")
;; (modus-themes-with-colors
;;   (set-face-attribute 'modus-themes-lang-error nil
;;                       :underline nil :foreground red :background bg-red-nuanced)
;;   (set-face-attribute 'modus-themes-lang-warning nil
;;                       :underline nil :foreground yellow :background bg-yellow-nuanced)
;;   (set-face-attribute 'modus-themes-lang-note nil
;;                       :underline nil :foreground fg-alt :background cyan-faint)
;;   ))


(defun ktz--theme-org-faces ()
  "Adjust org related faces"

  ;; org-todo-keyword-faces
  ;;  '(("ARCH" . (:inherit ('regular 'org-done)))
  ;;    ("DONE" . (:inherit ('regular 'org-done))))

  (modus-themes-with-colors
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
                        :foreground fg-dim)
    (set-face-attribute 'org-priority nil
                        :foreground fg-dim)
    ;; (set-face-attribute 'org-ref-cite nil
    ;;                     :foreground green)

    ;; org agenda

    ;; org-imminent-deadline ← org-agenda-deadline-faces
    ;;   (missed/close deadlines)
    ;; org-upcoming-deadline
    ;;   (regular deadlines)

    ;; org-agenda-deadline-faces:
    ;;
    ;;   ((1.0 . org-imminent-deadline)
    ;;    (0.5 . org-upcoming-deadline)
    ;;    (0.0 . org-upcoming-distant-deadline))
    ;;
    ;; Each car is a fraction of the head-warning time that must
    ;; have passed for this the face in the cdr to be used for
    ;; display.
    (set-face-attribute 'org-upcoming-distant-deadline nil
                        :foreground fg-dim)
    (set-face-attribute 'org-upcoming-deadline nil
                        :weight 'normal)
    (set-face-attribute 'org-imminent-deadline nil
                        :weight 'normal :foreground magenta)

    ;; org-cite
    (set-face-attribute 'org-cite-key nil
                        :foreground green)
    ))



(defun ktz--theme-custom-faces ()
  ;; follows modus-themes-to-toggle
  ;; do not use custom-set-faces but set-face-attribute;
  ;; the former saves to .emacs
  (if (eq ktz-theme-current 'modus-vivendi)
      (setq ktz-theme-current 'modus-operandi)
    (setq ktz-theme-current 'modus-vivendi))

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
  (defun ktz--theme-god-hook ()
    ;; not using modus-themes-with-colors for now for performance reasons
    (if (or god-local-mode buffer-read-only)
        (setq cursor-type '(hbar . 5))
      (set-cursor-color (if (eq ktz-theme-current 'modus-vivendi)
                            ktz-c-white ktz-c-black))
      (setq cursor-type 'box)))

  (use-package god-mode
    :hook
    (god-mode-enabled . ktz--theme-god-hook)
    (god-mode-disabled . ktz--theme-god-hook))

  ;; misc not worth their own functions
  (modus-themes-with-colors
    ;; header-line
    (modus-themes-with-colors
      (set-face-attribute
       'header-line nil
       :background bg-dim
       :underline border
       :overline border
       :box `(:line-width 5 :color ,bg-dim)))

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
    ;; outline
    ;; (set-face-attribute 'outline-minor-1 nil
    ;;                     :foreground cyan-intense
    ;;                     :height 1.3)
    ;; (set-face-attribute 'outline-minor-2 nil
    ;;                     :foreground cyan
    ;;                     :height 1.2)
    ;; (set-face-attribute 'outline-minor-3 nil
    ;;                     :foreground cyan-warmer
    ;;                     :height 1.1)

    ;;; other packages
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

  ;; more involved groups
  (ktz--theme-modus-faces)
  (ktz--theme-org-faces)
  (when (fboundp 'ktz-modeline-set-faces)
    (ktz-modeline-set-faces)))

;;;; Initialization

(defun ktz--init-theme ()

  (when (display-graphic-p)
    ;; disable window clutter
    (menu-bar-mode 0)
    (tool-bar-mode 0)
    (scroll-bar-mode 0)

    ;; section 6.12 "Font configurations for Org and others"
    (if (eq system-type 'windows-nt)
        (progn
          (set-face-attribute
           'variable-pitch nil
           :foundry "IBM" :family "IBM Plex Sans"
           :height 1.0 :width 'medium)

          (set-face-attribute
           'fixed-pitch nil
           :family "Cascadia Mono" :foundry "outline"
           :height 1.0 :slant 'normal :weight 'normal :width 'medium :weight 'normal))

      (progn
        (set-face-attribute
         'variable-pitch nil
         :foundry "IBM" :family "IBM Plex Sans"
         :height 1.0 :width 'medium :slant 'normal)

        (set-face-attribute
         'fixed-pitch nil
         :foundry "IBM" :family "IBM Plex Mono"
         :height 1.0 :width 'medium :slant 'normal))))

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
    (setq modus-themes-to-toggle '(modus-vivendi modus-operandi))
    (defvar ktz-theme-current (cdr modus-themes-to-toggle))

    (keymap-global-set "<f6>" #'modus-themes-toggle)
    (keymap-global-set "C-<f6>" #'modus-themes-toggle)

    (add-hook
     'modus-themes-after-load-theme-hook
     #'ktz--theme-custom-faces)

    (dolist (theme modus-themes-to-toggle)
      (load-theme theme :no-confirm))

    (modus-themes-toggle)))

(defun ktz-load-theme ()
  (interactive)
  (ktz--init-theme))

(provide 'ktz-theme)
;;; ktz-theme.el ends here
