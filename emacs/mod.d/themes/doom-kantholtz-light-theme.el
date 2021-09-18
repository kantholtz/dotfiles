;;; doom-kantholtz-light-theme.el --- based on doom-plain-theme.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Felix Hamann <https://github.com/kantholtz>
;; Created: 2021
;; Version: 0.0.1
;; Keywords: custom themes, faces
;; Homepage: https://github.com/kantholtz/dotfiles
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;
;;; Commentary:
;;
;;    list-faces-display
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-kantholtz-light-theme nil
  "Options for the `doom-kantholtz-light' theme."
  :group 'doom-themes)

(defcustom doom-kantholtz-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-kantholtz-light-theme
  :type '(or integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-kantholtz-light
  "Theme inspired doom-plain"

  ;; name      default/256/16
  ((bg         '("#ffffff"))
   (bg-alt     (doom-darken bg 0.15))
   (base0      '("#999999")) ;; builtins, variables, etc.
   (base1      '("#efefef")) ;; selection
   (base2      '("#000000")) ;; cursor
   (base3      '("#cccccc")) ;; line numbers
   (base4      '("#d9d9d9")) ;; region
   (base5      '("#bbbbbb")) ;; commentary
   (base6      '("#ff0000")) ;; unused
   (base7      '("#00ff00")) ;; unused
   (base8      '("#efefef")) ;; search selection
   (fg         '("#333333"))
   (fg-alt     (doom-lighten fg 0.15))

   (grey       fg)
   (red        fg)
   (blue       fg)
   (dark-blue  fg)
   (orange     fg)
   (green      fg)
   (teal       fg)
   (yellow     fg)
   (magenta    fg)
   (violet     fg)
   (cyan       fg)
   (dark-cyan  fg)

   ;; face categories -- required for all themes
   (highlight      base2)
   (vertical-bar   base5)
   (selection      base1)
   (builtin        base0)
   (comments       base5)
   (doc-comments   base5)
   (constants      base0)
   (functions      fg)
   (keywords       fg)
   (methods        fg)
   (operators      fg)
   (type           fg)
   (strings        base0)
   (variables      base0)
   (numbers        base0)
   (region         base4)
   (error          (doom-blend red "#ff0000" 0.4))
   (warning        base2)
   (success        green)
   (vc-modified    base5)
   (vc-added       (doom-lighten fg 0.7))
   (vc-deleted     base2)

   ;; custom categories
   (-modeline-pad
    (when doom-kantholtz-light-padded-modeline
      (if (integerp doom-kantholtz-light-padded-modeline) doom-kantholtz-light-padded-modeline 4)))

   (modeline-bg              (doom-darken bg-alt 0.15))
   (modeline-bg-alt          (doom-darken bg-alt 0.1))
   (modeline-bg-inactive     (doom-darken bg-alt 0.1))
   (modeline-bg-inactive-alt bg-alt)
   (modeline-fg              fg)
   (modeline-fg-alt          (doom-darken modeline-bg-inactive 0.35)))

  ;;;; Base theme face overrides
  ((error   :underline `(:style wave :color ,error))
   (warning :underline `(:style wave :color ,warning))
   ((font-lock-constant-face &override)      :slant 'italic)
   ((font-lock-comment-face &override)       :slant 'italic)
   ((font-lock-function-name-face &override) :slant 'italic)
   ((font-lock-type-face &override)          :slant 'italic)
   (hl-line :background base1)
   ((line-number &override) :foreground base3)
   ((line-number-current-line &override) :foreground base2)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground highlight)

   ;;;; doom-modeline
   (doom-modeline-bar :background modeline-bg)
   (doom-modeline-bar-inactive :inherit 'doom-modeline-bar)
   (doom-modeline-project-dir :foreground fg)
   (doom-modeline-buffer-file :foreground fg)
   (doom-modeline-buffer-modified :weight 'bold :foreground "#000000")
   (doom-modeline-panel :inherit 'mode-line-highlight :background base3 :foreground fg)
   ;;;; ivy
   (ivy-posframe :background bg-alt)
   ;;;; magit
   ((magit-diff-added-highlight &override)   :foreground fg :background (doom-blend vc-added bg 0.3))
   ((magit-diff-removed &override)           :foreground (doom-lighten fg 0.4) :background (doom-blend vc-deleted bg 0.1))
   ((magit-diff-removed-highlight &override) :foreground fg :background (doom-blend vc-deleted bg 0.22))
   ;;;; lsp-mode
   (lsp-headerline-breadcrumb-symbols-face :foreground keywords :weight 'bold)
   ;;;; outline <built-in>
   (outline-1 :slant 'italic :foreground fg-alt)
   (outline-2 :inherit 'outline-1 :foreground base2)
   (outline-3 :inherit 'outline-2)
   (outline-4 :inherit 'outline-3)
   (outline-5 :inherit 'outline-4)
   (outline-6 :inherit 'outline-5)
   (outline-7 :inherit 'outline-6)
   (outline-8 :inherit 'outline-7)
   ;;;; org <built-in>
   ((org-block &override) :background bg-alt)
   ((org-block-begin-line &override) :foreground base5)
   ;;;; helm
   ((helm-source--header-line &override)
    :background base1
    :foreground fg)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt)))))

;;; doom-kantholtz-light-theme.el ends here

