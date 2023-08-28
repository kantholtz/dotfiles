
(defun ktz--init-theme ()

  ;; non-terminal mode only
  (when (display-graphic-p)

    ;; disable window clutter
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1))


  (use-package modus-themes
    :config

    ;; section 6.12 "Font configurations for Org and others"
    ;; (set-face-attribute
    ;;  'default nil :family "IBM Plex Mono")

    (set-face-attribute
     'variable-pitch nil
     :foundry "IBM" :family "IBM Plex Sans"
     :height 1.0 :width 'medium :slant 'normal)

    (set-face-attribute
     'fixed-pitch nil
     :foundry "IBM" :family "IBM Plex Mono"
     :height 1.0 :width 'medium :slant 'normal)

    ;; general (non-modus) config
    (setq-default line-spacing 0.1)

    (setq

     org-todo-keyword-faces
      '(("ARCH" . (:inherit (regular org-done)))
        ("DONE" . (:inherit (regular org-done))))
     ;; Constructs

     modus-themes-italic-constructs t
     modus-themes-bold-constructs t
     modus-themes-mixed-fonts t
     modus-themes-variable-pitch-ui t

     modus-themes-custom-auto-reload t
     modus-themes-disable-other-themes t

     ;; org mode related

     modus-themes-headings
     '((agenda-date . (light 1.1))
       (agenda-structure . (light 1.4))
       (1 . (variable-pitch semibold 1.3))
       (t . (variable-pitch semibold 1.1)))

     ;; Palette

     modus-operandi-palette-overrides
     '(

;;; Colors

       ;; The original palette offers many shades
       ;; and options. The ktz palette is much more reduced
       ;; (inspired by rougiers nano theme).

;;; Basic values

       (bg-main   "#FFFFFF")  ;; white
       (bg-dim    "#FAFAFA")  ;; grey 50
       (bg-active "#F5F5F5")  ;; grey 100

       (fg-dim   "#BDBDBD")  ;; grey 400
       (fg-main  "#757575")  ;; grey 600
       (fg-alt   "#000000")  ;; black

       (bg-inactive bg-main)
       (border fg-dim)

;;; Common accent foregrounds

       ;; pink
       (red                "#c2185b")  ;; 700
       (bg-red-intense     "#F48FB1")  ;; 200
       (bg-red-subtle      "#F8BBD0")  ;; 100
       (bg-red-nuanced     "#FCE4EC")  ;;  50

       ;; teal (tailwind)
       (green              "#00796B")  ;; 800
       (bg-green-intense   "#99f6e4")  ;; 200
       (bg-green-subtle    "#ccfbf1")  ;; 100
       (bg-green-nuanced   "#f0fdfa")  ;;  50

       ;; deep purple
       (magenta            "#4527A0")  ;; 800
       (bg-magenta-intense "#B39DDB")  ;; 200
       (bg-magenta-subtle  "#D1C4E9")  ;; 100
       (bg-magenta-nuanced "#EDE7F6")  ;;  50

       ;; amber
       (yellow            "#ff8f00")  ;; 800
       (bg-red-intense    "#FFE082")  ;; 200
       (bg-red-subtle     "#FFECB3")  ;; 100
       (bg-red-nuanced    "#FFF8E1")  ;;  50

       ;; blue grey
       (blue              "#546E7A")  ;; 600
       (bg-blue-intense   "#B0BEC5")  ;; 200
       (bg-blue-subtle    "#CFD8DC")  ;; 100
       (bg-blue-nuanced   "#ECEFF1")  ;;  50


;;;; Special purpose ✓

       (bg-completion       bg-magenta-nuanced)
       (bg-hover            bg-green-nuanced)
       (bg-hover-secondary  bg-yellow-nuanced)
       (bg-hl-line          bg-active)
       (bg-region           bg-active)
       (fg-region           fg-alt)

       (bg-mode-line-active        unspecified)
       (fg-mode-line-active        fg-alt)
       (border-mode-line-active    unspecified)
       (bg-mode-line-inactive      unspecified)
       (fg-mode-line-inactive      fg-dim)
       (border-mode-line-inactive  unspecified)

       (modeline-err     red)
       (modeline-warning magenta)
       (modeline-info    green)

       (bg-tab-bar      fg-dim)
       (bg-tab-current  bg-main)
       (bg-tab-other    fg-dim)

       ;; ??
       (bg-char-0 "#ff0000")
       (bg-char-1 "#00ff00")
       (bg-char-2 "#0000ff")


;;; Diffs ✓

       (bg-added           bg-green-nuanced)
       (bg-added-faint     bg-green-nuanced)
       (bg-added-refine    bg-green-nuanced)
       (bg-added-fringe    bg-green-nuanced)
       (fg-added           green)
       (fg-added-intense   green)

       (bg-changed         bg-yellow-nuanced)
       (bg-changed-faint   bg-yellow-nuanced)
       (bg-changed-refine  bg-yellow-nuanced)
       (bg-changed-fringe  bg-yellow-nuanced)
       (fg-changed         yellow)
       (fg-changed-intense yellow)

       (bg-removed         bg-red-nuanced)
       (bg-removed-faint   bg-red-nuanced)
       (bg-removed-refine  bg-red-nuanced)
       (bg-removed-fringe  bg-red-nuanced)
       (fg-removed         red)
       (fg-removed-intense red)

       (bg-diff-context    fg-dim)

;;; Paren match

       (bg-paren-match        bg-green-subtle)
       (bg-paren-expression   bg-magenta-subtle)
       (underline-paren-match unspecified)


;;; Mappings

;;;; General mappings

       (fringe unspecified)
       (cursor fg-main)

       (keybind magenta)
       (name magenta)
       (identifier magenta)

       (err red)
       (warning yellow)
       (info magenta)

       (underline-err red)
       (underline-warning yellow)
       (underline-note magenta)

       (bg-prominent-err bg-red-subtle)
       (fg-prominent-err red)
       (bg-prominent-warning bg-yellow-subtle)
       (fg-prominent-warning yellow)
       (bg-prominent-note bg-magenta-subtle)
       (fg-prominent-note magenta)

;;;; Code mappings

       (builtin magenta)
       (fnname magenta)
       (type magenta)

       (keyword fg-alt)
       (constant fg-alt)
       (variable fg-alt)

       (string green)

       (comment fg-dim)
       (docstring fg-dim)
       (docmarkup fg-dim)

       (preprocessor red)
       (rx-construct green)
       (rx-backslash magenta)

       (fnname magenta)

;;;; Accent mappings

       (accent-0 blue)
       (accent-1 magenta)
       (accent-2 green)
       (accent-3 red)

;;;; Completion mappings

       (fg-completion-match-0 magenta)
       (fg-completion-match-1 green)
       (fg-completion-match-2 fg-alt)
       (fg-completion-match-3 blue)
       (bg-completion-match-0 unspecified)
       (bg-completion-match-1 unspecified)
       (bg-completion-match-2 unspecified)
       (bg-completion-match-3 unspecified)

;;;; Date mappings

       (date-common fg-alt)
       (date-deadline fg-alt)
       (date-event "#FF0")
       (date-holiday "#0FF")
       (date-holiday-other "#0F0")
       (date-now "#F0F")
       (date-range "#00F")
       (date-scheduled "#A0C")
       (date-weekday fg-main)  ;; agenda heading
       (date-weekend fg-dim)  ;; agenda heading

;;;; Link mappings

       (fg-link magenta)
       (bg-link unspecified)
       (underline-link bg-magenta-subtle)

       (fg-link-symbolic green)
       (bg-link-symbolic unspecified)
       (underline-link-symbolic bg-green-subtle)

       (fg-link-visited magenta)
       (bg-link-visited unspecified)
       (underline-link-visited bg-magenta-subtle)

;;;; Mark mappings

       (bg-mark-delete bg-red-subtle)
       (fg-mark-delete red)
       (bg-mark-select bg-blue-subtle)
       (fg-mark-select blue)
       (bg-mark-other bg-yellow-subtle)
       (fg-mark-other yellow)

;;;; Prompt mappings

       (fg-prompt magenta)
       (bg-prompt unspecified)

;;;; Rainbow mappings

       (rainbow-0 fg)
       (rainbow-1 red)
       (rainbow-2 magenta)
       (rainbow-3 green)
       (rainbow-4 blue)
       (rainbow-5 red)
       (rainbow-6 magenta)
       (rainbow-7 green)
       (rainbow-8 blue)

;;;; Heading mappings

       (fg-heading-0 fg-alt)
       (fg-heading-1 fg-alt)
       (fg-heading-2 fg-alt)
       (fg-heading-3 fg-alt)
       (fg-heading-4 fg-alt)
       (fg-heading-5 fg-alt)
       (fg-heading-6 fg-alt)
       (fg-heading-7 fg-alt)
       (fg-heading-8 fg-alt)

;;;; Prose

       (prose-done fg-dim)

;;; End of overrides
       )
;; End of :config setq
     )


    ;; load base theme after all customization
    (load-theme 'modus-operandi :no-confirm)
    (ktz-log ".emacs" "Loaded modus theme")

    ) ;; /use-package
  ) ;; /ktz--init-theme


(defun ktz-load-theme ()
  (interactive)
  (ktz--init-theme))

(provide 'ktz-init-theme)
