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
    :straight (modus-themes-fork
               :host github
               :repo "protesilaos/modus-themes")

    :config

    (defvar ktz-theme-current 'light "Either 'light or 'dark")
    (setq
     modus-themes-to-toggle '(modus-vivendi modus-operandi)

     ;; ESSENTIAL to make the underline move to the bottom of the box:
     ;; TODO move to mode-line overrides
     x-underline-at-descent-line t

     ;; org-todo-keyword-faces
     ;;  '(("ARCH" . (:inherit ('regular 'org-done)))
     ;;    ("DONE" . (:inherit ('regular 'org-done))))
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
       (t . (variable-pitch normal  1.1)))

     ;; Palette

     modus-operandi-palette-overrides
     '(

       (bg-main   "#FFFFFF")  ;; white
       (bg-dim    "#FAFAFA")  ;; grey 50
       (bg-active "#F5F5F5")  ;; grey 100

       (fg-dim   "#BDBDBD")  ;; grey 400
       (fg-main  "#757575")  ;; grey 600
       (fg-alt   "#000000")  ;; black

       (bg-inactive bg-main)
       (border fg-dim)

       ;; using cyan to define additional shades of grey
       (cyan-faint         "#e5e5e5")  ;; 200
       (cyan-cooler        "#d4d4d4")  ;; 300
       (cyan-warmer        "#737373")  ;; 500
       (cyan               "#404040")  ;; 700
       (cyan-intense       "#171717")  ;; 900

       ;; pink
       (red                "#c2185b")  ;; 700
       (bg-red-intense     "#F48FB1")  ;; 200
       (bg-red-subtle      "#F8BBD0")  ;; 100
       (bg-red-nuanced     "#FCE4EC")  ;;  50

       ;; teal (tailwind)
       (green              "#0d9488")  ;; 600
       (bg-green-intense   "#99f6e4")  ;; 200
       (bg-green-subtle    "#ccfbf1")  ;; 100
       (bg-green-nuanced   "#f0fdfa")  ;;  50

       ;; deep purple
       (magenta            "#4527A0")  ;; 800
       (bg-magenta-intense "#B39DDB")  ;; 200
       (bg-magenta-subtle  "#D1C4E9")  ;; 100
       (bg-magenta-nuanced "#EDE7F6")  ;;  50

       ;; amber
       (yellow             "#ff8f00")  ;; 800
       (bg-yellow-intense  "#FFE082")  ;; 200
       (bg-yellow-subtle   "#FFECB3")  ;; 100
       (bg-yellow-nuanced  "#FFF8E1")  ;;  50

       ;; blue grey
       (blue               "#546E7A")  ;; 600
       (bg-blue-intense    "#B0BEC5")  ;; 200
       (bg-blue-subtle     "#CFD8DC")  ;; 100
       (bg-blue-nuanced    "#ECEFF1")  ;;  50

       ;; completion and search
       (bg-completion       bg-magenta-nuanced)
       (bg-hover            bg-green-nuanced)
       (bg-hover-secondary  bg-yellow-nuanced)
       (bg-hl-line          bg-active)
       (bg-region           bg-active)
       (fg-region           fg-alt)

       (bg-mode-line-active        bg-dim)
       (bg-mode-line-inactive      bg-dim)
       (fg-mode-line-active        fg-alt)
       (fg-mode-line-inactive      fg-dim)
       (border-mode-line-active    fg-dim)
       (border-mode-line-inactive  fg-dim)

       ;; modeline
       (modeline-err     red)
       (modeline-warning magenta)
       (modeline-info    green)

       (bg-tab-bar      bg-dim)
       (bg-tab-current  bg-main)
       (bg-tab-other    fg-dim)

       ;; ??
       (bg-char-0 "#ff0000")
       (bg-char-1 "#00ff00")
       (bg-char-2 "#0000ff")

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
       (bg-paren-expression   bg-magenta-subtle)
       (underline-paren-match unspecified)

       (fg-paren-match        green)
       (bg-paren-match        bg-green-subtle)

       ;; mappings

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

       ;; code

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

       (accent-0 blue)
       (accent-1 magenta)
       (accent-2 green)
       (accent-3 red)

       (fg-completion-match-0 magenta)
       (fg-completion-match-1 green)
       (fg-completion-match-2 fg-alt)
       (fg-completion-match-3 blue)
       (bg-completion-match-0 unspecified)
       (bg-completion-match-1 unspecified)
       (bg-completion-match-2 unspecified)
       (bg-completion-match-3 unspecified)

       (date-common fg-alt)
       (date-deadline fg-alt)
       (date-event "#FF0")
       (date-holiday "#0FF")
       (date-holiday-other "#0F0")
       (date-range "#00F")
       (date-scheduled "#A0C")
       (date-now fg-main)  ;; "now" bar in agenda
       (date-weekday fg-main)  ;; agenda heading
       (date-weekend fg-dim)  ;; agenda heading

       (fg-link magenta)
       (bg-link unspecified)
       (underline-link bg-magenta-subtle)

       (fg-link-symbolic green)
       (bg-link-symbolic unspecified)
       (underline-link-symbolic bg-green-subtle)

       (fg-link-visited magenta)
       (bg-link-visited unspecified)
       (underline-link-visited bg-magenta-subtle)

       (bg-mark-delete bg-red-subtle)
       (fg-mark-delete red)
       (bg-mark-select bg-blue-subtle)
       (fg-mark-select blue)
       (bg-mark-other bg-green-subtle)
       (fg-mark-other green)

       (fg-prompt magenta)
       (bg-prompt unspecified)

       (rainbow-0 fg)
       (rainbow-1 red)
       (rainbow-2 magenta)
       (rainbow-3 green)
       (rainbow-4 blue)
       (rainbow-5 red)
       (rainbow-6 magenta)
       (rainbow-7 green)
       (rainbow-8 blue)

       (fg-heading-0 fg-alt)
       (fg-heading-1 fg-alt)
       (fg-heading-2 fg-alt)
       (fg-heading-3 fg-alt)
       (fg-heading-4 fg-alt)
       (fg-heading-5 fg-alt)
       (fg-heading-6 fg-alt)
       (fg-heading-7 fg-alt)
       (fg-heading-8 fg-alt)

       (prose-done fg-dim)

       ) ;; end of overrides for operandi

     modus-vivendi-palette-overrides
     '(

       ;; stone
       ;; (bg-main   "#000000") ;; TODO add "OLED mode"
       (bg-main   "#1c1917")
       (bg-dim    "#292524")  ;; 800
       (bg-active "#44403c")  ;; 700

       (fg-dim   "#57534e")  ;; 600
       (fg-main  "#a8a29e")  ;; 400
       (fg-alt   "#ffffff")  ;; white

       (bg-inactive bg-main)
       (border fg-dim)

       ;; using cyan to define additional shades of stone
       (cyan-faint         "#0c0a09")  ;; 950
       (cyan-cooler        "#78716c")  ;; 500
       (cyan-warmer        "#d6d3d1")  ;; 300
       (cyan               "#e7e5e4")  ;; 200
       (cyan-intense       "#f5f5f4")  ;; 100

       ;; pink
       (red                "#db2777")  ;; 600
       (bg-red-intense     "#9d174d")  ;; 800
       (bg-red-subtle      "#500724")  ;; 950
       (bg-red-nuanced     "#2e0515")  ;; custom

       ;; cyan/sky
       (green              "#67e8f9")  ;; 400
       (bg-green-intense   "#075985")  ;; 800
       (bg-green-subtle    "#0c4a6e")  ;; 900
       (bg-green-nuanced   "#082f49")  ;; 950

       ;; stone
       (magenta             "#e7e5e4")  ;; 200
       (bg-magenta-intense  "#292524")  ;; 800
       (bg-magenta-subtle   "#1c1917")  ;; 900
       (bg-magenta-nuanced  "#0c0a09")  ;; 950

       ;; orange
       (yellow             "#fed7aa")  ;; 200
       (bg-yellow-intense  "#9a3412")  ;; 800
       (bg-yellow-subtle   "#7c2d12")  ;; 900
       (bg-yellow-nuanced  "#431407")  ;; 950

       ;; slate
       (blue               "#e2e8f0")  ;; 200
       (bg-blue-intense    "#1e293b")  ;; 800
       (bg-blue-subtle     "#0f172a")  ;; 900
       (bg-blue-nuanced    "#020617")  ;; 950

       ;; completion and search
       (bg-completion       bg-magenta-nuanced)
       (bg-hover            bg-green-nuanced)
       (bg-hover-secondary  bg-yellow-nuanced)
       (bg-hl-line          bg-active)
       (bg-region           bg-active)
       (fg-region           fg-alt)

       ;; modeline
       (bg-mode-line-active        bg-dim)
       (bg-mode-line-inactive      bg-dim)
       (fg-mode-line-active        fg-alt)
       (fg-mode-line-inactive      fg-dim)
       (border-mode-line-active    fg-dim)
       (border-mode-line-inactive  fg-dim)

       (modeline-err     red)
       (modeline-warning yellow)
       (modeline-info    green)

       (bg-tab-bar      fg-dim)
       (bg-tab-current  bg-main)
       (bg-tab-other    fg-dim)

       ;; ??
       (bg-char-0 "#ff0000")
       (bg-char-1 "#00ff00")
       (bg-char-2 "#0000ff")

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

       (fg-paren-match green)
       (bg-paren-match bg-green-subtle)

       (bg-diff-context    fg-dim)
       (bg-paren-expression   bg-magenta-subtle)
       (underline-paren-match unspecified)

       ;; mappings

       (fringe unspecified)
       (cursor fg-main)

       (keybind yellow)
       (name yellow)
       (identifier yellow)

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

       ;; code

       (builtin yellow)
       (fnname yellow)
       (type yellow)

       (keyword fg-alt)
       (constant fg-alt)
       (variable fg-alt)

       (string green)

       (comment fg-dim)
       (docstring fg-dim)
       (docmarkup fg-dim)

       (preprocessor red)
       (rx-construct green)
       (rx-backslash yellow)

       (fnname yellow)

       (accent-0 blue)
       (accent-1 yellow)
       (accent-2 green)
       (accent-3 red)

       (fg-completion-match-0 yellow)
       (fg-completion-match-1 green)
       (fg-completion-match-2 fg-alt)
       (fg-completion-match-3 blue)
       (bg-completion-match-0 unspecified)
       (bg-completion-match-1 unspecified)
       (bg-completion-match-2 unspecified)
       (bg-completion-match-3 unspecified)

       (date-common fg-alt)
       (date-deadline fg-alt)
       (date-event "#FF0")
       (date-holiday "#0FF")
       (date-holiday-other "#0F0")
       (date-range "#00F")
       (date-scheduled "#A0C")
       (date-now fg-main)  ;; "now" bar in agenda
       (date-weekday fg-main)  ;; agenda heading
       (date-weekend fg-dim)  ;; agenda heading

       (fg-link yellow)
       (bg-link unspecified)
       (underline-link bg-yellow-subtle)

       (fg-link-symbolic green)
       (bg-link-symbolic unspecified)
       (underline-link-symbolic bg-green-subtle)

       (fg-link-visited yellow)
       (bg-link-visited unspecified)
       (underline-link-visited bg-yellow-subtle)

       (bg-mark-delete bg-red-subtle)
       (fg-mark-delete red)
       (bg-mark-select bg-blue-subtle)
       (fg-mark-select blue)
       (bg-mark-other bg-green-subtle)
       (fg-mark-other green)

       (fg-prompt yellow)
       (bg-prompt unspecified)

       (rainbow-0 fg)
       (rainbow-1 red)
       (rainbow-2 yellow)
       (rainbow-3 green)
       (rainbow-4 blue)
       (rainbow-5 red)
       (rainbow-6 yellow)
       (rainbow-7 green)
       (rainbow-8 blue)

       (fg-heading-0 fg-alt)
       (fg-heading-1 fg-alt)
       (fg-heading-2 fg-alt)
       (fg-heading-3 fg-alt)
       (fg-heading-4 fg-alt)
       (fg-heading-5 fg-alt)
       (fg-heading-6 fg-alt)
       (fg-heading-7 fg-alt)
       (fg-heading-8 fg-alt)

       (prose-done fg-dim)

       ) ;; end of overrides for vivendi
     ) ;; end of setq


    (defun ktz--theme-modus-faces ()
      "Adjust modus-themes-* faces."
      (modus-themes-with-colors
        (set-face-attribute 'modus-themes-lang-error nil
                            :underline nil :foreground red :background bg-red-nuanced)
        (set-face-attribute 'modus-themes-lang-warning nil
                            :underline nil :foreground yellow :background bg-yellow-nuanced)
        (set-face-attribute 'modus-themes-lang-note nil
                            :underline nil :foreground fg-alt :background cyan-faint)
        ))

    (defun ktz--theme-org-faces ()
      "Adjust org related faces"
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

        ))


    (defun ktz--theme-custom-faces ()
      ;; follows modus-themes-to-toggle
      (if (eq ktz-theme-current 'light)
          (setq ktz-theme-current 'dark)
        (setq ktz-theme-current 'light))

      ;; add some space between windows
      (when (display-graphic-p)
        (setq window-divider-default-places 'bottom-only
              window-divider-default-bottom-width 15)
        (window-divider-mode)
        (modus-themes-with-colors
          (set-face-attribute 'window-divider nil :foreground bg-main)))

      ;; change cursor based on god-mode state
      (defun ktz--theme-god-hook ()
        ;; not using modus-themes-with-colors for now for performance reasons
        (if (or god-local-mode buffer-read-only)
            (progn
              (set-cursor-color "gray")
              (setq cursor-type 'hollow))
          (set-cursor-color (if (eq ktz-theme-current 'light) "black" "white"))
          (setq cursor-type 'box)))
      (add-hook 'post-command-hook #'ktz--theme-god-hook)

      ;; misc not worth their own functions
      (modus-themes-with-colors

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
        ;; headerline
        (set-face-attribute 'header-line nil
                            :background bg-dim)

        ;;; other packages
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
        (ktz-modeline-set-faces))

      ;; move to own file at some point?
      (modus-themes-with-colors
        (custom-set-faces
         `(header-line
           ((,c :underline ,border-mode-line-active
                :overline ,border-mode-line-active
                :box (:line-width 5 :color ,bg-mode-line-active)))))))


    (add-hook
     'modus-themes-after-load-theme-hook
     #'ktz--theme-custom-faces)

    (dolist (theme modus-themes-to-toggle)
      (load-theme theme :no-confirm))

    (modus-themes-toggle) ;; hooks are not called otherwise

    (define-key global-map (kbd "<f6>") #'modus-themes-toggle)
    (define-key global-map (kbd "C-<f6>") #'modus-themes-toggle)))


(defun ktz-load-theme ()
  (interactive)
  (ktz--init-theme))

(provide 'ktz-theme)
