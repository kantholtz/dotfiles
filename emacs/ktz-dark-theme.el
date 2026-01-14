(require 'ktz-themes)



(defconst ktz--themes-dark-fallback ktz-c-fuchsia-500
  "This is the default fallback value for unset colours")


(defun ktz--themes-dark-colour-shades (target name)
  "Create different shades of base colour NAME for TARGET.
Return palette entries for modus theme integration."
  (let ((name-prefix (format "ktz-c-%s" name))
        (target-fg-str (symbol-name target))
        (target-bg-str (format "bg-%s" target)))
    (cl-flet ((s #'ktz--themes-make-symbol)
              (v #'ktz--themes-make-colour))

      `(;; foreground colours come as:
        ;; *, faint, cooler, warmer, and intense

        (,(s target-fg-str "intense")  ,(v name-prefix "200"))
        (,target                       ,(v name-prefix "300"))
        (,(s target-fg-str "warmer")   ,(v name-prefix "400"))
        (,(s target-fg-str "cooler")   ,(v name-prefix "500"))
        (,(s target-fg-str "faint")    ,(v name-prefix "800"))

        ;; background colours come as:
        ;; intense, subtle, and nuanced
        (,(s target-bg-str "intense")  ,(v name-prefix "800"))
        (,(s target-bg-str "subtle")   ,(v name-prefix "900"))
        (,(s target-bg-str "nuanced")  ,(v name-prefix "950"))

        ))))

;; (ktz--themes-dark-colour-shades 'rose 'red)

(defun ktz--themes-dark-colour-diff (face name)
  "Create different diff related shades for FACE using colour NAME"
  (let ((face-str (symbol-name face))
        (name-str (symbol-name name)))
    (cl-flet ((s #'ktz--themes-make-symbol))

      `((,(s "bg" face-str)           ,(s "bg" name-str "subtle"))
        (,(s "bg" face-str "faint")   ,(s "bg" name-str "nuanced"))
        (,(s "bg" face-str "refine")  ,(s "bg" name-str "intense"))
        (,(s "bg" face-str "fringe")  ,(s "bg" name-str "intense"))
        (,(s "fg" face-str)           ,(s name-str))
        (,(s "fg" face-str "intense") ,(s name-str "intense"))

        ))))

;;(ktz--themes-dark-colour-diff 'added 'green)

(defconst ktz--themes-dark-rainbow
  '(fg-alt           ; 0
    yellow-intense   ; 1
    green-intense    ; 2
    blue-intense     ; 3
    yellow-intense   ; 4
    green-intense    ; 5
    blue-intense     ; 6
    yellow-intense   ; 7
    green-intense))  ; 8


;;; MODUS SPECIFIC DEFINITIONS

(defconst ktz-dark-theme-custom-faces
  '(
    `(modus-themes-completion-selected
      ((,c :foreground ,fg-alt :background ,bg-completion)))
    `(pulsar-green
      ((,c :foreground ,magenta :background ,bg-magenta-subtle)))))


(defvar ktz-dark-palette
  `(; following modus-themes-list-colors

    ;; base colours

    (bg-main       "#141417")
    (bg-dim        "#1a1a1f")
    (bg-active     "#2a2d34")

    (bg-inactive   bg-dim)

    (fg-main   ,ktz-c-neutral-400)
    (fg-dim    ,ktz-c-neutral-600)
    (fg-alt    ,ktz-c-neutral-50)

    (border    bg-active)
    (cursor    fg-alt)

    ;; highlighting colors
    ;; concept:
    ;;   - red, yellow, green (used semantically to render errors,
    ;;     warnings, success or danger, risk, no danger, etc.
    ;;   - magenta, blue, cyan: primary, secondary, tertiary (the
    ;;     defining theme colours)
    ,@(ktz--themes-dark-colour-shades 'red     'rose)
    ,@(ktz--themes-dark-colour-shades 'yellow  'gold)
    ,@(ktz--themes-dark-colour-shades 'green   'emerald)
    ,@(ktz--themes-dark-colour-shades 'magenta 'teal)
    ,@(ktz--themes-dark-colour-shades 'blue    'gloam)

    ,@(ktz--themes-dark-colour-shades 'cyan    'neutral)    ; neutral

    ;; custom mappings

    (bg-prominent-err bg-red-subtle)
    (fg-prominent-err red)
    (bg-prominent-warning bg-yellow-subtle)
    (fg-prominent-warning yellow)
    (bg-prominent-note bg-magenta-subtle)
    (fg-prominent-note magenta)

    (err red)
    (warning yellow)
    (info magenta)

    (bg-hover            bg-green-nuanced)
    (bg-hover-secondary  bg-yellow-nuanced)
    (bg-hl-line          bg-active)
    (bg-region           bg-active)
    (fg-region           fg-alt)

    (bg-search-current  bg-magenta-nuanced)
    (fg-search-current  magenta)
    (bg-search-lazy     bg-yellow-nuanced)
    (fg-search-lazy     yellow)
    (bg-search-static   bg-green-nuanced)
    (fg-search-static   green)
    (bg-search-replace  bg-red-nuanced)
    (fg-search-replace  red)

    ;; code

    (builtin fg-alt)
    (fnname  fg-alt)
    (type    fg-alt)

    (keyword  magenta)
    (constant magenta)
    (variable magenta)

    (string green)

    (comment   fg-dim)
    (docstring fg-dim)
    (docmarkup fg-dim)

    ;; editor ui elements

    (fg-link           yellow-intense)
    (fg-link-symbolic  yellow-intense)
    (fg-link-visited   yellow-intense)

    (bg-mode-line-active   bg-active)
    (fg-mode-line-active   yellow-intense)

    (bg-mode-line-inactive bg-inactive)
    (fg-mode-line-inactive fg-dim)

    (bg-completion bg-active)
    (fg-completion-match-0 magenta)
    (fg-completion-match-1 blue)
    (fg-completion-match-2 yellow)
    (fg-completion-match-3 cyan)

    ;; magit diffs
    ,@(ktz--themes-dark-colour-diff 'added 'green)
    ,@(ktz--themes-dark-colour-diff 'changed 'yellow)
    ,@(ktz--themes-dark-colour-diff 'removed 'red)

    ;; applies to org-mode
    (prose-done comment)

    ;; eight mappings for enumerated colours
    ;; (for some reason they are not inherited)
    ,@(ktz--themes-rainbow 'rainbow              ktz--themes-dark-rainbow)
    ,@(ktz--themes-rainbow 'fg-heading           ktz--themes-dark-rainbow)
    ,@(ktz--themes-rainbow 'modus-themes-heading ktz--themes-dark-rainbow)

    ;; stylistic variants
    (underline-link unspecified)
    (underline-link-visited unspecified)
    (underline-link-symbolic unspecified)

    ;;; DISABLED COLOURS

    ,@(ktz--themes-unset-colours
       ktz--themes-dark-fallback
       '(rust gold olive slate indigo maroon pink
              bg-clay bg-ochre bg-lavender bg-sage
              fg-clay fg-ochre fg-lavender fg-sage
              bg-graph-red-0 bg-graph-red-1
              bg-graph-green-0 bg-graph-green-1
              bg-graph-yellow-0 bg-graph-yellow-1
              bg-graph-blue-0 bg-graph-blue-1
              bg-graph-magenta-0 bg-graph-magenta-1
              bg-graph-cyan-0 bg-graph-cyan-1))

    )
  "Kantholtz' Dark Colour Palette")


;; for quickly reloading the theme after changes
;; (global-set-key
;;  (kbd "<f6>")
;;  (lambda () (interactive) (modus-themes-select 'ktz-dark)))
;;(modus-themes-select 'ktz-dark)

;; maybe I don't need this, since I configure the palette
;; myself anyway and nobody else is using these themes
;; (defcustom ktz-dark-palette-overrides nil
;;   "Overrides for the `ktz-dark' theme."
;;   :type '(repeat (list symbol (choice symbol string)))
;;   :link '(info-link "(modus-themes) Palette overrides"))


(modus-themes-theme
 'ktz-dark
 'ktz-themes
 "Kantholtz' Dark Theme"
 'dark
 'modus-vivendi-deuteranopia-palette
 'ktz-dark-palette
 nil
 'ktz-dark-theme-custom-faces)
