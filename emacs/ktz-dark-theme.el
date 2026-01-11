(require 'ktz-themes)

(defconst ktz--themes-dark-fallback ktz-c-fuchsia-500
  "This is the default fallback value for unset colours")



(defun ktz--themes-dark-colour-shades (target name)
  "Create different shades of base colour NAME for TARGET.
Return palette entries for modus theme integration."
  (let ((name-prefix (format "ktz-c-%s" name))
        (target-fg-str (symbol-name target))
        (target-bg-str (format "bg-%s" target)))
    (cl-flet ((make-sym (prefix suffix)
                (intern (format "%s-%s" prefix suffix))))

      `(;; foreground colours come as:
        ;; *, faint, cooler, warmer, and intense

        (,(make-sym target-fg-str "intense")
         ,(symbol-value (make-sym name-prefix "200")))
        (,target
         ,(symbol-value (make-sym name-prefix "300")))
        (,(make-sym target-fg-str "warmer")
         ,(symbol-value (make-sym name-prefix "400")))
        (,(make-sym target-fg-str "cooler")
         ,(symbol-value (make-sym name-prefix "500")))
        (,(make-sym target-fg-str "faint")
         ,(symbol-value (make-sym name-prefix "800")))

        ;; background colours come as:
        ;; intense, subtle, and nuanced
        (,(make-sym target-bg-str "intense")
         ,(symbol-value (make-sym name-prefix "700")))
        (,(make-sym target-bg-str "subtle")
         ,(symbol-value (make-sym name-prefix "800")))
        (,(make-sym target-bg-str "nuanced")
         ,(symbol-value (make-sym name-prefix "900")))

        ))))

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


(defvar ktz-dark-palette
  `(;; following modus-themes-list-colors

    ;; base colours
    (bg-main       ,ktz-c-zinc-900)
    (bg-dim        ,ktz-c-zinc-800)
    (bg-active     ,ktz-c-zinc-700)
    (bg-inactive   bg-dim)

    (fg-main   ,ktz-c-zinc-300)
    (fg-dim    ,ktz-c-zinc-500)
    (fg-alt    ,ktz-c-zinc-50)

    (border    bg-dim)
    (cursor    fg-alt)

    ;; highlighting colors
    ,@(ktz--themes-dark-colour-shades 'red     'rose)    ; warm
    ,@(ktz--themes-dark-colour-shades 'yellow  'amber)   ; warm
    ,@(ktz--themes-dark-colour-shades 'green   'emerald) ; cold
    ,@(ktz--themes-dark-colour-shades 'blue    'cyan)    ; cold
    ,@(ktz--themes-dark-colour-shades 'magenta 'teal)    ; cold
    ,@(ktz--themes-dark-colour-shades 'cyan    'zinc)    ; neutral

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

    (bg-completion       bg-magenta-nuanced)
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

    (bg-mode-line-active   bg-active)
    (fg-mode-line-active   fg-alt)
    (bg-mode-line-inactive bg-dim)
    (fg-mode-line-inactive fg-dim)

    ;; applies to org-mode
    (prose-done comment)

    ;; eight mappings for enumerated colours
    ;; (for some reason they are not inherited)
    ,@(ktz--themes-rainbow 'rainbow              ktz--themes-dark-rainbow)
    ,@(ktz--themes-rainbow 'fg-heading           ktz--themes-dark-rainbow)
    ,@(ktz--themes-rainbow 'modus-themes-heading ktz--themes-dark-rainbow)

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
 nil)
