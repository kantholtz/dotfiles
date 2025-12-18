;;; ktz-theme-dark-el --- Overwriting faces for work during the night
;;
;;; Commentary:
;;   - is used by ktz-theme.el
;;
;;; Code:
(require 'ktz-colors)

(defun ktz--init-theme-dark ()
  (setq
   modus-vivendi-palette-overrides
   `(
     ;; zinc
     (bg-main   ,ktz-c-zinc-900)
     (bg-dim    ,ktz-c-zinc-800)
     (bg-active ,ktz-c-zinc-700)

     (fg-dim   ,ktz-c-zinc-600)
     (fg-main  ,ktz-c-zinc-400)
     (fg-alt   ,ktz-c-white)

     (bg-inactive bg-main)
     (border fg-dim)

     ;; using cyan to define additional neutral shades
     (cyan-faint         ,ktz-c-zinc-950)
     (cyan-cooler        ,ktz-c-zinc-500)
     (cyan-warmer        ,ktz-c-zinc-300)
     (cyan               ,ktz-c-zinc-200)
     (cyan-intense       ,ktz-c-zinc-100)

     (red                ,ktz-c-rose-600)
     (bg-red-intense     ,ktz-c-rose-800)
     (bg-red-subtle      ,ktz-c-rose-950)
     (bg-red-nuanced     "#2e0515")  ;; custom

     (green              ,ktz-c-cyan-400)
     (bg-green-intense   ,ktz-c-cyan-800)
     (bg-green-subtle    ,ktz-c-cyan-900)
     (bg-green-nuanced   ,ktz-c-cyan-950)

     (magenta             ,ktz-c-zinc-200)
     (bg-magenta-intense  ,ktz-c-zinc-800)
     (bg-magenta-subtle   ,ktz-c-zinc-900)
     (bg-magenta-nuanced  ,ktz-c-zinc-950)

     (yellow             ,ktz-c-emerald-200)
     (bg-yellow-intense  ,ktz-c-emerald-800)
     (bg-yellow-subtle   ,ktz-c-emerald-900)
     (bg-yellow-nuanced  ,ktz-c-emerald-950)

     (blue               ,ktz-c-slate-200)
     (bg-blue-intense    ,ktz-c-slate-800)
     (bg-blue-subtle     ,ktz-c-slate-900)
     (bg-blue-nuanced    ,ktz-c-slate-950)

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

     (rainbow-0 fg-alt)
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
   ))

(provide 'ktz-theme-dark)
;;; ktz-theme-dark.el ends here
