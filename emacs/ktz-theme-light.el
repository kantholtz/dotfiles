;;; ktz-theme-light-el --- Overwriting faces for work during the day
;;
;;; Commentary:
;;   - is used by ktz-theme.el
;;
;;; Code:
(require 'ktz-colors)

(defun ktz--init-theme-light ()
  (setq
   modus-operandi-palette-overrides
   `(
     (bg-main   ,ktz-c-white)
     (bg-dim    ,ktz-c-neutral-50)
     (bg-active ,ktz-c-neutral-100)

     (fg-dim    ,ktz-c-neutral-400)
     (fg-main   ,ktz-c-neutral-600)
     (fg-alt    ,ktz-c-black)

     (bg-inactive bg-main)
     (border fg-dim)

     ;; using cyan to define additional neutral shades
     (cyan-faint         ,ktz-c-neutral-200)
     (cyan-cooler        ,ktz-c-neutral-300)
     (cyan-warmer        ,ktz-c-neutral-500)
     (cyan               ,ktz-c-neutral-700)
     (cyan-intense       ,ktz-c-neutral-900)

     (red                ,ktz-c-pink-700)
     (bg-red-intense     ,ktz-c-pink-200)
     (bg-red-subtle      ,ktz-c-pink-100)
     (bg-red-nuanced     ,ktz-c-pink-50)

     (green              ,ktz-c-teal-600)
     (bg-green-intense   ,ktz-c-teal-200)
     (bg-green-subtle    ,ktz-c-teal-100)
     (bg-green-nuanced   ,ktz-c-teal-50)

     (magenta            ,ktz-c-violet-800)
     (bg-magenta-intense ,ktz-c-violet-200)
     (bg-magenta-subtle  ,ktz-c-violet-100)
     (bg-magenta-nuanced ,ktz-c-violet-50)

     ;; amber
     (yellow             ,ktz-c-amber-800)
     (bg-yellow-intense  ,ktz-c-amber-200)
     (bg-yellow-subtle   ,ktz-c-amber-100)
     (bg-yellow-nuanced  ,ktz-c-amber-50)

     ;; blue grey
     (blue               ,ktz-c-slate-600)
     (bg-blue-intense    ,ktz-c-slate-200)
     (bg-blue-subtle     ,ktz-c-slate-100)
     (bg-blue-nuanced    ,ktz-c-slate-50)

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

     modeline
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
   ))

(provide 'ktz-theme-light)
;;; ktz-theme-light.el ends here
