(defun ktz-configuration ()
  "Customize ktz"
  (interactive)
  (customize-group "ktz"))


(defgroup ktz nil
  "Kantholz' dotfiles customizations")

(setq ktz--defcustom-choice
      '(choice
        (const :tag "Disabled" nil)
        (string :tag "Directory or File")))


(defcustom ktz-modules '()
  "Select what modules to load."
  :type '(set (const :tag "IDE: programming support" ide)
              (const :tag "Org: org, roam and agenda" org)
              (const :tag "Sci: latex, bibtex, and pdf support" sci)
              (const :tag "Server: infrastructure modes" srv)
              (const :tag "Modeline: clean and shiny" modeline)
              (const :tag "Theme: both dark and light" theme))
  :group 'ktz)

(defcustom ktz-god-default nil
  "Whether to start god-mode globally by default"
  :type 'boolean
  :group 'ktz)

(defcustom ktz-theme-default nil
  "The ktz theme loaded by default"
  :type '(choice (const :tag "Dark" t)
                 (const :tag "Light" nil))
  :group 'ktz)

;; (defcustom ktz-openai-api-key nil
;;   "OpenAI API access"
;;   :type '(choice string (const nil))
;;   :group 'ktz)

;; (defcustom ktz-openrouter-api-key nil
;;   "OpenRouter API access"
;;   :type '(choice string (const nil))
;;   :group 'ktz)

(defcustom ktz-org-dir nil
  "Directory containing org/*org (e.g. psi:Roam)"
  :type ktz--defcustom-choice
  :group 'ktz)

(provide 'ktz-custom)
