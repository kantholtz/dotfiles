(defun ktz-configuration ()
  "Customize ktz"
  (interactive)
  (customize-group "ktz"))


(defgroup ktz nil
  "Kantholz' dotfiles customizations")

(setq ktz--defcustom-choice-dir
      '(choice (const :tag "None" nil)
               (directory :tag "Directory")))


(defcustom ktz-modules '()
  "Select what modules to load."
  :type '(set (const :tag "IDE: programming support" ide)
              (const :tag "Org: org, roam and agenda" org)
              (const :tag "LLM: llm integration" llm)
              (const :tag "Sci: latex, bibtex, and pdf support" sci)
              (const :tag "Server: infrastructure modes" srv)
              (const :tag "Modeline: clean and shiny" modeline))
  :group 'ktz)

(defcustom ktz-god-default nil
  "Whether to start god-mode globally by default"
  :type 'boolean
  :group 'ktz)

(defcustom ktz-theme-default nil
  "The ktz theme loaded by default"
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Dark" dark)
                 (const :tag "Light" light))
  :group 'ktz)

(defcustom ktz-org-dir nil
  "Directory containing 'org/*org' (e.g. psi:Roam/)"
  :type ktz--defcustom-choice-dir
  :group 'ktz)

(defcustom ktz-prompt-dir nil
  "Directory containing LLM 'prompts/*' (e.g. psi:Roam/)"
  :type ktz--defcustom-choice-dir
  :group 'ktz)

(provide 'ktz-custom)
