(defun ktz-configuration ()
  "Customize ktz"
  (interactive)
  (customize-group "ktz"))


;; TODO when this gets too large, consider subgroups


(defgroup ktz nil
  "Kantholz' dotfiles customizations")

(setq ktz--defcustom-choice
      '(choice
        (string :tag "Directory or File")
        (const :tag "Disabled" nil)))


(defcustom ktz-modules '()
  "Select what modules to load."
  :type '(set (const :tag "IDE: programming support" ide)
              (const :tag "Org: org, roam and agenda" org)
              (const :tag "Sci: latex, bibtex, and pdf support" sci)
              (const :tag "Lex: textual assistance" lex)
              (const :tag "Server: infrastructure modes" srv)
              (const :tag "Modeline: clean and shiny" modeline)
              (const :tag "Theme: both dark and light" theme))
  :group 'ktz)

(defcustom ktz-god-default nil
  "Whether to start god-mode globally by default"
  :type 'boolean
  :group 'ktz)


;; ORG

(defcustom ktz-org-dir nil
  "Directory where the .org files reside (e.g. path/to/Roam)"
  :type ktz--defcustom-choice
  :group 'ktz)


;; LEX

;; follow configuration in https://github.com/joshcho/ChatGPT.el
(defcustom ktz-lex-openai-api-key nil
  "OpenAI API access"
  :type '(choice string (const nil))
  :group 'ktz)


(defcustom ktz-lex-ltex-ls-path nil
  "Path to ltex-ls"
  :type '(choice string (const nil))
  :group 'ktz)


(provide 'ktz-custom)
