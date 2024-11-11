(defun ktz-configuration ()
  "Customize ktz"
  (interactive)
  (customize-group "ktz"))


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
              (const :tag "Tex: latex, bibtex, and pdf support" tex)
              (const :tag "Lexi: textual assistance" lexi)
              (const :tag "Server: infrastructure modes" server)
              (const :tag "Modeline: clean and shiny" modeline)
              (const :tag "Theme: both dark and light" theme))
  :group 'ktz)

(defcustom ktz-god-default nil
  "Whether to start god-mode globally by default"
  :type 'boolean
  :group 'ktz)

;; org

(defcustom ktz-org-dir nil
  "Directory where the .org files reside (e.g. path/to/Roam)"
  :type ktz--defcustom-choice
  :group 'ktz)


;; if this is set to nil (the default) you can always
;; invoke M-x ktz-org RET to manually initialize org+roam
(defcustom ktz-org-enable-headless nil
  "Whether to initialize the whole org+roam setup in the terminal"
  :type 'boolean
  :group 'ktz)


(defcustom ktz-languagetool-host nil
  "Languagetool server host"
  :type '(choice string (const nil))
  :group 'ktz)

(defcustom ktz-languagetool-port nil
  "Languagetool server port"
  :type '(choice integer (const nil))
  :group 'ktz)


;; follow configuration in https://github.com/joshcho/ChatGPT.el
(defcustom ktz-openai-api-key nil
  "OpenAI API access"
  :type '(choice string (const nil))
  :group 'ktz)


;; conda

(defcustom ktz-conda-dir nil
  "Directory where the conda installation can be found"
  :type ktz--defcustom-choice
  :group 'ktz)

(defcustom ktz-conda-env "base"
  "Default conda environment"
  :type 'string
  :group 'ktz)

(defcustom ktz-conda-paths nil
  "Possible conda installation paths; Required for tramp+lsp."
  :type '(repeat directory))


(provide 'ktz-custom)
