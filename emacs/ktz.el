;;; ktz.el --- Kantholtz' Emacs Configuration.


;;; Code:
(defun ktz-log (name message)
  "Write message to message buffer.

NAME Usually the module which emitted the message.
MESSAGE String to emit."
  (message (format "[ktz] (%s): %s" name message)))


;; user interface

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


;; mail

(defcustom ktz-mail-dir nil
  "Directory where a ktz-mu4e.el file and local mail folders are found"
  :type ktz--defcustom-choice
  :group 'ktz)

(defcustom ktz-mail-mu4e-dir nil
  "Directory where the configure file of mu can be found"
  :type ktz--defcustom-choice
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


;; install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(put 'dired-find-alternate-file 'disabled nil)

;; use straight by default (allows to omit :straight t)
;; to not use straight, set :straight nil accordingly
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;; important: on some systems
;; (observed on headless ubuntu with emacs28-nox)
;; the order of setting straight-use-package-by-default
;; and the requiring of the ktz-init-* files matters.


;; https://github.com/radian-software/straight.el/issues/1146
(straight-use-package 'project)


;; modes and mode config
(require 'ktz-config)
(require 'ktz-init-minimal)

;; append
;; (goto-char (point-max))
;; save current point
;; (save-excursion ...)
;; get things at point:
;; (thing-at-point SYM) where
;;   SYM in { 'word, 'sentence, 'sexp, 'url, ... }
;; (thing-at-point SYM t) returns only the raw text
;; (search-forward ...) (search-backward ...)

(defun ktz-splash ()
  (when (= 1 (length command-line-args))

    (let ((splash-buffer-name "*ktz*"))
      (with-output-to-temp-buffer splash-buffer-name
        ;; select and style
        (switch-to-buffer splash-buffer-name)
        (delete-other-windows)

        (set-window-margins nil 10)
        (newline 4)
        (insert-file-contents
         (concat ktz-root-dir "ktz-splash.md"))

        ;; style and interaction
        (markdown-mode)
        ;; doesn't work atm; maybe because god-mode
        ;; hooks overwrite the cursor-type
        (setq cursor-type nil)
        (setq buffer-read-only t)))
    ))


(defun ktz-show-splash ()
  (interactive)
  (ktz-splash))


(defun ktz-init ()
  "Initializes the environment based on the chosen modules"
  (ktz-log "main" (format "initializing (root-dir=%s)" ktz-root-dir))

  ;; mode initialization
  ;; always provide minimal configuration
  (ktz--init-minimal)

  (setq mods '((ide      ktz-init-ide         ktz--init-ide)
               (org      ktz-init-org         ktz--init-org)
               (tex      ktz-init-tex         ktz--init-tex)
               (lexi     ktz-init-lexi        ktz--init-lexi)
               (server   ktz-init-server      ktz--init-server)
               (modeline ktz-modeline         ktz--init-modeline)
               (theme    ktz-theme            ktz--init-theme)))

  (ktz-log "main" (format "selected modules: %s" ktz-modules))
  (dolist (moddef mods)
    (let ((mod (nth 0 moddef))
          (req (nth 1 moddef))
          (fun (nth 2 moddef)))

      (when (member mod ktz-modules)
        (ktz-log "main" (format "initializing mod=%s req=%s fun=%s" mod req fun))
        (require req)
        (funcall fun))))

  (when ktz-mail-dir
    (load (concat ktz-mail-dir "/ktz-mu4e.el")))

  (ktz-log "main" (format "initialization time: %s" (emacs-init-time)))

  ;; splash screen
  (ktz-splash)

  ) ;; /ktz-init


(defun ktz-reload ()
  (interactive)
  (ktz-init))


;; initialization
(defvar ktz-root-dir nil)
(when load-file-name
  (setq ktz-root-dir (file-name-directory load-file-name)))

(ktz--init-config)
(add-hook 'emacs-startup-hook 'ktz-init)
(provide 'ktz)
;;; ktz.el ends here

