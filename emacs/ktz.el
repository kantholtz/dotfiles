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

(setq ktz--defcustom-choice-dir
      '(choice
          (string :tag "Directory")
          (const :tag "Disabled" nil)))

(defcustom ktz-init-type 'minimal
  "How minimalistic the configuration should be"
  :type '(choice
	  (const :tag "Disabled: only bootstrap straight.el" nil)
	  (const :tag "Minimum: for configuration on servers" minimal)
	  (const :tag "Programming: IDE features" programming))
  :group 'ktz)


;; org

(defcustom ktz-org-dir nil
  "Directory where the .org files reside (e.g. path/to/Roam)"
  :type ktz--defcustom-choice-dir
  :group 'ktz)


;; if this is set to nil (the default) you can always
;; invoke M-x ktz-org RET to manually initialize org+roam
(defcustom ktz-org-enable-headless nil
  "Whether to initialize the whole org+roam setup in the terminal"
  :type 'boolean
  :group 'ktz)


;; mail

(defcustom ktz-mail-dir nil
  "Directory where a ktz-mu4e.el file and local mail folders are found"
  :type ktz--defcustom-choice-dir
  :group 'ktz)

(defcustom ktz-mail-mu4e-dir nil
  "Directory where the configure file of mu can be found"
  :type ktz--defcustom-choice-dir
  :group 'ktz)


;; conda

(defcustom ktz-conda-dir nil
  "Directory where the conda installation can be found"
  :type ktz--defcustom-choice-dir
  :group 'ktz)

(defcustom ktz-conda-env "base"
  "Default conda environment"
  :type 'string
  :group 'ktz)

(defcustom ktz-conda-paths nil
  "Possible conda installation paths; Required for tramp+lsp."
  :type '(repeat directory))


;; bootstrapping

;;
;;  update packages with M-x straight-pull-all
;;  remove packages:
;;    - simply remove the corresponding (straight-use-package ...)
;;    - if they need to be gone completely: M-x straight-remove-unused-repos

;; intialization: bootstrapping straight.el
;; code from: https://github.com/raxod502/straight.el#getting-started
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


;; modes and mode config
(require 'ktz-config)
(require 'ktz-init-minimal)
(require 'ktz-init-programming)
(require 'ktz-init-org)
(require 'ktz-modeline)
(require 'ktz-theme)


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
  (let ((splash-buffer-name "*ktz*"))
    (with-output-to-temp-buffer splash-buffer-name
      (switch-to-buffer splash-buffer-name)
      (delete-other-windows)

      (insert-file-contents
       (concat ktz-root-dir "ktz-splash.txt"))

      t))

  )

(defun ktz-show-splash ()
  (interactive)
  (ktz-splash))


(defun ktz-init ()
  "Initializes the environment based on the ktz-init-type"
  (ktz-log "main" (format "initializing (type=%s) (root-dir=%s)" ktz-init-type ktz-root-dir))

   ;; mode initialization

  (cond ((eq ktz-init-type 'minimal)
	 (ktz--init-minimal))

	((eq ktz-init-type 'programming)
	 (progn
	   (ktz--init-minimal)
	   (ktz--init-programming)
     (ktz--init-org)
     (ktz--init-modeline)
     (ktz--init-theme))))

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
