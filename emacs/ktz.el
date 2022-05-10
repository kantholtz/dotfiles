;;; ktz.el --- Kantholtz' Emacs Configuration.

;; user interface

(defun ktz-configuration ()
    "Customize ktz"
  (interactive)
  (customize-group "ktz"))


(defgroup ktz nil
  "Kantholz' dotfiles customizations")

(defcustom ktz-init-type 'minimal
  "How minimalistic the configuration should be"
  :type '(choice
	  (const :tag "Disabled: only bootstrap straight.el" nil)
	  (const :tag "Minimum: for configuration on servers" minimal)
	  (const :tag "Programming: headless systems" programming)
	  (const :tag "Desktop: desktop configuration with Roam, LaTex etc." desktop))
  :group 'ktz)

(defcustom ktz-org-dir nil
  "Directory where the .org files reside (e.g. path/to/Roam)"
  :type 'directory
  :group 'ktz)

(defcustom ktz-mail-dir nil
  "Directory where a ktz-mu4e.el file and local mail folders are found"
  :type 'directory
  :group 'ktz)

(defcustom ktz-mail-mu4e-dir nil
  "Directory where the configure file of mu can be found"
  :type 'directory
  :group 'ktz)

(defcustom ktz-conda-dir nil
  "Directory where the conda installation can be found"
  :type 'directory
  :group 'ktz)

(defcustom ktz-conda-env "base"
  "Default conda environment"
  :type 'string
  :group 'ktz)

(defcustom ktz-conda-paths nil
  "Possible conda installation paths; Required for tramp+lsp."
  :type '(repeat directory))

(when (display-graphic-p)
  (defcustom ktz-font-size 12
    "Font size in pt"
    :type 'integer
    :group 'ktz)

  (defcustom ktz-font-monospace "Source Code Pro"
    "Name of the monospace font to use"
    :type 'string
    :group 'ktz)

  (defcustom ktz-font-proportional "Source Serif Pro"
    "Name of the proportional font to use"
    :type 'string
    :group 'ktz))


;; --------------------


(require 'ktz-config)
(require 'ktz-init-minimal)
(require 'ktz-init-programming)
(require 'ktz-init-desktop)
(require 'ktz-init-org)


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


(defun ktz-init ()
  "Initializes the environment based on the ktz-init-type"
  (message (format "[ktz] initializing (type=%s) (root-dir=%s)" ktz-init-type ktz-root-dir))

  (cond ((eq ktz-init-type 'minimal)
	 (ktz--init-minimal))

	((eq ktz-init-type 'programming)
	 (progn
	   (ktz--init-minimal)
	   (ktz--init-programming)
     (ktz--init-org)))

	((eq ktz-init-type 'desktop)
	 (progn
	   (ktz--init-minimal)
	   (ktz--init-programming)
	   (ktz--init-desktop)
     (ktz--init-org))))

  (when ktz-mail-dir
    (load (concat ktz-mail-dir "/ktz-mu4e.el")))

  t)


;; initialization
(defvar ktz-root-dir nil)
(when load-file-name
  (setq ktz-root-dir (file-name-directory load-file-name)))

(ktz--init-config)
(add-hook 'emacs-startup-hook 'ktz-init)
(provide 'ktz)
