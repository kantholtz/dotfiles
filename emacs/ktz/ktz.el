;;; ktz.el --- Kantholtz' Emacs Configuration.

;;
;;  update packages with M-x straight-pull-all
;;  remove packages:
;;    - simply remove the corresponding (straight-use-package ...)
;;    - if they need to be gone completely: M-x straight-remove-unused-repos


(require 'ktz-config)
(require 'ktz-init-minimal)
(require 'ktz-init-programming)
(require 'ktz-init-desktop)


;; customization

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
	  (const :tag "desktop: desktop configuration with Roam, LaTex etc." desktop))
  :group 'ktz)


;; bootstrapping


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
  (message (format "KTZ: initializing KTZ (%s)" ktz-init-type))

  (cond ((eq ktz-init-type 'minimal)
	 (ktz--init-minimal))

	((eq ktz-init-type 'programming)
	 (progn
	   (ktz--init-minimal)
	   (ktz--init-programming)))

	((eq ktz-init-type 'desktop)
	 (progn
	   (ktz--init-minimal)
	   (ktz--init-programming)
	   (ktz--init-desktop))))

  (message "KTZ: initialization finished"))


;; initialization
(add-hook 'emacs-startup-hook 'ktz-init)


(provide 'ktz)
