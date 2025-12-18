;;; ktz.el --- Kantholtz' Emacs Configuration.


;;; Code:
(defun ktz-log (name message)
  "Write message to message buffer.

NAME Usually the module which emitted the message.
MESSAGE String to emit."
  (message (format "[ktz] (%s): %s" name message)))


(require 'ktz-custom)


;; PACKAGES
;; install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use straight by default (allows to omit :straight t)
;; to not use straight, set :straight nil accordingly
(setq straight-use-package-by-default t)
(setq package-enable-at-startup nil)

;; important: on some systems
;; (observed on headless ubuntu with emacs28-nox)
;; the order of setting straight-use-package-by-default
;; and the requiring of the ktz-init-* files matters.

;; https://github.com/radian-software/straight.el/issues/1146
(use-package project)

;; dependencies
(use-package f)

;; load org early to avoid conflicts
;; TODO should only be loaded when 'org is a member of ktz-modules
;; however, ktz-modules is nil here? dunno
(use-package org)

;; CONFIGURATION
;; modes and mode config
(require 'ktz-init-min)
(require 'ktz-splash)


(defun ktz-init ()
  "Initializes the environment based on the chosen modules"
  (ktz-log "main" (format "initializing (root-dir=%s)" ktz-root-dir))

  ;; mode initialization
  ;; always provide minimal configuration
  (ktz--init-min)

  (setq mods '((ide      ktz-init-ide         ktz--init-ide)
               (org      ktz-init-org         ktz--init-org)
               (sci      ktz-init-sci         ktz--init-sci)
               (srv      ktz-init-srv         ktz--init-srv)
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

(add-hook 'emacs-startup-hook 'ktz-init)
(provide 'ktz)
;;; ktz.el ends here

