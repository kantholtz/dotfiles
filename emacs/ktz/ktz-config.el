;; always only ask for y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; If a variable is buffer-local, then setq sets its local value in
;; the current buffer and setq-default sets the global default
;; value.

(setq visible-bell nil)
(setq inhibit-splash-screen t)
(setq calendar-week-start-day 1)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq explicit-shell-file-name "/bin/bash")
(setq tramp-default-method "sshx")

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default truncate-lines t)

(setq magit-last-seen-setup-instructions "1.4.0")


(defun ktz--init-config ()

  (add-to-list
   'load-path
   (concat ktz-root-dir "/lib"))

  (when (boundp 'custom-theme-load-path)
    (add-to-list
     'custom-theme-load-path
     (concat ktz-root-dir "/themes")))
  t)


(provide 'ktz-config)


;; legacy:

;; this bug occurs when using use-package... maybe it won't with straight.el
;; nasty bug plagues my debian experience (stock emacs currently is 26.x) :(
;; (if (version< emacs-version "27.0")
;;     (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
