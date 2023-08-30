;; customize variables

;; disables the extremely annoying *Warnings* buffer from
;; focusing on every native compilation warning
;; (customize-set-variable 'warning-minimum-level :error)


;; always only ask for y or n
(fset 'yes-or-no-p 'y-or-n-p)
(customize-set-variable 'visible-bell nil)

;; No startup screens or messages
;; No message in scratch buffer
(customize-set-variable 'inhibit-splash-screen t)
(customize-set-variable 'inhibit-startup-screen t)
(customize-set-variable 'inhibit-startup-message t)
(customize-set-variable 'inhibit-startup-echo-area-message t)
(customize-set-variable 'initial-scratch-message nil)

(setq calendar-week-start-day 1
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      explicit-shell-file-name "/bin/bash"
      tramp-default-method "sshx")

(setq-default indent-tabs-mode nil
              tab-width 2
              truncate-lines t)

;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)


;; ---

(defun ktz--init-config ()
  (add-to-list
   'load-path
   (concat ktz-root-dir "/lib")))

(provide 'ktz-config)
