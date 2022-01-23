;; always only ask for y or n
(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell nil)

;; No startup screens or messages
;; No message in scratch buffer
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

(setq calendar-week-start-day 1)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq explicit-shell-file-name "/bin/bash")
(setq tramp-default-method "sshx")

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default truncate-lines t)


;; taken from nano

;; No frame title
(setq frame-title-format nil)

;; No file dialog
(setq use-file-dialog nil)

;; No dialog box
(setq use-dialog-box nil)

;; No popup windows
(setq pop-up-windows nil)

;; No empty line indicators
(setq indicate-empty-lines nil)

;; No cursor in inactive windows
(setq cursor-in-non-selected-windows nil)

;; Moderate font lock
(setq font-lock-maximum-decoration nil)

;; No limit on font lock
(setq font-lock-maximum-size nil)

;; No line break space points
(setq auto-fill-mode nil)

;; Fill column at 80
(setq fill-column 80)

;; Navigate windows using shift+direction
(windmove-default-keybindings)

;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)


;; ---


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
