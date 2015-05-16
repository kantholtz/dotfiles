;;
;;    * ye glorious emacs conf *
;;    --------------------------
;;
;;    package management
;;

(require 'package)
(add-to-list 'package-archives
	'("melpa" .
	  "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq nvrn-package-list
      '(auto-complete
        fish-mode
        haskell-mode
        ido-vertical-mode
        jedi
        magit
        multiple-cursors
        python-mode
        yasnippet))

(defun nvrn-install-packages (package-list)
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

;; just eval this upon first start
;; (package-list-packages)
;; (nvrn-install-packages nvrn-package-list)


;;
;; loose config
;;

(auto-fill-mode t)
(column-number-mode t)
(show-paren-mode t)
(ido-mode t)

(setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(global-set-key (kbd "M-n") 'forward-list)
(global-set-key (kbd "M-p") 'backward-list)

;; for faster reactions
(fset 'yes-or-no-p 'y-or-n-p)

;; no annoying beeps
(setq visible-bell t)


;;
;;   package config
;;


;; linum-mode config
(setq linum-format "%4d \u2502 ")
(global-linum-mode t)


;; magit config
(global-set-key (kbd "C-x g") 'magit-status)


;; multiple cursors config
(require 'multiple-cursors)
(global-set-key (kbd "C-x n") 'mc/mark-next-like-this)


;; yasnippet config
(require 'yasnippet)
(yas-global-mode t)


;; auto complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")


;; ispell config
(setq ispell-program-name "/usr/local/bin/ispell")


;; aggregate all backups in one place
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))


;; use gfm-mode for readme files
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))


;;
;;   c programming
;;
(add-to-list 'load-path "~/.emacs.d/cc-mode/")
(require 'cc-mode)

(setq c-basic-offset 4
      c-default-style "linux")

(define-key c-mode-base-map
  (kbd "RET") 'newline-and-indent)

;;(add-hook 'find-file-hook 'flymake-find-file-hook)

(defun make-run ()
  (interactive)
  (switch-to-buffer-other-window "the executor")
  (erase-buffer)
  (start-process-shell-command
   "executor" "the executor" "make run"))

  ;; (shell-command "make run") "the executor")

(add-hook 'compilation-mode-hook 'visual-line-mode)


(global-set-key (kbd "C-x [") 'compile)
(setq compilation-scroll-output t)

(global-set-key (kbd "C-x ]") 'make-run)
(global-set-key (kbd "C-x C-]") 'gdb)

;;
;;   python programming
;;
(setq python-basic-offset 2)
(put 'dired-find-alternate-file 'disabled nil)
