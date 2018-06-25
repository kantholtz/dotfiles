;;
;;  load the emacs config modules
;;


;; configuration

(defvar nvrn-packages-common   ;; used for both server and desktop
  '(helm
    magit
    nlinum
    fish-mode
    multiple-cursors
    ido-vertical-mode
    apache-mode))

(defvar nvrn-packages-desktop   ;; additional packages for desktop
  '(company

    ;; python
    elpy
    sphinx-doc
    flymake-python-pyflakes

    neotree))

;;
;; initialization routines
;;

;; pack related

(require 'package)
(add-to-list 'package-archives
	     '("melpa" .
	       "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defun nvrn-install-packages (package-list)
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

(defun nvrn-prepend-to-file (fname str)
  (with-temp-buffer
    (insert-file-contents fname)
    (insert (concat str "\n"))
    (write-file fname)))

;; mod.d related

(defun nvrn-load-config (mod-dir mod-list)
  "load all specified *el files from the specified directory"
  (dolist (mod mod-list)
    (load (concat mod-dir "/" (symbol-name mod)))))


(defun nvrn-init (is-server)
  "install and load packages, load configuration"
  (let (mod-dir mods-desktop mods-server)
    (setq mod-dir "~/.emacs.d/mod.d")

    ;; configure which mods to load here
    (setq mods-desktop '(desktop c py))
    (setq mods-server '())

    (unless (boundp 'nvrn-packages-installed)
      (progn
	(package-list-packages)
	(nvrn-install-packages nvrn-packages-common)
	(if (not is-server) (nvrn-install-packages nvrn-packages-desktop))
	(nvrn-prepend-to-file
	 "~/.emacs"
	 "(setq nvrn-packages-installed t)")))

    (nvrn-load-config mod-dir '(common))
    (if is-server
	(nvrn-load-config mod-dir mods-server)
      (nvrn-load-config mod-dir mods-desktop))))
