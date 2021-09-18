;; -*- coding: utf-8 -*-

;; used for both server and desktop
(defvar ktz/packages-common
  '(helm
    magit
    conda
    lsp-mode
    dumb-jump
    markdown-mode
    multiple-cursors
    ido-vertical-mode

    yaml-mode
    fish-mode
    nginx-mode

    ;; python
    ein                ;; jupyter notebooks
    blacken            ;; autoformatting
    flycheck           ;; syntax checks and more
    anaconda-mode      ;; replaces elpy, has nothing to do with conda
    company-anaconda   ;; python completion backend for company

    ;; frontend
    vue-mode
    prettier-js
    typescript-mode
    ))

;; additional packages for desktop
(defvar ktz/packages-desktop
  '(;; general
    auctex
    pdf-tools
    visual-fill-column
    flyspell-correct-helm

    doom-themes
    doom-modeline

    org-roam
    org-bullets
    ))

;;
;; initialization routines
;;

;; pack related

(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")
        ))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; install all packages that are referenced by use-package
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; ensure all ktz/packages-*
;; it is not possible to invoke use-package on that
(dolist
    (pkg (if ktz/is-server
	  ktz/packages-common
	  (append ktz/packages-common ktz/packages-desktop)))
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; load all desired *el files
(dolist (mod '("common"))
  (load (concat ktz/mod-dir "/" mod)))

(unless ktz/is-server
  (dolist (mod '("desktop"))
    (load (concat ktz/mod-dir "/" mod))))

(add-to-list
 'custom-theme-load-path
 (concat ktz/mod-dir "/themes"))

;; manually installed packages
(add-to-list
 'load-path
 (concat ktz/mod-dir "/lib"))

;; lay your weary pinky to rest
(require 'control-lock)
(control-lock-keys)
(global-set-key (kbd "C-`") 'control-lock-enable)
