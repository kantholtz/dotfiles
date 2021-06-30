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
    ein
    blacken
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
    visual-fill-column
    pdf-tools
    doom-themes
    doom-modeline

    flyspell-correct-helm
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
(dolist (mod '("common" "defun"))
  (load (concat ktz/mod-dir "/" mod)))
(unless ktz/is-server (load (concat ktz/mod-dir "/desktop")))
