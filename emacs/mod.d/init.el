;;
;;  load the emacs config modules
;;


;; configuration


(defvar ktz/packages-common   ;; used for both server and desktop
  '(helm
    multiple-cursors
    ido-vertical-mode
    magit
    dumb-jump
    conda

    fish-mode
    nginx-mode
    apache-mode))

(defvar ktz/packages-desktop   ;; additional packages for desktop
  '(;; general
    auctex
    helm
    company
    pdf-tools
    doom-themes
    doom-modeline
    markdown-mode
    flyspell-correct-helm
    ;; python
    elpy
    sphinx-doc
    flymake-python-pyflakes
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
