(require 'package)
(add-to-list 'package-archives
	'("melpa" .
	  "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar nvrn-package-list
      '(magit
        company
        multiple-cursors
        edit-server

        php-mode
        fish-mode
        haskell-mode
        yaml-mode

        ;; frontend stuff
        json-mode
        flymake-json
        sass-mode
        flymake-sass

        ;; python related
        elpy
        sphinx-doc
        ;; python-mode
        ;; yasnippet
        ;; jedi

        ;; theme
        ido-vertical-mode
        doom-themes
        neotree))


(defun nvrn-install-packages (package-list)
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

(defun nvrn-prepend-to-file (fname str)
  (with-temp-buffer
    (insert-file-contents fname)
    (insert (concat str "\n"))
    (write-file fname)))

(unless (boundp 'nvrn-packages-installed)
  (progn
    (package-list-packages)
    (nvrn-install-packages nvrn-package-list)
    (nvrn-prepend-to-file
     "~/.emacs"
     "(setq nvrn-packages-installed t)")))
