(require 'package)
(add-to-list 'package-archives
	'("melpa" .
	  "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq nvrn-package-list
      '(magit
        auto-complete
        multiple-cursors
        ido-vertical-mode
        edit-server

        ;; programming languages
        php-mode
        fish-mode
        haskell-mode

        ;; python related
        python-mode
        yasnippet
        sphinx-doc
        jedi))

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
