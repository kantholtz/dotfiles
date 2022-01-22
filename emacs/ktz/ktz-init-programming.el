;;; ktz-init-programming.el --- Programming initialization.

(defvar ktz--pkgs-programming
  '(

    dumb-jump

    ;; python
    ein
    conda
    blacken
    flycheck
    anaconda-mode
    company-anaconda

    ;; frontend
    vue-mode
    prettier-js
    typescript-mode))


(defun ktz--init-programming-python ()
  ;; - elpy and anaconda-mode do the same thing
  ;; - anaconda-mode seems easier and more lighweight
  ;; - flycheck is more powerful than the built-in flymake

  (require 'python)
  (require 'company)
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda))
  (setq anaconda-mode-localhost-address "localhost")

  (dolist (mode
	   '(anaconda-mode
	     anaconda-eldoc-mode
	     blacken-mode
	     flycheck-mode))
    (add-hook 'python-mode-hook mode))

  t)


(defun ktz--init-programming-frontend ()
  ;; VUE
  ;; https://azzamsa.com/n/vue-emacs/

  ;; (require 'lsp-mode)
  ;; (require 'prettier)
  (add-hook 'after-init-hook #'global-prettier-mode)

  (setq js-indent-level 2)
  (setq typescript-indent-level 2)

  ;; (add-hook 'vue-mode-hook #'lsp)
  (dolist
      (hook
       '(js-mode-hook
	 web-mode-hook
	 typescript-mode-hook
	 vue-mode-hook))
    (add-hook hook 'prettier-js-mode))

  t)


(defun ktz--init-programming ()
  "Setup programming configuration"
  (dolist (pkg ktz--pkgs-programming)
    (straight-use-package pkg))

  ;; dumb-jump
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)

  (ktz--init-programming-python)
  (ktz--init-programming-frontend)

  t)


(provide 'ktz-init-programming)
