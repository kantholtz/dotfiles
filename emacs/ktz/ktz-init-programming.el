;;; ktz-init-programming.el --- Programming initialization.

(defvar ktz--pkgs-programming
  '(

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


(defun ktz--init-programming ()
  "Setup programming configuration"
  (dolist (pkg ktz--pkgs-programming)
    (straight-use-package pkg))
  (message "KTZ: initialized [programming]"))


(provide 'ktz-init-programming)
