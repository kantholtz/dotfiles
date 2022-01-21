;;; ktz-init-minimal.el --- Minimal initialization.


(defvar ktz--pkgs-minimal
  '(
    helm
    magit
    yasnippet
    multiple-cursors

    ;; server
    yaml-mode
    fish-mode
    nginx-mode
    apache-mode
    markdown-mode))


(defun ktz--init-minimal ()
  "Setup minimal configuration"
  (dolist (pkg ktz--pkgs-minimal)
    (straight-use-package pkg))
  (message "KTZ: initialized [minimal]"))


(provide 'ktz-init-minimal)

