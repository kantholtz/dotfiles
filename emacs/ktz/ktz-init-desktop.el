;;; ktz-init-desktop.el --- Desktop initialization.


(defvar ktz--pkgs-desktop
  '(
    auctex
    pdf-tools
    visual-fill-column
    flyspell-correct-helm

    doom-themes
    doom-modeline

    org-roam
    org-bullets
    org-super-agenda))


(defun ktz--init-desktop ()
  "Setup desktop configuration - includes Roam, LaTex etc."
  (dolist (pkg ktz--pkgs-desktop)
    (straight-use-package pkg))
  (message "KTZ: initialized [desktop]"))

(provide 'ktz-init-desktop)
