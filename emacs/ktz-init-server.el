(defun ktz--init-server ()
  (use-package yaml-mode)
  (use-package fish-mode)
  (use-package nginx-mode)
  (use-package apache-mode)
  (use-package markdown-toc)
  (use-package dockerfile-mode)

  ;; thanks https://github.com/daviwil
  ;; https://codeberg.org/daviwil/dotfiles/src/branch/master/emacs/init.el#L107
  (defun ktz--server-clear-bg (&optional frame)
    (or frame (setq frame (selected-frame)))
    "unsets the background color in terminal mode"
    (unless (display-graphic-p frame)
      (set-face-background 'default "unspecified-bg" frame)))

  ;; TODO load wombat as dark theme
  ;; TODO determine good light theme
  (unless (display-graphic-p)
    (add-hook 'after-make-frame-functions 'ktz--server-clear-bg)
    (add-hook 'window-setup-hook 'ktz--server-clear-bg)
    (add-hook 'ef-themes-post-load-hook 'ktz--server-clear-bg))

  (set-display-table-slot
   standard-display-table 'vertical-border (make-glyph-code ?│)))



(defun ktz-init-server ()
  "Initialize org related config manually"
  (interactive)
  (ktz--init-server))

(provide 'ktz-init-server)
